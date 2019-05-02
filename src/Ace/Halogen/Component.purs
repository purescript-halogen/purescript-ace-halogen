module Ace.Halogen.Component
  ( aceComponent
  , AceQuery(..)
  , AceMessage(..)
  , Autocomplete(..)
  , CompleteFn
  ) where

import Prelude

import Ace as Ace
import Ace.Editor as Editor
import Ace.EditSession as Session
import Ace.Ext.LanguageTools as LanguageTools
import Ace.Ext.LanguageTools.Completer as Completer
import Ace.Types (Editor, Completion, Position, EditSession)
import Data.DateTime.Instant (unInstant)
import Data.Either (either)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Effect (Effect)
import Effect.Aff (Aff, runAff)
import Effect.Aff.Class (class MonadAff)
import Effect.Now (now)
import Effect.Random (random)
import Effect.Ref (Ref, read, write, modify)
import Foreign.Object (Object)
import Foreign.Object as FO
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Web.HTML (HTMLElement)

-- | Effectful knot of autocomplete functions. It's needed because
-- | `languageTools.addCompleter` is global and adds completer to
-- | all editors
foreign import completeFns ∷ Ref (Object CompleteFn)

-- | This flag is used to determine if `languageTools` initialized
foreign import initialized ∷ Ref Boolean

-- | Global key of currently focused component. Used only to take
-- | autocomplete function
foreign import focused ∷ Ref String

-- | Get `dataset` property of element
foreign import dataset
  ∷ HTMLElement
  → Effect (Object String)

-- | Take completion function for currently selected component
completeFnFocused ∷ Effect CompleteFn
completeFnFocused = do
  focusedKey ← read focused
  mFns ← read completeFns
  maybe (pure emptyCompleteFn) pure $ FO.lookup focusedKey mFns
  where
    emptyCompleteFn ∷ CompleteFn
    emptyCompleteFn _ _ _ _ = pure []

-- | Set autocomplete resume
setAutocompleteResume
  ∷ Maybe Autocomplete → Editor → Effect Unit
setAutocompleteResume Nothing editor = do
  Editor.setEnableBasicAutocompletion false editor
  Editor.setEnableLiveAutocompletion false editor
setAutocompleteResume (Just Basic) editor = do
  Editor.setEnableLiveAutocompletion false editor
  Editor.setEnableBasicAutocompletion true editor
setAutocompleteResume (Just Live) editor = do
  Editor.setEnableLiveAutocompletion true editor
  Editor.setEnableBasicAutocompletion true editor

-- | Language tools and autocomplete initializer. Runs once.
enableAutocomplete ∷ Effect Unit
enableAutocomplete = do
  languageToolsInitialized ← read initialized
  when (not languageToolsInitialized) do
    completer ← Completer.mkCompleter globalCompleteFn
    tools ← LanguageTools.languageTools
    LanguageTools.addCompleter completer tools
    write true initialized
  where
  globalCompleteFn editor session position prefix cb = do
    fn ← completeFnFocused
    void
      $ runAff (either (const (pure unit)) (cb <<< Just))
      $ fn editor session position prefix

-- | Generate unique key for component
genKey ∷ Effect String
genKey = do
  rn1 ← random
  rn2 ← random
  instant ← now
  pure $ show rn1 <> show (unwrap (unInstant instant)) <> show rn2

data Autocomplete = Live | Basic

-- | Ace query algebra
-- | - `SetElement` - used to capture a reference to the component's element
-- | - `Init` - used internally to handle initialization of component
-- | - `Quit` - used internally to handle finalizing of component.
-- | - `GetText` - gets the current text value
-- | - `SetText` - alters the current text value
-- | - `SetAutocomplete` - sets autocomplete resume:
-- |   - `Nothing` - turns it off
-- |   - `Just Basic` - enables basic autocompletions (triggered by `Alt + Space` or `Ctrl + Space`)
-- |   - `Just Live` - enables live autocomplete
-- | - `SetCompleteFn` - sets function providing autocomplete variants.
-- | - `GetEditor` - returns ace editor instance handled by this component.
data AceQuery a
  = Init a
  | Quit a
  | GetText (String → a)
  | SetText String a
  | SetAutocomplete (Maybe Autocomplete) a
  | SetCompleteFn CompleteFn a
  | GetEditor (Maybe Editor → a)
  | HandleChange (H.SubscribeStatus -> a)

-- | Ace output messages
-- | - `AceValueChanged` - raised when the value in the editor is changed.
data AceMessage = TextChanged String

-- | The type for autocomplete function s. Takes editor, session, text position,
-- | prefix, and returns array of possible completions in the `Aff` monad.
type CompleteFn
  = Editor
  → EditSession
  → Position
  → String
  → Aff (Array Completion)

-- | Ace component state.
-- | - `key` - unique key of this instance
-- | - `editor` - Ace editor instance wrapped by this component
type AceState =
  { key ∷ Maybe String
  , editor ∷ Maybe Editor
  }

type DSL m = H.ComponentDSL AceState AceQuery AceMessage m

-- | An initial empty state value.
initialState ∷ AceState
initialState =
  { key: Nothing
  , editor: Nothing
  }

-- | The Ace component.
aceComponent
  ∷ ∀ m
  . MonadAff m
  ⇒ (Editor → m Unit)
  → Maybe Autocomplete
  → H.Component HH.HTML AceQuery Unit AceMessage m
aceComponent setup resume =
  H.lifecycleComponent
    { initialState: const initialState
    , render
    , eval: eval setup resume
    , initializer: Just (H.action Init)
    , finalizer: Just (H.action Quit)
    , receiver: const Nothing
    }

render ∷ AceState → H.ComponentHTML AceQuery
render = const $ HH.div [ HP.ref (H.RefLabel "container") ] []

eval
 ∷ ∀ m
 . MonadAff m
 ⇒ (Editor → m Unit)
 → Maybe Autocomplete
 → AceQuery
 ~> DSL m
eval setup resume = case _ of

  Init next → do
    H.getHTMLElementRef (H.RefLabel "container") >>= traverse_ \el → do
      key ← H.gets _.key >>= maybe (H.liftEffect genKey) pure
      editor ← H.liftEffect $ Ace.editNode el Ace.ace
      H.put { key: Just key, editor: Just editor }
      H.liftEffect do
        enableAutocomplete
        setAutocompleteResume resume editor
        Editor.onFocus editor $ write key focused
      session ← H.liftEffect $ Editor.getSession editor
      H.subscribe $ H.eventSource_ (\f -> Session.onChange session (\_ -> f)) (H.request HandleChange)
      H.lift $ setup editor
    pure next

  Quit next → do
    H.gets _.key
      >>= traverse_ \key →
      H.liftEffect $ modify (FO.delete key) completeFns
    pure next

  GetEditor k →
    map k $ H.gets _.editor

  GetText k →
    pure <<< k =<< getText

  SetText text next → do
    H.gets _.editor
      >>= traverse_ \editor → do
        current ← H.liftEffect $ Editor.getValue editor
        when (text /= current) $ void
          $ H.liftEffect (Editor.setValue text Nothing editor)
    pure next

  SetAutocomplete mbAc next → do
    H.gets _.editor
      >>= traverse_ (H.liftEffect <<< setAutocompleteResume mbAc)
    pure next

  SetCompleteFn fn next → do
    H.gets _.key
      >>= traverse_ \key →
      H.liftEffect $ modify (FO.insert key fn) completeFns
    pure next

  HandleChange k → do
    H.raise <<< TextChanged =<< getText
    pure $ k H.Listening

  where

  getText :: DSL m String
  getText =
    maybe (pure "") (H.liftEffect <<< Editor.getValue) =<< H.gets _.editor
