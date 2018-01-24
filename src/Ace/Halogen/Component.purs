module Ace.Halogen.Component
  ( aceComponent
  , AceQuery(..)
  , AceMessage(..)
  , AceEffects
  , Autocomplete(..)
  , CompleteFn
  ) where

import Prelude

import Ace as Ace
import Ace.EditSession as Session
import Ace.Editor as Editor
import Ace.Ext.LanguageTools as LanguageTools
import Ace.Ext.LanguageTools.Completer as Completer
import Ace.Types (Editor, Completion, Position, EditSession, ACE)
import Control.Monad.Aff (Aff, runAff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Now (NOW, now)
import Control.Monad.Eff.Random (random, RANDOM)
import Control.Monad.Eff.Ref (Ref, REF, readRef, writeRef, modifyRef)
import DOM (DOM)
import DOM.HTML.Types (HTMLElement)
import Data.DateTime.Instant (unInstant)
import Data.Either (either)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.StrMap (StrMap)
import Data.StrMap as Sm
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

-- | Effectful knot of autocomplete functions. It's needed because
-- | `languageTools.addCompleter` is global and adds completer to
-- | all editors
foreign import completeFns ∷ ∀ eff. Ref (StrMap (CompleteFn eff))

-- | This flag is used to determine if `languageTools` initialized
foreign import initialized ∷ Ref Boolean

-- | Global key of currently focused component. Used only to take
-- | autocomplete function
foreign import focused ∷ Ref String

-- | Get `dataset` property of element
foreign import dataset
  ∷ ∀ eff
  . HTMLElement
  → Eff (dom ∷ DOM | eff) (StrMap String)

-- | Take completion function for currently selected component
completeFnFocused ∷ ∀ eff. Eff (AceEffects eff) (CompleteFn eff)
completeFnFocused = do
  focusedKey ← readRef focused
  mFns ← readRef completeFns
  maybe (pure emptyCompleteFn) pure $ Sm.lookup focusedKey mFns
  where
  emptyCompleteFn ∷ CompleteFn eff
  emptyCompleteFn _ _ _ _ = pure []

-- | Set autocomplete resume
setAutocompleteResume
  ∷ ∀ eff. Maybe Autocomplete → Editor → Eff (AceEffects eff) Unit
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
enableAutocomplete ∷ ∀ eff. Eff (AceEffects eff) Unit
enableAutocomplete = do
  languageToolsInitialized ← readRef initialized
  when (not languageToolsInitialized) do
    completer ← Completer.mkCompleter globalCompleteFn
    tools ← LanguageTools.languageTools
    LanguageTools.addCompleter completer tools
    writeRef initialized true
  where
  globalCompleteFn editor session position prefix cb = do
    fn ← completeFnFocused
    void
      $ runAff (either (const (pure unit)) (cb <<< Just))
      $ fn editor session position prefix

-- | Generate unique key for component
genKey ∷ ∀ eff. Eff (now ∷ NOW, random ∷ RANDOM | eff) String
genKey = do
  rn1 ← random
  rn2 ← random
  instant ← now
  pure $ show rn1 <> show (unwrap (unInstant instant)) <> show rn2

data Autocomplete = Live | Basic

type AceEffects eff =
  ( random ∷ RANDOM
  , now ∷ NOW
  , ref ∷ REF
  , ace ∷ ACE
  , avar ∷ AVAR
  , dom ∷ DOM
  | eff
  )

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
  | SetCompleteFn (∀ eff. CompleteFn eff) a
  | GetEditor (Maybe Editor → a)
  | HandleChange (H.SubscribeStatus -> a)

-- | Ace output messages
-- | - `AceValueChanged` - raised when the value in the editor is changed.
data AceMessage = TextChanged String

-- | The type for autocomplete function s. Takes editor, session, text position,
-- | prefix, and returns array of possible completions in the `Aff` monad.
type CompleteFn eff
  = Editor
  → EditSession
  → Position
  → String
  → Aff (AceEffects eff) (Array Completion)

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
  ∷ ∀ eff m
  . MonadAff (AceEffects eff) m
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

eval ∷ ∀ eff m
 . MonadAff (AceEffects eff) m
 ⇒ (Editor → m Unit)
 → Maybe Autocomplete
 → AceQuery
 ~> DSL m
eval setup resume = case _ of

  Init next → do
    H.getHTMLElementRef (H.RefLabel "container") >>= traverse_ \el → do
      key ← H.gets _.key >>= maybe (H.liftEff genKey) pure
      editor ← H.liftEff $ Ace.editNode el Ace.ace
      H.put { key: Just key, editor: Just editor }
      H.liftEff do
        enableAutocomplete
        setAutocompleteResume resume editor
        Editor.onFocus editor $ writeRef focused key
      session ← H.liftEff $ Editor.getSession editor
      H.subscribe $ H.eventSource_ (Session.onChange session) (H.request HandleChange)
      H.lift $ setup editor
    pure next

  Quit next → do
    H.gets _.key
      >>= traverse_ \key →
      H.liftEff $ modifyRef completeFns $ Sm.delete key
    pure next

  GetEditor k →
    map k $ H.gets _.editor

  GetText k →
    pure <<< k =<< getText

  SetText text next → do
    H.gets _.editor
      >>= traverse_ \editor → do
        current ← H.liftEff $ Editor.getValue editor
        when (text /= current) $ void
          $ H.liftEff (Editor.setValue text Nothing editor)
    pure next

  SetAutocomplete mbAc next → do
    H.gets _.editor
      >>= traverse_ (H.liftEff <<< setAutocompleteResume mbAc)
    pure next

  SetCompleteFn fn next → do
    H.gets _.key
      >>= traverse_ \key →
      H.liftEff $ modifyRef completeFns $ Sm.insert key fn
    pure next

  HandleChange k → do
    H.raise <<< TextChanged =<< getText
    pure $ k H.Listening

  where

  getText :: DSL m String
  getText =
    maybe (pure "") (H.liftEff <<< Editor.getValue) =<< H.gets _.editor
