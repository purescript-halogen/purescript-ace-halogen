module Ace.Halogen.Component
  ( aceComponent
  , aceConstructor
  , AceQuery(..)
  , AceState(..)
  , initialAceState
  , AceEffects()
  , Autocomplete(..)
  , CompleteFn()
  ) where

import Prelude

import Control.Monad (when)
import Control.Monad.Aff (Aff(), runAff)
import Control.Monad.Aff.AVar (AVAR())
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Random (random, RANDOM())
import Control.Monad.Eff.Ref (Ref(), REF(), readRef, writeRef, modifyRef)

import Data.Date (nowEpochMilliseconds, Now())
import Data.Foldable (traverse_)
import Data.Functor (($>))
import Data.Maybe (Maybe(..), maybe)
import Data.NaturalTransformation (Natural())
import Data.StrMap (StrMap())
import Data.StrMap as Sm
import Data.Time (Milliseconds(..))

import DOM (DOM())
import DOM.HTML.Types (HTMLElement())

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP

import Ace.Editor as Editor
import Ace.EditSession as Session
import Ace.Ext.LanguageTools as LanguageTools
import Ace.Ext.LanguageTools.Completer as Completer
import Ace.Types (Editor(), Completion(), Position(), EditSession(), ACE())

-- | Effectful knot of autocomplete functions. It's needed because
-- | `languageTools.addCompleter` is global and adds completer to
-- | all editors
foreign import completeFns :: forall eff. Ref (StrMap (CompleteFn eff))

-- | This flag is used to determine if `languageTools` initialized
foreign import initialized :: Ref Boolean

-- | Global key of currently focused component. Used only to take
-- | autocomplete function
foreign import focused :: Ref String

-- | Get `dataset` property of element
foreign import dataset
  :: forall eff
   . HTMLElement
  -> Eff (dom :: DOM | eff) (StrMap String)


-- | Take completion function for currently selected component
completeFnFocused :: forall eff. Eff (AceEffects eff) (CompleteFn eff)
completeFnFocused = do
  focusedKey <- readRef focused
  mFns <- readRef completeFns
  maybe (pure emptyCompleteFn) pure $ Sm.lookup focusedKey mFns
  where
  emptyCompleteFn :: CompleteFn eff
  emptyCompleteFn _ _ _ _ = pure []

-- | Set autocomplete resume
setAutocompleteResume
  :: forall eff. Maybe Autocomplete -> Editor -> Eff (AceEffects eff) Unit
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
enableAutocomplete :: forall eff. Eff (AceEffects eff) Unit
enableAutocomplete = do
  languageToolsInitialized <- readRef initialized
  when (not languageToolsInitialized) do
    completer <- Completer.mkCompleter globalCompleteFn
    tools <- LanguageTools.languageTools
    LanguageTools.addCompleter completer tools
    writeRef initialized true
  where
  globalCompleteFn editor session position prefix cb = do
    fn <- completeFnFocused
    runAff (const $ cb Nothing) (cb <<< Just)
      $ fn editor session position prefix

-- | Generate unique key for component
genKey :: forall eff. Eff (now :: Now, random :: RANDOM | eff) String
genKey = do
  rn1 <- random
  rn2 <- random
  (Milliseconds time) <- nowEpochMilliseconds
  pure $ show rn1 <> show time <> show rn2

data Autocomplete = Live | Basic

type AceEffects eff =
  ( random :: RANDOM
  , now :: Now
  , ref :: REF
  , ace :: ACE
  , avar :: AVAR
  , dom :: DOM
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
-- | - `TextChanged` - raised internally when the value in the editor is
-- |   changed. Allows for parent component to observe when the value changes
-- |   via the `peek` mechanism.
data AceQuery a
  = SetElement (Maybe HTMLElement) a
  | Init a
  | Quit a
  | GetText (String -> a)
  | SetText String a
  | SetAutocomplete (Maybe Autocomplete) a
  | SetCompleteFn (forall eff. CompleteFn eff) a
  | GetEditor (Maybe Editor -> a)
  | TextChanged a

-- | The type for autocomplete function s. Takes editor, session, text position,
-- | prefix, and returns array of possible completions in the `Aff` monad.
type CompleteFn eff
   = Editor
  -> EditSession
  -> Position
  -> String
  -> Aff (AceEffects eff) (Array Completion)

-- | Ace component state.
-- | - `key` - unique key of this instance
-- | - `editor` - Ace editor instance wrapped by this component
type AceState =
  { key :: Maybe String
  , editor :: Maybe Editor
  , element :: Maybe HTMLElement
  }

-- | An initial empty state value.
initialAceState :: AceState
initialAceState =
  { key: Nothing
  , editor: Nothing
  , element: Nothing
  }

-- | The Ace component.
aceComponent
  :: forall eff
   . (Editor -> Aff (AceEffects eff) Unit)
  -> Maybe Autocomplete
  -> H.Component AceState AceQuery (Aff (AceEffects eff))
aceComponent setup resume = H.lifecycleComponent
    { render
    , eval
    , initializer: Just (H.action Init)
    , finalizer: Just (H.action Quit)
    }
  where
  render :: AceState -> H.ComponentHTML AceQuery
  render = const $ HH.div [ HP.ref (\el -> H.action (SetElement el)) ] []

  eval :: Natural AceQuery (H.ComponentDSL AceState AceQuery (Aff (AceEffects eff)))
  eval (SetElement el next) =
    H.modify (_ { element = el }) $> next
  eval (Init next) = do
    el <- H.gets _.element
    case el of
      Nothing -> pure unit
      Just el' -> do
        key <- H.gets _.key >>= maybe (H.fromEff genKey) pure
        editor <- H.fromEff $ Ace.editNode el' Ace.ace
        H.set { key: Just key, editor: Just editor, element: Just el' }
        H.fromEff do
          enableAutocomplete
          setAutocompleteResume resume editor
          Editor.onFocus editor $ writeRef focused key
        session <- H.fromEff $ Editor.getSession editor
        H.subscribe $ H.eventSource_ (Session.onChange session) do
          pure $ H.action TextChanged
        H.liftH $ setup editor
    pure next

  eval (Quit next) = do
    H.gets _.key
      >>= traverse_ \key ->
      H.fromEff $ modifyRef completeFns $ Sm.delete key
    pure next

  eval (GetEditor k) =
    map k $ H.gets _.editor

  eval (GetText k) =
    H.gets _.editor
      >>= maybe (pure "") (H.fromEff <<< Editor.getValue)
      >>= k >>> pure

  eval (SetText text next) = do
    H.gets _.editor
      >>= traverse_ \editor -> do
        current <- H.fromEff $ Editor.getValue editor
        when (text /= current) $ void
          $ H.fromEff (Editor.setValue text Nothing editor)
    pure next

  eval (SetAutocomplete mbAc next) = do
    H.gets _.editor
      >>= traverse_ (H.fromEff <<< setAutocompleteResume mbAc)
    pure next

  eval (SetCompleteFn fn next) = do
    H.gets _.key
      >>= traverse_ \key ->
      H.fromEff $ modifyRef completeFns $ Sm.insert key fn
    pure next

  eval (TextChanged next) = pure next

-- | A convenience function for creating a `SlotConstructor` for an Ace
-- | component.
aceConstructor
  :: forall p eff
   . p
  -> (Editor -> Aff (AceEffects eff) Unit)
  -> Maybe Autocomplete
  -> H.SlotConstructor AceState AceQuery (Aff (AceEffects eff)) p
aceConstructor p setup mbAc =
  H.SlotConstructor p \_ ->
    { component: aceComponent setup mbAc
    , initialState: initialAceState
    }
