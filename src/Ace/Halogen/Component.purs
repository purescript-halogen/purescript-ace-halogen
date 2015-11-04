module Ace.Halogen.Component
  ( aceComponent
  , aceConstructor
  , module Ace.Halogen.Component.Query
  , module Ace.Halogen.Component.State
  ) where

import Prelude

import Control.Monad (when)
import Control.Monad.Eff.Class (MonadEff)

import Data.Maybe (Maybe(..))

import Halogen (Natural(), Component(), ComponentHTML(), ComponentDSL(), SlotConstructor(..), component, action, liftEff', modify, gets)
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P

import Ace.Types (ACE())
import Ace.Editor as Editor
import Ace.Halogen.Component.Query
import Ace.Halogen.Component.State

-- | The Ace component.
aceComponent :: forall g eff. (MonadEff (ace :: ACE | eff) g) => Maybe String -> Component AceState AceQuery g
aceComponent initialText = component render eval
  where

  render :: AceState -> ComponentHTML AceQuery
  render = const $ H.div [ P.initializer \el -> action (Init el) ] []

  eval :: Natural AceQuery (ComponentDSL AceState AceQuery g)
  eval (Init el next) = do
    editor <- liftEff' (Ace.editNode el Ace.ace)
    modify $ const $ AceState (Just editor)
    case initialText of
      Nothing -> pure unit
      Just text -> void $ liftEff' $ Editor.setValue text Nothing editor
    pure next
  eval (GetText k) = do
    state <- gets runAceState
    case state of
      Nothing -> pure (k "")
      Just editor -> do
        text <- liftEff' (Editor.getValue editor)
        pure (k text)
  eval (SetText text next) = do
    state <- gets runAceState
    case state of
      Nothing -> pure unit
      Just editor -> do
        current <- liftEff' (Editor.getValue editor)
        when (text /= current) $ void $ liftEff' (Editor.setValue text Nothing editor)
    pure next

-- | A convenience function for creating a `SlotConstructor` for an Ace
-- | component.
aceConstructor :: forall g p eff. (MonadEff (ace :: ACE | eff) g) => p -> Maybe String -> SlotConstructor AceState AceQuery g p
aceConstructor p initialText = SlotConstructor p \_ -> { component: aceComponent initialText, initialState: initialAceState }
