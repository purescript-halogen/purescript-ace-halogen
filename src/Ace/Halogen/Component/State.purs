module Ace.Halogen.Component.State (AceState(..), runAceState, initialAceState) where

import Data.Maybe (Maybe(..))

import Ace.Types (Editor())

-- | The Ace component state value. This is used to hold a reference to the
-- | underlying Ace editor.
newtype AceState = AceState (Maybe Editor)

-- | Extracts the current editor reference from the state.
runAceState :: AceState -> Maybe Editor
runAceState (AceState e) = e

-- | An initial empty state for the Ace component.
initialAceState :: AceState
initialAceState = AceState Nothing
