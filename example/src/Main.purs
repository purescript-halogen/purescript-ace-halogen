module Main where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Now (NOW)

import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)

import Ace.Editor as Editor
import Ace.EditSession as Session
import Ace.Halogen.Component (AceQuery, AceMessage(..), aceComponent)
import Ace.Types (ACE, Editor)

data Query a
  = HandleMessage AceMessage a

type State =
  { text ∷ String
  }

initialState ∷ State
initialState =
  { text : "Name: Ace Editor"
  }

type AceSlot = Unit

type MainEffects = HA.HalogenEffects (random ∷ RANDOM, now ∷ NOW, ace ∷ ACE)
type MainAff = Aff MainEffects

type MainHtml = H.ParentHTML Query AceQuery AceSlot MainAff
type MainDSL = H.ParentDSL State Query AceQuery AceSlot Void MainAff

ui ∷ H.Component HH.HTML Query Unit Void MainAff
ui =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  render ∷ State → MainHtml
  render state =
    HH.div_
      [ HH.slot unit (component state) unit (HE.input HandleMessage)
      , HH.div_ [ HH.text state.text ]
      ]

  component :: State → H.Component HH.HTML AceQuery Unit AceMessage MainAff
  component state = aceComponent (initEditor state) Nothing

  initEditor ∷ State → Editor → MainAff Unit
  initEditor state editor = liftEff $ do
    session ← Editor.getSession editor
    Session.setMode "ace/mode/yaml" session
    Editor.setValue state.text Nothing editor
    pure unit

  eval ∷ Query ~> MainDSL
  eval = case _ of
    HandleMessage (TextChanged text) next → do
      H.modify (_ { text = text })
      pure next

main ∷ Eff MainEffects Unit
main = HA.runHalogenAff (runUI ui unit =<< HA.awaitBody)
