module Main where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)

import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)

import Ace.Editor as Editor
import Ace.EditSession as Session
import Ace.Halogen.Component (AceQuery, AceMessage(..), aceComponent)
import Ace.Types (Editor)

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

type MainHtml = H.ParentHTML Query AceQuery AceSlot Aff
type MainDSL = H.ParentDSL State Query AceQuery AceSlot Void Aff

ui ∷ H.Component HH.HTML Query Unit Void Aff
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

  component :: State → H.Component HH.HTML AceQuery Unit AceMessage Aff
  component state = aceComponent (initEditor state) Nothing

  initEditor ∷ State → Editor → Aff Unit
  initEditor state editor = liftEffect $ do
    session ← Editor.getSession editor
    Session.setMode "ace/mode/yaml" session
    _ ← Editor.setValue state.text Nothing editor
    pure unit

  eval ∷ Query ~> MainDSL
  eval = case _ of
    HandleMessage (TextChanged text) next → do
      _ <- H.modify (_ { text = text })
      pure next

main ∷ Effect Unit
main = HA.runHalogenAff (runUI ui unit =<< HA.awaitBody)
