module Main where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Now (NOW)

import Data.Functor.Coproduct (Coproduct)
import Data.Maybe (Maybe (..), fromMaybe)

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.Util (runHalogenAff, awaitBody)

import Ace.Editor as Editor
import Ace.EditSession as Session
import Ace.Halogen.Component (AceState, AceQuery (TextChanged, GetText), aceConstructor)
import Ace.Types (ACE, Editor)

data Query a
  = UpdateText a

type State =
  { text ∷ String
  }

initialState ∷ State
initialState =
  { text : "Name: Ace Editor"
  }

type AceSlot = Unit

type StateP g = H.ParentState State AceState Query AceQuery g AceSlot
type QueryP = Coproduct Query (H.ChildF AceSlot AceQuery)
type MainHtml g = H.ParentHTML AceState Query AceQuery g AceSlot
type MainEffects = H.HalogenEffects (random ∷ RANDOM, now ∷ NOW, ref ∷ REF, ace ∷ ACE)
type MainAff = Aff MainEffects
type MainDSL = H.ParentDSL State AceState Query AceQuery MainAff AceSlot

ui ∷ H.Component (StateP MainAff) QueryP MainAff
ui = H.parentComponent { render, eval, peek: Just (peek <<< H.runChildF) }
  where

  render ∷ State → MainHtml MainAff
  render state =
    HH.div_
      [ HH.Slot $ aceConstructor unit (initEditor state) Nothing
      , HH.div_ [ HH.text state.text ]
      ]

  initEditor ∷ State → Editor → MainAff Unit
  initEditor state editor = liftEff $ do
    session ← Editor.getSession editor
    Session.setMode "ace/mode/yaml" session
    Editor.setValue state.text Nothing editor
    pure unit

  eval ∷ Query ~> MainDSL
  eval (UpdateText next) =
    pure next

  peek ∷ forall x. AceQuery x → MainDSL Unit
  peek (TextChanged _) = do
    text ← H.query unit $ H.request GetText
    H.modify (_ { text = fromMaybe "" text })
  peek _ =
    pure unit

main ∷ Eff MainEffects Unit
main = runHalogenAff $ H.runUI ui (H.parentState initialState) =<< awaitBody
