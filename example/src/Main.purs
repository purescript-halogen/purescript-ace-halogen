module Main where

import Prelude

import Ace.Editor as Editor
import Ace.EditSession as Session
import Ace.Halogen.Component (AceState (), AceQuery (TextChanged, GetText), initialAceState)
import qualified Ace.Halogen.Component as Ace

import Control.Monad.Aff (Aff (), runAff)
import Control.Monad.Eff (Eff ())
import Control.Monad.Eff.Class (MonadEff, liftEff)
import Control.Monad.Eff.Exception (throwException)
import Control.Plus (Plus)

import Data.Functor.Coproduct (Coproduct())
import Data.Maybe (Maybe (..), fromMaybe)

import Halogen
import Halogen.Util (appendToBody, onLoad)
import qualified Halogen.HTML.Indexed as H

-- Effects
import Control.Monad.Eff.Random (RANDOM ())
import Control.Monad.Eff.Ref (REF ())
import Ace.Types (ACE ())
import Data.Date (Now ())


data Query a
    = UpdateText a

type State =
    { text :: String
    }

initialState :: State
initialState =
    { text : "Name: Ace Editor"
    }

data AceSlot = AceSlot

instance eqAceSlot :: Eq AceSlot where
    eq AceSlot AceSlot = true

instance ordAceSlot :: Ord AceSlot where
    compare AceSlot AceSlot = EQ


type StateP g = InstalledState State AceState Query AceQuery g AceSlot
type QueryP = Coproduct Query (ChildF AceSlot AceQuery)
type MainHtml g = ParentHTML AceState Query AceQuery g AceSlot
type MainEffects = HalogenEffects (random :: RANDOM, now :: Now, ref :: REF, ace :: ACE)
type MainAff = Aff MainEffects

ui :: Component (StateP MainAff) QueryP MainAff
ui = parentComponent' render eval peek
    where

    render :: State -> MainHtml MainAff
    render state =
        H.div_
            [ H.slot AceSlot \_ ->
                { component :
                    Ace.aceComponent
                        (\editor -> liftEff $ do
                            session <- Editor.getSession editor
                            Session.setMode "ace/mode/yaml" session
                            Editor.setValue state.text Nothing editor
                            pure unit
                        )
                        Nothing
                , initialState :
                    initialAceState
                }
            , H.div_
                [ H.text state.text ]
            ]

    eval :: EvalParent Query State AceState Query AceQuery MainAff AceSlot
    eval (UpdateText next) = do
        pure next

    peek :: Peek (ChildF AceSlot AceQuery) State AceState Query AceQuery MainAff AceSlot
    peek (ChildF p q) =
        case q of
            TextChanged _ -> do
                text <- query AceSlot $ request GetText
                modify (_ { text = fromMaybe "" text })
                pure unit
            _ ->
                pure unit

main :: Eff MainEffects Unit
main = runAff throwException (const (pure unit)) $ do
    app <- runUI ui (installedState initialState)
    onLoad $ appendToBody app.node
