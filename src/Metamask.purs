module Network.Eth.Metamask
       (
         checkStatus
       , loggedIn
       , currentUserAddress
       , checkTxStatus
       , getNetwork
       , METAMASK
       , MetamaskStatus(..)
       ) where

import Prelude
import Control.Monad.Eff           (Eff, kind Effect)
import Control.Monad.Aff           (Aff, makeAff)
import Control.Monad.Aff.Class     (liftAff)
import Network.Eth     as E

foreign import data METAMASK ∷ Effect

data MetamaskStatus = LoggedOut | LoggedIn
instance showMetamaskStatus ∷ Show MetamaskStatus where
  show val = case val of
    LoggedOut → "Metamask is logged out."
    LoggedIn  → "Metamask is logged in."

foreign import checkStatusImpl ∷ ∀ e. Unit → Eff e Boolean
foreign import currentUserImpl ∷ ∀ e. Unit → Eff e String
foreign import checkTxStatusImpl ∷ ∀ e. (String → Eff e Unit) → String → Eff e Unit
foreign import getNetworkImpl ∷ ∀ e. (String → Eff e Unit) → Eff e Unit

checkStatus ∷ ∀ e. Eff (metamask ∷ METAMASK | e) MetamaskStatus
checkStatus = do
  res ← checkStatusImpl unit
  if res then pure LoggedIn else pure LoggedOut

currentUserAddress ∷ ∀ e. Eff (metamask ∷ METAMASK | e) E.EthAddress
currentUserAddress = E.eaMkAddr <$> currentUserImpl unit

type StringNetId = String
getNetwork ∷ ∀ e. Aff e StringNetId
getNetwork = liftAff $ makeAff (\_ s → getNetworkImpl s)

loggedIn ∷ ∀ e. Eff (metamask ∷ METAMASK | e) Boolean
loggedIn = do
  status ← checkStatus
  case status of
    LoggedOut → pure false
    LoggedIn  → pure true

checkTxStatus ∷ ∀ e. E.TX → Aff (metamask ∷ METAMASK | e) E.TxStatus
checkTxStatus tx = do
  E.rawToTxStatus <$> (liftAff $ makeAff (\_ s → checkTxStatusImpl s $ E.txStr tx))
