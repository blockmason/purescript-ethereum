module Network.Eth.Metamask
       (
         checkStatus
       , hasWeb3
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

foreign import web3DefinedImpl ∷ ∀ e. Unit → Eff e Boolean
foreign import checkStatusImpl ∷ ∀ e. (Boolean → Eff e Unit) → Eff e Unit
foreign import currentUserImpl ∷ ∀ e. (String → Eff e Unit)  → Eff e Unit
foreign import checkTxStatusImpl ∷ ∀ e. (String → Eff e Unit) → String → Eff e Unit
foreign import getNetworkImpl ∷ ∀ e. (String → Eff e Unit) → Eff e Unit

hasWeb3 ∷ ∀ e. Eff (metamask ∷ METAMASK | e) Boolean
hasWeb3 = web3DefinedImpl unit

checkStatus ∷ ∀ e. Aff (metamask ∷ METAMASK | e) MetamaskStatus
checkStatus = do
  res ← makeAff (\_ s → checkStatusImpl s)
  if res then pure LoggedIn else pure LoggedOut

currentUserAddress ∷ ∀ e. Aff (metamask ∷ METAMASK | e) E.EthAddress
currentUserAddress = do
  addrStr ← makeAff (\_ s → currentUserImpl s)
  pure $ E.eaMkAddr addrStr

type StringNetId = String
getNetwork ∷ ∀ e. Aff (metamask ∷ METAMASK | e) StringNetId
getNetwork = liftAff $ makeAff (\_ s → getNetworkImpl s)

loggedIn ∷ ∀ e. Aff (metamask ∷ METAMASK | e) Boolean
loggedIn = do
  status ← checkStatus
  case status of
    LoggedOut → pure false
    LoggedIn  → pure true

checkTxStatus ∷ ∀ e. E.TX → Aff (metamask ∷ METAMASK | e) E.TxStatus
checkTxStatus tx = do
  E.rawToTxStatus <$> (liftAff $ makeAff (\_ s → checkTxStatusImpl s $ E.txStr tx))
