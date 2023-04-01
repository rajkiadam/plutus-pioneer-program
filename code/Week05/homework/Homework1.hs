{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}

module Homework1 where

import           Plutus.V1.Ledger.Interval (after)
import           Plutus.V2.Ledger.Api (BuiltinData, MintingPolicy, POSIXTime,
                                       PubKeyHash, ScriptContext,
                                       mkMintingPolicyScript, 
                                       TxInfo (txInfoValidRange), 
                                       ScriptContext (scriptContextTxInfo))
import           Plutus.V2.Ledger.Contexts (txSignedBy)
import qualified PlutusTx
import           PlutusTx.Prelude     (Bool (False), ($), (&&), traceIfFalse)
import           Utilities            (wrapPolicy)

{-# INLINABLE mkDeadlinePolicy #-}
-- This policy should only allow minting (or burning) of tokens if the owner of the specified PubKeyHash
-- has signed the transaction and if the specified deadline has not passed.
mkDeadlinePolicy :: PubKeyHash -> POSIXTime -> () -> ScriptContext -> Bool
mkDeadlinePolicy _pkh _deadline () _ctx = (traceIfFalse "wrong signiture" $ txSignedBy (scriptContextTxInfo _ctx) _pkh) &&
                                            (traceIfFalse "not before deadline" $ afterDeadline)
    where
        info :: TxInfo
        info = scriptContextTxInfo _ctx

        afterDeadline :: Bool
        afterDeadline = after (_deadline) $ txInfoValidRange info

{-# INLINABLE mkWrappedDeadlinePolicy #-}
mkWrappedDeadlinePolicy :: PubKeyHash -> POSIXTime -> BuiltinData -> BuiltinData -> ()
mkWrappedDeadlinePolicy pkh deadline = wrapPolicy $ mkDeadlinePolicy pkh deadline

deadlinePolicy :: PubKeyHash -> POSIXTime -> MintingPolicy
deadlinePolicy pkh deadline = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| mkWrappedDeadlinePolicy ||])
        `PlutusTx.applyCode` PlutusTx.liftCode pkh
        `PlutusTx.applyCode` PlutusTx.liftCode deadline
