{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Cardano.Api.Orphans () where

import           Cardano.Api.Script
import           Cardano.Ledger.Alonzo.Rules.Bbody (AlonzoBbodyPredFail)
import           Cardano.Ledger.Alonzo.Rules.Utxow (UtxowPredicateFail (..))
import           Cardano.Ledger.BaseTypes (strictMaybeToMaybe)
import           Cardano.Ledger.Chain
import           Cardano.Ledger.Compactible (Compactible (fromCompact))
import           Cardano.Ledger.Crypto (StandardCrypto)
import           Cardano.Ledger.Shelley.API hiding (ShelleyBasedEra)
import           Cardano.Ledger.Shelley.PParams (PParamsUpdate)
import           Cardano.Ledger.Shelley.Rules.Bbody
import           Cardano.Ledger.Shelley.Rules.Deleg
import           Cardano.Ledger.Shelley.Rules.Delegs
import           Cardano.Ledger.Shelley.Rules.Delpl
import           Cardano.Ledger.Shelley.Rules.Epoch
import           Cardano.Ledger.Shelley.Rules.Ledger
import           Cardano.Ledger.Shelley.Rules.Ledgers
import           Cardano.Ledger.Shelley.Rules.Mir
import           Cardano.Ledger.Shelley.Rules.NewEpoch
import           Cardano.Ledger.Shelley.Rules.Newpp
import           Cardano.Ledger.Shelley.Rules.Pool
import           Cardano.Ledger.Shelley.Rules.PoolReap
import           Cardano.Ledger.Shelley.Rules.Ppup
import           Cardano.Ledger.Shelley.Rules.Rupd
import           Cardano.Ledger.Shelley.Rules.Snap
import           Cardano.Ledger.Shelley.Rules.Tick
import           Cardano.Ledger.Shelley.Rules.Upec
import           Cardano.Ledger.Shelley.Rules.Utxo
import           Cardano.Ledger.Shelley.Rules.Utxow
import           Cardano.Ledger.UnifiedMap (UnifiedMap)
import           Cardano.Prelude
import           Cardano.Protocol.TPraos.API (ChainTransitionError (ChainTransitionError))
import           Cardano.Protocol.TPraos.BHeader (LastAppliedBlock, labBlockNo)
import           Cardano.Protocol.TPraos.OCert (KESPeriod (KESPeriod))
import           Cardano.Protocol.TPraos.Rules.OCert
import           Cardano.Protocol.TPraos.Rules.Overlay
import           Cardano.Protocol.TPraos.Rules.Prtcl
import           Cardano.Protocol.TPraos.Rules.Tickn
import           Cardano.Protocol.TPraos.Rules.Updn
import           Cardano.Slotting.Block (BlockNo (..))
import           Cardano.Slotting.Slot (EpochNo, SlotNo (..))
import           Cardano.Slotting.Time (SystemStart (..))
import           Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object, (.=))
import           Data.Aeson.Types (ToJSONKey (..), toJSONKeyText)
import           Data.BiMap (BiMap (..), Bimap)
import           Data.Functor.Contravariant (Contravariant (..))
import           Data.UMap (Trip (Triple), UMap (UnifiedMap))
import           Data.VMap (VB, VMap, VP)
import qualified Data.VMap as VMap

import qualified Cardano.Ledger.Babbage as Babbage
import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Ledger.Era as Ledger
import qualified Cardano.Ledger.Shelley.PoolRank as Shelley
import           Control.State.Transition (STS (State))

import qualified Cardano.Binary as CBOR
import qualified Cardano.Crypto.Hash.Class as Crypto
import qualified Cardano.Ledger.Alonzo.Data as Alonzo
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo

import qualified Cardano.Api.Address as Api
import qualified Cardano.Api.Certificate as Api
import qualified Cardano.Api.Script as Api
import qualified Cardano.Api.SerialiseRaw as Api
import qualified Cardano.Api.SerialiseTextEnvelope as Api
import qualified Cardano.Api.TxBody as Api
import qualified Cardano.Crypto.Hash as Hash
import qualified Cardano.Crypto.VRF.Class as Crypto
import qualified Cardano.Ledger.Alonzo as Alonzo
import qualified Cardano.Ledger.Alonzo.Language as Alonzo
import qualified Cardano.Ledger.Alonzo.PlutusScriptApi as Alonzo
import qualified Cardano.Ledger.Alonzo.Rules.Utxo as Alonzo
import qualified Cardano.Ledger.Alonzo.Rules.Utxos as Alonzo
import qualified Cardano.Ledger.Alonzo.Scripts as Ledger
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import qualified Cardano.Ledger.Alonzo.TxInfo as Alonzo
import qualified Cardano.Ledger.AuxiliaryData as Core
import qualified Cardano.Ledger.Babbage.PParams as Babbage
import qualified Cardano.Ledger.Babbage.TxBody as Babbage
import qualified Cardano.Ledger.Coin as Shelley
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Ledger.Crypto as Core
import qualified Cardano.Ledger.Crypto as Crypto
import qualified Cardano.Ledger.Hashes as Ledger
import qualified Cardano.Ledger.Mary.Value as Mary
import qualified Cardano.Ledger.PoolDistr as Ledger
import qualified Cardano.Ledger.SafeHash as Ledger
import qualified Cardano.Ledger.SafeHash as SafeHash
import qualified Cardano.Ledger.Shelley.API as Shelley
import qualified Cardano.Ledger.Shelley.Constraints as Shelley
import qualified Cardano.Ledger.Shelley.EpochBoundary as ShelleyEpoch
import qualified Cardano.Ledger.Shelley.LedgerState as ShelleyLedger
import qualified Cardano.Ledger.Shelley.Rewards as Shelley
import qualified Cardano.Ledger.Shelley.RewardUpdate as Shelley
import qualified Cardano.Ledger.ShelleyMA.Rules.Utxo as MA
import qualified Cardano.Ledger.ShelleyMA.Timelocks as MA
import qualified Cardano.Ledger.TxIn as Ledger
import qualified Cardano.Prelude as CP
import qualified Cardano.Protocol.TPraos.BHeader as Protocol
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Short as Short
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Ouroboros.Consensus.Ledger.SupportsMempool (txId)
import qualified Ouroboros.Consensus.Ledger.SupportsMempool as SupportsMempool
import qualified Ouroboros.Consensus.Protocol.Ledger.HotKey as HotKey
import           Ouroboros.Consensus.Protocol.TPraos (TPraosCannotForge (..))
import           Ouroboros.Consensus.Shelley.Eras as Consensus (StandardAlonzo)
import qualified Ouroboros.Consensus.Shelley.Eras as Consensus
import           Ouroboros.Consensus.Shelley.Ledger hiding (TxId)
import           Ouroboros.Consensus.Shelley.Ledger.Inspect
import           Ouroboros.Consensus.Util.Condense (condense)
import           Ouroboros.Network.Block (blockHash, blockNo, blockSlot)
import           Ouroboros.Network.Point (WithOrigin, withOriginToMaybe)
import qualified Plutus.V1.Ledger.Api as Plutus
import qualified Plutus.V1.Ledger.Api as PV1
import qualified PlutusCore
import qualified PlutusCore.Core as Plutus
import qualified PlutusCore.DeBruijn
import qualified PlutusCore.Evaluation.Machine.ExBudget as Cek
import qualified PlutusCore.Evaluation.Machine.Exception as PlutusCore
import qualified PlutusTx.AssocMap as AssocMap
import qualified UntypedPlutusCore.Core.Type
import qualified UntypedPlutusCore.Evaluation.Machine.Cek.Internal as Cek

import           Prelude hiding (map, show, (.))

-- Orphan instances involved in the JSON output of the API queries.
-- We will remove/replace these as we provide more API wrapper types

-- deriving newtype instance ToJSON (Core.AuxiliaryDataHash StandardCrypto)
deriving newtype instance Core.Crypto crypto => ToJSON (Core.AuxiliaryDataHash crypto)

instance ToJSON (Mary.Value era) where
  toJSON = object . toMaryValuePairs
  toEncoding = Aeson.pairs . mconcat . toMaryValuePairs

toMaryValuePairs :: Aeson.KeyValue a => Mary.Value crypto -> [a]
toMaryValuePairs (Mary.Value !l !ps) =
  [ "lovelace" .= l
  , "policies" .= ps
  ]

instance ToJSONKey Mary.AssetName where
  toJSONKey = toJSONKeyText render
    where
      render = Text.decodeLatin1 . B16.encode . Short.fromShort . Mary.assetName

instance ToJSON (Mary.PolicyID era) where
  toJSON (Mary.PolicyID (Shelley.ScriptHash h)) = Aeson.String (hashToText h)

instance ToJSONKey (Mary.PolicyID era) where
  toJSONKey = toJSONKeyText render
    where
      render (Mary.PolicyID (Shelley.ScriptHash h)) = hashToText h

instance ToJSON Mary.AssetName where
  toJSON = Aeson.String . Text.decodeLatin1 . B16.encode . Short.fromShort . Mary.assetName

instance ToJSON Shelley.AccountState where
  toJSON = object . toAccountStatePairs
  toEncoding = Aeson.pairs . mconcat . toAccountStatePairs

toAccountStatePairs :: Aeson.KeyValue a => ShelleyLedger.AccountState -> [a]
toAccountStatePairs (Shelley.AccountState !tr !rs) =
  [ "treasury" .= tr
  , "reserves" .= rs
  ]

instance forall era.
         ( Consensus.ShelleyBasedEra era
         , ToJSON (Core.TxOut era)
         , ToJSON (Core.PParams era)
         , ToJSON (Core.PParamsDelta era)
         ) => ToJSON (Shelley.EpochState era) where
  toJSON = object . toEpochStatePairs
  toEncoding = Aeson.pairs . mconcat . toEpochStatePairs

toEpochStatePairs ::
  ( Consensus.ShelleyBasedEra era
  , ToJSON (Core.TxOut era)
  , ToJSON (Core.PParamsDelta era)
  , ToJSON (Core.PParams era)
  , Aeson.KeyValue a
  )
  => ShelleyLedger.EpochState era
  -> [a]
toEpochStatePairs eState =
  let !esAccountState = Shelley.esAccountState eState
      !esSnapshots = Shelley.esSnapshots eState
      !esLState = Shelley.esLState eState
      !esPrevPp = Shelley.esPrevPp eState
      !esPp = Shelley.esPp eState
      !esNonMyopic = Shelley.esNonMyopic eState
  in  [ "esAccountState" .= esAccountState
      , "esSnapshots" .= esSnapshots
      , "esLState" .= esLState
      , "esPrevPp" .= esPrevPp
      , "esPp" .= esPp
      , "esNonMyopic" .= esNonMyopic
      ]


instance ( Consensus.ShelleyBasedEra era
         , ToJSON (Core.TxOut era)
         , ToJSON (Core.PParamsDelta era)
         ) => ToJSON (Shelley.LedgerState era) where
  toJSON = object . toLedgerStatePairs
  toEncoding = Aeson.pairs . mconcat . toLedgerStatePairs

toLedgerStatePairs ::
  ( Consensus.ShelleyBasedEra era
  , ToJSON (Core.TxOut era)
  , ToJSON (Core.PParamsDelta era)
  , Aeson.KeyValue a
  ) => ShelleyLedger.LedgerState era -> [a]
toLedgerStatePairs lState =
  let !lsUTxOState = Shelley.lsUTxOState lState
      !lsDPState = Shelley.lsDPState lState
  in  [ "utxoState" .= lsUTxOState
      , "delegationState" .= lsDPState
      ]

instance Crypto.Crypto crypto => ToJSON (ShelleyLedger.IncrementalStake crypto) where
  toJSON = object . toIncrementalStakePairs
  toEncoding = Aeson.pairs . mconcat . toIncrementalStakePairs

toIncrementalStakePairs ::
  ( Aeson.KeyValue a
  , Crypto.Crypto crypto
  ) => ShelleyLedger.IncrementalStake crypto -> [a]
toIncrementalStakePairs iStake =
  let !credentials = Map.toList (ShelleyLedger.credMap iStake)
      !pointers = Map.toList (ShelleyLedger.ptrMap iStake)
  in  [ "credentials" .= credentials
      , "pointers" .= pointers
      ]

instance ( Consensus.ShelleyBasedEra era
         , ToJSON (Core.TxOut era)
         , ToJSON (Core.PParamsDelta era)
         ) => ToJSON (Shelley.UTxOState era) where
  toJSON = object . toUtxoStatePairs
  toEncoding = Aeson.pairs . mconcat . toUtxoStatePairs

toUtxoStatePairs ::
  ( Aeson.KeyValue a
  , Consensus.ShelleyBasedEra era
  , ToJSON (Core.TxOut era)
  , ToJSON (Control.State.Transition.State (Core.EraRule "PPUP" era))
  ) => ShelleyLedger.UTxOState era -> [a]
toUtxoStatePairs utxoState =
  let !utxo = Shelley._utxo utxoState
      !deposited = Shelley._deposited utxoState
      !fees = Shelley._fees utxoState
      !ppups = Shelley._ppups utxoState
      !stakeDistro = Shelley._stakeDistro utxoState
  in  [ "utxo" .= utxo
      , "deposited" .= deposited
      , "fees" .= fees
      , "ppups" .= ppups
      , "stake" .= stakeDistro
      ]

instance ( ToJSON (Core.PParamsDelta era)
         , Shelley.UsesPParams era
         ) => ToJSON (Shelley.PPUPState era) where
  toJSON = object . toPpupStatePairs
  toEncoding = Aeson.pairs . mconcat . toPpupStatePairs

toPpupStatePairs ::
  ( Aeson.KeyValue a
  , ToJSON (Core.PParamsDelta era)
  , Shelley.UsesPParams era
  ) => ShelleyLedger.PPUPState era -> [a]
toPpupStatePairs ppUpState =
  let !proposals = Shelley.proposals ppUpState
      !futureProposals = Shelley.futureProposals ppUpState
  in  [ "proposals" .= proposals
      , "futureProposals" .= futureProposals
      ]

instance ( ToJSON (Core.PParamsDelta era)
         , Shelley.UsesPParams era
         ) => ToJSON (Shelley.ProposedPPUpdates era) where
  toJSON (Shelley.ProposedPPUpdates ppUpdates) = toJSON $ Map.toList ppUpdates
  toEncoding (Shelley.ProposedPPUpdates ppUpdates) = toEncoding $ Map.toList ppUpdates

instance ToJSON (PParamsUpdate era) where
  toJSON pp =
    Aeson.object $
        [ "minFeeA"               .= x | x <- mbfield (Shelley._minfeeA pp) ]
     ++ [ "minFeeB"               .= x | x <- mbfield (Shelley._minfeeB pp) ]
     ++ [ "maxBlockBodySize"      .= x | x <- mbfield (Shelley._maxBBSize pp) ]
     ++ [ "maxTxSize"             .= x | x <- mbfield (Shelley._maxTxSize pp) ]
     ++ [ "maxBlockHeaderSize"    .= x | x <- mbfield (Shelley._maxBHSize pp) ]
     ++ [ "keyDeposit"            .= x | x <- mbfield (Shelley._keyDeposit pp) ]
     ++ [ "poolDeposit"           .= x | x <- mbfield (Shelley._poolDeposit pp) ]
     ++ [ "eMax"                  .= x | x <- mbfield (Shelley._eMax pp) ]
     ++ [ "nOpt"                  .= x | x <- mbfield (Shelley._nOpt pp) ]
     ++ [ "a0"                    .= x | x <- mbfield (Shelley._a0 pp) ]
     ++ [ "rho"                   .= x | x <- mbfield (Shelley._rho pp) ]
     ++ [ "tau"                   .= x | x <- mbfield (Shelley._tau pp) ]
     ++ [ "decentralisationParam" .= x | x <- mbfield (Shelley._d pp) ]
     ++ [ "extraEntropy"          .= x | x <- mbfield (Shelley._extraEntropy pp) ]
     ++ [ "protocolVersion"       .= x | x <- mbfield (Shelley._protocolVersion pp) ]
     ++ [ "minUTxOValue"          .= x | x <- mbfield (Shelley._minUTxOValue pp) ]
     ++ [ "minPoolCost"           .= x | x <- mbfield (Shelley._minPoolCost pp) ]

instance ToJSON (Babbage.PParamsUpdate era) where
  toJSON pp =
    Aeson.object $
        [ "minFeeA"               .= x | x <- mbfield (Babbage._minfeeA pp) ]
     ++ [ "minFeeB"               .= x | x <- mbfield (Babbage._minfeeB pp) ]
     ++ [ "maxBlockBodySize"      .= x | x <- mbfield (Babbage._maxBBSize pp) ]
     ++ [ "maxTxSize"             .= x | x <- mbfield (Babbage._maxTxSize pp) ]
     ++ [ "maxBlockHeaderSize"    .= x | x <- mbfield (Babbage._maxBHSize pp) ]
     ++ [ "keyDeposit"            .= x | x <- mbfield (Babbage._keyDeposit pp) ]
     ++ [ "poolDeposit"           .= x | x <- mbfield (Babbage._poolDeposit pp) ]
     ++ [ "eMax"                  .= x | x <- mbfield (Babbage._eMax pp) ]
     ++ [ "nOpt"                  .= x | x <- mbfield (Babbage._nOpt pp) ]
     ++ [ "a0"                    .= x | x <- mbfield (Babbage._a0 pp) ]
     ++ [ "rho"                   .= x | x <- mbfield (Babbage._rho pp) ]
     ++ [ "tau"                   .= x | x <- mbfield (Babbage._tau pp) ]
     ++ [ "protocolVersion"       .= x | x <- mbfield (Babbage._protocolVersion pp) ]
     ++ [ "minPoolCost"           .= x | x <- mbfield (Babbage._minPoolCost pp) ]
     ++ [ "coinsPerUTxOByte"      .= x | x <- mbfield (Babbage._coinsPerUTxOByte pp) ]
     ++ [ "costmdls"              .= x | x <- mbfield (Babbage._costmdls pp) ]
     ++ [ "prices"                .= x | x <- mbfield (Babbage._prices pp) ]
     ++ [ "maxTxExUnits"          .= x | x <- mbfield (Babbage._maxTxExUnits pp) ]
     ++ [ "maxBlockExUnits"       .= x | x <- mbfield (Babbage._maxBlockExUnits pp) ]
     ++ [ "maxValSize"            .= x | x <- mbfield (Babbage._maxValSize pp) ]
     ++ [ "collateralPercentage"  .= x | x <- mbfield (Babbage._collateralPercentage pp) ]
     ++ [ "maxCollateralInputs"   .= x | x <- mbfield (Babbage._maxCollateralInputs pp) ]

instance ToJSON (Babbage.PParams (Babbage.BabbageEra Consensus.StandardCrypto)) where
  toJSON pp =
    Aeson.object
      [ "minFeeA" .= Babbage._minfeeA pp
      , "minFeeB" .= Babbage._minfeeB pp
      , "maxBlockBodySize" .= Babbage._maxBBSize pp
      , "maxTxSize" .= Babbage._maxTxSize pp
      , "maxBlockHeaderSize" .= Babbage._maxBHSize pp
      , "keyDeposit" .= Babbage._keyDeposit pp
      , "poolDeposit" .= Babbage._poolDeposit pp
      , "eMax" .= Babbage._eMax pp
      , "nOpt" .= Babbage._nOpt pp
      , "a0" .= Babbage._a0 pp
      , "rho" .= Babbage._rho pp
      , "tau" .= Babbage._tau pp
      , "protocolVersion" .= Babbage._protocolVersion pp
      , "minPoolCost" .= Babbage._minPoolCost pp
      , "coinsPerUTxOByte" .= Babbage._coinsPerUTxOByte pp
      , "costmdls" .= Babbage._costmdls pp
      , "prices" .= Babbage._prices pp
      , "maxTxExUnits" .= Babbage._maxTxExUnits pp
      , "maxBlockExUnits" .= Babbage._maxBlockExUnits pp
      , "maxValSize" .= Babbage._maxValSize pp
      , "collateralPercentage" .= Babbage._collateralPercentage pp
      , "maxCollateralInputs" .= Babbage._maxCollateralInputs pp
      ]

mbfield :: StrictMaybe a -> [a]
mbfield SNothing  = []
mbfield (SJust x) = [x]

instance ( Ledger.Era era
         , ToJSON (Core.Value era)
         , ToJSON (Babbage.Datum era)
         , ToJSON (Core.Script era)
         , Ledger.Crypto era ~ Consensus.StandardCrypto
         ) => ToJSON (Babbage.TxOut era) where
  toJSON = object . toBabbageTxOutPairs
  toEncoding = Aeson.pairs . mconcat . toBabbageTxOutPairs

toBabbageTxOutPairs ::
  ( Aeson.KeyValue a
  , Ledger.Era era
  , ToJSON (Core.Value era)
  , ToJSON (Core.Script era)
  , Ledger.Crypto era ~ Consensus.StandardCrypto
  ) => Babbage.TxOut era -> [a]
toBabbageTxOutPairs (Babbage.TxOut !addr !val !dat !mRefScript) =
  [ "address" .= addr
  , "value" .= val
  , "datum" .= dat
  , "referenceScript" .= mRefScript
  ]

instance ( Ledger.Era era
         , Ledger.Crypto era ~ Consensus.StandardCrypto
         ) => ToJSON (Babbage.Datum era) where
  toJSON d =
    case Alonzo.datumDataHash d of
      SNothing -> Aeson.Null
      SJust dH -> toJSON $ ScriptDataHash dH
  toEncoding d =
    case Alonzo.datumDataHash d of
      SNothing -> toEncoding Aeson.Null
      SJust dH -> toEncoding $ ScriptDataHash dH

instance ToJSON (Alonzo.Script (Babbage.BabbageEra Consensus.StandardCrypto)) where
  toJSON = Aeson.String . Text.decodeUtf8 . B16.encode . CBOR.serialize'

-- instance ToJSON (Alonzo.Script (Babbage.BabbageEra Consensus.StandardCrypto)) where
--   toJSON s = Aeson.String . serialiseToRawBytesHexText
--                $ Api.ScriptHash $ Ledger.hashScript @(Babbage.BabbageEra Consensus.StandardCrypto) s

instance Crypto.Crypto crypto => ToJSON (Shelley.DPState crypto) where
  toJSON = object . toDpStatePairs
  toEncoding = Aeson.pairs . mconcat . toDpStatePairs

toDpStatePairs ::
  ( Aeson.KeyValue a
  , Crypto.Crypto crypto
  ) => ShelleyLedger.DPState crypto -> [a]
toDpStatePairs dpState =
  let !dstate = Shelley.dpsDState dpState
      !pstate = Shelley.dpsPState dpState
  in  [ "dstate" .= dstate
      , "pstate" .= pstate
      ]

instance (ToJSON coin, ToJSON ptr, ToJSON pool) => ToJSON (Trip coin ptr pool) where
  toJSON = object . toTripPair
  toEncoding = Aeson.pairs . mconcat . toTripPair

toTripPair ::
  ( Aeson.KeyValue a
  , ToJSON coin
  , ToJSON ptr
  , ToJSON pool
  ) => Trip coin ptr pool -> [a]
toTripPair (Triple !coin !ptr !pool) =
  [ "coin" .= coin
  , "ptr" .= ptr
  , "pool" .= pool
  ]

instance Crypto.Crypto crypto => ToJSON (UnifiedMap crypto) where
  toJSON = object . toUnifiedMapPair
  toEncoding = Aeson.pairs . mconcat . toUnifiedMapPair

toUnifiedMapPair ::
  ( Aeson.KeyValue a
  , ToJSON coin
  , ToJSON ptr
  , ToJSON pool
  , ToJSON cred
  , ToJSONKey cred
  , ToJSONKey ptr
  ) => UMap coin cred pool ptr -> [a]
toUnifiedMapPair (UnifiedMap !m1 !m2) =
  [ "credentials" .= m1
  , "pointers" .= m2
  ]

instance Crypto.Crypto crypto => ToJSON (Shelley.DState crypto) where
  toJSON = object . toDStatePair
  toEncoding = Aeson.pairs . mconcat . toDStatePair

toDStatePair ::
  ( Aeson.KeyValue a
  , Crypto.Crypto crypto
  ) => ShelleyLedger.DState crypto -> [a]
toDStatePair dState =
  let !unifiedRewards = Shelley._unified dState
      !fGenDelegs = Map.toList (Shelley._fGenDelegs dState)
      !genDelegs = Shelley._genDelegs dState
      !irwd = Shelley._irwd dState
  in  [ "unifiedRewards" .= unifiedRewards
      , "fGenDelegs" .= fGenDelegs
      , "genDelegs" .= genDelegs
      , "irwd" .= irwd
      ]

instance Crypto.Crypto crypto => ToJSON (ShelleyLedger.FutureGenDeleg crypto) where
  toJSON fGenDeleg =
    object [ "fGenDelegSlot" .= ShelleyLedger.fGenDelegSlot fGenDeleg
           , "fGenDelegGenKeyHash" .= ShelleyLedger.fGenDelegGenKeyHash fGenDeleg
           ]

instance Crypto.Crypto crypto => ToJSON (Shelley.GenDelegs crypto) where
  toJSON (Shelley.GenDelegs delegs) = toJSON delegs
  toEncoding (Shelley.GenDelegs delegs) = toEncoding delegs

instance Crypto.Crypto crypto => ToJSON (Shelley.InstantaneousRewards crypto) where
  toJSON = object . toInstantaneousRewardsPair
  toEncoding = Aeson.pairs . mconcat . toInstantaneousRewardsPair

toInstantaneousRewardsPair ::
  ( Aeson.KeyValue a
  , Crypto.Crypto crypto
  ) => ShelleyLedger.InstantaneousRewards crypto -> [a]
toInstantaneousRewardsPair iRwds =
  let !iRReserves = Shelley.iRReserves iRwds
      !iRTreasury = Shelley.iRTreasury iRwds
  in  [ "iRReserves" .= iRReserves
      , "iRTreasury" .= iRTreasury
      ]

instance
  Crypto.Crypto crypto =>
  ToJSON (Bimap Shelley.Ptr (Shelley.Credential Shelley.Staking crypto))
  where
  toJSON = object . toPtrCredentialStakingPair
  toEncoding = Aeson.pairs . mconcat . toPtrCredentialStakingPair

toPtrCredentialStakingPair ::
  ( Aeson.KeyValue a
  , Crypto.Crypto crypto
  ) => Bimap Shelley.Ptr (Shelley.Credential Shelley.Staking crypto) -> [a]
toPtrCredentialStakingPair (MkBiMap ptsStakeM stakePtrSetM) =
  let !stakedCreds = Map.toList ptsStakeM
      !credPtrR = stakePtrSetM
  in  [ "stakedCreds" .= stakedCreds
      , "credPtrR" .= credPtrR
      ]

deriving newtype instance ToJSON Shelley.CertIx
deriving newtype instance ToJSON Shelley.TxIx

instance ToJSON Shelley.Ptr where
  toJSON = object . toPtrPair
  toEncoding = Aeson.pairs . mconcat . toPtrPair

instance ToJSONKey Shelley.Ptr

toPtrPair :: Aeson.KeyValue a => Shelley.Ptr -> [a]
toPtrPair (Shelley.Ptr !slotNo !txIndex !certIndex) =
  [ "slot" .= unSlotNo slotNo
  , "txIndex" .= txIndex
  , "certIndex" .= certIndex
  ]


instance Crypto.Crypto crypto => ToJSON (Shelley.PState crypto) where
  toJSON = object . toPStatePair
  toEncoding = Aeson.pairs . mconcat . toPStatePair

toPStatePair ::
  ( Aeson.KeyValue a
  , Crypto.Crypto crypto
  ) => ShelleyLedger.PState crypto -> [a]
toPStatePair pState =
  let !pParams = Shelley._pParams pState
      !fPParams = Shelley._fPParams pState
      !retiring = Shelley._retiring pState
  in  [ "pParams pState" .= pParams
      , "fPParams pState" .= fPParams
      , "retiring pState" .= retiring
      ]

instance ( Consensus.ShelleyBasedEra era
         , ToJSON (Core.TxOut era)
         ) => ToJSON (Shelley.UTxO era) where
  toJSON (Shelley.UTxO utxo) = toJSON utxo
  toEncoding (Shelley.UTxO utxo) = toEncoding utxo

instance ( Consensus.ShelleyBasedEra era
         , ToJSON (Core.Value era)
         ) => ToJSON (Shelley.TxOut era) where
  toJSON = object . toTxOutPair
  toEncoding = Aeson.pairs . mconcat . toTxOutPair

toTxOutPair ::
  ( Ledger.Era era
  , Aeson.KeyValue a
  , ToJSON (Core.Value era)
  , Show (Core.Value era))
  => Shelley.TxOut era -> [a]
toTxOutPair (Shelley.TxOut !addr !amount) =
  [ "address" .= addr
  , "amount" .= amount
  ]

instance Crypto.Crypto crypto => ToJSON (Shelley.TxIn crypto) where
  toJSON = toJSON . txInToText
  toEncoding = toEncoding . txInToText

instance Crypto.Crypto crypto => ToJSONKey (Shelley.TxIn crypto) where
  toJSONKey = toJSONKeyText txInToText

txInToText :: Shelley.TxIn crypto -> Text
txInToText (Shelley.TxIn (Shelley.TxId txidHash) ix) =
  hashToText (SafeHash.extractHash txidHash)
    <> Text.pack "#"
    <> Text.pack (show ix)

hashToText :: Crypto.Hash crypto a -> Text
hashToText = Text.decodeLatin1 . Crypto.hashToBytesAsHex

instance Crypto.Crypto crypto => ToJSON (Shelley.NonMyopic crypto) where
  toJSON = object . toNonMyopicPair
  toEncoding = Aeson.pairs . mconcat . toNonMyopicPair

toNonMyopicPair ::
  ( Aeson.KeyValue a
  , Crypto.Crypto crypto
  ) => Shelley.NonMyopic crypto -> [a]
toNonMyopicPair nonMy =
  let !likelihoodsNM = Shelley.likelihoodsNM nonMy
      !rewardPotNM = Shelley.rewardPotNM nonMy
  in  [ "likelihoodsNM" .= likelihoodsNM
      , "rewardPotNM" .= rewardPotNM
      ]

instance ToJSON Shelley.Likelihood where
  toJSON (Shelley.Likelihood llhd) =
    toJSON $ fmap (\(Shelley.LogWeight f) -> exp $ realToFrac f :: Double) llhd
  toEncoding (Shelley.Likelihood llhd) =
    toEncoding $ fmap (\(Shelley.LogWeight f) -> exp $ realToFrac f :: Double) llhd

instance Crypto.Crypto crypto => ToJSON (Shelley.SnapShots crypto) where
  toJSON = object . toSnapShotsPair
  toEncoding = Aeson.pairs . mconcat . toSnapShotsPair

toSnapShotsPair ::
  ( Aeson.KeyValue a
  , Crypto.Crypto crypto
  ) => ShelleyEpoch.SnapShots crypto -> [a]
toSnapShotsPair ss =
  let !pstakeMark = Shelley._pstakeMark ss
      !pstakeSet = Shelley._pstakeSet ss
      !pstakeGo = Shelley._pstakeGo ss
      !feeSS = Shelley._feeSS ss
  in  [ "pstakeMark" .= pstakeMark
      , "pstakeSet" .= pstakeSet
      , "pstakeGo" .= pstakeGo
      , "feeSS" .= feeSS
      ]

instance Crypto.Crypto crypto => ToJSON (Shelley.SnapShot crypto) where
  toJSON = object . toSnapShotPair
  toEncoding = Aeson.pairs . mconcat . toSnapShotPair

toSnapShotPair ::
  ( Aeson.KeyValue a
  , Crypto.Crypto crypto
  ) => ShelleyEpoch.SnapShot crypto -> [a]
toSnapShotPair ss =
  let !stake = Shelley._stake ss
      !delegations = ShelleyEpoch._delegations ss
      !poolParams = Shelley._poolParams ss
  in  [ "stake" .= stake
      , "delegations" .= delegations
      , "poolParams" .= poolParams
      ]

instance Crypto.Crypto crypto => ToJSON (Shelley.Stake crypto) where
  toJSON (Shelley.Stake s) = toJSON s
  toEncoding (Shelley.Stake s) = toEncoding s

instance Crypto.Crypto crypto => ToJSON (Shelley.RewardUpdate crypto) where
  toJSON = object . toRewardUpdatePair
  toEncoding = Aeson.pairs . mconcat . toRewardUpdatePair

toRewardUpdatePair ::
  ( Aeson.KeyValue a
  , Crypto.Crypto crypto
  ) => Shelley.RewardUpdate crypto -> [a]
toRewardUpdatePair rUpdate =
  let !deltaT = Shelley.deltaT rUpdate
      !deltaR = Shelley.deltaR rUpdate
      !rs = Shelley.rs rUpdate
      !deltaF = Shelley.deltaF rUpdate
      !nonMyopic = Shelley.nonMyopic rUpdate
  in  [ "deltaT" .= deltaT
      , "deltaR" .= deltaR
      , "rs" .= rs
      , "deltaF" .= deltaF
      , "nonMyopic" .= nonMyopic
      ]

instance Crypto.Crypto crypto => ToJSON (Shelley.PulsingRewUpdate crypto) where
  toJSON  = \case
    Shelley.Pulsing _ _ -> Aeson.Null
    Shelley.Complete ru -> toJSON ru
  toEncoding  = \case
    Shelley.Pulsing _ _ -> toEncoding Aeson.Null
    Shelley.Complete ru -> toEncoding ru

instance ToJSON Shelley.DeltaCoin where
  toJSON (Shelley.DeltaCoin i) = toJSON i
  toEncoding (Shelley.DeltaCoin i) = toEncoding i

instance Crypto.Crypto crypto => ToJSON (Ledger.PoolDistr crypto) where
  toJSON (Ledger.PoolDistr m) = toJSON m
  toEncoding (Ledger.PoolDistr m) = toEncoding m

instance Crypto.Crypto crypto => ToJSON (Ledger.IndividualPoolStake crypto) where
  toJSON = object . toIndividualPoolStakePair
  toEncoding = Aeson.pairs . mconcat . toIndividualPoolStakePair

toIndividualPoolStakePair ::
  ( Aeson.KeyValue a
  , Crypto.HashAlgorithm (Crypto.HASH crypto)
  ) => Ledger.IndividualPoolStake crypto -> [a]
toIndividualPoolStakePair indivPoolStake =
  let !individualPoolStake = Ledger.individualPoolStake indivPoolStake
      !individualPoolStakeVrf = Ledger.individualPoolStakeVrf indivPoolStake
  in  [ "individualPoolStake" .= individualPoolStake
      , "individualPoolStakeVrf" .= individualPoolStakeVrf
      ]

instance Crypto.Crypto crypto => ToJSON (Shelley.Reward crypto) where
  toJSON = object . toRewardPair
  toEncoding = Aeson.pairs . mconcat . toRewardPair

toRewardPair ::
  ( Aeson.KeyValue a
  , Crypto.Crypto crypto
  ) => Shelley.Reward crypto -> [a]
toRewardPair reward =
  let !rewardType = Shelley.rewardType reward
      !rewardPool = Shelley.rewardPool reward
      !rewardAmount = Shelley.rewardAmount reward
  in  [ "rewardType" .= rewardType
      , "rewardPool" .= rewardPool
      , "rewardAmount" .= rewardAmount
      ]

instance ToJSON Shelley.RewardType where
  toJSON Shelley.MemberReward = "MemberReward"
  toJSON Shelley.LeaderReward = "LeaderReward"

instance Crypto.Crypto c => ToJSON (SafeHash.SafeHash c a) where
  toJSON = toJSON . SafeHash.extractHash
  toEncoding = toEncoding . SafeHash.extractHash

-----

deriving newtype instance ToJSON SystemStart
deriving newtype instance FromJSON SystemStart


instance Crypto.Crypto crypto => ToJSON (VMap VB VB (Shelley.Credential 'Shelley.Staking crypto) (Shelley.KeyHash 'Shelley.StakePool crypto)) where
  toJSON = toJSON . VMap.toMap
  toEncoding = toEncoding . VMap.toMap

instance Crypto.Crypto crypto => ToJSON (VMap VB VB (Shelley.KeyHash    'Shelley.StakePool crypto) (Shelley.PoolParams crypto)) where
  toJSON = toJSON . VMap.toMap
  toEncoding = toEncoding . VMap.toMap

instance Crypto.Crypto crypto => ToJSON (VMap VB VP (Shelley.Credential 'Shelley.Staking   crypto) (Shelley.CompactForm Shelley.Coin)) where
  toJSON = toJSON . fmap fromCompact . VMap.toMap
  toEncoding = toEncoding . fmap fromCompact . VMap.toMap

instance ToJSON (PredicateFailure (Core.EraRule "LEDGER" era)) => ToJSON (ApplyTxError era) where
  toJSON (ApplyTxError es) = toJSON es

instance
  ( ShelleyBasedEra era
  , ToJSON (Core.Tx era)
  , ToJSON (TxId (Ledger.Crypto era))
  ) => ToJSON (GenTx (ShelleyBlock protocol era)) where
  toJSON tx = object [ "txid" .= Text.take 8 (renderTxId (txId tx)) ]

instance ToJSON (SupportsMempool.TxId (GenTx (ShelleyBlock protocol era))) where
  toJSON = String . Text.take 8 . renderTxId

instance
  ( ShelleyCompatible protocol era
  , ShelleyBasedEra era
  , ToJSON (ShelleyHash (Ledger.Crypto era))
  , ToJSON (Protocol.BHeader (Ledger.Crypto era))
  ) => ToJSON (Header (ShelleyBlock protocol era)) where
  toJSON b = object
    [ "kind"      .= String "ShelleyBlock"
    , "hash"      .= do condense (blockHash b)  :: String
    , "slotNo"    .= do condense (blockSlot b)  :: String
    , "blockNo"   .= do condense (blockNo b)    :: String
    -- , "delegate"  .= condense (headerSignerVk h)
    ]

instance Core.Crypto crypto => ToJSON (TPraosCannotForge crypto) where
  toJSON (TPraosCannotForgeKeyNotUsableYet wallClockPeriod keyStartPeriod) = object
    [ "kind"      .= String "TPraosCannotForgeKeyNotUsableYet"
    , "keyStart"  .= do keyStartPeriod  :: KESPeriod
    , "wallClock" .= do wallClockPeriod :: KESPeriod
    ]
  toJSON (TPraosCannotForgeWrongVRF genDlgVRFHash coreNodeVRFHash) = object
    [ "kind"      .= String "TPraosCannotLeadWrongVRF"
    , "expected"  .= do genDlgVRFHash   :: Crypto.Hash (Crypto.HASH crypto) (VerKeyVRF crypto)
    , "actual"    .= do coreNodeVRFHash :: Crypto.Hash (Crypto.HASH crypto) (VerKeyVRF crypto)
    ]

deriving newtype instance ToJSON KESPeriod

instance ToJSON HotKey.KESInfo where
  toJSON HotKey.KESInfo { kesStartPeriod, kesEndPeriod, kesEvolution } = object
    [ "kind"        .= String "KESInfo"
    , "startPeriod" .= do kesStartPeriod  :: KESPeriod
    , "endPeriod"   .= do kesEndPeriod    :: KESPeriod
    , "evolution"   .= do kesEvolution    :: Word
    ]

instance ToJSON HotKey.KESEvolutionError where
  toJSON (HotKey.KESCouldNotEvolve kesInfo targetPeriod) = object
    [ "kind"          .= String "KESCouldNotEvolve"
    , "kesInfo"       .= do kesInfo       :: HotKey.KESInfo
    , "targetPeriod"  .= do targetPeriod  :: KESPeriod
    ]
  toJSON (HotKey.KESKeyAlreadyPoisoned kesInfo targetPeriod) = object
    [ "kind"          .= String "KESKeyAlreadyPoisoned"
    , "kesInfo"       .= do kesInfo       :: HotKey.KESInfo
    , "targetPeriod"  .= do targetPeriod  :: KESPeriod
    ]

instance
  ( ShelleyBasedEra era
  , ToJSON (PredicateFailure (UTXO era))
  , ToJSON (PredicateFailure (UTXOW era))
  , ToJSON (PredicateFailure (Core.EraRule "BBODY" era))
  , ToJSON (BlockTransitionError era)
  ) => ToJSON (ShelleyLedgerError era) where
  toJSON (BBodyError (BlockTransitionError fs)) = object
    [ "kind"      .= String "BBodyError"
    , "failures"  .= do map toJSON fs  :: [Value]
    ]

instance
  ( ShelleyBasedEra era
  , ToJSON (Ledger.PParamsDelta era)
  , ToJSON (Crypto.OutputVRF (Core.VRF (Ledger.Crypto era)))
  ) => ToJSON (ShelleyLedgerUpdate era) where
  toJSON (ShelleyUpdatedProtocolUpdates updates) = object
    [ "kind"    .= String "ShelleyUpdatedProtocolUpdates"
    , "updates" .= do map toJSON updates :: [Value]
    ]

instance
  ( Ledger.Era era, ToJSON (Ledger.PParamsDelta era)
  , ToJSON (Crypto.OutputVRF (Core.VRF (Ledger.Crypto era)))
  ) => ToJSON (ProtocolUpdate era) where
  toJSON ProtocolUpdate{protocolUpdateProposal, protocolUpdateState} = object
    [ "proposal" .= do protocolUpdateProposal :: UpdateProposal era
    , "state"    .= do protocolUpdateState    :: UpdateState (Consensus.EraCrypto era)
    ]

instance ToJSON (Ledger.PParamsDelta era)
         => ToJSON (UpdateProposal era) where
  toJSON UpdateProposal{proposalParams, proposalVersion, proposalEpoch} = object
    [ "params"  .= do proposalParams  :: Ledger.PParamsDelta era
    , "version" .= do proposalVersion :: Maybe ProtVer
    , "epoch"   .= do proposalEpoch   :: EpochNo
    ]

instance
  ( Core.Crypto crypto
  , ToJSON (Crypto.OutputVRF (Core.VRF crypto))
  ) => ToJSON (UpdateState crypto) where
  toJSON UpdateState{proposalVotes, proposalReachedQuorum} = object
    [ "proposal"      .= do proposalVotes         :: [KeyHash 'Genesis crypto]
    , "reachedQuorum" .= do proposalReachedQuorum :: Bool
    ]

instance
  ( Core.Crypto crypto
  , ToJSON (Crypto.CertifiedVRF (Core.VRF crypto) Nonce)
  , ToJSON (Crypto.OutputVRF (Core.VRF crypto))
  , ToJSON Ledger.ActiveSlotCoeff
  ) => ToJSON (ChainTransitionError crypto) where
  toJSON (ChainTransitionError fs) = object
    [ "kind"      .= String "ChainTransitionError"
    , "failures"  .= do map toJSON fs :: [Value]
    ]

instance ToJSON ChainPredicateFailure where
  toJSON (HeaderSizeTooLargeCHAIN hdrSz maxHdrSz) = object
    [ "kind"          .= String "HeaderSizeTooLarge"
    , "headerSize"    .= do hdrSz     :: Natural
    , "maxHeaderSize" .= do maxHdrSz  :: Natural
    ]
  toJSON (BlockSizeTooLargeCHAIN blkSz maxBlkSz) = object
    [ "kind"          .= String "BlockSizeTooLarge"
    , "blockSize"     .= do blkSz     :: Natural
    , "maxBlockSize"  .= do maxBlkSz  :: Natural
    ]
  toJSON (ObsoleteNodeCHAIN currentPtcl supportedPtcl) = object
    [ "kind"              .= String "ObsoleteNode"
    , "explanation"       .= String explanation
    , "currentProtocol"   .= do currentPtcl   :: Natural
    , "supportedProtocol" .= do supportedPtcl :: Natural
    ]
      where
        explanation = "A scheduled major protocol version change (hard fork) \
                      \has taken place on the chain, but this node does not \
                      \understand the new major protocol version. This node \
                      \must be upgraded before it can continue with the new \
                      \protocol version."

instance
  ( ToJSON (Protocol.PrevHash crypto)
  , ToJSON (WithOrigin (LastAppliedBlock crypto))
  , ToJSON BlockNo
  ) => ToJSON (PrtlSeqFailure crypto) where
  toJSON (WrongSlotIntervalPrtclSeq (SlotNo lastSlot) (SlotNo currSlot)) = object
    [ "kind"        .= String "WrongSlotInterval"
    , "lastSlot"    .= do lastSlot :: Word64
    , "currentSlot" .= do currSlot :: Word64
    ]
  toJSON (WrongBlockNoPrtclSeq lab currentBlockNo) = object
    [ "kind"                .= String "WrongBlockNo"
    , "lastAppliedBlockNo"  .= do showLastAppBlockNo lab :: Text
    , "currentBlockNo"      .= (String . textShow $ unBlockNo currentBlockNo)
    ]
  toJSON (WrongBlockSequencePrtclSeq lastAppliedHash currentHash) = object
    [ "kind"                  .= String "WrongBlockSequence"
    , "lastAppliedBlockHash"  .= String (textShow lastAppliedHash)
    , "currentBlockHash"      .= String (textShow currentHash)
    ]

instance
  ( ShelleyBasedEra era
  , ToJSON (PredicateFailure (UTXO era))
  , ToJSON (PredicateFailure (UTXOW era))
  , ToJSON (PredicateFailure (Core.EraRule "LEDGER" era))
  , ToJSON (PredicateFailure (Core.EraRule "LEDGERS" era))
  ) => ToJSON (BbodyPredicateFailure era) where
  toJSON (WrongBlockBodySizeBBODY actualBodySz claimedBodySz) = object
    [ "kind"                  .= String "WrongBlockBodySizeBBODY"
    , "actualBlockBodySize"   .= do actualBodySz  :: Int
    , "claimedBlockBodySize"  .= do claimedBodySz :: Int
    ]
  toJSON (InvalidBodyHashBBODY actualHash claimedHash) = object
    [ "kind"            .= String "InvalidBodyHashBBODY"
    , "actualBodyHash"  .= do textShow actualHash   :: Text
    , "claimedBodyHash" .= do textShow claimedHash  :: Text
    ]
  toJSON (LedgersFailure f) = toJSON f


instance
  ( ShelleyBasedEra era
  , ToJSON (PredicateFailure (UTXO era))
  , ToJSON (PredicateFailure (UTXOW era))
  , ToJSON (PredicateFailure (Core.EraRule "LEDGER" era))
  ) => ToJSON (LedgersPredicateFailure era) where
  toJSON (LedgerFailure f) = object
    [ "kind"  .= String "LedgerFailure"
    , "value" .= do f :: LedgerPredicateFailure era
    ]

instance
  ( ShelleyBasedEra era
  , ToJSON (PredicateFailure (UTXO era))
  , ToJSON (PredicateFailure (UTXOW era))
  , ToJSON (PredicateFailure (Core.EraRule "DELEGS" era))
  , ToJSON (PredicateFailure (Core.EraRule "UTXOW" era))
  ) => ToJSON (LedgerPredicateFailure era) where
  toJSON (UtxowFailure f) = object
    [ "kind"  .= String "UtxowFailure"
    , "value" .= do f :: PredicateFailure (Ledger.EraRule "UTXOW" era)
    ]
  toJSON (DelegsFailure f) = object
    [ "kind"  .= String "DelegsFailure"
    , "value" .= do f :: PredicateFailure (Ledger.EraRule "DELEGS" era)
    ]

instance ToJSON (Alonzo.ScriptPurpose StandardCrypto) where
  toJSON = \case
    Alonzo.Minting pid -> object
      [ "minting" .= toJSON pid
      ]
    Alonzo.Spending txin -> object
      [ "spending" .= Api.fromShelleyTxIn txin
      ]
    Alonzo.Rewarding rwdAcct -> object
      [ "rewarding" .= String (Api.serialiseAddress $ Api.fromShelleyStakeAddr rwdAcct)
      ]
    Alonzo.Certifying cert -> object
      [ "certifying" .= toJSON (Api.textEnvelopeDefaultDescr $ Api.fromShelleyCertificate cert)
      ]

instance ToJSONKey (Shelley.ScriptHash StandardCrypto) where
  toJSONKey = contramap (Api.serialiseToRawBytesHexText . Api.ScriptHash) toJSONKey

instance
  ( ToJSON (Core.AuxiliaryDataHash StandardCrypto)
  ) => ToJSON (UtxowPredicateFail (Alonzo.AlonzoEra StandardCrypto)) where
  toJSON (WrappedShelleyEraFailure utxoPredFail) = toJSON utxoPredFail
  toJSON (MissingRedeemers scripts) = object
    [ "kind"    .= String "MissingRedeemers"
    , "scripts" .= do Map.fromList $ fmap swap scripts :: Map (Shelley.ScriptHash StandardCrypto) (Alonzo.ScriptPurpose StandardCrypto)
    ]
  toJSON (MissingRequiredDatums required received) = object
    [ "kind"      .= String "MissingRequiredDatums"
    , "required"  .= do map (Crypto.hashToTextAsHex . SafeHash.extractHash) (Set.toList required) :: [Text]
    , "received"  .= do map (Crypto.hashToTextAsHex . SafeHash.extractHash) (Set.toList received) :: [Text]
    ]
  toJSON (PPViewHashesDontMatch ppHashInTxBody ppHashFromPParams) = object
    [ "kind"        .= String "PPViewHashesDontMatch"
    , "fromTxBody"  .= do strictMaybeToMaybe ppHashInTxBody    :: Maybe (Alonzo.ScriptIntegrityHash StandardCrypto)
    , "fromPParams" .= do strictMaybeToMaybe ppHashFromPParams :: Maybe (Alonzo.ScriptIntegrityHash StandardCrypto)
    ]
  toJSON (MissingRequiredSigners missingKeyWitnesses) = object
    [ "kind"      .= String "MissingRequiredSigners"
    , "witnesses" .= do Set.toList missingKeyWitnesses :: [KeyHash 'Witness StandardCrypto]
    ]
  toJSON (UnspendableUTxONoDatumHash txins) = object
    [ "kind"  .= String "MissingRequiredSigners"
    , "txins" .= do Set.toList txins :: [TxIn StandardCrypto]
    ]
  toJSON (NonOutputSupplimentaryDatums disallowed acceptable) = object
    [ "kind"        .= String "NonOutputSupplimentaryDatums"
    , "disallowed"  .= do Set.toList disallowed :: [Ledger.DataHash StandardCrypto]
    , "acceptable"  .= do Set.toList acceptable :: [Ledger.DataHash StandardCrypto]
    ]
  toJSON (ExtraRedeemers rdmrs) = object
    [ "kind"  .= String "ExtraRedeemers"
    , "rdmrs" .= do map (Api.renderScriptWitnessIndex . Api.fromAlonzoRdmrPtr) rdmrs :: [String]
    ]

instance
  ( ShelleyBasedEra era
  , ToJSON (PredicateFailure (UTXO era))
  , ToJSON (PredicateFailure (Core.EraRule "UTXO" era))
  , ToJSON (Core.AuxiliaryDataHash (Ledger.Crypto era))
  ) => ToJSON (UtxowPredicateFailure era) where
  toJSON (ExtraneousScriptWitnessesUTXOW extraneousScripts) = object
    [ "kind"              .= String "InvalidWitnessesUTXOW"
    , "extraneousScripts" .= do extraneousScripts :: Set (Shelley.ScriptHash (Ledger.Crypto era))
    ]
  toJSON (InvalidWitnessesUTXOW wits') = object
    [ "kind"              .= String "InvalidWitnessesUTXOW"
    , "invalidWitnesses"  .= do map textShow wits' :: [Text]
    ]
  toJSON (MissingVKeyWitnessesUTXOW wits') = object
    [ "kind"              .= String "MissingVKeyWitnessesUTXOW"
    , "missingWitnesses"  .= do wits' :: Set (KeyHash 'Witness (Ledger.Crypto era))
    ]
  toJSON (MissingScriptWitnessesUTXOW missingScripts) = object
    [ "kind"            .= String "MissingScriptWitnessesUTXOW"
    , "missingScripts"  .= do missingScripts :: Set (Shelley.ScriptHash (Ledger.Crypto era))
    ]
  toJSON (ScriptWitnessNotValidatingUTXOW failedScripts) = object
    [ "kind"          .= String "ScriptWitnessNotValidatingUTXOW"
    , "failedScripts" .= do failedScripts :: Set (Shelley.ScriptHash (Ledger.Crypto era))
    ]
  toJSON (UtxoFailure f) = toJSON f
  toJSON (MIRInsufficientGenesisSigsUTXOW genesisSigs) = object
    [ "kind"        .= String "MIRInsufficientGenesisSigsUTXOW"
    , "genesisSigs" .= do genesisSigs :: Set (KeyHash 'Witness (Ledger.Crypto era))
    ]
  toJSON (MissingTxBodyMetadataHash metadataHash) = object
    [ "kind"          .= String "MissingTxBodyMetadataHash"
    , "metadataHash"  .= do metadataHash :: Core.AuxiliaryDataHash (Ledger.Crypto era)
    ]
  toJSON (MissingTxMetadata txBodyMetadataHash) = object
    [ "kind"                .= String "MissingTxMetadata"
    , "txBodyMetadataHash"  .= do txBodyMetadataHash :: Core.AuxiliaryDataHash (Ledger.Crypto era)
    ]
  toJSON (ConflictingMetadataHash txBodyMetadataHash fullMetadataHash) = object
    [ "kind"                .= String "ConflictingMetadataHash"
    , "txBodyMetadataHash"  .= do txBodyMetadataHash  :: Core.AuxiliaryDataHash (Ledger.Crypto era)
    , "fullMetadataHash"    .= do fullMetadataHash    :: Core.AuxiliaryDataHash (Ledger.Crypto era)
    ]
  toJSON InvalidMetadata = object
    [ "kind"  .= String "InvalidMetadata"
    ]

instance
  ( ShelleyBasedEra era
  , ToJSON (Core.Value era)
  , ToJSON (Core.TxOut era)
  , ToJSON (PredicateFailure (Core.EraRule "PPUP" era))
  ) => ToJSON (UtxoPredicateFailure era) where
  toJSON (BadInputsUTxO badInputs) = object
    [ "kind"      .= String "BadInputsUTxO"
    , "badInputs" .= do badInputs :: Set (TxIn (Ledger.Crypto era))
    , "error"     .= renderBadInputsUTxOErr badInputs
    ]
  toJSON (ExpiredUTxO ttl slot) = object
    [ "kind" .= String "ExpiredUTxO"
    , "ttl"  .= do ttl  :: SlotNo
    , "slot" .= do slot :: SlotNo
    ]
  toJSON (MaxTxSizeUTxO txsize maxtxsize) = object
    [ "kind"    .= String "MaxTxSizeUTxO"
    , "size"    .= do txsize    :: Integer
    , "maxSize" .= do maxtxsize :: Integer
    ]
  -- TODO: Add the minimum allowed UTxO value to OutputTooSmallUTxO
  toJSON (OutputTooSmallUTxO badOutputs) = object
    [ "kind"    .= String "OutputTooSmallUTxO"
    , "outputs" .= do badOutputs :: [Ledger.TxOut era]
    , "error"   .= String "The output is smaller than the allow minimum UTxO value defined in the protocol parameters"
    ]
  toJSON (OutputBootAddrAttrsTooBig badOutputs) = object
    [ "kind"    .= String "OutputBootAddrAttrsTooBig"
    , "outputs" .= do badOutputs :: [Ledger.TxOut era]
    , "error"   .= String "The Byron address attributes are too big"
    ]
  toJSON InputSetEmptyUTxO = object
    [ "kind" .= String "InputSetEmptyUTxO"
    ]
  toJSON (FeeTooSmallUTxO minfee txfee) = object
    [ "kind"    .= String "FeeTooSmallUTxO"
    , "minimum" .= do minfee  :: Coin
    , "fee"     .= do txfee   :: Coin
    ]
  toJSON (ValueNotConservedUTxO consumed produced) = object
    [ "kind"      .= String "ValueNotConservedUTxO"
    , "consumed"  .= do consumed  :: Ledger.Value era
    , "produced"  .= do produced  :: Ledger.Value era
    , "error"     .= renderValueNotConservedErr consumed produced
    ]
  toJSON (UpdateFailure f) = object
    [ "kind"  .= String "UpdateFailure"
    , "value" .= do f :: PredicateFailure (Ledger.EraRule "PPUP" era)
    ]
  toJSON (WrongNetwork network addrs) = object
    [ "kind"    .= String "WrongNetwork"
    , "network" .= do network :: Network
    , "addrs"   .= do addrs   :: Set (Addr (Ledger.Crypto era))
    ]
  toJSON (WrongNetworkWithdrawal network addrs) = object
    [ "kind"    .= String "WrongNetworkWithdrawal"
    , "network" .= do network :: Network
    , "addrs"   .= do addrs   :: Set (RewardAcnt (Ledger.Crypto era))
    ]

instance ToJSON MA.ValidityInterval where
  toJSON vi = object $
        [ "invalidBefore"    .= x | x <- mbfield' (MA.invalidBefore    vi) ]
     ++ [ "invalidHereafter" .= x | x <- mbfield' (MA.invalidHereafter vi) ]
    where mbfield' SNothing  = []
          mbfield' (SJust x) = [x]

instance ( ShelleyBasedEra era
         , ToJSON (Core.Value era)
         , ToJSON (Core.TxOut era)
         , ToJSON (PredicateFailure (Core.EraRule "PPUP" era))
         ) => ToJSON (MA.UtxoPredicateFailure era) where
  toJSON (MA.BadInputsUTxO badInputs) = object
    [ "kind"      .= String "BadInputsUTxO"
    , "badInputs" .= do badInputs                               :: Set (TxIn (Ledger.Crypto era))
    , "error"     .= do renderBadInputsUTxOErr badInputs :: Value
    ]
  toJSON (MA.OutsideValidityIntervalUTxO validityInterval slot) = object
    [ "kind"              .= String "ExpiredUTxO"
    , "validityInterval"  .= do validityInterval  :: MA.ValidityInterval
    , "slot"              .= do slot              :: SlotNo
    ]
  toJSON (MA.MaxTxSizeUTxO txsize maxtxsize) = object
    [ "kind"    .= String "MaxTxSizeUTxO"
    , "size"    .= do txsize    :: Integer
    , "maxSize" .= do maxtxsize :: Integer
    ]
  toJSON MA.InputSetEmptyUTxO = object
    [ "kind"  .= String "InputSetEmptyUTxO"
    ]
  toJSON (MA.FeeTooSmallUTxO minfee txfee) = object
    [ "kind"    .= String "FeeTooSmallUTxO"
    , "minimum" .= do minfee  :: Coin
    , "fee"     .= do txfee   :: Coin
    ]
  toJSON (MA.ValueNotConservedUTxO consumed produced) = object
    [ "kind"      .= String "ValueNotConservedUTxO"
    , "consumed"  .= do consumed                                            :: Ledger.Value era
    , "produced"  .= do produced                                            :: Ledger.Value era
    , "error"     .= do renderValueNotConservedErr consumed produced :: Value
    ]
  toJSON (MA.WrongNetwork network addrs) = object
    [ "kind"    .= String "WrongNetwork"
    , "network" .= do network :: Network
    , "addrs"   .= do addrs   :: Set (Addr (Ledger.Crypto era))
    ]
  toJSON (MA.WrongNetworkWithdrawal network addrs) = object
    [ "kind"    .= String "WrongNetworkWithdrawal"
    , "network" .= do network :: Network
    , "addrs"   .= do addrs   :: Set (RewardAcnt (Ledger.Crypto era))
    ]
  -- TODO: Add the minimum allowed UTxO value to OutputTooSmallUTxO
  toJSON (MA.OutputTooSmallUTxO badOutputs) = object
    [ "kind"    .= String "OutputTooSmallUTxO"
    , "outputs" .= do badOutputs :: [Ledger.TxOut era]
    , "error"   .= String "The output is smaller than the allow minimum UTxO value defined in the protocol parameters"
    ]
  toJSON (MA.UpdateFailure f) = toJSON f
  toJSON (MA.OutputBootAddrAttrsTooBig badOutputs) = object
    [ "kind"    .= String "OutputBootAddrAttrsTooBig"
    , "outputs" .= do badOutputs :: [Ledger.TxOut era]
    , "error"   .= String "The Byron address attributes are too big"
    ]
  toJSON MA.TriesToForgeADA = object
    [ "kind"  .= String "TriesToForgeADA"
    ]
  toJSON (MA.OutputTooBigUTxO badOutputs) = object
    [ "kind"    .= String "OutputTooBigUTxO"
    , "outputs" .= do badOutputs :: [Ledger.TxOut era]
    , "error"   .= String "Too many asset ids in the tx output"
    ]

instance
  ( Ledger.Era era
  ) => ToJSON (PpupPredicateFailure era) where
  toJSON (NonGenesisUpdatePPUP proposalKeys genesisKeys) = object
    [ "kind"  .= String "NonGenesisUpdatePPUP"
    , "keys"  .= do proposalKeys Set.\\ genesisKeys :: Set (KeyHash 'Genesis (Ledger.Crypto era))
    ]
  toJSON (PPUpdateWrongEpoch currEpoch intendedEpoch votingPeriod) = object
    [ "kind"          .= String "PPUpdateWrongEpoch"
    , "currentEpoch"  .= do currEpoch     :: EpochNo
    , "intendedEpoch" .= do intendedEpoch :: EpochNo
    , "votingPeriod"  .= String (show votingPeriod)
    ]
  toJSON (PVCannotFollowPPUP badPv) = object
    [ "kind"                .= String "PVCannotFollowPPUP"
    , "badProtocolVersion"  .= do badPv :: ProtVer
    ]

instance ( ShelleyBasedEra era
         , ToJSON (PredicateFailure (Core.EraRule "DELPL" era))
         ) => ToJSON (DelegsPredicateFailure era) where
  toJSON (DelegateeNotRegisteredDELEG targetPool) = object
    [ "kind"        .= String "DelegateeNotRegisteredDELEG"
    , "targetPool"  .= do targetPool :: KeyHash 'StakePool (Ledger.Crypto era)
    ]
  toJSON (WithdrawalsNotInRewardsDELEGS incorrectWithdrawals) = object
    [ "kind"                  .= String "WithdrawalsNotInRewardsDELEGS"
    , "incorrectWithdrawals"  .= do incorrectWithdrawals :: Map (RewardAcnt (Ledger.Crypto era)) Coin
    ]
  toJSON (DelplFailure f) = toJSON do f :: PredicateFailure (Ledger.EraRule "DELPL" era)

instance ( ToJSON (PredicateFailure (Core.EraRule "POOL"  era))
         , ToJSON (PredicateFailure (Core.EraRule "DELEG" era))
         ) => ToJSON (DelplPredicateFailure era) where
  toJSON (PoolFailure   f) = toJSON do f :: PredicateFailure (Ledger.EraRule "POOL"  era)
  toJSON (DelegFailure  f) = toJSON do f :: PredicateFailure (Ledger.EraRule "DELEG" era)

instance
  ( Ledger.Era era
  ) => ToJSON (DelegPredicateFailure era) where
  toJSON (StakeKeyAlreadyRegisteredDELEG alreadyRegistered) = object
    [ "kind"        .= String "StakeKeyAlreadyRegisteredDELEG"
    , "credential"  .= String (textShow alreadyRegistered)
    , "error"       .= String "Staking credential already registered"
    ]
  toJSON (StakeKeyInRewardsDELEG alreadyRegistered) = object
    [ "kind"        .= String "StakeKeyInRewardsDELEG"
    , "credential"  .= String (textShow alreadyRegistered)
    , "error"       .= String "Staking credential registered in rewards map"
    ]
  toJSON (StakeKeyNotRegisteredDELEG notRegistered) = object
    [ "kind"        .= String "StakeKeyNotRegisteredDELEG"
    , "credential"  .= String (textShow notRegistered)
    , "error"       .= String "Staking credential not registered"
    ]
  toJSON (StakeKeyNonZeroAccountBalanceDELEG remBalance) = object
    [ "kind"              .= String "StakeKeyNonZeroAccountBalanceDELEG"
    , "remainingBalance"  .= remBalance
    ]
  toJSON (StakeDelegationImpossibleDELEG unregistered) = object
    [ "kind"        .= String "StakeDelegationImpossibleDELEG"
    , "credential"  .= String (textShow unregistered)
    , "error"       .= String "Cannot delegate this stake credential because it is not registered"
    ]
  toJSON WrongCertificateTypeDELEG = object
    [ "kind" .= String "WrongCertificateTypeDELEG"
    ]
  toJSON (GenesisKeyNotInMappingDELEG (KeyHash genesisKeyHash)) = object
    [ "kind"            .= String "GenesisKeyNotInMappingDELEG"
    , "unknownKeyHash"  .= String (textShow genesisKeyHash)
    , "error"           .= String "This genesis key is not in the delegation mapping"
    ]
  toJSON (DuplicateGenesisDelegateDELEG (KeyHash genesisKeyHash)) = object
    [ "kind"              .= String "DuplicateGenesisDelegateDELEG"
    , "duplicateKeyHash"  .= String (textShow genesisKeyHash)
    , "error"             .= String "This genesis key has already been delegated to"
    ]
  toJSON (InsufficientForInstantaneousRewardsDELEG mirpot neededMirAmount reserves) = object
    [ "kind"          .= String "InsufficientForInstantaneousRewardsDELEG"
    , "pot"           .= String potText
    , "neededAmount"  .= do neededMirAmount :: Coin
    , "reserves"      .= do reserves        :: Coin
    ]
    where potText = case mirpot of
            ReservesMIR -> "Reserves"
            TreasuryMIR -> "Treasury"
  toJSON (MIRCertificateTooLateinEpochDELEG currSlot boundSlotNo) = object
    [ "kind"                        .= String "MIRCertificateTooLateinEpochDELEG"
    , "currentSlotNo"               .= do currSlot    :: SlotNo
    , "mustBeSubmittedBeforeSlotNo" .= do boundSlotNo :: SlotNo
    ]
  toJSON (DuplicateGenesisVRFDELEG vrfKeyHash) = object
    [ "kind"    .= String "DuplicateGenesisVRFDELEG"
    , "keyHash" .= do vrfKeyHash :: Crypto.Hash (Crypto.HASH (Ledger.Crypto era)) (VerKeyVRF (Ledger.Crypto era))
    ]
  toJSON MIRTransferNotCurrentlyAllowed = object
    [ "kind" .= String "MIRTransferNotCurrentlyAllowed"
    ]
  toJSON MIRNegativesNotCurrentlyAllowed = object
    [ "kind" .= String "MIRNegativesNotCurrentlyAllowed"
    ]
  toJSON (InsufficientForTransferDELEG mirpot attempted available) = object
    [ "kind"      .= String "DuplicateGenesisVRFDELEG"
    , "pot"       .= String potText
    , "attempted" .= do attempted :: Coin
    , "available" .= do available :: Coin
    ]
    where potText = case mirpot of
            ReservesMIR -> "Reserves"
            TreasuryMIR -> "Treasury"
  toJSON MIRProducesNegativeUpdate = object
    [ "kind" .= String "MIRProducesNegativeUpdate"
    ]
  toJSON (MIRNegativeTransfer pot coin) = object
    [ "kind"    .= String "MIRNegativeTransfer"
    , "error"   .= String "Attempt to transfer a negative amount from a pot."
    , "pot"     .= String potText
    , "amount"  .= do coin :: Coin
    ]
    where potText = case pot of
            ReservesMIR -> "Reserves"
            TreasuryMIR -> "Treasury"

instance
  ( Core.Crypto (Ledger.Crypto era)
  ) => ToJSON (PoolPredicateFailure era) where
  toJSON (StakePoolNotRegisteredOnKeyPOOL (KeyHash unregStakePool)) = object
    [ "kind"                .= String "StakePoolNotRegisteredOnKeyPOOL"
    , "unregisteredKeyHash" .= String (textShow unregStakePool)
    , "error"               .= String "This stake pool key hash is unregistered"
    ]
  toJSON (StakePoolRetirementWrongEpochPOOL currentEpoch intendedRetireEpoch maxRetireEpoch) = object
    [ "kind"                    .= String "StakePoolRetirementWrongEpochPOOL"
    , "currentEpoch"            .= String (textShow currentEpoch)
    , "intendedRetirementEpoch" .= String (textShow intendedRetireEpoch)
    , "maxEpochForRetirement"   .= String (textShow maxRetireEpoch)
    ]
  toJSON (StakePoolCostTooLowPOOL certCost protCost) = object
    [ "kind"              .= String "StakePoolCostTooLowPOOL"
    , "certificateCost"   .= String (textShow certCost)
    , "protocolParCost"   .= String (textShow protCost)
    , "error"             .= String "The stake pool cost is too low"
    ]
  toJSON (PoolMedataHashTooBig poolID hashSize) = object
    [ "kind"      .= String "PoolMedataHashTooBig"
    , "poolID"    .= String (textShow poolID)
    , "hashSize"  .= String (textShow hashSize)
    , "error"     .= String "The stake pool metadata hash is too large"
    ]

-- Apparently this should never happen according to the Shelley exec spec
  toJSON (WrongCertificateTypePOOL index) =
    case index of
      0 -> object
        [ "kind"  .= String "WrongCertificateTypePOOL"
        , "error" .= String "Wrong certificate type: Delegation certificate"
        ]
      1 -> object
        [ "kind"  .= String "WrongCertificateTypePOOL"
        , "error" .= String "Wrong certificate type: MIR certificate"
        ]
      2 -> object
        [ "kind"  .= String "WrongCertificateTypePOOL"
        , "error" .= String "Wrong certificate type: Genesis certificate"
        ]
      k -> object
        [ "kind"            .= String "WrongCertificateTypePOOL"
        , "certificateType" .= do k :: Word8
        , "error"           .= String "Wrong certificate type: Unknown certificate type"
        ]

  toJSON (WrongNetworkPOOL networkId listedNetworkId poolId) = object
    [ "kind"            .= String "WrongNetworkPOOL"
    , "networkId"       .= String (textShow networkId)
    , "listedNetworkId" .= String (textShow listedNetworkId)
    , "poolId"          .= String (textShow poolId)
    , "error"           .= String "Wrong network ID in pool registration certificate"
    ]

instance ( ToJSON (PredicateFailure (Core.EraRule "NEWEPOCH" era))
         , ToJSON (PredicateFailure (Core.EraRule "RUPD" era))
         ) => ToJSON (TickPredicateFailure era) where
  toJSON (NewEpochFailure f) = object
    [ "kind"  .= String "NewEpochFailure"
    , "value" .= do f :: PredicateFailure (Ledger.EraRule "NEWEPOCH" era)
    ]
  toJSON (RupdFailure f) = object
    [ "kind"  .= String "RupdFailure"
    , "value" .= do f :: PredicateFailure (Ledger.EraRule "RUPD" era)
    ]

instance ToJSON TicknPredicateFailure where
  toJSON x = case x of {} -- no constructors

instance ( ToJSON (PredicateFailure (Core.EraRule "EPOCH" era))
         , ToJSON (PredicateFailure (Core.EraRule "MIR" era))
         , Core.Crypto (Ledger.Crypto era)
         ) => ToJSON (NewEpochPredicateFailure era) where
  toJSON (EpochFailure f) = object
    [ "kind"    .= String "EpochFailure"
    , "update"  .= do f :: PredicateFailure (Ledger.EraRule "EPOCH" era)
    ]
  toJSON (MirFailure f) = object
    [ "kind"    .= String "MirFailure"
    , "update"  .= do f :: PredicateFailure (Ledger.EraRule "MIR" era)
    ]
  toJSON (CorruptRewardUpdate update) = object
    [ "kind"    .= String "CorruptRewardUpdate"
    , "update"  .= String (show update)
    ]

instance ( ToJSON (PredicateFailure (Core.EraRule "POOLREAP" era))
         , ToJSON (PredicateFailure (Core.EraRule "SNAP" era))
         , ToJSON (PredicateFailure (Core.EraRule "UPEC" era))
         ) => ToJSON (EpochPredicateFailure era) where
  toJSON (PoolReapFailure f) = object
    [ "kind"    .= String "PoolReapFailure"
    , "update"  .= do f :: PredicateFailure (Ledger.EraRule "POOLREAP" era)
    ]
  toJSON (SnapFailure f) = object
    [ "kind"    .= String "SnapFailure"
    , "update"  .= do f :: PredicateFailure (Ledger.EraRule "SNAP" era)
    ]
  toJSON (UpecFailure f) = object
    [ "kind"    .= String "UpecFailure"
    , "update"  .= do f :: PredicateFailure (Ledger.EraRule "UPEC" era)
    ]

instance ToJSON (PoolreapPredicateFailure era) where
  toJSON x = case x of {} -- no constructors

instance ToJSON (SnapPredicateFailure era) where
  toJSON x = case x of {} -- no constructors

-- TODO: Need to elaborate more on this error
instance ToJSON (NewppPredicateFailure era) where
  toJSON (UnexpectedDepositPot outstandingDeposits depositPot) = object
    [ "kind"                .= String "UnexpectedDepositPot"
    , "outstandingDeposits" .= String (textShow outstandingDeposits)
    , "depositPot"          .= String (textShow depositPot)
    ]

instance ToJSON (MirPredicateFailure era) where
  toJSON x = case x of {} -- no constructors


instance ToJSON (RupdPredicateFailure era) where
  toJSON x = case x of {} -- no constructors


instance
  ( Core.Crypto crypto
  , ToJSON Ledger.ActiveSlotCoeff
  , ToJSON (Crypto.OutputVRF (Core.VRF crypto))
  , ToJSON (Crypto.CertifiedVRF (Core.VRF crypto) Nonce)
  ) => ToJSON (PrtclPredicateFailure crypto) where
  toJSON (OverlayFailure f) = object
    [ "kind"    .= String "OverlayFailure"
    , "update"  .= do f :: OverlayPredicateFailure crypto
    ]
  toJSON (UpdnFailure f) = object
    [ "kind"    .= String "UpdnFailure"
    , "update"  .= do f :: UpdnPredicateFailure crypto
    ]

instance
  ( Core.Crypto crypto
  , ToJSON (Crypto.OutputVRF (Core.VRF crypto))
  , ToJSON (Crypto.CertifiedVRF (Core.VRF crypto) Nonce)
  , ToJSON Ledger.ActiveSlotCoeff
  ) => ToJSON (OverlayPredicateFailure crypto) where
  toJSON (UnknownGenesisKeyOVERLAY (KeyHash genKeyHash)) = object
    [ "kind"            .= String "UnknownGenesisKeyOVERLAY"
    , "unknownKeyHash"  .= String (textShow genKeyHash)
    ]
  toJSON (VRFKeyBadLeaderValue seedNonce (SlotNo currSlotNo) prevHashNonce leaderElecVal) = object
    [ "kind"                .= String "VRFKeyBadLeaderValueOVERLAY"
    , "seedNonce"           .= String (textShow seedNonce)
    , "currentSlot"         .= String (textShow currSlotNo)
    , "previousHashAsNonce" .= String (textShow prevHashNonce)
    , "leaderElectionValue" .= String (textShow leaderElecVal)
    ]
  toJSON (VRFKeyBadNonce seedNonce (SlotNo currSlotNo) prevHashNonce blockNonce) = object
    [ "kind"                .= String "VRFKeyBadNonceOVERLAY"
    , "seedNonce"           .= String (textShow seedNonce)
    , "currentSlot"         .= String (textShow currSlotNo)
    , "previousHashAsNonce" .= String (textShow prevHashNonce)
    , "blockNonce"          .= String (textShow blockNonce)
    ]
  toJSON (VRFKeyWrongVRFKey issuerHash regVRFKeyHash unregVRFKeyHash) = object
    [ "kind"                    .= String "VRFKeyWrongVRFKeyOVERLAY"
    , "poolHash"                .= textShow issuerHash
    , "registeredVRFKeHash"     .= textShow regVRFKeyHash
    , "unregisteredVRFKeyHash"  .= textShow unregVRFKeyHash
    ]
  toJSON (VRFKeyUnknown (KeyHash kHash)) = object
    [ "kind"    .= String "VRFKeyUnknownOVERLAY"
    , "keyHash" .= String (textShow kHash)
    ]
  toJSON (VRFLeaderValueTooBig leadElecVal weightOfDelegPool actSlotCoefff) = object
    [ "kind"                  .= String "VRFLeaderValueTooBigOVERLAY"
    , "leaderElectionValue"   .= String (textShow leadElecVal)
    , "delegationPoolWeight"  .= String (textShow weightOfDelegPool)
    , "activeSlotCoefficient" .= String (textShow actSlotCoefff)
    ]
  toJSON (NotActiveSlotOVERLAY notActiveSlotNo) = object
    [ "kind" .= String "NotActiveSlotOVERLAY"
    , "slot" .= String (textShow notActiveSlotNo)
    ]
  toJSON (WrongGenesisColdKeyOVERLAY actual expected) = object
    [ "kind"      .= String "WrongGenesisColdKeyOVERLAY"
    , "actual"    .= do actual    :: KeyHash 'BlockIssuer crypto
    , "expected"  .= do expected  :: KeyHash 'GenesisDelegate crypto
    ]
  toJSON (WrongGenesisVRFKeyOVERLAY issuer actual expected) = object
    [ "kind"      .= String "WrongGenesisVRFKeyOVERLAY"
    , "issuer"    .= do issuer    :: KeyHash 'BlockIssuer crypto
    , "actual"    .= do actual    :: Crypto.Hash (Crypto.HASH crypto) (VerKeyVRF crypto)
    , "expected"  .= do expected  :: Crypto.Hash (Crypto.HASH crypto) (VerKeyVRF crypto)
    ]
  toJSON (OcertFailure f) = toJSON do f :: OcertPredicateFailure crypto

instance
  ( Core.Crypto crypto
  ) => ToJSON (OcertPredicateFailure crypto) where
  toJSON (KESBeforeStartOCERT (KESPeriod oCertstart) (KESPeriod current)) = object
    [ "kind"                  .= String "KESBeforeStartOCERT"
    , "opCertKESStartPeriod"  .= String (textShow oCertstart)
    , "currentKESPeriod"      .= String (textShow current)
    , "error"                 .= String "Your operational certificate's KES start period is before the KES current period."
    ]
  toJSON (KESAfterEndOCERT (KESPeriod current) (KESPeriod oCertstart) maxKESEvolutions) = object
    [ "kind"                  .= String "KESAfterEndOCERT"
    , "currentKESPeriod"      .= String (textShow current)
    , "opCertKESStartPeriod"  .= String (textShow oCertstart)
    , "maxKESEvolutions"      .= String  (textShow maxKESEvolutions)
    , "error"                 .= String "The operational certificate's KES start period is greater than the max number of KES + the KES current period"
    ]
  toJSON (CounterTooSmallOCERT lastKEScounterUsed currentKESCounter) = object
    [ "kind"              .= String "CounterTooSmallOCert"
    , "currentKESCounter" .= String (textShow currentKESCounter)
    , "lastKESCounter"    .= String (textShow lastKEScounterUsed)
    , "error"             .= String "The operational certificate's last KES counter is greater than the current KES counter."
    ]
  toJSON (InvalidSignatureOCERT oCertCounter oCertKESStartPeriod) = object
    [ "kind"                  .= String "InvalidSignatureOCERT"
    , "opCertKESStartPeriod"  .= String (textShow oCertKESStartPeriod)
    , "opCertCounter"         .= String (textShow oCertCounter)
    ]
  toJSON (InvalidKesSignatureOCERT currKESPeriod startKESPeriod expectedKESEvolutions err) = object
    [ "kind"                        .= String "InvalidKesSignatureOCERT"
    , "opCertKESStartPeriod"        .= String (textShow startKESPeriod)
    , "opCertKESCurrentPeriod"      .= String (textShow currKESPeriod)
    , "opCertExpectedKESEvolutions" .= String (textShow expectedKESEvolutions)
    , "error"                       .= do err :: String
    ]
  toJSON (NoCounterForKeyHashOCERT (KeyHash stakePoolKeyHash)) = object
    [ "kind" .= String "NoCounterForKeyHashOCERT"
    , "stakePoolKeyHash" .= String (textShow stakePoolKeyHash)
    , "error" .= String "A counter was not found for this stake pool key hash"
    ]

instance ToJSON (UpdnPredicateFailure crypto) where
  toJSON x = case x of {} -- no constructors

instance ToJSON (UpecPredicateFailure era) where
  toJSON (NewPpFailure (UnexpectedDepositPot totalOutstanding depositPot)) = object
    [ "kind"              .= String "UnexpectedDepositPot"
    , "totalOutstanding"  .= String (textShow totalOutstanding)
    , "depositPot"        .= String (textShow depositPot)
    ]


--------------------------------------------------------------------------------
-- Alonzo related
--------------------------------------------------------------------------------


instance ToJSON (Alonzo.UtxoPredicateFailure (Alonzo.AlonzoEra StandardCrypto)) where
  toJSON (Alonzo.BadInputsUTxO badInputs) = object
    [ "kind"      .= String "BadInputsUTxO"
    , "badInputs" .= do badInputs                               :: Set (TxIn (Ledger.Crypto (Consensus.AlonzoEra StandardCrypto)))
    , "error"     .= do renderBadInputsUTxOErr badInputs :: Value
    ]
  toJSON (Alonzo.OutsideValidityIntervalUTxO validtyInterval slot) = object
    [ "kind"              .= String "ExpiredUTxO"
    , "validityInterval"  .= do validtyInterval :: MA.ValidityInterval
    , "slot"              .= do slot            :: SlotNo
    ]
  toJSON (Alonzo.MaxTxSizeUTxO txsize maxtxsize) = object
    [ "kind"    .= String "MaxTxSizeUTxO"
    , "size"    .= do txsize    :: Integer
    , "maxSize" .= do maxtxsize :: Integer
    ]
  toJSON Alonzo.InputSetEmptyUTxO = object
    [ "kind" .= String "InputSetEmptyUTxO"
    ]
  toJSON (Alonzo.FeeTooSmallUTxO minfee currentFee) = object
    [ "kind"    .= String "FeeTooSmallUTxO"
    , "minimum" .= do minfee      :: Coin
    , "fee"     .= do currentFee  :: Coin
    ]
  toJSON (Alonzo.ValueNotConservedUTxO consumed produced) = object
    [ "kind"      .= String "ValueNotConservedUTxO"
    , "consumed"  .= do consumed                                            :: Mary.Value StandardCrypto
    , "produced"  .= do produced                                            :: Mary.Value StandardCrypto
    , "error"     .= do renderValueNotConservedErr consumed produced :: Value
    ]
  toJSON (Alonzo.WrongNetwork network addrs) = object
    [ "kind"    .= String "WrongNetwork"
    , "network" .= do network :: Network
    , "addrs"   .= do addrs   :: Set (Addr (Ledger.Crypto (Consensus.AlonzoEra StandardCrypto)))
    ]
  toJSON (Alonzo.WrongNetworkWithdrawal network addrs) = object
    [ "kind"    .= String "WrongNetworkWithdrawal"
    , "network" .= network
    , "addrs"   .= addrs
    ]
  toJSON (Alonzo.OutputTooSmallUTxO badOutputs) = object
    [ "kind"    .= String "OutputTooSmallUTxO"
    , "outputs" .= badOutputs
    , "error"   .= String "The output is smaller than the allow minimum UTxO value defined in the protocol parameters"
    ]
  toJSON (Alonzo.UtxosFailure predFailure) = object
    [ "kind"    .= String "UtxosFailure"
    , "error"   .= toJSON predFailure
    ]
  toJSON (Alonzo.OutputBootAddrAttrsTooBig txouts) = object
    [ "kind"    .= String "OutputBootAddrAttrsTooBig"
    , "outputs" .= txouts
    , "error"   .= String "The Byron address attributes are too big"
    ]
  toJSON Alonzo.TriesToForgeADA = object
    [ "kind"  .= String "TriesToForgeADA"
    ]
  toJSON (Alonzo.OutputTooBigUTxO badOutputs) = object
    [ "kind"    .= String "OutputTooBigUTxO"
    , "outputs" .= badOutputs
    , "error"   .= String "Too many asset ids in the tx output"
    ]
  toJSON (Alonzo.InsufficientCollateral computedBalance suppliedFee) = object
    [ "kind"    .= String "InsufficientCollateral"
    , "balance" .= computedBalance
    , "txfee"   .= suppliedFee
    ]
  toJSON (Alonzo.ScriptsNotPaidUTxO utxos) = object
    [ "kind"  .= String "ScriptsNotPaidUTxO"
    , "utxos" .= utxos
    ]
  toJSON (Alonzo.ExUnitsTooBigUTxO pParamsMaxExUnits suppliedExUnits) = object
    [ "kind"        .= String "ExUnitsTooBigUTxO"
    , "maxexunits"  .= pParamsMaxExUnits
    , "exunits"     .= suppliedExUnits
    ]
  toJSON (Alonzo.CollateralContainsNonADA inputs) = object
    [ "kind"    .= String "CollateralContainsNonADA"
    , "inputs"  .= inputs
    ]
  toJSON (Alonzo.WrongNetworkInTxBody actualNetworkId netIdInTxBody) = object
    [ "kind"            .= String "WrongNetworkInTxBody"
    , "networkid"       .= actualNetworkId
    , "txbodyNetworkId" .= netIdInTxBody
    ]
  toJSON (Alonzo.OutsideForecast slotNum) = object
    [ "kind" .= String "OutsideForecast"
    , "slot" .= slotNum
    ]
  toJSON (Alonzo.TooManyCollateralInputs maxCollateralInputs numberCollateralInputs) = object
    [ "kind"    .= String "TooManyCollateralInputs"
    , "max"     .= maxCollateralInputs
    , "inputs"  .= numberCollateralInputs
    ]
  toJSON Alonzo.NoCollateralInputs = object
    [ "kind"  .= String "NoCollateralInputs"
    ]

instance ToJSON (Alonzo.UtxosPredicateFailure (Alonzo.AlonzoEra StandardCrypto)) where
  toJSON (Alonzo.ValidationTagMismatch isValidating reason) = object
    [ "kind"          .= String "ValidationTagMismatch"
    , "isvalidating"  .= isValidating
    , "reason"        .= do reason :: Alonzo.TagMismatchDescription
    ]
  toJSON (Alonzo.CollectErrors errors) = object
    [ "kind"    .= String "CollectErrors"
    , "errors"  .= errors
    ]
  toJSON (Alonzo.UpdateFailure pFailure) = toJSON pFailure

deriving newtype instance ToJSON Alonzo.IsValid

instance ToJSON (Alonzo.CollectError StandardCrypto) where
  toJSON = \case
    Alonzo.NoRedeemer sPurpose -> object
      [ "kind"          .= String "CollectError"
      , "error"         .= String "NoRedeemer"
      , "scriptpurpose" .= do sPurpose :: Alonzo.ScriptPurpose StandardCrypto
      ]
    Alonzo.NoWitness sHash -> object
      [ "kind" .= String "CollectError"
      , "error" .= String "NoWitness"
      , "scripthash" .= toJSON sHash
      ]
    Alonzo.NoCostModel lang -> object
      [ "kind" .= String "CollectError"
      , "error" .= String "NoCostModel"
      , "language" .= toJSON lang
      ]
    Alonzo.BadTranslation err -> object
      [ "kind" .= String "PlutusTranslationError"
      , "error" .= case err of
          Alonzo.ByronTxOutInContext txOutSource ->
            object
              [ "kind" .= String "ByronTxOutInContext"
              , "txOutSource" .= txOutSource
              ]
          Alonzo.TranslationLogicMissingInput txin ->
            object
              [ "kind" .= String "TranslationLogicMissingInput"
              , "txin" .= txin
              ]
          Alonzo.RdmrPtrPointsToNothing ptr ->
            object
              [ "kind" .= String "RedeemerPointerPointsToNothing"
              , "ptr" .= Api.renderScriptWitnessIndex (Api.fromAlonzoRdmrPtr ptr)
              ]
          Alonzo.LanguageNotSupported lang ->
            object
              [ "kind" .= String "LanguageNotSupported"
              , "lang" .= toJSON lang
              ]
          Alonzo.InlineDatumsNotSupported txOutSource ->
            object
              [ "kind" .= String "InlineDatumsNotSupported"
              , "txOutSource" .= toJSON txOutSource
              ]
          Alonzo.ReferenceScriptsNotSupported txOutSource ->
            object
              [ "kind" .= String "ReferenceScriptsNotSupported"
              , "txOutSource" .= toJSON txOutSource
              ]
          Alonzo.ReferenceInputsNotSupported txins ->
            object
              [ "kind" .= String "ReferenceInputsNotSupported"
              , "txins" .= toJSON txins
              ]
          Alonzo.TimeTranslationPastHorizon msg ->
            object
              [ "kind" .= String "TimeTranslationPastHorizon"
              , "msg" .= toJSON msg
              ]
      ]

instance ToJSON (Alonzo.TxOutSource StandardCrypto) where
  toJSON = \case
    Alonzo.TxOutFromInput txin ->
      object
        [ "kind" .= String "TxOutFromInput"
        , "msg" .= toJSON txin
        ]
    Alonzo.TxOutFromOutput txix ->
      object
        [ "kind" .= String "TxOutFromOutput"
        , "msg" .= toJSON txix
        ]

instance ToJSON Alonzo.FailureDescription where
  toJSON (Alonzo.PlutusFailure _t bs) = object
    [ "kind"                  .= String "FailureDescription"
    , "error"                 .= String "PlutusFailure"
    , "reconstructionDetail"  .= do Alonzo.debugPlutus (BSU.toString bs) :: Alonzo.PlutusDebugInfo
    -- , "description"           .= t
    ]

instance
  ( ToJSON (Core.AuxiliaryDataHash StandardCrypto)
  ) => ToJSON (AlonzoBbodyPredFail (Alonzo.AlonzoEra StandardCrypto)) where
  toJSON err = object
    [ "kind"  .= String "AlonzoBbodyPredFail"
    , "error" .= String (show err)
    ]

--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

instance ToJSON Alonzo.PlutusDebugInfo where
  toJSON = \case
    Alonzo.DebugSuccess budget -> object
      [ "kind"    .= String "DebugSuccess"
      , "budget"  .= do budget :: Cek.ExBudget
      ]
    Alonzo.DebugCannotDecode msg -> object
      [ "kind"    .= String "DebugCannotDecode"
      , "message" .= do msg :: String
      ]
    Alonzo.DebugInfo texts e d -> object
      [ "kind"  .= String "DebugInfo"
      , "texts" .= do texts :: [Text]
      , "error" .= do e     :: Alonzo.PlutusError
      , "debug" .= do d     :: Alonzo.PlutusDebug
      ]
    Alonzo.DebugBadHex msg -> object
      [ "kind"    .= String "DebugBadHex"
      , "message" .= do msg :: String
      ]

instance ToJSON Alonzo.PlutusError where
  toJSON = \case
    Alonzo.PlutusErrorV1 evaluationError -> toJSON do evaluationError :: PV1.EvaluationError
    Alonzo.PlutusErrorV2 evaluationError -> toJSON do evaluationError :: PV1.EvaluationError

instance ToJSON Alonzo.PlutusDebug where
  toJSON = \case
    Alonzo.PlutusDebugV1 _costModel exUnits sbs ds protVer -> object
      [ "exUnits"     .= do exUnits                                               :: Ledger.ExUnits
      , "sbs"         .= do Text.decodeLatin1 (B16.encode (Short.fromShort sbs))  :: Text
      , "scriptHash"  .= do scriptHashOf Alonzo.PlutusV1 sbs                      :: Text
      , "dsSummary"   .= do plutusDataToDsSummary ds                              :: Aeson.Value
      , "protVer"     .= do protVer                                               :: ProtVer
      -- , "ds"          .= toJSON ds
      -- , "costModel"   .= costModel
      ]
    Alonzo.PlutusDebugV2 _costModel exUnits sbs ds protVer -> object
      [ "exUnits"     .= do exUnits                                               :: Ledger.ExUnits
      , "sbs"         .= do Text.decodeLatin1 (B16.encode (Short.fromShort sbs))  :: Text
      , "scriptHash"  .= do scriptHashOf Alonzo.PlutusV2 sbs                      :: Text
      , "dsSummary"   .= do plutusDataToDsSummary ds                              :: Aeson.Value
      , "protVer"     .= do protVer                                               :: ProtVer
      -- , "ds"          .= toJSON ds
      -- , "costModel"   .= costModel
      ]

plutusDataToDsSummary :: [Plutus.Data] -> Aeson.Value
plutusDataToDsSummary [dat, redeemer, info] = Aeson.object
  [ "data"      .= do dat                             :: Plutus.Data
  , "redeemer"  .= do redeemer                        :: Plutus.Data
  , "info"      .= do plutusInfoDataToDsSummary info  :: Value
  ]
plutusDataToDsSummary [dat, info] = Aeson.object
  [ "data"      .= do dat                             :: Plutus.Data
  , "info"      .= do plutusInfoDataToDsSummary info  :: Value
  ]
plutusDataToDsSummary _ = Aeson.Null

plutusInfoDataToDsSummary :: Plutus.Data -> Aeson.Value
plutusInfoDataToDsSummary info = case PV1.fromData info of
  Nothing -> String "no-info"
  Just PV1.ScriptContext { PV1.scriptContextTxInfo, PV1.scriptContextPurpose} -> object
    [ "txInfo"  .= do txInfoToJson scriptContextTxInfo          :: Value
    , "purpose" .= do scriptPurposeToJson scriptContextPurpose  :: Value
    ]

txInfoToJson :: PV1.TxInfo -> Value
txInfoToJson txInfo = Aeson.object
  [ "inputs"      .= do PV1.txInfoInputs      txInfo :: [PV1.TxInInfo]
  , "outputs"     .= do PV1.txInfoOutputs     txInfo :: [PV1.TxOut]
  , "fee"         .= do PV1.txInfoFee         txInfo :: PV1.Value
  , "mint"        .= do PV1.txInfoMint        txInfo :: PV1.Value
  , "dCert"       .= do PV1.txInfoDCert       txInfo :: [PV1.DCert]
  , "wdrl"        .= do PV1.txInfoWdrl        txInfo :: [(PV1.StakingCredential, Integer)]
  , "validRange"  .= do PV1.txInfoValidRange  txInfo :: (PV1.Interval PV1.POSIXTime)
  , "signatories" .= do PV1.txInfoSignatories txInfo :: [PV1.PubKeyHash]
  , "data"        .= do PV1.txInfoData        txInfo :: [(PV1.DatumHash, PV1.Datum)]
  , "id"          .= do PV1.txInfoId          txInfo :: PV1.TxId
  ]

instance ToJSON PV1.Datum where
  toJSON v = toJSON do PV1.builtinDataToData (PV1.getDatum v) :: Plutus.Data

instance ToJSON PV1.DatumHash where
  toJSON v = toJSON do show v :: Text

instance ToJSON PV1.DCert where
  toJSON = \case
    PV1.DCertDelegRegKey stakingCredential -> object
      [ "DCertDelegRegKey" .= do stakingCredential :: PV1.StakingCredential
      ]
    PV1.DCertDelegDeRegKey stakingCredential -> object
      [ "DCertDelegDeRegKey" .= do stakingCredential :: PV1.StakingCredential
      ]
    PV1.DCertDelegDelegate delegator delagatee -> object
      [ "DCertDelegDelegate" .= object
        [ "delegator" .= do delegator :: PV1.StakingCredential
        , "delegatee" .= do delagatee :: PV1.PubKeyHash
        ]
      ]
    PV1.DCertPoolRegister poolId poolVfr -> object
      [ "DCertPoolRegister" .= object
        [ "poolId"  .= do poolId  :: PV1.PubKeyHash
        , "poolVfr" .= do poolVfr :: PV1.PubKeyHash
        ]
      ]
    PV1.DCertPoolRetire pkh n -> object
      [ "DCertPoolRetire" .= object
        [ "stakePoolId"   .= do pkh :: PV1.PubKeyHash
        , "epochRetiring" .= do n   :: Integer
        ]
      ]
    PV1.DCertGenesis -> String "DCertGenesis"
    PV1.DCertMir -> String "DCertMir"

instance ToJSON (PV1.Interval PV1.POSIXTime) where
  toJSON (PV1.Interval lo hi) = toJSON $
    lowerBoundToJsonArray lo <>
    upperBoundToJsonArray hi
    where
      lowerBoundToJsonArray :: PV1.LowerBound PV1.POSIXTime -> [Value]
      lowerBoundToJsonArray = \case
        PV1.LowerBound PV1.PosInf     _     -> ["(", "+∞"                 ]
        PV1.LowerBound PV1.NegInf     _     -> ["(", "-∞"                 ]
        PV1.LowerBound (PV1.Finite a) True  -> ["[", toJSON (PV1.toData a)]
        PV1.LowerBound (PV1.Finite a) False -> ["(", toJSON (PV1.toData a)]

      upperBoundToJsonArray :: PV1.UpperBound PV1.POSIXTime -> [Value]
      upperBoundToJsonArray = \case
        PV1.UpperBound PV1.PosInf     _     -> ["+∞"                 , ")"]
        PV1.UpperBound PV1.NegInf     _     -> ["-∞"                 , ")"]
        PV1.UpperBound (PV1.Finite a) True  -> [toJSON (PV1.toData a), "]"]
        PV1.UpperBound (PV1.Finite a) False -> [toJSON (PV1.toData a), ")"]

instance ToJSON PV1.PubKeyHash where
  toJSON v = toJSON do show v :: Text

instance ToJSON PV1.StakingCredential where
  toJSON = \case
    PV1.StakingHash credential -> object
      [ "StakingHash" .= do credential :: PV1.Credential
      ]
    PV1.StakingPtr a b c -> toJSON do [a, b, c] :: [Integer]

instance ToJSON PV1.Credential where
  toJSON = \case
    PV1.PubKeyCredential pubKeyHash -> object
      [ "PubKeyCredential" .= do pubKeyHash :: PV1.PubKeyHash
      ]
    PV1.ScriptCredential validatorHash -> object
      [ "ScriptCredential" .= do validatorHash :: PV1.ValidatorHash
      ]

instance ToJSON PV1.ValidatorHash where
  toJSON h = toJSON do show h :: Text

instance ToJSON PV1.TxId where
  toJSON v = toJSON do show v :: Text

instance ToJSON PV1.TxInInfo where
  toJSON v = object
    [ "outRef"    .= toJSON do PV1.txInInfoOutRef   v :: PV1.TxOutRef
    , "resolved"  .= toJSON do PV1.txInInfoResolved v :: PV1.TxOut
    ]

instance ToJSON PV1.TxOut where
  toJSON v = object
    [ "address"   .= do PV1.txOutAddress   v :: PV1.Address
    , "value"     .= do PV1.txOutValue     v :: PV1.Value
    , "datumHash" .= do PV1.txOutDatumHash v :: Maybe PV1.DatumHash
    ]

instance ToJSON PV1.Address where
  toJSON v = object
    [ "credential"        .= do PV1.addressCredential        v :: PV1.Credential
    , "stakingCredential" .= do PV1.addressStakingCredential v :: Maybe PV1.StakingCredential
    ]

instance ToJSON PV1.Value where
  toJSON (PV1.Value m) = toJSON do m :: AssocMap.Map PV1.CurrencySymbol (AssocMap.Map PV1.TokenName Integer)

instance ToJSON PV1.TokenName where
  toJSON v = toJSON do show v :: Text

instance ToJSONKey PV1.TokenName where
  toJSONKey = contramap (builtinByteStringToBase16Text . PV1.unTokenName) toJSONKey -- toJSONKeyText $ show @PV1.TokenName @Text

builtinByteStringToBase16Text :: PV1.BuiltinByteString -> Text
builtinByteStringToBase16Text bs = Text.filter (/= '"') (Text.pack (show bs)) -- TODO is there a better way to encode as Text

instance ToJSON PV1.CurrencySymbol where
  toJSON (PV1.CurrencySymbol bs) = toJSON do show bs :: Text

instance ToJSONKey PV1.CurrencySymbol where
  toJSONKey = toJSONKeyText $ show @PV1.CurrencySymbol @Text

instance (ToJSONKey k, Ord k, ToJSON a) => ToJSON (AssocMap.Map k a) where
  toJSON = toJSON . Map.fromList . AssocMap.toList

instance ToJSON PV1.TxOutRef where
  toJSON (PV1.TxOutRef txid idx) = toJSON
    [ toJSON do txid  :: PV1.TxId
    , toJSON do idx   :: Integer
    ]

scriptPurposeToJson :: PV1.ScriptPurpose -> Value
scriptPurposeToJson = \case
  PV1.Minting currencySymbol -> Aeson.object
    [ "kind"  .= String "Minting"
    , "value" .= do currencySymbol :: PV1.CurrencySymbol
    ]
  PV1.Spending outRef -> Aeson.object
    [ "kind"  .= String "Spending"
    , "value" .= toJSON outRef
    ]
  PV1.Rewarding stakingCredential -> Aeson.object
    [ "kind"  .= String "Rewarding"
    , "value" .= do stakingCredential :: PV1.StakingCredential
    ]
  PV1.Certifying dCert -> Aeson.object
    [ "kind"  .= String "Certifying"
    , "value" .= do dCert :: PV1.DCert
    ]

scriptHashOf :: Alonzo.Language -> Short.ShortByteString -> Text
scriptHashOf lang sbs = Text.pack $ Hash.hashToStringAsHex h
  where Ledger.ScriptHash h = case lang of
          Alonzo.PlutusV1 -> Ledger.hashScript @Consensus.StandardAlonzo (Ledger.PlutusScript lang sbs)
          Alonzo.PlutusV2 -> error "not implemented"

instance ToJSON Plutus.EvaluationError where
  toJSON = \case
    Plutus.CekError e -> object
      [ "kind"    .= String "CekError"
      , "error"   .= do show e  :: Text
      , "value"   .= do e       :: (Cek.ErrorWithCause
                                      (Cek.EvaluationError Cek.CekUserError (PlutusCore.MachineError PlutusCore.DefaultFun))
                                      (UntypedPlutusCore.Core.Type.Term
                                        PlutusCore.DeBruijn.NamedDeBruijn
                                        PlutusCore.DefaultUni
                                        PlutusCore.DefaultFun
                                        ()))
      ]
    Plutus.DeBruijnError e -> object
      [ "kind"    .= String "DeBruijnError"
      , "error"   .= do show e  :: Text
      ]
    Plutus.CodecError e -> object
      [ "kind"    .= String "CodecError"
      , "error"   .= do show e  :: Text
      ]
    Plutus.IncompatibleVersionError actual -> object
      [ "kind"    .= String "IncompatibleVersionError"
      , "actual"  .= do actual  :: UntypedPlutusCore.Core.Type.Version ()
      ]
    Plutus.CostModelParameterMismatch -> object
      [ "kind"    .= String "CostModelParameterMismatch"
      ]

instance ToJSON (Plutus.Version ann) where
  toJSON (Plutus.Version _ i j k) = object
    [ "i" .= do i :: Natural
    , "j" .= do j :: Natural
    , "k" .= do k :: Natural
    ]

instance ToJSON Plutus.Data where
  toJSON = \case
    Plutus.Constr t as -> object
      [ "Constr" .= do toJSON (t :: Integer):fmap toJSON (as :: [Plutus.Data]) :: [Value]
      ]
    Plutus.Map es -> object
      [ "Map" .= do fmap dataEntryToJson es :: [Value]
      ]
    Plutus.List es  -> toJSON do es :: [Plutus.Data]
    Plutus.I n      -> toJSON do n :: Integer
    Plutus.B bs     -> toJSON do Text.decodeLatin1 (B16.encode bs) :: Text

dataEntryToJson :: (Plutus.Data, Plutus.Data) -> Value
dataEntryToJson (k, v) = toJSON [toJSON k, toJSON v]

instance ToJSON Cek.CekUserError where
  toJSON = \case
    Cek.CekOutOfExError (Cek.ExRestrictingBudget res) -> object
      [ "kind"    .= String "CekOutOfExError"
      , "budget"  .= do res :: Cek.ExBudget
      ]
    Cek.CekEvaluationFailure -> object
      [ "kind"  .= String "CekEvaluationFailure"
      ]

instance (ToJSON name, ToJSON fun) => ToJSON (Cek.CekEvaluationException name uni fun) where

instance (ToJSON name, ToJSON fun) => ToJSON (UntypedPlutusCore.Core.Type.Term name uni fun ann) where
  toJSON = \case
    UntypedPlutusCore.Core.Type.Var {} -> Aeson.object
      [ "kind" .= String "Var"
      ]
    UntypedPlutusCore.Core.Type.LamAbs {} -> Aeson.object
      [ "kind" .= String "LamAbs"
      ]
    UntypedPlutusCore.Core.Type.Apply {} -> Aeson.object
      [ "kind" .= String "Apply"
      ]
    UntypedPlutusCore.Core.Type.Force {} -> Aeson.object
      [ "kind" .= String "Force"
      ]
    UntypedPlutusCore.Core.Type.Delay {} -> Aeson.object
      [ "kind" .= String "Delay"
      ]
    UntypedPlutusCore.Core.Type.Constant {} -> Aeson.object
      [ "kind" .= String "Constant"
      ]
    UntypedPlutusCore.Core.Type.Builtin {} -> Aeson.object
      [ "kind" .= String "Builtin"
      ]
    UntypedPlutusCore.Core.Type.Error {} -> Aeson.object
      [ "kind" .= String "Error"
      ]

-- Used by ToJSON (Cek.CekEvaluationException name uni fun)
instance ToJSON fun => ToJSON (Cek.EvaluationError Cek.CekUserError (PlutusCore.MachineError fun)) where
  toJSON = \case
    PlutusCore.InternalEvaluationError internal -> object
      [ "InternalEvaluationError" .= do internal  :: PlutusCore.MachineError fun
      ]
    PlutusCore.UserEvaluationError user -> object
      [ "UserEvaluationError"     .= do user      :: Cek.CekUserError
      ]

instance ToJSON PlutusCore.NamedDeBruijn where

instance ToJSON PlutusCore.DeBruijn.Index where

instance ToJSON PlutusCore.DefaultFun where

instance (forall a. ToJSON (f a)) => ToJSON (PlutusCore.Some f) where
  toJSON (PlutusCore.Some a) = object
    [ "kind"  .= String "Some"
    , "value" .= do a
    ]

instance (ToJSON (uni (PlutusCore.Esc a)), ToJSON a) => ToJSON (PlutusCore.ValueOf uni a) where
  toJSON (PlutusCore.ValueOf u a) = object
    [ "kind"  .= String "ValueOf"
    , "uni"   .= do u :: uni (PlutusCore.Esc a)
    , "a"     .= do a :: a
    ]

instance ToJSON fun => ToJSON (PlutusCore.MachineError fun) where
  toJSON = \case
    PlutusCore.NonPolymorphicInstantiationMachineError -> "NonPolymorphicInstantiationMachineError"
    PlutusCore.NonWrapUnwrappedMachineError -> "NonWrapUnwrappedMachineError"
    PlutusCore.NonFunctionalApplicationMachineError -> "NonFunctionalApplicationMachineError"
    PlutusCore.OpenTermEvaluatedMachineError -> "OpenTermEvaluatedMachineError"
    PlutusCore.UnliftingMachineError (PlutusCore.UnliftingErrorE t) -> object
      [ "UnliftingMachineError" .= object
        [ "UnliftingError" .= do t :: Text
        ]
      ]
    PlutusCore.BuiltinTermArgumentExpectedMachineError -> "BuiltinTermArgumentExpectedMachineError"
    PlutusCore.UnexpectedBuiltinTermArgumentMachineError -> "UnexpectedBuiltinTermArgumentMachineError"
    PlutusCore.EmptyBuiltinArityMachineError -> "EmptyBuiltinArityMachineError"
    PlutusCore.UnknownBuiltin fun -> object
      [ "UnknownBuiltin" .= do fun :: fun
      ]

textShow :: Show a => a -> Text
textShow = Text.pack . CP.show

showLastAppBlockNo :: WithOrigin (LastAppliedBlock crypto) -> Text
showLastAppBlockNo wOblk =  case withOriginToMaybe wOblk of
                     Nothing -> "Genesis Block"
                     Just blk -> textShow . unBlockNo $ labBlockNo blk

-- The following instances aren't used above

instance ToJSON Alonzo.TagMismatchDescription where
  toJSON = \case
    Alonzo.PassedUnexpectedly -> object
      [ "kind"  .= String "TagMismatchDescription"
      , "error" .= String "PassedUnexpectedly"
      ]
    Alonzo.FailedUnexpectedly forReasons -> object
      [ "kind"            .= String "TagMismatchDescription"
      , "error"           .= String "FailedUnexpectedly"
      , "reconstruction"  .= do NEL.toList forReasons :: [Alonzo.FailureDescription]
      ]

renderTxId :: SupportsMempool.TxId (GenTx (ShelleyBlock protocol era)) -> Text
renderTxId (ShelleyTxId shelleyTxId) =
    Text.decodeLatin1
  . B16.encode
  . Crypto.hashToBytes
  . Ledger.extractHash
  . Ledger._unTxId
  $ shelleyTxId

renderBadInputsUTxOErr ::  Set (TxIn era) -> Value
renderBadInputsUTxOErr txIns
  | Set.null txIns = String "The transaction contains no inputs."
  | otherwise = String "The transaction contains inputs that do not exist in the UTxO set."

renderValueNotConservedErr :: Show val => val -> val -> Value
renderValueNotConservedErr consumed produced = String $
  "This transaction consumed " <> show consumed <> " but produced " <> show produced
