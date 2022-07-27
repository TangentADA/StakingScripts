{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

import Data.Aeson as Json ( encode )
import Data.ByteString.Lazy qualified as LB
import qualified Data.ByteString.Char8       as BS8
import System.Environment ( getArgs )
import Prelude
import Ledger.Address              (toPubKeyHash)
import Data.String (fromString) 
import Ledger.Tx.CardanoAPI (fromCardanoAddress, FromCardanoError)
import Data.Text (unpack, Text)
import Cardano.Api( scriptDataToJson, ScriptDataJsonSchema(ScriptDataJsonDetailedSchema), deserialiseAddress, AsType(AsAlonzoEra,AsAddressInEra) )
import Cardano.Api.Shelley ( fromPlutusData )
import qualified PlutusTx
import Data.Maybe (fromJust)
import Data.Either (fromRight, fromLeft)
import Data.Time.Clock.POSIX as Time 
import Ledger                      (Address, PubKeyHash)
import Stake           (PersonalStake(PersonalStake), StakeDatum(PStake))



mkPersonalStake :: PubKeyHash -> PersonalStake
mkPersonalStake pkh = PersonalStake pkh 

mkUserDatum :: PubKeyHash -> StakeDatum
mkUserDatum pkh = PStake $ mkPersonalStake pkh 

main :: IO ()
main = do
  [pkh'] <- getArgs
  let pkh = fromString pkh' :: PubKeyHash
      datum   = mkUserDatum pkh
  print $ show pkh
  writeData ("datum-user.json") datum 
  putStrLn "Done"


writeData :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeData file isData = do
  print file
  LB.writeFile file (toJsonString isData)

toJsonString :: PlutusTx.ToData a => a -> LB.ByteString
toJsonString =
  Json.encode
    . scriptDataToJson ScriptDataJsonDetailedSchema
    . fromPlutusData
    . PlutusTx.toData