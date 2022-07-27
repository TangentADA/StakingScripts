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
import Data.ByteString.Lazy qualified as LBS
import qualified Data.ByteString.Char8       as BS8
import System.Environment ( getArgs )
import Prelude
import Ledger.Address              (toPubKeyHash)
import Data.String (fromString) 
import Ledger.Tx.CardanoAPI (fromCardanoAddress, FromCardanoError)
import Data.Text (unpack, Text)
import Cardano.Api
import           Cardano.Api.Shelley   (PlutusScript (..))
import qualified PlutusTx
import Data.Maybe (fromJust)
import Data.Either (fromRight, fromLeft)
import Data.Time.Clock.POSIX as Time 
import Ledger                      (Address, PubKeyHash)
import Stake           (StakeRedeemer(Use,Remove))
import           Codec.Serialise       (serialise)
import qualified Data.ByteString.Short as SBS
import           PlutusTx              (Data (..))
import qualified PlutusTx



dataToScriptData :: Data -> ScriptData
dataToScriptData (Constr n xs) = ScriptDataConstructor n $ dataToScriptData <$> xs
dataToScriptData (Map xs)      = ScriptDataMap [(dataToScriptData x, dataToScriptData y) | (x, y) <- xs]
dataToScriptData (List xs)     = ScriptDataList $ dataToScriptData <$> xs
dataToScriptData (I n)         = ScriptDataNumber n
dataToScriptData (B bs)        = ScriptDataBytes bs

writeJSON :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeJSON file = LBS.writeFile file . encode . scriptDataToJson ScriptDataJsonDetailedSchema . dataToScriptData . PlutusTx.toData

{-- 
mkRedeemer :: String -> String -> IO()
mkRedeemer f "use" = writeJSON f Use
mkRedeemer f "remove" = writeJSON f Remove
mkRedeemer f x = putStrLn "Wrong Input" 
--}

mkRedeemer :: String -> String -> IO()
mkRedeemer f a = case a of
   "use" -> writeJSON f Use
   "remove" -> writeJSON f Remove
   _       -> putStrLn "Wrong input"

main :: IO()
main = do
    [file, a'] <- getArgs
 --   let a = show a' 
    putStrLn "This is a:"
    putStrLn a' 
    mkRedeemer file a'
    putStrLn "Done."   