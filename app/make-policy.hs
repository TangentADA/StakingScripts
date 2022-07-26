import Control.Exception           (throwIO)
import Data.String                 (IsString (..))
import System.Environment          (getArgs)
import Stake           (stakeValidatorHash, stakeValidatorScript, stakeValidatorAddress)
import Plutus.V1.Ledger.Credential as Plutus
import Plutus.V1.Ledger.Crypto     as Plutus
import qualified Ledger            as Plutus
import Ledger                      (Address, PubKeyHash)
import Ledger.Address              (toPubKeyHash)
import SUtils                      (unsafeReadTxOutRef, writeMintingPolicy, writeValidator)


main :: IO ()
main = do
    [file, pkh'] <- getArgs 
    let pkh = fromString pkh' :: PubKeyHash
        p = stakeValidatorScript [pkh]
    e <- writeValidator file p 
    case e of 
        Left err -> throwIO $ userError $ show err
        Right () -> return ()