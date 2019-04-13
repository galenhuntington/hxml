import System.IO
import System.Exit (die)
import qualified Data.ByteString.Lazy as BL
import Text.Hxml

main :: IO ()
main = do
   inp <- BL.getContents
   case parseHxml inp of
      Left err -> die $ "error:\n" <> show err <> "\n"
      Right str -> BL.hPut stdout str
