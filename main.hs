import System.IO
import qualified Data.ByteString.Lazy as BL
import Hxml

main :: IO ()
main = do
   inp <- BL.getContents
   case parseHxml inp of
      Left err -> hPutStr stderr $ "error:\n" <> show err <> "\n"
      Right str -> BL.hPut stdout str
