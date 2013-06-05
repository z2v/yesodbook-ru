import Network.HTTP.Conduit
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as L
import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [urlString] ->
            case parseUrl urlString of
                Nothing -> putStrLn "Извините, некорректный URL"
                Just req -> withManager $ \manager -> do
                    Response _ _ _ lbs <- httpLbs req manager
                    liftIO $ L.putStr lbs
        _ -> putStrLn "Извините, передавайте, пожалуйста, только один URL"
