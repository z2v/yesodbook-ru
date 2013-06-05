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
                    res <- httpLbs req manager
                    liftIO $ L.putStr $ responseBody res
        _ -> putStrLn "Извините, передавайте, пожалуйста, только один URL"
