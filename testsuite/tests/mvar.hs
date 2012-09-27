import GHC.Dup
import Control.Concurrent.MVar
import Control.Concurrent

main = do
    v <- newEmptyMVar :: IO (MVar Int)
    case deepDup (putMVar v 0) of Box a -> a
    readMVar v >>= print
