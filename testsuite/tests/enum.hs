import GHC.Dup
import System.Environment

main = do
    a <- getArgs
    let l = [length a .. 10000]
    case dup l of Box x -> print (length x)
    print (last l)
