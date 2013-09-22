import System.IO (readFile)
import Habbel.Parser
import Habbel.Interpretor
import Habbel.Parser.Tokens (CodeFile)
import Data.Maybe
import Debug.Trace

filename = "test.fnp"

doRun :: Maybe CodeFile -> IO ()
doRun (Just codeFile) = runProgram codeFile
doRun Nothing = return ()

trace1 a = trace (show a) a

main = 
    compile filename >>= doRun
