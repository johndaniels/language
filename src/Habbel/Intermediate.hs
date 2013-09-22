import Funcoop.Parser.Tokens
import Data.Map (Map, insert, (!))
import qualified Data.Map
import Control.Monad (foldM_)

type RegId = String

data Register = 
    Register RegId

data Value = 
    Constant Integer |
    Intermediate Register

data FuncCall =
    FuncCall Value Register

data Ir = 
    Ir [FuncCall]

module Funcoop.Interpretor (runProgram) where


type VarsMap = Map String Double

data ProgramState = ProgramState VarsMap

variables (ProgramState map) = map

showExpr :: VarsMap -> Expression -> String
showExpr vars expr = show (evaluate vars expr)

printFunc :: ProgramState -> [Expression] -> IO ProgramState
printFunc state exprs = putStrLn (show (map (showExpr (variables state)) exprs)) >> return state

performOp :: VarsMap -> (Double -> Double -> Double) -> Expression -> Expression -> Double
performOp vars f a b = f (evaluate vars a) (evaluate vars b)

evaluateOp :: VarsMap -> OperatorType -> Expression -> Expression -> Double
evaluateOp vars Add a b = performOp vars (+) a b
evaluateOp vars Subtract a b = performOp vars (-) a b
evaluateOp vars Multiply a b = performOp vars (*) a b
evaluateOp vars Divide a b = performOp vars (/) a b

evaluate :: VarsMap -> Expression -> Double
evaluate vars (IdentifierExpression identifier) = vars ! (getIdName identifier)
evaluate vars (IntegerLiteral int) = fromInteger int
evaluate vars (Operator opType a b) = evaluateOp vars opType a b 

performStatement :: ProgramState -> Statement -> IO ProgramState
performStatement state (Assignment id expr) = 
    let vars = variables state in
        return (ProgramState (insert (getIdName id) (evaluate vars expr) vars))
performStatement state (FunctionCallStatement funcCall) = 
    printFunc state (getFuncArgs funcCall)

runProgram :: CodeFile -> IO ()
runProgram (CodeFile statements) = foldM_ performStatement (ProgramState Data.Map.empty) statements



getIr :: Codefile -> Ir
    
