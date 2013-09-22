module Funcoop.Parser.Tokens where

data CodeFile = 
    CodeFile [Statement]
    deriving Show

data Identifier = 
    Identifier String
    deriving Show

getIdName (Identifier name) = name

data OperatorType =
    Add |
    Subtract |
    Multiply |
    Divide
    deriving Show

data FunctionCall =
    FunctionCall Identifier [Expression]
    deriving Show

getFuncName (FunctionCall id _) = getIdName id
getFuncArgs (FunctionCall _ args) = args

data Statement = 
    Assignment Identifier Expression |
    FunctionCallStatement FunctionCall
    deriving Show

data Expression = 
    IdentifierExpression Identifier |
    IntegerLiteral Integer |
    Operator OperatorType Expression Expression 
    deriving Show

