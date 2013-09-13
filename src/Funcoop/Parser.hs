module Funcoop.Parser (compile) where

import Funcoop.Parser.Tokens

import Text.Parsec.Prim
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec

import Data.Functor.Identity

import System.IO (readFile)

type CharOperator = Parsec [Char] () (Expression -> Expression -> Expression)

returnLeft :: (Monad m) => m a -> m b -> m a
returnLeft a b = a >>= \x -> b >> return x

whitespace = many (choice [char ' ', char '\t', char '\n', char '\r'])
nonLineWhitespace = many (choice [char ' ', char '\t'])
newLine = lexeme (char '\n')

lexeme :: Parsec [Char] u a -> Parsec [Char] u a
lexeme parser = returnLeft parser nonLineWhitespace

operatorChar :: Char -> CharOperator
operatorChar '+' = char '+' >> return (Operator Add)
operatorChar '-' = char '-' >> return (Operator Subtract)

mulOperator :: CharOperator
mulOperator = char '*' >> return (Operator Multiply)
divOperator :: CharOperator
divOperator = char '/' >> return (Operator Divide)

secondOperator = lexeme $ choice [
        mulOperator,
        divOperator
    ]

firstOperator :: CharOperator
firstOperator = lexeme $ choice [
        char '+' >> return (Operator Add),
        char '-' >> return (Operator Subtract)
    ]

getLiteral :: String -> Expression
getLiteral x = IntegerLiteral $ read x

number :: Parsec [Char] () Expression
number =
    do
        x <- lexeme (many1 digit)
        return (getLiteral x)

identifier :: Parsec String () Identifier
identifier = 
    lexeme (
    do
        first <- letter
        remaining <- (many alphaNum)
        return (Identifier (first:remaining))
    )

identifierExpr :: Parsec String () Expression
identifierExpr = identifier >>= \id -> return (IdentifierExpression id)

parenExpr :: Parsec String () Expression
parenExpr = between (lexeme (char '(')) (lexeme (char ')')) expr

term = choice [
        number,
        identifierExpr,
        parenExpr
    ]

expr2 = chainl1 term secondOperator
expr1 = chainl1 expr2 firstOperator
expr = expr1

argumentList = sepBy expr (lexeme (char ','))

functionCall :: Parsec String () FunctionCall
functionCall = do
    id <- identifier
    args <- between (lexeme (char '(')) (lexeme (char ')')) argumentList
    return (FunctionCall id args)

functionCallStatement =
    functionCall >>= \funcCall -> (return (FunctionCallStatement funcCall))

assignment :: Parsec String () Statement
assignment = do
    id <- identifier
    lexeme (char '=')
    expression <- expr
    return (Assignment id expression)

statement = 
    do
    s <- choice [
            try assignment,
            functionCallStatement
        ]
    newLine
    whitespace
    return s

file = (whitespace >> returnLeft (many1 statement) eof)
    >>= \statements -> (return (CodeFile statements))

getCompiled :: String -> String -> IO (Maybe CodeFile)
getCompiled filename text = 
    case parse file filename text of
        Left e -> putStrLn (show e) >> return Nothing
        Right r -> return (Just r)


compile :: String -> IO (Maybe CodeFile)
compile filename =
    do 
        text <- readFile filename
        getCompiled filename text
        
