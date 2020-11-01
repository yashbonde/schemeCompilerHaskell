-- https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/Parsing

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad ( liftM )

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

-- define data type
data LispVal = Atom String -- An Atom, which stores a String naming the atom
    -- A List, which stores a list of other LispVals (Haskell lists are denoted by brackets);
    -- also called a proper list
    | List [LispVal]
    -- A DottedList, representing the Scheme form (a b . c); also called an improper list.
    -- This stores a list of all elements but the last, and then stores the last element as another field
    | DottedList [LispVal] LispVal
    | Number Integer -- A Number, containing a Haskell Integer
    | String String -- A String, containing a Haskell String
    | Bool Bool -- A Bool, containing a Haskell boolean value

-- define parser for different types
-- exercise 2: Change parseString so that \" gives a literal quote character instead of terminating
-- the string. So what is happening originally is that while parsing character by character if it sees
-- \" it simply skips that "Hello \" World" --> "Hello World", while we would like to see it as
-- "Hello " World"
-- Hint: You may want to replace noneOf "\"" with a new parser action that accepts either a non-quote
-- character or a backslash followed by a quote mark.
-- https://github.com/dstcruz/Write-Yourself-A-Scheme-In-48-Hours/blob/master/ch02/parsing/exercise_3.hs
parseString :: Parser LispVal
parseString = do
    -- In general, use >> if the actions don't return a value,
    -- >>= if you'll be immediately passing that value into the next action
    -- and do-notation otherwise.
    char '"'
    -- x <- many (noneOf "\"")
    x <- many (escapeCharacters <|> (noneOf ['\\', '"']))
    char '"'
    return $ String x

escapeCharacters :: Parser Char
escapeCharacters = do
    -- pass the values that are to be allowed
    char '\\'
    x <- oneOf ['\\', '"', "n", "t", "r"]
    return $ case x of
        "\\" -> x
        '"'  -> x
        'n'  -> '\n'
        't'  -> '\t'
        'r'  -> '\r'

parseAtom :: Parser LispVal
parseAtom = do
    -- <|> the choice operator, This tries the first parser, then if it fails, tries the second.
    -- If either succeeds, then it returns the value returned by that parser.
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)

    -- Note : is "con" operator we can also do as below, but since "first" is a char make a list
    -- let atom = [first] ++ rest
    let atom = first:rest
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        _    -> Atom atom

-- Unfortunately, the result of many1 digit is actually a Parser String so our combined
-- Number . read still can't operate on it. We need a way to tell it to just operate on
-- the value inside the monad, giving us back a Parser LispVal. The standard function
-- liftM does exactly that, so we apply liftM to our Number . read function, and then
-- apply the result of that to our parser.
-- The function composition operator . creates a function that applies its right argument
-- and then passes the result to the left argument.

-- parseNumber :: Parser LispVal
-- parseNumber = liftM (Number . read) $ many1 digit

-- in the above code we see that the flow is get the String from many1 digit
-- 1. get value of many1 digit -> String
-- 2. Convert the String to Int using read
-- 3. return Number type

-- exercise 1a: rewrite parseNumber using do
-- parseNumber :: Parser LispVal
-- parseNumber = do
--     val <- many1 digit
--     return $ Number (read val)

-- exercise 1b: rewrite parseNumber using >>=
-- parseNumber :: Parser LispVal
-- parseNumber = many1 digit >>= (\x -> return ((Number . read) x))

-- exercise 4. Change parseNumber to support the Scheme standard for different bases
-- I like the do format in 1a so will be building on top of it. The problem is like this
-- 
parseSimpleNumber :: Parser LispVal
parseSimpleNumber = do
    val <- many1 digit
    return $ Number (read val)

parseHex :: Parser LispVal
parseHex = char '#' >>
    (

    )


parseNumber :: Parser LispVal
parseNumber = parseSimpleNumber <|> parseHex

parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseString <|> parseNumber

readExpr :: String -> String
-- readExpr input = case parse symbol "lisp" input of
-- readExpr input = case parse (spaces >> symbol) "lisp" input of
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No Match, " ++ show err
    -- Right val -> "Found Value, " ++ show val
    Right _ -> "Found Value"

main :: IO ()
main = do
    (expr:_) <- getArgs
    putStrLn (readExpr expr)
