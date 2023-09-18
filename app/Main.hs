module Main where
import Data.Char (isSpace, isDigit, isLetter)

-- Test your tokenizer
main :: IO ()
main = do
    let jsonText = "{\"name\": \"John\", \"age\": 30}"
    let tokens = tokenize jsonText
    print tokens
    let parsedJSON = parseJSON tokens
    print parsedJSON

-- Begin met het definiëren van een datatype voor tokens en schrijf een functie die de invoertekst tokeniseert.
data Token = TString String
           | TNumber Integer
           | TBool Bool
           | TNull
           | TStartObject
           | TEndObject
           | TStartArray
           | TEndArray
           | TDoubleQuotes
           | TColon
           | TComma
           deriving (Show, Eq) -- deriving wordt gebruikt om 2 redenen: 1. weergeven van output in main, 2. vergelijken tussen verschillende tokens

tokenize :: String -> [Token]
tokenize [] = []
tokenize (x:xs)
  | isSpace x = tokenize xs
  | x == '{' = TStartObject : tokenize xs
  | x == '}' = TEndObject : tokenize xs
  | x == '[' = TStartArray : tokenize xs
  | x == ']' = TEndArray : tokenize xs
  | x == ',' = TComma : tokenize xs
  | x == '"' = TDoubleQuotes : tokenize xs
  | x == ':' = TColon : tokenize xs
  | isDigit x =
    let (numStr, rest) = span isDigit (x:xs)
    in TNumber (read numStr) : tokenize rest
  | isLetter x =
    let (str, rest) = span isLetter (x:xs)
    in TString str : tokenize rest
  | otherwise = error ("Ongeldige JSON syntax: onbekend karakter '" ++ [x] ++ "'")

-- Maak datatype aan die JSON waardes representeert
data JSONValue
    = JSONObject [(String, JSONValue)]
    | JSONString String
    | JSONNumber Integer
    | JSONBool Bool
    | JSONNull
    deriving (Show)

-- Helper function to parse a JSON string
parseJSONString :: [Token] -> (JSONValue, [Token])
parseJSONString (TString s : rest) = (JSONString s, rest)
parseJSONString _ = error "Expected a JSON string"

-- Helper function to parse a JSON number
parseJSONNumber :: [Token] -> (JSONValue, [Token])
parseJSONNumber (TNumber n : rest) = (JSONNumber n, rest)
parseJSONNumber _ = error "Expected a JSON number"

-- ... Define similar parsing functions for objects, arrays, booleans, null, etc.

-- Main JSON parsing function
parseJSON :: [Token] -> JSONValue
parseJSON tokens =
  case tokens of
    (TString _) : rest -> fst (parseJSONString tokens)
    (TNumber _) : rest -> fst (parseJSONNumber tokens)
    -- Add cases for objects, arrays, booleans, null, etc. using similar pattern matching
    _ -> error "Invalid JSON"