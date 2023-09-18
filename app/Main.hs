module Main where
import Data.Char (isSpace, isDigit, isLetter)

-- Test your tokenizer
main :: IO ()
main = do
    let jsonText = "{\"name\": \"John\", \"age\": 30, \"school\": \"HAN\", \"isHanStudent\": true}"
    --print $ tokenize jsonText
    let tokens = tokenize jsonText
    print ("Tokens: " ++ show tokens)
    let parsedJSON = parseJSON tokens
    print parsedJSON

-- Begin met het definiëren van een datatype voor tokens.
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

-- Een functie die de invoertekst tokeniseert.
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
    in
      case str of
        "true" -> TBool True : tokenize rest
        "false" -> TBool False : tokenize rest
        _ -> TString str : tokenize rest    
  | otherwise = error ("Ongeldige JSON syntax: onbekend karakter '" ++ [x] ++ "'")

-- Maak datatype aan die JSON waardes representeert
data JSONValue
    = JSONObject [(String, JSONValue)]
    | JSONArray [JSONValue]
    | JSONString String
    | JSONNumber Integer
    | JSONBool Bool
    | JSONNull
    deriving (Show)

-- Helper functie om een JSONString te parsen
parseJSONString :: [Token] -> (JSONValue, [Token])
parseJSONString (TString s : rest) = (JSONString s, rest)
parseJSONString _ = error "Expected a JSON string"

-- Helper functie om een JSONNumber to parsen
parseJSONNumber :: [Token] -> (JSONValue, [Token])
parseJSONNumber (TNumber n : rest) = (JSONNumber n, rest)
parseJSONNumber _ = error "Expected a JSON number"

-- Helper functie voor het parsen van een JSON-array
parseJSONArray :: [Token] -> ([JSONValue], [Token])
parseJSONArray tokens =
    case tokens of
        TStartArray : rest ->
            let (elements, restAfterArray) = parseArrayElements rest
            in (elements, restAfterArray)
        _ -> error "Ongeldige JSON-array"

-- Helper functie om array-elementen te parsen
parseArrayElements :: [Token] -> ([JSONValue], [Token])
parseArrayElements tokens =
    case tokens of
        TEndArray : rest -> ([], rest)
        TComma : rest -> parseArrayElements rest
        _ ->
            let (element, restAfterElement) = parseJSON tokens
                (remainingElements, restAfterElements) = parseArrayElements restAfterElement
            in (element : remainingElements, restAfterElements)

-- Helper functie om een JSON-object te parsen
parseJSONObject :: [Token] -> (JSONValue, [Token])
parseJSONObject tokens =
    case tokens of
        TStartObject : rest ->
            let (objectPairs, restAfterObject) = parseObjectPairs rest
            in (JSONObject objectPairs, restAfterObject)
        _ -> error ("Ongeldig JSON-object. Tokens: " ++ show tokens)

-- Helper functie om objectparen te parsen (key-value pairs)
parseObjectPairs :: [Token] -> ([(String, JSONValue)], [Token])
parseObjectPairs tokens =
    case tokens of
        TEndObject : rest -> ([], rest)
        TComma : rest -> parseObjectPairs rest
        TDoubleQuotes : TString key : TDoubleQuotes : TColon : rest ->
            let (value, restAfterValue) = parseJSONValue rest -- Hier wordt de functie parseJSONValue gebruikt om de waarde te matchen
                (remainingPairs, restAfterPairs) = parseObjectPairs restAfterValue
            in ((key, value) : remainingPairs, restAfterPairs)
        _ -> error ("Ongeldige objectparen. Tokens: " ++ show tokens)

-- Functie om JSONValue te parsen die onderdeel is van een key-value pair
-- Values kan een string, number of boolean zijn
parseJSONValue :: [Token] -> (JSONValue, [Token])
parseJSONValue (TDoubleQuotes : TString s : TDoubleQuotes : rest) = (JSONString s, rest)
parseJSONValue (TNumber n : rest) = (JSONNumber n, rest)
parseJSONValue (TBool b : rest) = (JSONBool b, rest)
parseJSONValue _ = error "Ongeldige JSON-value"

-- Helper functie om JSON-boolean te parsen
parseJSONBool :: [Token] -> (JSONValue, [Token])
parseJSONBool tokens =
    case tokens of
        TBool b : rest -> (JSONBool b, rest)
        _ -> error "Verwachtte een JSON-boolean"

-- Main JSON parsing function
parseJSON :: [Token] -> (JSONValue, [Token])
parseJSON tokens =
  case tokens of
    (TString s) : rest -> (JSONString s, rest)
    (TNumber n) : rest -> (JSONNumber n, rest)
    (TStartArray : rest) ->
        let (array, restAfterArray) = parseJSONArray rest
        in (JSONArray array, restAfterArray)
    (TStartObject : rest) ->
        let (object, restAfterObject) = parseJSONObject tokens  -- Geef tokens in plaats van rest door
        in (object, restAfterObject)
    (TBool _) : rest ->
        let (bool, restAfterBool) = parseJSONBool tokens
        in (bool, restAfterBool)
    -- Voeg andere gevallen toe voor null en andere elementen
    t -> error ("Ongeldig JSON" ++ show t)