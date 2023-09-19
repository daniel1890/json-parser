module Main where
import Data.Char (isSpace, isDigit, isLetter)

-- Test your tokenizer
main :: IO ()
main = do
    let jsonText = "{\"name\": \"John\", \"age\": 30, \"school\": \"HAN\", \"isHanStudent\": true, \"adres\": null}"
    let jsonTextWithArray = "{\"name\": \"John\", \"age\": 30, \"school\": \"HAN\", \"isHanStudent\": true, \"courses\": [\"APP\", \"SWA\", \"PS\"]}"

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
        "null" -> TNull : tokenize rest
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

-- Functie om een JSONString te parsen
parseJSONString :: [Token] -> (JSONValue, [Token])
parseJSONString (TDoubleQuotes : TString s : TDoubleQuotes : rest) = (JSONString s, rest)
parseJSONString e = error ("Verwachtte een JSON string. Foutive token: " ++ show e)

-- Functie om een JSONNumber to parsen
parseJSONNumber :: [Token] -> (JSONValue, [Token])
parseJSONNumber (TNumber n : rest) = (JSONNumber n, rest)
parseJSONNumber e = error ("Verwachtte een JSON number. Foutive token: " ++ show e)

-- Functie om JSONBool te parsen
parseJSONBool :: [Token] -> (JSONValue, [Token])
parseJSONBool (TBool b : rest) = (JSONBool b, rest)
parseJSONBool e = error ("Verwachtte een JSON boolean. Foutive token: " ++ show e)

-- Functie om een JSONNull to parsen
parseJSONNull :: [Token] -> (JSONValue, [Token])
parseJSONNull (TNull : rest) = (JSONNull, rest)
parseJSONNull e = error ("Verwachtte een JSON null. Foutive token: " ++ show e)

-- Functie om een JSONObject te parsen
parseJSONObject :: [Token] -> (JSONValue, [Token])
parseJSONObject tokens =
    case tokens of
        TStartObject : rest ->
            let (objectPairs, restAfterObject) = parseObjectPairs rest
            in (JSONObject objectPairs, restAfterObject)
        _ -> error ("Ongeldig JSON-object. Tokens: " ++ show tokens)

-- Functie om objectparen te parsen (key-value pairs)
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
-- Value kan een string, number of boolean zijn
parseJSONValue :: [Token] -> (JSONValue, [Token])
parseJSONValue tokens =
    case tokens of
        (TDoubleQuotes : TString s : TDoubleQuotes : rest) -> 
            let (JSONString s, rest) = parseJSONString tokens
            in (JSONString s, rest)
        (TNumber n : rest) -> 
            let (JSONNumber n, rest) = parseJSONNumber tokens
            in (JSONNumber n, rest)
        (TBool b : rest) -> 
            let (JSONBool b, rest) = parseJSONBool tokens
            in (JSONBool b, rest)
        (TNull : rest) -> 
            let (JSONNull, rest) = parseJSONNull tokens
            in (JSONNull, rest)
        _ -> error ("Ongeldige JSON-value" ++ show tokens)

-- Main JSON parsing functie
parseJSON :: [Token] -> JSONValue
parseJSON tokens =
  case tokens of
    (TStartObject : rest) ->
        let (object, restAfterObject) = parseJSONObject tokens
        in object
    t -> error ("Ongeldig JSON" ++ show t)


{- JSON ARRAY PARSING
-- Main JSON parsing function
parseJSON :: [Token] -> (JSONValue, [Token])
parseJSON tokens =
  case tokens of
    (TString s) : rest -> (JSONString s, rest)
    (TNumber n) : rest -> (JSONNumber n, rest)
    (TStartArray : rest) ->
        let (array, restAfterArray) = parseJSONArray tokens
        in (JSONArray array, restAfterArray)
    (TStartObject : rest) ->
        let (object, restAfterObject) = parseJSONObject tokens  -- Geef tokens in plaats van rest door
        in (object, restAfterObject)
    (TBool _) : rest ->
        let (bool, restAfterBool) = parseJSONBool tokens
        in (bool, restAfterBool)
    -- Voeg andere gevallen toe voor null en andere elementen
    t -> error ("Ongeldig JSON" ++ show t)


-- Helper functie voor het parsen van een JSON-array
parseJSONArray :: [Token] -> ([JSONValue], [Token])
parseJSONArray tokens =
    case tokens of
        TStartArray : rest ->
            let (elements, restAfterArray) = parseArrayElements rest
            in (elements, restAfterArray)
        _ -> error ("Ongeldige JSON-array" ++ show tokens)

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

-- Functie om JSONValue te parsen die onderdeel is van een key-value pair
-- Values kan een string, number of boolean zijn
parseJSONValue :: [Token] -> (JSONValue, [Token])
parseJSONValue tokens =
    case tokens of
        (TDoubleQuotes : TString s : TDoubleQuotes : rest) -> (JSONString s, rest)
        (TNumber n : rest) -> (JSONNumber n, rest)
        (TBool b : rest) -> (JSONBool b, rest)
        TStartArray : rest ->
            let (array, restAfterArray) = parseJSONArray tokens
            in (JSONArray array, TStartArray : restAfterArray)
        _ -> error ("Ongeldige JSON-value" ++ show tokens)
-}

{- parseJSON functie met empty array op einde []
-- Main JSON parsing functie
parseJSON :: [Token] -> (JSONValue, [Token])
parseJSON tokens =
  case tokens of
    (TString s) : rest -> 
        let (string, restAfterString) = parseJSONString tokens
        in (string, restAfterString)
    (TNumber n) : rest -> 
        let (num, restAfterNum) = parseJSONNumber tokens
        in (num, restAfterNum)
    TNull : rest -> (JSONNull, rest)
    (TStartObject : rest) ->
        let (object, restAfterObject) = parseJSONObject tokens  -- Geef tokens in plaats van rest door
        in (object, restAfterObject)
    (TBool _) : rest ->
        let (bool, restAfterBool) = parseJSONBool tokens
        in (bool, restAfterBool)
    t -> error ("Ongeldig JSON" ++ show t)

(JSONObject [("name",JSONString "John"),("age",JSONNumber 30),("school",JSONString "HAN"),("isHanStudent",JSONBool True)],[])

Nieuwe -> JSONObject [("name",JSONString "John"),("age",JSONNumber 30),("school",JSONString "HAN"),("isHanStudent",JSONBool True)]

-}