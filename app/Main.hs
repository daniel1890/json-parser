module Main where
import Data.Char (isSpace, isDigit, isLetter)
import System.IO

main :: IO ()
main = do
    -- Test 1: Geldige JSON
    jsonText1 <- readJSONFromFile "app/json-files/valid1.json"
    let tokens1 = tokenize jsonText1
    putStrLn ("Tokens (Test 1): " ++ show tokens1)
    let parsedJSON1 = parseJSONObject tokens1
    putStrLn ("Parsed JSON (Test 1): " ++ show parsedJSON1)

    -- -- Test 2: JSON met dubbele komma's
    -- jsonText2 <- readJSONFromFile "app/json-files/invalid1.json"
    -- let tokens2 = tokenize jsonText2
    -- putStrLn ("Tokens (Test 2): " ++ show tokens2)
    -- let parsedJSON2 = parseJSONObject tokens2
    -- putStrLn ("Parsed JSON (Test 2): " ++ show parsedJSON2)

    -- -- Test 3: JSON met ongeldig } teken in het midden
    -- jsonText3 <- readJSONFromFile "app/json-files/invalid2.json"
    -- let tokens3 = tokenize jsonText3
    -- putStrLn ("Tokens (Test 3): " ++ show tokens3)
    -- let parsedJSON3 = parseJSONObject tokens3
    -- putStrLn ("Parsed JSON (Test 3): " ++ show parsedJSON3)

    -- -- Test 4: JSON met ontbrekende dubbele aanhalingstekens rond een key
    -- jsonText4 <- readJSONFromFile "app/json-files/invalid4.json"
    -- let tokens4 = tokenize jsonText4
    -- putStrLn ("Tokens (Test 4): " ++ show tokens4)
    -- let parsedJSON4 = parseJSONObject tokens4
    -- putStrLn ("Parsed JSON (Test 4): " ++ show parsedJSON4)

    -- -- Test 5: JSON met ontbrekende { om object te openen
    -- jsonText5 <- readJSONFromFile "app/json-files/invalid5.json"
    -- let tokens5 = tokenize jsonText5
    -- putStrLn ("Tokens (Test 5): " ++ show tokens5)
    -- let parsedJSON5 = parseJSONObject tokens5
    -- putStrLn ("Parsed JSON (Test 5): " ++ show parsedJSON5)

    -- -- Test 6: JSON met ongeldige nummernotatie (ontbreekt decimaalteken voor een getal)
    -- jsonText6 <- readJSONFromFile "app/json-files/invalid3.json"
    -- let tokens6 = tokenize jsonText6
    -- putStrLn ("Tokens (Test 6): " ++ show tokens6)
    -- let parsedJSON6 = parseJSONObject tokens6
    -- putStrLn ("Parsed JSON (Test 6): " ++ show parsedJSON6)

-- Helper functie om JSON in te lezen van een .json bestand
readJSONFromFile :: FilePath -> IO String
readJSONFromFile = readFile

-- Begin met het definiëren van een datatype voor tokens.
data Token = TString String
           | TNumber Integer
           | TBool Bool
           | TNull
           | TStartObject
           | TEndObject
           | TDoubleQuotes
           | TColon
           | TComma
           deriving (Show) -- deriving wordt gebruikt om 2 redenen: 1. weergeven van output in main

-- Een functie die de invoertekst tokeniseert.
tokenize :: String -> [Token]
tokenize [] = []
tokenize (x:xs)
  | isSpace x = tokenize xs
  | x == '{' = TStartObject : tokenize xs
  | x == '}' = TEndObject : tokenize xs
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
            let (objectPairs, restAfterObject) = parseObjectPairs rest 0
            in
                if null restAfterObject -- Als restAfterObject geen tokens meer bevat return pairs + rest
                then (JSONObject objectPairs, restAfterObject)
                else error ("Onverwachte tokens resterend na het parsen van JSON-object: " ++ show restAfterObject) -- Als nog tokens in de rest array zijn error
        _ -> error ("Ongeldig JSON-object. Tokens: " ++ show tokens)

-- Functie om key-value pairs te parsen 
-- Gebruik commaCount variabele om bij te houden hoeveel komma's in de JSON staan om key-pair values te scheiden
-- commaCount moet bij return altijd lager zijn dan 0, groter dan -2 anders error
parseObjectPairs :: [Token] -> Int -> ([(String, JSONValue)], [Token])
parseObjectPairs tokens commaCount =
    case tokens of
        TEndObject : rest -> if commaCount >= -1 && commaCount < 0
                             then ([], rest)
                             else error ("Commacount moet lager zijn dan 0 en groter dan -2, huidige commacount:"
                             ++ show commaCount ++ ". Controleer voor teveel/weinig komma's in uw JSON input.")
        TComma : rest -> parseObjectPairs rest (commaCount + 1)
        TDoubleQuotes : TString key : TDoubleQuotes : TColon : rest ->
            let (value, restAfterValue) = parseJSONValue rest
                (remainingPairs, restAfterPairs) = parseObjectPairs restAfterValue (commaCount - 1)
            in ((key, value) : remainingPairs, restAfterPairs)
        e -> error ("Ongeldige key-value pairs. Tokens: " ++ show e)

-- Functie om JSONValue te parsen die onderdeel is van een key-value pair
-- Value kan een string, number, null of boolean zijn
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
        e -> error ("Ongeldige JSON-value" ++ show e)

-- -- Main JSON parsing functie
-- parseJSON :: [Token] -> JSONValue
-- parseJSON tokens =
--   case tokens of
--     (TStartObject : rest) ->
--         let (object, restAfterObject) = parseJSONObject tokens
--         in object
--     t -> error ("Ongeldig JSON" ++ show t)


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
