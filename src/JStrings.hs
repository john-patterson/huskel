module JStrings(
    parseNumber
) where

parseNumber :: String -> Maybe Integer
parseNumber input = case reads input :: [(Integer, String)] of
    [(i, "")] -> Just i
    _ -> Nothing