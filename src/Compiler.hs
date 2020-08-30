module Compiler where
import JStrings

data Operator = Add | Subtract | Multiply deriving (Eq, Show)
data Expression a =
    ConstantExpression a
    | BinaryExpression a Operator (Expression a)
    deriving (Eq, Show)

data Token a =
    Function Operator
    | Constant a
    | Invalid
    deriving (Eq, Show)

performOperation :: Operator -> Integer -> Integer -> Integer
performOperation Add = (+)
performOperation Subtract = (-)
performOperation Multiply = (*)

classify :: String -> Token Integer
classify "+" = Function Add
classify "-" = Function Subtract
classify "*" = Function Multiply
classify x = case parseNumber x of
    Just i -> Constant i
    Nothing -> Invalid

tokenize :: String -> [Token Integer]
tokenize input = (map classify (words input))

parseExpr :: [Token Integer] -> Expression Integer
parseExpr [(Constant a)] = ConstantExpression a
parseExpr ((Constant a):(Function op):rest) = BinaryExpression a op (parseExpr rest)
parseExpr _ = error "Malformed expression."

interpretExpr :: Expression Integer -> Integer 
interpretExpr (ConstantExpression a) = a
interpretExpr (BinaryExpression a op expr) = performOperation op a (interpretExpr expr)

main :: IO ()
main = do
        displayWithInput $ id
        displayWithInput $ tokenize
        displayWithInput $ parseExpr . tokenize
        displayWithInput $ interpretExpr . parseExpr . tokenize
    where   input = "23 + 10 * 3"
            displayWithInput f = putStrLn $ show $ f $ input