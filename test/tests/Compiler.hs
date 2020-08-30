module Tests.Compiler (suite) where
import Test.HUnit
import qualified John.TestInterop as Interop
import Compiler

performOperation_Add :: Test
performOperation_Add = TestCase ( do
        (assertEqual "performOperation Add zero" (performOperation Add 0 5) 5)
        (assertEqual "performOperation Add negative" (performOperation Add (-10) 5) (-5))
        (assertEqual "performOperation Add positive" (performOperation Add (10) 5) (15))
    )

performOperation_Subtract :: Test
performOperation_Subtract = TestCase ( do
        (assertEqual "performOperation Subtract zero" (performOperation Subtract 0 5) (-5))
        (assertEqual "performOperation Subtract negative" (performOperation Subtract (-10) 5) (-15))
        (assertEqual "performOperation Subtract positive" (performOperation Subtract (10) 5) (5))
    )

performOperation_Multiply :: Test
performOperation_Multiply = TestCase ( do
        (assertEqual "performOperation Multiply zero" (performOperation Multiply 0 5) 0)
        (assertEqual "performOperation Multiply one" (performOperation Multiply 1 5) 5)
        (assertEqual "performOperation Multiply negative" (performOperation Multiply (-10) 5) (-50))
        (assertEqual "performOperation Multiply positive" (performOperation Multiply (10) 5) (50))
    )

tokenize_Number :: Test
tokenize_Number = TestCase ( do
        (assertEqual "tokenize number positive" (tokenize "123") [Constant 123])
        (assertEqual "tokenize number zero" (tokenize "0") [Constant 0])
    )

tokenize_Operator :: Test
tokenize_Operator = TestCase ( do
        (assertEqual "tokenize operator +" (tokenize "+") [Function Add])
        (assertEqual "tokenize operator -" (tokenize "-") [Function Subtract])
        (assertEqual "tokenize operator *" (tokenize "*") [Function Multiply])
    )

tokenize_Invalid :: Test
tokenize_Invalid = TestCase ( do
        (assertEqual "tokenize invalid no ws" (tokenize "1+2") [Invalid])
        (assertEqual "tokenize identifier" (tokenize "test") [Invalid])
    )

tokenize_Expression :: Test
tokenize_Expression = TestCase ( do
        (assertEqual "tokenize expr" (tokenize "1 + 2 * 3 - 4 * test") [
                Constant 1,
                Function Add,
                Constant 2,
                Function Multiply,
                Constant 3,
                Function Subtract,
                Constant 4,
                Function Multiply,
                Invalid
            ])
    )

parse_Number :: Test
parse_Number = TestCase ( do
        (assertEqual "parse 0" (parseExpr [Constant 0]) (ConstantExpression 0))
        (assertEqual "parse 10" (parseExpr [Constant 10]) (ConstantExpression 10))
        (assertEqual "parse -10" (parseExpr [Constant $ -10]) (ConstantExpression $ -10))
    )

parse_Expression :: Test
parse_Expression = TestCase ( do
        (assertEqual "parse simple expression" (parseExpr [Constant 10
                , Function Add
                , Constant 5
            ]) (BinaryExpression 10 Add (ConstantExpression 5)))
        (assertEqual "parse chained expression" (parseExpr [Constant 10
                , Function Add
                , Constant 5
                , Function Multiply
                , Constant 1
                , Function Subtract
                , Constant 30
            ]) (BinaryExpression 10 Add(
                    BinaryExpression 5 Multiply (
                        BinaryExpression 1 Subtract (
                            ConstantExpression 30)))))
        (assertEqual "parse malformed non-arity matching expression"
            (parseExpr [Constant 10, Function Add])
            (BinaryExpression 10 Add Malformed))
        (assertEqual "parse malformed arity-matching expression"
            (parseExpr [Constant 10, Function Add, Function Add])
            (BinaryExpression 10 Add Malformed))
    )

interpret_Number :: Test
interpret_Number = TestCase ( do
        (assertEqual "interpret positive int" (interpretExpr $ ConstantExpression 10) (Just 10))
        (assertEqual "interpret zero int" (interpretExpr $ ConstantExpression 0) (Just 0))
        (assertEqual "interpret negative int" (interpretExpr $ ConstantExpression (-10)) (Just (-10)))
    )

interpret_Expression :: Test
interpret_Expression = TestCase ( do
        (assertEqual "interpret simple add" (interpretExpr $ BinaryExpression 5 Add $ ConstantExpression 10) (Just $ 15))
        (assertEqual "interpret simple subtraction" (interpretExpr $ BinaryExpression 5 Subtract $ ConstantExpression 10) (Just $ -5))
        (assertEqual "interpret simple multiply" (interpretExpr $ BinaryExpression 5 Subtract $ ConstantExpression 10) (Just $ -5))
        (assertEqual "interpret complex add/sub" (interpretExpr $
            BinaryExpression 10 Add $ BinaryExpression 20 Subtract $ ConstantExpression 5) (Just 25))
        (assertEqual "interpret mul no precedence" (interpretExpr $
            BinaryExpression 10 Multiply $ BinaryExpression 20 Add $ ConstantExpression 2) (Just 220))
    )

interpret_Invalid :: Test
interpret_Invalid = TestCase ( do
        (assertEqual "interpret simple invalid" (interpretExpr Malformed) Nothing)
        (assertEqual "interpret invalid in expr" (interpretExpr
            (BinaryExpression 10 Add Malformed)) Nothing)
        (assertEqual "interpret complex in expr" (interpretExpr
            (BinaryExpression 10 Add $ BinaryExpression 20 Multiply Malformed)) Nothing)
    )

suite = Interop.TestSuite {
    Interop.name = "Main.hs tests",
    Interop.tests = [
        performOperation_Add,
        performOperation_Subtract,
        performOperation_Multiply,
        tokenize_Number,
        tokenize_Operator,
        tokenize_Invalid,
        tokenize_Expression,
        parse_Number,
        parse_Expression,
        interpret_Number,
        interpret_Expression,
        interpret_Invalid
    ]
}