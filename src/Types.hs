module Types where

data Op = Add | Subtract | Divide | Multiply | LessThan deriving (Show, Ord, Eq)
data Expression = BinaryOp Op Expression Expression
                  | Digit Double
                  | Var String
                  | Call String [Expression]
                  | Function String [String] Expression
                  | Extern String [String] deriving Show

