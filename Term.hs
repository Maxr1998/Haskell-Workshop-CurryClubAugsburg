module Term where

data Exp = Var | Add Exp Exp | Sub Exp Exp | Mul Exp Exp | Lit Double
    deriving (Show,Eq)

term1 = (Sub (Add (Lit 2) (Lit 5.1)) (Lit 0.2))
