data Expr = Lit Integer | Op Ops Expr Expr

data Ops = Add | Sub | Mul | Div | Mod

eval :: Expr -> Integer
eval (Lit n) = n
eval (Op Add e1 e2) = eval e1 + eval e2
eval (Op Sub e1 e2) = eval e1 - eval e2
eval (Op Mul e1 e2) = eval e1 * eval e2
eval (Op Div e1 e2) = eval e1 `div` eval e2
eval (Op Mod e1 e2) = eval e1 `mod` eval e2

showExpr :: Expr -> String
showExpr (Lit n) = show n
showExpr (Op Add e1 e2) = "(" ++ showExpr e1 ++ " + " ++ showExpr e2 ++ ")"
showExpr (Op Sub e1 e2) = "(" ++ showExpr e1 ++ " - " ++ showExpr e2 ++ ")"
showExpr (Op Mul e1 e2) = "(" ++ showExpr e1 ++ " * " ++ showExpr e2 ++ ")"
showExpr (Op Div e1 e2) = "(" ++ showExpr e1 ++ " / " ++ showExpr e2 ++ ")"
showExpr (Op Mod e1 e2) = "(" ++ showExpr e1 ++ " % " ++ showExpr e2 ++ ")"

size :: Expr -> Integer
size (Lit _) = 0
size (Op _ e1 e2) = 1 + size e1 + size e2

expr = Op Add (Lit 1) (Op Mul (Lit 2) (Lit 3))
-- expr = Op Add (Lit 1) (Op Mul (Lit 2) (Op Sub (Lit 3) (Lit 4)))
