-- Numerical expressions
data Expr = Lit Integer                -- Literal value
          | Op Ops Expr Expr           -- Operation on two expressions
          | If BExp Expr Expr          -- Conditional expression: if b then e1 else e2

-- Boolean expressions
data BExp = BoolLit Bool               -- Boolean literal
          | And BExp BExp              -- Boolean conjunction (AND)
          | Not BExp                   -- Boolean negation (NOT)
          | Equal Expr Expr            -- Equality of two expressions
          | Greater Expr Expr          -- Greater-than comparison of two expressions

-- Arithmetic operations
data Ops = Add | Sub | Mul | Div | Mod  -- Operations on integers

eval :: Expr -> Integer
eval (Lit n) = n
eval (Op Add e1 e2) = eval e1 + eval e2
eval (Op Sub e1 e2) = eval e1 - eval e2
eval (Op Mul e1 e2) = eval e1 * eval e2
eval (Op Div e1 e2) = eval e1 `div` eval e2
eval (Op Mod e1 e2) = eval e1 `mod` eval e2
eval (If b e1 e2) = if bEval b then eval e1 else eval e2  -- Conditional evaluation


bEval :: BExp -> Bool
bEval (BoolLit b) = b
bEval (And b1 b2) = bEval b1 && bEval b2
bEval (Not b) = not (bEval b)
bEval (Equal e1 e2) = eval e1 == eval e2
bEval (Greater e1 e2) = eval e1 > eval e2

instance Show Ops where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"
    show Mod = "%"

instance Show Expr where
    show (Lit n) = show n
    show (Op op e1 e2) = "(" ++ show e1 ++ " " ++ show op ++ " " ++ show e2 ++ ")"
    show (If b e1 e2) = "if " ++ show b ++ " then " ++ show e1 ++ " else " ++ show e2

instance Show BExp where
    show (BoolLit b) = show b
    show (And b1 b2) = "(" ++ show b1 ++ " && " ++ show b2 ++ ")"
    show (Not b) = "not " ++ show b
    show (Equal e1 e2) = "(" ++ show e1 ++ " == " ++ show e2 ++ ")"
    show (Greater e1 e2) = "(" ++ show e1 ++ " > " ++ show e2 ++ ")"


expr1 = If (Greater (Lit 10) (Lit 5)) (Lit 1) (Lit 0)
-- This represents: if 10 > 5 then 1 else 0
-- eval expr1 should return 1

expr2 = If (And (BoolLit True) (Equal (Lit 4) (Lit 4))) (Lit 10) (Lit 20)
-- This represents: if True && (4 == 4) then 10 else 20
-- eval expr2 should return 10
