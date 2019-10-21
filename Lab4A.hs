-- Authors: Axel Widerstrand, Fredrik Hamrefors, Mohammed Jaber
-- Date: 21/10 - 2019

import Poly
import Test.QuickCheck


-- Use the following simple data type for binary operators
data BinOp = AddOp | MulOp

--------------------------------------------------------------------------------
-- * A1
-- Define a data type Expr which represents three kinds of expression:
-- binary operators (use BinOp as a helper type) applied to two expressions,
-- numbers (use Int), and exponentiation x^n.
-- Note that since we consider expressions containing just a single variable,
-- x, your data type should not use String or Char anywhere, since this is
-- not needed.

data Expr = Bin BinOp Expr Expr
          | Numb Int
          | X Int 

--An Example used for testing
ex1 :: Expr
ex1 = Bin MulOp (Bin MulOp (Bin AddOp (Numb 5) (X 4)) (X 0)) (Numb 6)
--------------------------------------------------------------------------------
-- * A2
-- Define the data type invariant that checks that exponents are never negative
prop_Expr :: Expr -> Bool
prop_Expr (X n) = n >= 0
prop_Expr _     = True 

--------------------------------------------------------------------------------
-- * A3
-- Make Expr an instance of Show (along the lines of the example in the lecture)
-- You can use Haskell notation for powers: x^2
-- You should show x^1 as just x. 
showExpr :: Expr -> String
showExpr (Numb n)          = show n
showExpr (X 0)             = show 1
showExpr (X 1)             = "x"
showExpr (X n)             = "x" ++ "^" ++ show n
showExpr (Bin AddOp e1 e2) = showExpr e1 ++ "+" ++ showExpr e2
showExpr (Bin MulOp e1 e2) = showFactor e1 ++ "*" ++ showFactor e2

showFactor :: Expr -> String
showFactor e@(Bin AddOp  _ _) = "(" ++ showExpr e ++ ")"
showFactor e                  = showExpr e


instance Show Expr where
 show = showExpr

--------------------------------------------------------------------------------
-- * A4
-- Make Expr and instance of Arbitrary.
-- Now you can check the data type invariant that you defined in A3 using
-- quickCheck

-- (Optional)
-- Add a definition of function shrink :: Expr -> [Expr] to Arbitrary
-- which gives hints to quickCheck on possible smaller expressions that it
-- could use to find a smaller counterexample for failing tests

instance Arbitrary Expr
  where arbitrary = sized rExpr 
                  

rExpr s = frequency [(1, rNum), (1, rExpo), (s, rBin)]
    where rNum = do
                  n <- arbitrary
                  return (Numb n) 
          rExpo = do 
                  e <- choose (1,9)
                  return $ X e
          rBin  = do
                  op <- elements [AddOp,MulOp]
                  e1 <- rExpr s'
                  e2 <- rExpr s'
                  return $ Bin op e1 e2
                    where s' = s `div` 2

--------------------------------------------------------------------------------
-- * A5
-- Define the eval function which takes a value for x and an expression and
-- evaluates it

eval :: Int -> Expr -> Int
eval x (Numb e)          = e
eval x (X e)             = x^e
eval x (Bin AddOp e1 e2) = eval x e1 + eval x e2
eval x (Bin MulOp e1 e2) = eval x e1 * eval x e2


--------------------------------------------------------------------------------
-- * A6
-- Define
exprToPoly :: Expr -> Poly
-- Which converts an expression into a polynomial.
-- Here it is important to think recursively to just solve the bigger problem
-- by solving the smaller problems and combining them in the right way. 

exprToPoly (Numb n)          = fromList [n]
exprToPoly (X a)             = fromList ([1] ++ replicate a 0)
exprToPoly (Bin AddOp e1 e2) = (exprToPoly e1) + (exprToPoly e2)
exprToPoly (Bin MulOp e1 e2) = (exprToPoly e1) * (exprToPoly e2)

-- Define (and check) prop_exprToPoly, which checks that evaluating the
-- polynomial you get from exprToPoly gives the same answer as evaluating
-- the expression

prop_exprToPoly :: Expr -> Int -> Bool
prop_exprToPoly e i  = (eval i e) == (evalPoly i (exprToPoly e))

--------------------------------------------------------------------------------

-- * A7
-- Now define the function going in the other direction, 
polyToExpr :: Poly -> Expr
polyToExpr p = p2e (toList p)

p2e :: [Int] -> Expr
p2e []      = Numb 0
p2e [x]     = Numb x
p2e (x:xs)  = add (mul (Numb x) (X s)) (p2e xs)
  where s   = length xs

add (Numb 0) e        = e
add e (Numb 0)        = e
add (Numb m) (Numb n) = (Numb (m+n))
add e1 e2             = (Bin AddOp e1 e2)

mul (Numb 0) e        = (Numb 0)
mul e (Numb 0)        = (Numb 0)
mul (Numb 1) e        = e
mul e (Numb 1)        = e
mul (Numb m) (Numb n) = (Numb (m*n))
mul e1 e2             = (Bin MulOp e1 e2)

-- Write (and check) a quickCheck property for this function similar to
-- question 6. 
prop_polyToExpr :: Poly -> Int -> Bool
prop_polyToExpr p i   = (evalPoly i p) == (eval i (polyToExpr p))  

--------------------------------------------------------------------------------
-- * A8
-- Write a function
simplify :: Expr -> Expr
-- which simplifies an expression by converting it to a polynomial
-- and back again
simplify e = (polyToExpr (exprToPoly e))

--------------------------------------------------------------------------------
-- * A9
-- Write a quickCheck property
prop_noJunk :: Expr -> Bool

--that checks that a simplified expression does not contain any "junk":
--where junk is defined to be multiplication by one or zero,
--addition of zero, addition or multiplication of numbers, or x to the
--power zero. (You may need to fix A7)

prop_noJunk e = noJunk (simplify e)

noJunk :: Expr -> Bool
noJunk (Numb n)          = True
noJunk (X n)             = n > 0
noJunk (Bin AddOp e1 e2) = addCheck e1 e2
noJunk (Bin MulOp e1 e2) = mulCheck e1 e2

--Checks if there is a + 0 in the expression
addCheck :: Expr -> Expr -> Bool
addCheck e1 e2 = add' e1 e2

add' _ (Numb 0) = False
add' (Numb 0) _ = False
add' e1 e2      = noJunk e1 && noJunk e2

--Checks if there is a 0 * or 1 * in the expression
mulCheck :: Expr -> Expr -> Bool
mulCheck e1 e2 = mul' e1 e2

mul' e (Numb 0) = False
mul' (Numb 0) e = False
mul' e (Numb 1) = False
mul' (Numb 1) e = False
mul' e1 e2      = noJunk e1 && noJunk e2

--------------------------------------------------------------------------------
