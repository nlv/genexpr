{-# LANGUAGE OverloadedStrings #-}

import Test.QuickCheck
import Test.QuickCheck.Instances()
import Data.List
import Control.Applicative

data Expr = 
    Mul   Expr Expr |
    Div   Expr Expr |
    Minus Expr Expr |
    Plus  Expr Expr |
    Num   Int       |
    Neg   Expr      
    deriving Show

gen0 :: Int -> Int -> Int -> Gen Expr
gen0 mn mx cnt = do
  n <- choose (mn, mx)
  gen mn mx n cnt

gen :: Int -> Int -> Int -> Int -> Gen Expr
gen mn mx n cnt = do
  opi <- genop cnt {- `suchThat` (\a -> a == 1 || a == 5) -} {- (/= 6) -}
  
  if opi <= 4 then
    gen2 mn mx n cnt opi
  else
    gen1 mn mx n cnt opi

gen2 :: Int -> Int -> Int -> Int -> Int -> Gen Expr
gen2 mn mx n cnt opi = do
    let op = op2 opi
    (n', n'') <- ((,) <$> choose (mn, mx) <*> choose (mn, mx)) `suchThat` valide2 mn mx opi n

    cnt' <- choose (1, cnt - 1)
    let cnt'' = cnt - cnt'

    e' <- gen mn mx n' cnt'
    e'' <- gen mn mx n'' cnt''

    return $ expr e' e''

  where expr :: Expr -> Expr -> Expr
        expr = expr2 opi

valid :: Int -> Int -> Int -> Bool
valid mn mx a = a >= mn && a <= mx

valide2 :: Int -> Int -> Int -> Int -> (Int, Int) -> Bool
valide2 mn mx 2   n (a, b) = b /= 0 && a `mod` b == 0 && valid mn mx a && valid mn mx b && a `div` b == n
valide2 mn mx opi n (a, b) = valid mn mx a && valid mn mx b && a `op` b == n
  where op = op2 opi

gen1 :: Int -> Int -> Int -> Int -> Int -> Gen Expr
gen1 mn mx n cnt 5 = return $ Num n
gen1 mn mx n cnt 6 = Neg <$> gen (-mx) (-mn) (-n) cnt

genop :: Int -> Gen Int
genop 1 = frequency [(10, return 5), (1, return 6)]
genop _ = choose (1, 6)

expr2 :: Int -> (Expr -> Expr -> Expr)
expr2 1 = Mul
expr2 2 = Div
expr2 3 = Minus
expr2 4 = Plus


op2 :: Int -> (Int -> Int -> Int)
op2 1 = (*)
op2 2 = div
op2 3 = (-)
op2 4 = (+)

main :: IO ()
main = do
  s <- sample' $ gen0 (-100) 100 4
  -- mapM_ print $ map ((++ "\n\n") . show . (\(e, s, c) -> e)) s
  mapM_ print $ map ss s
  --mapM_ print $ map ((++ "\n\n") . show) s

ss :: Expr -> String
ss (Num i) = show i
ss (Neg e) = "-(" ++ ss e ++ ")"
ss (Mul e1 e2) = "(" ++ ss e1 ++ ") * (" ++ ss e2 ++ ")"
ss (Div e1 e2) = "(" ++ ss e1 ++ ") / (" ++ ss e2 ++ ")"
ss (Minus e1 e2) = "(" ++ ss e1 ++ ") - (" ++ ss e2 ++ ")"
ss (Plus e1 e2) = "(" ++ ss e1 ++ ") + (" ++ ss e2 ++ ")"

