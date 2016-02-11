{-# LANGUAGE OverloadedStrings #-}

import Test.QuickCheck
import Test.QuickCheck.Instances()
import Data.List
import Control.Applicative

-- | Арифмитическое выражение
data Expr = 
      Mul   Expr Expr -- ^ Умножение
    | Div   Expr Expr -- ^ Деление
    | Minus Expr Expr -- ^ Вычитание
    | Plus  Expr Expr -- ^ Сложение
    | Num   Int       -- ^ Число 
    | Neg   Expr      -- ^ Изменение знака
    deriving Show

-- | Сгенерировать арифмитическое выражение со значением в указанном диапазоне
gen0 :: 
      Int -- ^ Минимальное значение всего выражения и чисел в выражении
    -> Int -- ^ Максимальное значение всего выражения и чисел в выражении
    -> Int -- ^ Максимальное кол-во чисел в выражении
    -> Gen Expr
gen0 mn mx cnt = do
  n <- choose (mn, mx)
  gen mn mx n cnt

-- | Сгенерировать арифмитическое выражение с указанным значением
gen :: 
      Int -- ^ Минимальное значение чисел в выражении
    -> Int -- ^ Максимальное значение чисел в выражении
    -> Int -- ^ Значение всего выражения
    -> Int -- ^ Максимальное кол-во чисел в выражении
    -> Gen Expr
gen mn mx n cnt = do
  -- фиксируем числовой код операции
  opi <- genop cnt {- `suchThat` (\a -> a == 1 || a == 5) -} {- (/= 6) -}
  
  if opi <= 4 then
    -- бинарная операция
    gen2 mn mx n cnt opi
  else
    -- унарная операция (отрицание) или число
    gen1 mn mx n cnt opi

-- | Сгенерировать указанное бинарное арифмитическое выражение с указанным значением
gen2 :: 
      Int -- ^ Минимальное значение чисел в выражении
    -> Int -- ^ Максимальное значение чисел в выражении
    -> Int -- ^ Значение всего выражения
    -> Int -- ^ Максимальное кол-во чисел в выражении
    -> Int -- ^ Числовой код бинарной операции 
    -> Gen Expr
gen2 mn mx n cnt opi = do
    let op = op2 opi

    -- Определяем какие значения должны принять левая и правая часть выражения
    (n', n'') <- ((,) <$> choose (mn, mx) <*> choose (mn, mx)) `suchThat` valide2 mn mx opi n

    -- Определяем максимальное количество чисел в левой и правой частях выражения
    cnt' <- choose (1, cnt - 1)
    let cnt'' = cnt - cnt'

    -- Генерируем левую и правую часть выражения
    e' <- gen mn mx n' cnt'
    e'' <- gen mn mx n'' cnt''

    return $ expr2 opi e' e''

-- | Попадает ли число в заданный промежуток
valid :: Int -> Int -> Int -> Bool
valid mn mx a = a >= mn && a <= mx

-- | Проверка допустимости значений левого и правого выражений для указанной бинарной операции
valide2 :: 
      Int        -- ^ Минимальное значение чисел в выражении
    -> Int        -- ^ Максимальное значение чисел в выражении
    -> Int        -- ^ Числовой код операции
    -> Int        -- ^ Итоговое значение бинарной операции
    -> (Int, Int) -- ^ Значение левого и правого выражений
    -> Bool
valide2 mn mx 2   n (a, b) = b /= 0 && a `mod` b == 0 && valid mn mx a && valid mn mx b && a `div` b == n
valide2 mn mx opi n (a, b) = valid mn mx a && valid mn mx b && a `op` b == n
  where op = op2 opi

-- | Сгенерировать указанное унарное арифмитическое выражение с указанным значением
gen1 :: 
      Int -- ^ Минимальное значение чисел в выражении
    -> Int -- ^ Максимальное значение чисел в выражении
    -> Int -- ^ Значение всего выражения
    -> Int -- ^ Максимальное кол-во чисел в выражении
    -> Int -- ^ Числовой код унарной операции 
    -> Gen Expr
gen1 mn mx n cnt 5 = return $ Num n
gen1 mn mx n cnt 6 = Neg <$> gen (-mx) (-mn) (-n) cnt

-- | Сгенерировать код операции, в которой может быть задействовано не более указанного количества чисел
genop :: Int -> Gen Int
genop 1 = frequency [(10, return 5), (1, return 6)] -- Понижаем чистату для унарного минуса, чтобы не было много цепочек: -(-(-(-10)))
genop _ = choose (1, 6)

-- | Таблица численных кодов бинарных операций (Expr)
expr2 :: Int -> (Expr -> Expr -> Expr)
expr2 1 = Mul
expr2 2 = Div
expr2 3 = Minus
expr2 4 = Plus

-- | Таблица численных кодов бинарных операций
op2 :: Int -> (Int -> Int -> Int)
op2 1 = (*)
op2 2 = div
op2 3 = (-)
op2 4 = (+)

-- | Вычислить значение выражения
eval :: Expr -> Int
eval (Num n) = n
eval (Neg e') = - (eval e')
eval e = (eval e') `op` (eval e'')
    where (op, e', e'') = case e of
                            Mul e' e''   -> ((*), e', e'') 
                            Div e' e''   -> (div, e', e'') 
                            Plus e' e''  -> ((+), e', e'') 
                            Minus e' e'' -> ((-), e', e'') 


main :: IO ()
main = do
  s <- sample' $ gen0 (-100) 100 4
  mapM_ print $ zipWith (,) (map ss s) (map eval s)


ss :: Expr -> String
ss (Num i) = show i
ss (Neg e) = "-(" ++ ss e ++ ")"
ss (Mul e1 e2) = "(" ++ ss e1 ++ ") * (" ++ ss e2 ++ ")"
ss (Div e1 e2) = "(" ++ ss e1 ++ ") / (" ++ ss e2 ++ ")"
ss (Minus e1 e2) = "(" ++ ss e1 ++ ") - (" ++ ss e2 ++ ")"
ss (Plus e1 e2) = "(" ++ ss e1 ++ ") + (" ++ ss e2 ++ ")"

