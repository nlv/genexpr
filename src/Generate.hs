{-# LANGUAGE OverloadedStrings #-}

import Test.QuickCheck
import Test.QuickCheck.Instances()
import Control.Applicative

import qualified Data.Configurator as C

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
gen1 _  _  n _   5 = return $ Num n
gen1 mn mx n cnt 6 = Neg <$> gen (-mx) (-mn) (-n) cnt
gen1 _  _  _ _   _ = error "error in algoritm"

-- | Сгенерировать код операции, в которой может быть задействовано не более указанного количества чисел
genop :: Int -> Gen Int
genop 1 = frequency [(10, return 5), (1, return 6)] -- Понижаем чистату для унарного минуса, чтобы не было много цепочек: -(-(-(-10)))
genop _ = frequency $ (1, return 6) : normal
  where normal = zip [10..] (map return [1 .. 5]) 

-- | Таблица численных кодов бинарных операций (Expr)
expr2 :: Int -> (Expr -> Expr -> Expr)
expr2 1 = Mul
expr2 2 = Div
expr2 3 = Minus
expr2 4 = Plus
expr2 _ = error "error in algoritm"

-- | Таблица численных кодов бинарных операций
op2 :: Int -> (Int -> Int -> Int)
op2 1 = (*)
op2 2 = div
op2 3 = (-)
op2 4 = (+)
op2 _ = error "error in algoritm"

-- | Вычислить значение выражения
eval :: Expr -> Int
eval (Num n) = n
eval (Neg e') = - (eval e')
eval e = (eval e') `op` (eval e'')
    where (op, e', e'') = case e of
                            Mul u' u''   -> ((*), u', u'') 
                            Div u' u''   -> (div, u', u'') 
                            Plus u' u''  -> ((+), u', u'') 
                            Minus u' u'' -> ((-), u', u'') 
                            _            -> error "error in algoritm"

-- | Избавляемся от отрицательных чисел: заменяем на минус положительного числа
eliminateNeg :: Expr -> Expr
eliminateNeg e@(Num n) 
    | n == 0    = Num 0
    | n <  0    = Neg (Num (-n))
    | otherwise = e
eliminateNeg (Neg e)        = Neg (eliminateNeg e)
eliminateNeg (Mul e' e'')   = Mul (eliminateNeg e') (eliminateNeg e'')
eliminateNeg (Div e' e'')   = Div (eliminateNeg e') (eliminateNeg e'')
eliminateNeg (Plus e' e'')  = Plus (eliminateNeg e') (eliminateNeg e'')
eliminateNeg (Minus e' e'') = Minus (eliminateNeg e') (eliminateNeg e'')

-- | Приоритет операций
prior :: Expr -> Int
prior (Mul   _ _) = 3
prior (Div   _ _) = 3
prior (Minus _ _) = 2
prior (Plus  _ _) = 2
prior (Num   _  ) = 4
prior (Neg _)     = 4

-- | режим обработки выражения
data Enclosed = 
      First       -- ^ выражение не является правым операндом никакого другого выражения
    | Enclosed    -- ^ выражение заключено в скобки (при обработке родительского выражения)
    | NotEnclosed -- ^ выражение не заключено в скобки (при обработке родительского выражения)

-- | Отобразить выражение, миимизировав количество скобок 
ss2 :: Expr -> String
ss2 e = ss2' First e

-- | Отобразить выражение, миимизировав количество скобок, с учетом режима обработки выражения
ss2' :: Enclosed -> Expr -> String
ss2' _ (Num i) = show i

ss2' First       (Neg e@(Num _)) = "-" ++ ss2' NotEnclosed e
ss2' Enclosed    (Neg e@(Num _)) = "-" ++ ss2' NotEnclosed e
ss2' NotEnclosed (Neg e@(Num _)) = "(-" ++ ss2' NotEnclosed e ++ ")"

ss2' NotEnclosed (Neg e) = "(-(" ++ ss2' Enclosed e ++ "))"
ss2' _ (Neg e) = "-(" ++ ss2' Enclosed e ++ ")"


ss2' encl e@(Mul e' e'') = l ++ "*" ++ r
    where l = if (prior e <= prior e') then ss2' encl e' else "(" ++ ss2' Enclosed e' ++ ")"
          r = case e'' of
               Mul _ _ -> ss2' NotEnclosed e''
               Num _   -> ss2' NotEnclosed e''
               _       -> "(" ++ ss2' Enclosed e'' ++ ")"

ss2' encl e@(Div e' e'') = l ++ "/" ++ r
    where l = if (prior e <= prior e') then ss2' encl e' else "(" ++ ss2' Enclosed e' ++ ")"
          r = case e'' of
               Num _   -> ss2' NotEnclosed e''
               _       -> "(" ++ ss2' Enclosed e'' ++ ")"

ss2' encl e@(Plus e' e'') = l ++ "+" ++ r
    where l = if (prior e <= prior e') then ss2' encl e' else "(" ++ ss2' Enclosed e' ++ ")"
          r = case e'' of
               Plus _ _ -> ss2' NotEnclosed e''
               Num _   -> ss2' NotEnclosed e''
               _       -> "(" ++ ss2' Enclosed e'' ++ ")"

ss2' encl e@(Minus e' e'') = l ++ "-" ++ r
    where l = if (prior e <= prior e') then ss2' encl e' else "(" ++ ss2' Enclosed e' ++ ")"
          r = case e'' of
               Num _   -> ss2' NotEnclosed e''
               _       -> "(" ++ ss2' Enclosed e'' ++ ")"

main :: IO ()
main = do
  conf <- C.load [C.Required "genexp.conf"]
  mn  <- maybe (-100) id <$> C.lookup conf "min"
  mx  <- maybe (100)  id <$> C.lookup conf "max"
  cnt <- maybe 4      id <$> C.lookup conf "cnt"
  s <- sample' $ gen0 mn mx cnt
  let s2 = map eliminateNeg s
  mapM_ print $ map (\e -> ss2 e ++ " = " ++ (show $ eval e)) s2
--  mapM_ print $ zipWith4 (,,,) (map ss s) (map eval s) (map ss2 s2) (map eval s2)

{--
ss :: Expr -> String
ss (Num i) = show i
ss (Neg e) = "-(" ++ ss e ++ ")"
ss (Mul e1 e2) = "(" ++ ss e1 ++ ") * (" ++ ss e2 ++ ")"
ss (Div e1 e2) = "(" ++ ss e1 ++ ") / (" ++ ss e2 ++ ")"
ss (Minus e1 e2) = "(" ++ ss e1 ++ ") - (" ++ ss e2 ++ ")"
ss (Plus e1 e2) = "(" ++ ss e1 ++ ") + (" ++ ss e2 ++ ")"
--}

