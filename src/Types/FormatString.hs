{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Types.FormatString where

import Data.Kind (Type)

data FormatString (pre :: Type) (post :: Type) where
  Const :: String -> FormatString xs xs
  IntVal :: FormatString xs (Int -> xs)
  StringVal :: FormatString xs (String -> xs)
  Append :: FormatString ys zs -> FormatString xs ys -> FormatString xs zs

example = Const "Name: " `Append` StringVal `Append` Const ", Gender: " `Append` StringVal `Append` Const ", Age: " `Append` IntVal `Append` Const ", Year: " `Append` IntVal

printfAux :: FormatString xs ys -> (String -> xs) -> String -> ys
printfAux (Const x) f s = f (s <> x)
printfAux IntVal f s = \ i -> f (s <> show i)
printfAux StringVal f s = \ s' -> f (s <> s')
printfAux (Append x y) f s = printfAux x (printfAux y f) s

printf :: FormatString String ys -> ys
printf fs = printfAux fs id ""

example1 = printf example

example2 = example1 "Priyansh" "Male" 22 2023

-- >>> example2
-- "Name: Priyansh, Gender: Male, Age: 22, Year: 2023"

