module Main (main) where

import Control.Monad (when)
import Language.Befunge
import System.Random

main :: IO ()
main = do
  expectProg prog1 "123456789"
  expectProg prog2 "Hello World!\n"
  expectProg prog3 "23571113171923293137"

prog1 = ">987v>.v \nv456<  : \n>321 ^ _@"

prog2 =
  ">25*\"!dlroW olleH\":v\n"
    ++ "                v:,_@\n"
    ++ "                >  ^"

prog3 = "2>:3g\" \"-!v\\  g30          <\n" ++ " |!`\"&\":+1_:.:03p>03g+:\"&\"`|\n" ++ " @               ^  p3\\\" \":<\n" ++ "2 2345678901234567890123456789012345678"

expectProg :: String -> String -> IO ()
expectProg prog res = when (actualRes /= res) (error $ "Failed, expected: " <> res <> ", actual " <> actualRes)
  where
    actualRes = interpret (mkStdGen 42) prog
