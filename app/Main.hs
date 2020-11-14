module Main (main) where

import Language.Befunge.TH

main :: IO ()
main = do
  putStrLn "Befunge qusi quoter tests:"
  putStrLn tstCountQQ
  putStrLn tstHelloWorldQQ
  putStrLn tst

tstCountQQ :: String
tstCountQQ =
  [befunge|
          |>987v>.v |
          |v456<  : |
          |>321 ^ _@|]

tstHelloWorldQQ :: String
tstHelloWorldQQ =
  [befunge|
          |>25*"!dlroW olleH":v |
          |                v:,_@|
          |                >  ^ |]

tst :: String
tst =
  [befunge|
        |2>:3g" "-!v\  g30          <           |
        | |!`"&":+1_:.:03p>03g+:"&"`|           |
        | @               ^  p3\" ":<           |
        |2 2345678901234567890123456789012345678|]
