module Main (main) where

import Language.Befunge.TH

main :: IO ()
main = do
  putStrLn "Befunge qusi quoter tests:"
  putStrLn countStr
  putStrLn helloWorldStr
  putStrLn fibStr
  putStrLn tst

countStr :: String
countStr =
  [befunge|
          |>987v>.v |
          |v456<  : |
          |>321 ^ _@|]

helloWorldStr :: String
helloWorldStr =
  [befunge|
          |>25*"!dlroW olleH":v |
          |                v:,_@|
          |                >  ^ |]

fibStr :: String
fibStr =
  [befunge|
          |62*1+v>01p001>+v>\:02p\:02gv|
          |     0       ^             <|
          |     .         :p           |
          |     "         .1           |
          |        v 0," "<0           |
          |     "  >1g12-+:|           |
          |     ,          @           |
          |     >^                     |]

tst :: String
tst =
  [befunge|
          |2>:3g" "-!v\  g30          <           |
          | |!`"&":+1_:.:03p>03g+:"&"`|           |
          | @               ^  p3\" ":<           |
          |2 2345678901234567890123456789012345678|]
