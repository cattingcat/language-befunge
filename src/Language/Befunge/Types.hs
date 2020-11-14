module Language.Befunge.Types
  ( CodeField (..),
    getAt,
    setAt,
  )
where

import Data.List ((!!))
import Utils (modifyAt)

newtype CodeField = CodeField [String]
  deriving (Show)

getAt :: (Int, Int) -> CodeField -> Char
getAt (x, y) (CodeField a) =
  let l = a !! (y `mod` length a)
   in l !! (x `mod` length l)

setAt :: (Int, Int) -> Char -> CodeField -> CodeField
setAt (x, y) val (CodeField a) =
  let normY = y `mod` length a
      line = a !! normY
      normX = x `mod` length line
   in CodeField (modifyAt normY (modifyAt normX (const val)) a)
