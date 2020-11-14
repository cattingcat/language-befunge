module Language.Befunge.TH (befunge) where

import Data.List qualified as L
import Data.List.Extra qualified as L
import Language.Befunge
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import System.Random

befunge :: QuasiQuoter
befunge =
  QuasiQuoter
    { quoteExp = compileExp,
      quotePat = err,
      quoteType = err,
      quoteDec = err
    }
  where
    err = error "Use 'befunge' only as expressions"

    compileExp = mkExp . execureProg . cleanupBeginsEnds . rmFist . L.lines

    rmFist :: [String] -> [String]
    rmFist (_ : as) = as
    rmFist [] = error "Empty befunge program"

    cleanupBeginsEnds :: [String] -> [String]
    cleanupBeginsEnds [a] = [rmInitialBound a]
    cleanupBeginsEnds (a : as) = rmBounds a : cleanupBeginsEnds as
    cleanupBeginsEnds _ = error "Impossible"

    rmBounds :: String -> String
    rmBounds = L.drop1 . L.dropEnd1 . L.trim

    rmInitialBound :: String -> String
    rmInitialBound = L.drop1 . L.trim

    execureProg :: [String] -> String
    execureProg ls = interpretCode (mkStdGen 44) (CodeField ls)

    mkExp :: String -> Q Exp
    mkExp l = pure $ LitE (StringL l)
