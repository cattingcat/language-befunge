module Language.Befunge
  ( interpret,
    interpretCode,
    CodeField (..),
  )
where

import Data.Char (digitToInt)
import Data.List (lines)
import Data.Text (singleton)
import Language.Befunge.Types
import System.Random
import Prelude hiding (lines)

data Direction = L | R | U | D
  deriving stock (Show, Eq, Ord, Enum, Bounded)

data InterprData = InterprData
  { stdGen :: StdGen,
    stack :: [Int],
    direction :: Direction,
    stringMode :: Bool,
    output :: String,
    position :: (Int, Int),
    finished :: Bool,
    skipNextCmd :: Bool
  }

initialState :: StdGen -> InterprData
initialState gen = InterprData gen [] R False "" (0, 0) False False

push :: Monad m => Int -> StateT InterprData m ()
push n = modify' (\dt -> dt {stack = n : stack dt})

pop :: Monad m => StateT InterprData m (Maybe Int)
pop = do
  dt <- get
  case stack dt of
    [] -> pure Nothing
    (x : xs) -> do
      put dt {stack = xs}
      pure (Just x)

tell' :: Monad m => String -> StateT InterprData m ()
tell' str = modify' (\dt -> dt {output = output dt ++ str})

finish :: Monad m => StateT InterprData m ()
finish = modify' (\dt -> dt {finished = True})

binOp :: Monad m => (Int -> Int -> Int) -> StateT InterprData m ()
binOp f = do
  a <- pop
  b <- pop
  case (a, b) of
    (Just a', Just b') -> push (b' `f` a')
    _ -> error "binOp empty stack"

divOp :: Monad m => (Int -> Int -> Int) -> StateT InterprData m ()
divOp f = do
  a <- pop
  b <- pop
  case (a, b) of
    (Just a', Just b') -> push (if a' == 0 then 0 else b' `f` a')
    _ -> error "divOp empty stack"

add, sub, mul, div', mod' :: Monad m => StateT InterprData m ()
add = binOp (+)
sub = binOp (-)
mul = binOp (*)
div' = divOp div
mod' = divOp mod

not' :: Monad m => StateT InterprData m ()
not' = do
  v <- pop
  case v of
    Just a -> push (if a == 0 then 1 else 0)
    _ -> error "not' empty stack"

cmp :: Monad m => StateT InterprData m ()
cmp = do
  a <- pop
  b <- pop
  case (a, b) of
    (Just a', Just b') -> push (if b' > a' then 1 else 0)
    _ -> error "cmp empty stack"

mv :: Monad m => Direction -> StateT InterprData m ()
mv d = modify' (\dt -> dt {direction = d})

mvL, mvR, mvU, mvD :: Monad m => StateT InterprData m ()
mvL = mv L
mvR = mv R
mvU = mv U
mvD = mv D

mvRnd :: Monad m => StateT InterprData m ()
mvRnd = do
  dt <- get
  let min' = fromEnum (minBound :: Direction)
  let max' = fromEnum (maxBound :: Direction)
  let (i, gen) = randomR (min', max') (stdGen dt)
  put (dt {stdGen = gen, direction = toEnum i})

mvHVar :: Monad m => StateT InterprData m ()
mvHVar = do
  v <- pop
  case v of
    Just a -> if a == 0 then mvR else mvL
    _ -> error "Empty stack: mvHVar"

mvVVar :: Monad m => StateT InterprData m ()
mvVVar = do
  v <- pop
  case v of
    Just a -> if a == 0 then mvD else mvU
    _ -> error "Empty stack: mvVVar"

setStrMode :: Monad m => Bool -> StateT InterprData m ()
setStrMode mode = modify' (\dt -> dt {stringMode = mode})

isStrMode :: Monad m => StateT InterprData m Bool
isStrMode = stringMode <$> get

beginStrMode, endStrMode :: Monad m => StateT InterprData m ()
beginStrMode = setStrMode True
endStrMode = setStrMode False

duplicateTop :: Monad m => StateT InterprData m ()
duplicateTop = do
  v <- pop
  case v of
    Just a -> push a >> push a
    Nothing -> push 0

swapTop2 :: Monad m => StateT InterprData m ()
swapTop2 = do
  v1 <- pop
  v2 <- pop
  case (v1, v2) of
    (Just a, Just b) -> push a >> push b
    (Just a, Nothing) -> push a >> push 0
    _ -> error "swapTop2 stack is empty"

takeCoord :: Monad m => StateT InterprData m (Int, Int)
takeCoord = do
  v1 <- pop
  v2 <- pop
  case (v1, v2) of
    (Just y, Just x) -> pure (x, y)
    _ -> error "swapTop2 stack is empty"

discardTop :: Monad m => StateT InterprData m ()
discardTop = pop >> pure ()

outInt :: Monad m => StateT InterprData m ()
outInt = do
  v <- pop
  case v of
    Just a -> tell' (show a)
    _ -> error "outInt empty stack"

outASCII :: Monad m => StateT InterprData m ()
outASCII = do
  v <- pop
  case v of
    Just a -> tell' [chr a]
    _ -> error "outASCII empty stack"

moveCursor :: Monad m => StateT InterprData m ()
moveCursor = do
  dt <- get
  let (x, y) = position dt
      newData =
        case direction dt of
          L -> dt {position = (x - 1, y)}
          R -> dt {position = (x + 1, y)}
          U -> dt {position = (x, y - 1)}
          D -> dt {position = (x, y + 1)}
  put newData

setSkipNextCmd :: Monad m => Bool -> StateT InterprData m ()
setSkipNextCmd b = modify' (\dt -> dt {skipNextCmd = b})

checkAndRestoreSkip :: Monad m => StateT InterprData m Bool
checkAndRestoreSkip = do
  dt <- get
  let skip = skipNextCmd dt
  setSkipNextCmd False
  pure skip

popXYV :: Monad m => StateT InterprData m (Int, Int, Int)
popXYV = do
  yv <- pop
  xv <- pop
  vv <- pop
  case (xv, yv, vv) of
    (Just x, Just y, Just v) -> pure (x, y, v)
    _ -> error "Empty stack at popXYV"

makeStep :: Monad m => CodeField -> StateT InterprData m CodeField
makeStep field = do
  InterprData g s d sm o p fin _ <- get
  skip <- checkAndRestoreSkip
  if
      | skip -> moveCursor >> pure field
      | fin -> pure field
      | otherwise -> do
        let c = getAt p field
        isSm <- isStrMode
        if isSm
          then (if c == '"' then endStrMode else push (ord c)) >> moveCursor >> pure field
          else case c of
            '"' -> beginStrMode >> moveCursor >> pure field
            'p' -> do
              (x, y, v) <- popXYV
              moveCursor >> pure (setAt (x, y) (chr v) field)
            other -> mapSymbolToCommand field other >> moveCursor >> pure field

mapSymbolToCommand :: Monad m => CodeField -> Char -> StateT InterprData m ()
mapSymbolToCommand field c = case c of
  digit | digit <= '9' && digit >= '0' -> push (digitToInt digit)
  '+' -> add
  '-' -> sub
  '*' -> mul
  '/' -> div'
  '%' -> mod'
  '!' -> not'
  '`' -> cmp
  '>' -> mvR
  '<' -> mvL
  '^' -> mvU
  'v' -> mvD
  '?' -> mvRnd
  '_' -> mvHVar
  '|' -> mvVVar
  ':' -> duplicateTop
  '\\' -> swapTop2
  '$' -> discardTop
  '.' -> outInt
  ',' -> outASCII
  '#' -> setSkipNextCmd True
  'p' -> error "Implement me: p"
  'g' -> do
    coord <- takeCoord
    push (ord (getAt coord field))
  '@' -> finish
  ' ' -> pure ()
  uch -> error ("Unknown command: " <> singleton uch)

run :: Monad m => CodeField -> StateT InterprData m String
run field = do
  newField <- makeStep field
  dt <- get
  if finished dt
    then pure (output dt)
    else run newField

interpret :: StdGen -> String -> String
interpret gen progStr = interpretCode gen (CodeField (lines progStr))

interpretCode :: StdGen -> CodeField -> String
interpretCode gen field = res
  where
    initState = initialState gen
    (res, _) = runIdentity $ runStateT (run field) initState
