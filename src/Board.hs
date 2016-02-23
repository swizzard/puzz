module Board
    (
      -- * @Board@ type
    , Board
    , emptyBoard
    , getPos
    , setPos
    ) where

import Control.Monad (join)
import qualified Data.Vector as V

import Puzz.Token

type B = V.Vector (V.Vector Token)

data Board = Board { w :: Int
                   , h :: Int
                   , b :: B
                   } deriving (Show)
instance Functor a => Board Int Int a where
    fmap (Board w h b) = Board w h (f b)

-- COLUMNS NOT ROWS
emptyBoard :: Int -> Int -> Board
emptyBoard x y = Board x y (V.replicate x $ V.replicate y noneToken)

randBoard :: (RandomGen g) => g -> Int -> Int -> Board
randomBoard g x y = let l = (x * y) in splitV (fromList (randTokens g l)) empty where
    splitV [] a = a
    splitV l a = let (n, r) = splitAt l y in splitV r (cons 

replaceIn :: Vector a -> Int -> a -> Vector a
replaceIn v i n = (V.take i) V.++
                  (V.singleton n) V.++
                  (V.drop (i + 1))

data BoardPos = BoardPos Int Int

replaceBoard :: Board -> B -> Board
replaceBoard (Board w h _) b
    | length b != (w * h) = error "board size mismatch"
    | otherwise = Board w h b

inBounds :: Board -> BoardPos -> Maybe BoardPos
inBounds (Board w h _) p@(BoardPos x y)
    | x < w && y < h = Just p
    | otherwise = Nothing

getPos :: Board -> Maybe BoardPos -> Maybe Token
getPos b (Just (BoardPos x y)) = join $ fmap (V.! y) $ b V.! x
getPos b Nothing = Nothing

setPos :: Board -> Maybe BoardPos -> Token -> Board
setPos board@(Board w h b) (Just d@(BoardPos x y)) t = let r = b V.! x in
        replaceBoard board $ replaceIn b x (replaceIn r y t)

-- move token at `s` to `d`, returning `b` unchanged if `s` or `d` are OOB
mvTo :: Board -> BoardPos -> BoardPos -> Board
mvTo (Board _ _ b) s@(BoardPos x1 y1) d@(BoardPos x2 y2) =
    if not (inBounds b d) then b else mv b (getPos b s) d
        where mv b Nothing _ = b
              mv b (Just t) d = setPos b d t

-- cascade tokens downward, adding a new token at the top of the column.
-- 'removing' a token is equivalent to replacing it with the token above it
dropDown :: Board BoardPos Token -> Board
dropDown (Board _ _ b) d@(BoardPos x y) t =
    if not (inBounds b d) then b else dd b d t
        where dd b d@(BoardPos _ 0) t = setPos b d t
              dd b d@(BoardPos x y) _ = let s = (BoardPos x $ y - 1) in
                                          dd (mvTo b s d) s t

remCol :: Board Int [Token] -> Board
remCol board@(Board w _ b) x ts =
    if x >= w then b else replaceIn b x $ fromList ts


