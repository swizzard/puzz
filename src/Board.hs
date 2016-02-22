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


data Board = Board { w :: Int
                   , h :: Int
                   , b :: V.Vector (V.Vector Token)
                   } deriving (Show)

-- COLUMNS NOT ROWS
emptyBoard :: Int -> Int -> Board
emptyBoard x y = Board x y (V.replicate x $ V.replicate y noneToken)

randBoard :: (RandomGen g) => g -> Int -> Int -> Board
randomBoard g x y = let l = (x * y) in splitV (fromList (randTokens g l)) empty where
    splitV [] a = a
    splitV l a = let (n, r) = splitAt l y in splitV r (cons 
    
BoardPos = newtype (Int, Int)

inBounds :: Board -> BoardPos -> Bool
inBounds (Board w h _) (BoardPos x y) = x < w && y < h

getPos :: Board -> BoardPos -> Maybe Token
getPos b x y = join $ fmap (V.!? y) $ b V.!? x

replaceIn :: Vector a -> Int -> a -> Vector a
replaceIn v i n = (V.take i) V.++
                  (V.singleton n) V.++
                  (V.drop (i + 1))

setPos :: Board -> BoardPos -> Token -> Board
setPos (Board _ _ b) d@(BoardPos x y) t =
    if not (inBounds b d) then b else let r = b V.! x in
        Board w h $ replaceIn b x (replaceIn r y t)

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


