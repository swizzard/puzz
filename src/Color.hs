module Color
    (
      -- * @Color@ type
      Color(..)
    , colors
    , Special(..)
      -- * @Status@ type
    , Status(..)
    ) where

import System.Random (Random, RandomGen, random, randomR, randoms)

data Color = Black | Blue | Green | Pink | Red | Yellow | White 
    deriving (Bounded, Enum, Eq, Show, Read)
instance Random Color where
    random g = let (idx, g') = randomR (fromEnum (minBound :: Color),
                                        fromEnum (maxBound :: Color)) g
               in ((toEnum idx) :: Color, g')
    randomR (lo, hi) g = let (idx, g') = randomR (fromEnum lo, fromEnum hi) g
                         in ((toEnum idx) :: Color, g')

colors :: (RandomGen g) => g -> [Color]
colors = randoms

data Special = Any | None
    deriving (Eq, Show, Read)


data Status = Free | Locked | LockedN Int deriving (Show)

