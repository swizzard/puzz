module Token
    (
      -- * @Status@ type
      Status(..)
      -- * token creation
    , token
    , noneToken
    , anyToken
    , randToken
      -- * token operations
    , getStatus
    , getColor
    , replaceStatus
    , replaceColor
    ) where

import System.Random (RandomGen, random)

import Color (Color(..), colors, Special(..), Status(..))

type TokenColor = Either Special Color

data Token = RegularToken Color Status | SpecialToken Special
     deriving (Show)

noneToken = SpecialToken None
anyToken = SpecialToken Any

token :: Color -> Token
token c = RegularToken c Free

randToken :: (RandomGen g) => g -> (Token, g)
randToken g = let (c, g') = (random g) in (token c, g')

randTokens :: (RandomGen g) => g -> Int -> [Token]
randTokens g n = map token $ take n $ colors g

getStatus :: Token -> Maybe Status
getStatus (RegularToken _ s) = Just s
getStatus (SpecialToken _) = Nothing

getColor :: Token -> TokenColor
getColor (RegularToken c _) = Right c
getColor (SpecialToken s) = Left s

replaceStatus :: Token -> Status -> Token
replaceStatus (RegularToken c _) s = RegularToken c s
replaceStatus (SpecialToken c) s = SpecialToken c

replaceColor :: Token -> Color -> Token
replaceColor (RegularToken _ s) c = RegularToken c s
replaceColor (SpecialToken _) c = RegularToken c Free

