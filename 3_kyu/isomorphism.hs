-- https://www.codewars.com/kata/5922543bf9c15705d0000020

module ISO where

import           Data.Void                      ( Void )

type ISO a b = (a -> b, b -> a)

substL :: ISO a b -> (a -> b)
substL = fst

substR :: ISO a b -> (b -> a)
substR = snd

isoBool :: ISO Bool Bool
isoBool = (id, id)

isoBoolNot :: ISO Bool Bool
isoBoolNot = (not, not)

refl :: ISO a a
refl = (id, id)

symm :: ISO a b -> ISO b a
symm (ab, ba) = (ba, ab)

trans :: ISO a b -> ISO b c -> ISO a c
trans (ab, ba) (bc, cb) = (bc . ab, ba . cb)

isoTuple :: ISO a b -> ISO c d -> ISO (a, c) (b, d)
isoTuple (ab, ba) (cd, dc) = (\(a, c) -> (ab a, cd c), \(b, d) -> (ba b, dc d))

isoList :: ISO a b -> ISO [a] [b]
isoList (ab, ba) = (fmap ab, fmap ba)

isoMaybe :: ISO a b -> ISO (Maybe a) (Maybe b)
isoMaybe (ab, ba) = (fmap ab, fmap ba)

isoEither :: ISO a b -> ISO c d -> ISO (Either a c) (Either b d)
isoEither (ab, ba) (cd, dc) = (f, g)
  where
    f (Left  a) = Left (ab a)
    f (Right c) = Right (cd c)
    g (Left  b) = Left (ba b)
    g (Right d) = Right (dc d)

isoFunc :: ISO a b -> ISO c d -> ISO (a -> c) (b -> d)
isoFunc (ab, ba) (cd, dc) = (\f -> cd . f . ba, \g -> dc . g . ab)

isoUnMaybe :: ISO (Maybe a) (Maybe b) -> ISO a b
isoUnMaybe (ab, ba) = (f ab, f ba)
  where
    f func = \x -> case (func $ Just x, func Nothing) of
        (Just y , _      ) -> y
        (_      , Just y ) -> y
        (Nothing, Nothing) -> undefined

isoEU :: ISO (Either [()] ()) (Either [()] Void)
isoEU = (f, g)
  where
    f (Left  xs) = Left (() : xs)
    f (Right ()) = Left []
    g (Left  []      ) = Right ()
    g (Left  (_ : xs)) = Left xs
    g (Right _       ) = undefined

isoSymm :: ((a -> b, b -> a) -> (b -> a, a -> b), (b -> a, a -> b) -> (a -> b, b -> a))
isoSymm = (f, f) where f (ab, ba) = (ba, ab)
