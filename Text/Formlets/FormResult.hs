{-# LANGUAGE PatternGuards #-}
module Text.Formlets.FormResult ({-maybeRead, maybeRead', asInteger, tryToEnum,-} toE, fromE, FormResult (..)) where

import Control.Applicative
import qualified Control.Applicative.Error as E

toE :: FormResult a -> E.Failing a
toE (Success a)      = E.Success a
toE (Failure f)      = E.Failure f
toE (NotAvailable e) = E.Failure [e]

fromE :: E.Failing a -> FormResult a
fromE (E.Success a) = Success a
fromE (E.Failure f) = Failure f


data FormResult a
  = Success a
  | Failure [String]
  | NotAvailable String
  deriving (Show) -- DEBUG

instance Applicative FormResult where
   pure = Success
   Failure msgs <*> Failure msgs' = Failure (msgs ++ msgs')
   Success _ <*> Failure msgs' = Failure msgs'
   Failure msgs' <*> Success _ = Failure msgs'
   Success f <*> Success x = Success (f x)
   NotAvailable x <*> _ = NotAvailable x
   _ <*> NotAvailable x = NotAvailable x

instance Functor FormResult where
  fmap f (Success a)      = Success (f a)
  fmap f (Failure msgs)   = Failure msgs
  fmap f (NotAvailable x) = NotAvailable x

-- maybeRead :: Read a => String -> Maybe a
-- maybeRead s | [(i, "")] <- readsPrec 0 s = Just i
--             | otherwise = Nothing
-- 
-- -- | Tries to read a value. Shows an error message when reading fails.
-- maybeRead' :: Read a => String -> String -> FormResult a
-- maybeRead' s msg | Just x <- maybeRead s = Success x
--                  | otherwise = Failure [msg]
-- 
-- -- | Tries to read an Integer
-- asInteger :: String -> FormResult Integer
-- asInteger s = maybeRead' s (s ++ " is not a valid integer")
-- 
-- -- | Tries conversion to an enum
-- tryToEnum :: Enum a => Int -> FormResult a
-- tryToEnum x | value <- toEnum x = Success value
--             | otherwise         = Failure ["Conversion error"]
-- 
