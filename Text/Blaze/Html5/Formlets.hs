{-# LANGUAGE OverloadedStrings #-}
module Text.Blaze.Html5.Formlets ( input, textarea, password
                                 , Html5Form, Html5Formlet
                                 , module Text.Formlets
                                 ) where

import Text.Formlets hiding (massInput)
import qualified Text.Formlets as F
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Data.Monoid (mappend)

import Control.Applicative
import Control.Applicative.Error
import Data.List (elemIndex)

type Html5Form m a    = Form H.Html m a
type Html5Formlet m a = Formlet H.Html m a

-- | An input field with an optional value
--
input :: Monad m => Html5Formlet m String
input = input' $ \n v -> H.input ! A.type_ "text"
                                 ! A.name (H.stringValue n)
                                 ! A.id (H.stringValue n)
                                 ! A.value (H.stringValue v)

-- | A textarea with optional rows and columns, and an optional value
--
textarea :: Monad m => Maybe Int -> Maybe Int -> Html5Formlet m String
textarea r c = input' $ \n v -> (applyAttrs n H.textarea) (H.string v)
  where
    applyAttrs n = (! A.name (H.stringValue n)) . rows r . cols c
    rows = maybe id $ \x -> (! A.rows (H.stringValue $ show x))
    cols = maybe id $ \x -> (! A.cols (H.stringValue $ show x))

-- | A password field with an optional value
--
password :: Monad m => Html5Formlet m String
password = input' $ \n v -> H.input ! A.type_ "password"
                                    ! A.name (H.stringValue n)
                                    ! A.id (H.stringValue n)
                                    ! A.value (H.stringValue v)
