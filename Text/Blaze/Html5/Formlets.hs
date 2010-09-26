{-# LANGUAGE OverloadedStrings #-}
module Text.Blaze.Html5.Formlets
    ( input
    , textarea
    , password
    , hidden
    , inputInteger
    , file
    , checkbox
    , radio
    , Html5Form
    , Html5Formlet
    , module Text.Formlets
    ) where

import Text.Formlets hiding (massInput)
import qualified Text.Formlets as F
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Data.Monoid (mappend, mconcat)

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

-- | A hidden input field
--
hidden :: Monad m => Html5Formlet m String
hidden = input' $ \n v -> H.input ! A.type_ "hidden"
                                  ! A.name (H.stringValue n)
                                  ! A.id (H.stringValue n)
                                  ! A.value (H.stringValue v)

-- | A validated integer component
--
inputInteger :: Monad m => Html5Formlet m Integer
inputInteger x = input (fmap show x) `check` asInteger

-- | A file upload form
--
file :: Monad m => Html5Form m File
file = inputFile $ \n -> H.input ! A.type_ "file"
                                 ! A.name (H.stringValue n)
                                 ! A.id (H.stringValue n)

-- | A checkbox with an optional default value
--
checkbox :: Monad m => Html5Formlet m Bool
checkbox d = (optionalInput (html d)) `check` asBool
  where
    asBool (Just _) = Success True
    asBool Nothing = Success False
    html (Just True) n = H.input ! A.type_ "checkbox" 
                                 ! A.name (H.stringValue n)
                                 ! A.id (H.stringValue n)
                                 ! A.value "on"
                                 ! A.checked "checked"
    html _ n = H.input ! A.type_ "checkbox"
                       ! A.name (H.stringValue n)
                       ! A.id (H.stringValue n)
                       ! A.value "on"

-- | A radio choice
--
radio :: Monad m => [(String, String)] -> Html5Formlet m String
radio choices = input' $ \n v ->
    mconcat $ map (makeRadio n v) $ zip choices [1 ..]
    -- todo: validate that the result was in the choices
  where
    makeRadio name selected ((value, label), idx) = do
        applyAttrs (radio name value id')
        H.label ! A.for (H.stringValue id')
                ! A.class_ "radio"
                $ H.string label
      where
        applyAttrs | selected == value = (! A.checked "checked")
                   | otherwise = id
        id' = name ++ "_" ++ show idx

    radio n v i = H.input ! A.type_ "radio"
                          ! A.name (H.stringValue n)
                          ! A.id (H.stringValue i)
                          ! A.class_ "radio"
                          ! A.value (H.stringValue v)
