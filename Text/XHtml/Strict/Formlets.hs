module Text.XHtml.Strict.Formlets ( input, textarea, password, file, checkbox
                                  , hidden, inputInteger, radio, enumRadio
                                  , label
                                  , selectXHtml, selectRaw, select, enumSelect
                                  , XHtmlForm
                                  , module Text.Formlets
                                  ) where

import Text.Formlets
import qualified Text.XHtml.Strict as X
import Text.XHtml.Strict ((!), (+++), (<<))
import Control.Applicative
import Control.Applicative.Error
import Data.List (elemIndex)

type XHtmlForm m a = Form X.Html m a

-- | An input field with an optional value
input :: Monad m => Maybe String -> XHtmlForm m String
input = input' (\n v -> X.textfield n ! [X.value v])

textarea :: Monad m => Maybe String -> XHtmlForm m String
textarea = input' (\n v -> X.textarea (X.toHtml v) ! [X.name n])

-- | A password field with an optional value
password :: Monad m => Maybe String -> XHtmlForm m String
password = input' (\n v -> X.password n ! [X.value v])

-- | A hidden input field
hidden  :: Monad m => Maybe String -> XHtmlForm m String
hidden  =  input' X.hidden

-- | A validated integer component
inputInteger :: Monad m => Maybe Integer -> XHtmlForm m Integer
inputInteger x = input (fmap show x) `check` asInteger 

file :: Monad m => XHtmlForm m File
file = inputFile X.afile

checkbox :: Monad m => Maybe Bool -> XHtmlForm m Bool
checkbox d = (optionalInput (xml d)) `check` asBool
  where asBool (Just _) = Success True
        asBool Nothing = Success False
        xml (Just True) n = X.widget "checkbox" n [X.value "on", X.checked]
        xml _ n = X.checkbox n "on"

-- | A radio choice
radio :: Monad m => [(String, String)] -> Maybe String -> XHtmlForm m String
radio choices = input' mkRadios -- todo: validate that the result was in the choices
 where radio n v i = X.input ! [X.thetype "radio", X.name n, X.identifier i, X.theclass "radio", X.value v]
       mkRadios name selected = X.concatHtml $ map (mkRadio name selected) (zip choices [1..])
       mkRadio  name selected ((value, label), idx) = (radio name value ident) ! attrs 
                                                   +++ X.label (X.toHtml label) ! [X.thefor ident, X.theclass "radio"]
        where attrs | selected == value = [X.checked]
                    | otherwise = []
              ident = name ++ "_" ++ show idx

-- | An radio choice for Enums
enumRadio :: (Monad m, Enum a) => [(a, String)] -> Maybe a -> XHtmlForm m a
enumRadio values defaultValue = radio (map toS values) (fmap (show . fromEnum) defaultValue) 
                                `check` convert `check` tryToEnum
 where toS = fmapFst (show . fromEnum)
       convert v = maybeRead' v "Conversion error" 

label str = xml $ X.label $ X.toHtml str

selectXHtml attr choices name selected = X.select ! (X.name name:attr) $ X.concatHtml $ map (mkChoice selected) choices
  where mkChoice  selected (value, label) = X.option ! (attrs ++ [X.value value]) << label
         where attrs | selected == value = [X.selected]
                     | otherwise = []

--selectRaw :: Monad m => [(String, String)] -> Maybe String -> XHtmlForm m String
selectRaw attrs choices = input' $ selectXHtml attrs choices -- todo: validate that the result was in the choices

-- | A drop-down for anything that is an instance of Eq
--select :: (Eq a, Monad m) => [(a, String)] -> Maybe a -> XHtmlForm m a
select attrs ls v = selectRaw attrs (map f $ zip [0..] ls) selected `check` asInt `check` convert
 where selected       = show <$> (v >>= flip elemIndex (map fst ls))
       f (idx, (_,l)) = (show idx, l)
       convert i      | i >= length ls || i < 0 = Failure ["Out of bounds"]
                      | otherwise               = Success $ fst $ ls !! i
       asInt   s      = maybeRead' s (s ++ " is not a valid int")

-- enumSelect :: (Show a, Bounded a, Enum a, Eq a, Monad m) => Maybe a -> XHtmlForm m a
enumSelect attrs = select attrs (zip items (map show items)) where items = [minBound..maxBound]
