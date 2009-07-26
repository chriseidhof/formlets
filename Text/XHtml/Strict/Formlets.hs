module Text.XHtml.Strict.Formlets ( input, textarea, password, file, checkbox
                                  , hidden, inputInteger, radio, enumRadio
                                  , label
                                  , selectXHtml, selectRaw, select, enumSelect
                                  , XHtmlForm, XHtmlFormlet
                                  , module Text.Formlets
                                  ) where

import Text.Formlets
import qualified Text.XHtml.Strict as X
import Text.XHtml.Strict ((!), (+++), (<<))
import Control.Applicative
import Control.Applicative.Error
import Data.List (elemIndex)

type XHtmlForm m a    = Form X.Html m a
type XHtmlFormlet m a = Formlet X.Html m a

-- | An input field with an optional value
input :: Monad m => XHtmlFormlet m String
input = input' (\n v -> X.textfield n ! [X.value v])

-- | A textarea with optional rows and columns, and an optional value
textarea :: Monad m => Maybe Int -> Maybe Int -> XHtmlFormlet m String
textarea r c = input' (\n v -> X.textarea (X.toHtml v) ! (attrs n))
  where rows = maybe [] (\x -> [X.rows $ show x]) r
        cols = maybe [] (\x -> [X.cols $ show x]) c
        attrs n = [X.name n] ++ rows ++ cols

-- | A password field with an optional value
password :: Monad m => XHtmlFormlet m String
password = input' (\n v -> X.password n ! [X.value v])

-- | A hidden input field
hidden  :: Monad m => XHtmlFormlet m String
hidden  =  input' X.hidden

-- | A validated integer component
inputInteger :: Monad m => XHtmlFormlet m Integer
inputInteger x = input (fmap show x) `check` asInteger 

-- | A file upload form
file :: Monad m => XHtmlForm m File
file = inputFile X.afile

-- | A checkbox with an optional default value
checkbox :: Monad m => XHtmlFormlet m Bool
checkbox d = (optionalInput (xml d)) `check` asBool
  where asBool (Just _) = Success True
        asBool Nothing = Success False
        xml (Just True) n = X.widget "checkbox" n [X.value "on", X.checked]
        xml _ n = X.checkbox n "on"

-- | A radio choice
radio :: Monad m => [(String, String)] -> XHtmlFormlet m String
radio choices = input' mkRadios -- todo: validate that the result was in the choices
 where radio n v i = X.input ! [X.thetype "radio", X.name n, X.identifier i, X.theclass "radio", X.value v]
       mkRadios name selected = X.concatHtml $ map (mkRadio name selected) (zip choices [1..])
       mkRadio  name selected ((value, label), idx) = (radio name value ident) ! attrs 
                                                   +++ X.label (X.toHtml label) ! [X.thefor ident, X.theclass "radio"]
        where attrs | selected == value = [X.checked]
                    | otherwise = []
              ident = name ++ "_" ++ show idx

-- | An radio choice for Enums
enumRadio :: (Monad m, Enum a) => [(a, String)] -> XHtmlFormlet m a
enumRadio values defaultValue = radio (map toS values) (fmap (show . fromEnum) defaultValue) 
                                `check` convert `check` tryToEnum
 where toS = fmapFst (show . fromEnum)
       convert v = maybeRead' v "Conversion error" 

label :: (Monad m, X.HTML h) => h -> Form X.Html m ()
label = xml . X.label . X.toHtml

-- | This is a helper function to generate select boxes
selectXHtml :: (X.HTML h) 
            => [X.HtmlAttr]  -- ^ Optional attributes for the select-box
            -> [(String, h)] -- ^ The values and their labels
            -> String        -- ^ The name
            -> String        -- ^ The value that is selected
            -> X.Html
selectXHtml attr choices name selected = X.select ! (X.name name:attr) $ X.concatHtml $ map (mkChoice selected) choices
  where mkChoice  selected (value, label) = X.option ! (attrs ++ [X.value value]) << label
         where attrs | selected == value = [X.selected]
                     | otherwise = []

-- | A drop-down for selecting values
selectRaw :: (Monad m, X.HTML h) 
          => [X.HtmlAttr]  -- ^ Optional attributes for the select-element
          -> [(String, h)] -- ^ Pairs of value/label
          -> XHtmlFormlet m String
selectRaw attrs choices = input' $ selectXHtml attrs choices -- todo: validate that the result was in the choices

-- | A drop-down for anything that is an instance of Eq
select :: (Eq a, Monad m, X.HTML h) => [X.HtmlAttr] -> [(a, h)] -> XHtmlFormlet m a
select attrs ls v = selectRaw attrs (map f $ zip [0..] ls) selected `check` asInt `check` convert
 where selected       = show <$> (v >>= flip elemIndex (map fst ls))
       f (idx, (_,l)) = (show idx, l)
       convert i      | i >= length ls || i < 0 = Failure ["Out of bounds"]
                      | otherwise               = Success $ fst $ ls !! i
       asInt   s      = maybeRead' s (s ++ " is not a valid int")

-- | A drop-down for all the options from |a|.
enumSelect :: (Enum a, Bounded a, Show a, Eq a, Monad m) 
           => [X.HtmlAttr] -- Optional attributes on the select-box
           -> XHtmlFormlet m a
enumSelect attrs = select attrs (zip items (map show items)) where items = [minBound..maxBound]
