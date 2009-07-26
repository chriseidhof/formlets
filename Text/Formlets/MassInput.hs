module Text.Formlets.MassInput where

import Text.XHtml.Strict.Formlets
import qualified Text.Formlets as F
import Text.XHtml.Strict ((!), (+++), (<<))
import Control.Applicative
import qualified Text.XHtml.Strict as X
import Data.Monoid

massInput :: (Applicative m, Monad m)
          => (XHtmlFormlet m a) -- ^ A formlet for a single a
          -> (X.Html -> X.Html) -- ^ This should add at least one wrapper tag around every item
          -> (X.Html -> X.Html) -- ^ This will add an optional wrapper tag around the whole list
          -> XHtmlFormlet m [a]
massInput formlet itemWrapper listWrapper defaults = 
  plug (buttons . listWrapper) $ F.massInput (plug' itemWrapper formlet) defaults
  where buttons x = buttonHtml +++ x
        buttonHtml = X.noHtml

