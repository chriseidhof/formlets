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
  plug (wrapperDiv . buttons . listWrapper) $ F.massInput (plug' (itemWrapper ! [X.theclass "massInputItem"]) formlet) defaults

wrapperDiv = X.thediv ! [X.theclass "massInput"]

buttons x = (X.thediv
  ((X.input ! [X.thetype "button", X.strAttr "onclick" "addItem(this); return false;", X.value "Add Item"]) +++
  (X.input ! [X.thetype "button", X.strAttr "onclick" "removeItem(this); return false;", X.value "Remove Last Item"]))) +++ x

jsMassInputCode = unlines
  ["function findItems(button) {"
  ,"  var mainDiv = $(button).parent();"
  ,"  while ( !mainDiv.hasClass('massInput') ) {"
  ,"    mainDiv = $(mainDiv).parent();"
  ,"  }"
  ,"  return $('.massInputItem', mainDiv);"
  ,"}"
  ,"function addItem(button) {"
  ,"  var items = findItems(button);"
  ,"  var item = $(items[items.length-1]);"
  ,"  var newItem = item.clone(true);"
  ,""
  ,"  newItem.html(newItem.html().replace(/fval\\[(\\d+\\.)*(\\d+)\\.(\\d+)\\]/g, "
  ,"    function(a, b, c, d) {"
  ,"      var newC = parseInt(c)+1;"
  ,"      return a.replace(/\\d+\\.\\d+\\]/, newC+'.'+d+']');"
  ,"    }"
  ,"  ));"
  ,""
  ,"  newItem.appendTo(item.parent());"
  ,"}"
  ,"function removeItem(button) {"
  ,"  var items = findItems(button);"
  ,"  if ( items.length > 1 ) {"
  ,"    var item = $(items[items.length-1]);"
  ,"    item.remove();"
  ,"  } else {"
  ,"    alert('Cannot remove any more rows');"
  ,"  }"
  ,"}"
  ]

