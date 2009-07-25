{-# LANGUAGE PatternGuards #-}
module Text.Formlets ( input', inputM', optionalInput, inputFile, fmapFst, nothingIfNull
                     , check, ensure, ensures
                     , ensureM, checkM, pureM
                     , runFormState 
                     , massInput
                     , xml, plug
                     , Env , Form
                     , File (..), ContentType (..), FormContentType (..)
                     , maybeRead, maybeRead', asInteger, tryToEnum, FailingForm (..)
                     )
                     where

import Data.Monoid
import Control.Applicative
import Control.Applicative.State
import Data.Maybe (isJust)
import Data.List (intercalate)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Traversable as T

-- START OF FAILINGFORM
data FailingForm a
  = Success a
  | Failure [String]
  | NotAvailable String
  deriving (Show) -- DEBUG

instance Applicative FailingForm where
   pure = Success
   Failure msgs <*> Failure msgs' = Failure (msgs ++ msgs')
   Success _ <*> Failure msgs' = Failure msgs'
   Failure msgs' <*> Success _ = Failure msgs'
   Success f <*> Success x = Success (f x)
   NotAvailable x <*> _ = NotAvailable x
   _ <*> NotAvailable x = NotAvailable x

instance Functor FailingForm where
  fmap f (Success a)      = Success (f a)
  fmap f (Failure msgs)   = Failure msgs
  fmap f (NotAvailable x) = NotAvailable x

maybeRead :: Read a => String -> Maybe a
maybeRead s | [(i, "")] <- readsPrec 0 s = Just i
            | otherwise = Nothing

-- | Tries to read a value. Shows an error message when reading fails.
maybeRead' :: Read a => String -> String -> FailingForm a
maybeRead' s msg | Just x <- maybeRead s = Success x
                 | otherwise = Failure [msg]

-- | Tries to read an Integer
asInteger :: String -> FailingForm Integer
asInteger s = maybeRead' s (s ++ " is not a valid integer")

-- | Tries conversion to an enum
tryToEnum :: Enum a => Int -> FailingForm a
tryToEnum x | value <- toEnum x = Success value
            | otherwise         = Failure ["Conversion error"]

-- END OF FAILINGFORM


-- Form stuff
type Env = [(String, Either String File)]
type FormState = [Integer]
type Name = String
data FormContentType = UrlEncoded | MultiPart deriving (Eq, Show, Read)
newtype Form xml m a = Form { deform :: Env -> State FormState (m (State FormState (FailingForm a)), m xml, FormContentType) }
data File = File {content :: BS.ByteString, fileName :: String, contentType :: ContentType} deriving (Eq, Show, Read)
data ContentType = ContentType { ctType :: String
                               , ctSubtype :: String
                               , ctParameters :: [(String, String)]
                               }
                               deriving (Eq, Show, Read)

-- | Apply a predicate to a value and return Success or Failure as appropriate
ensure :: Show a 
       => (a -> Bool) -- ^ The predicate
       -> String      -- ^ The error message, in case the predicate fails
       -> a           -- ^ The value
       -> FailingForm a
ensure p msg x | p x = Success x
               | otherwise = Failure [msg]

ensureM :: (Monad m, Show a)
       => (a -> m Bool) -- ^ The predicate
       -> String      -- ^ The error message, in case the predicate fails
       -> a           -- ^ The value
       -> m (FailingForm a)
ensureM p msg x = do result <- p x
                     return $ if result then Success x else Failure [msg]

-- | Apply multiple predicates to a value, return Success or all the Failure messages
ensures :: Show a
        => [(a -> Bool, String)] -- ^ List of predicate functions and error messages, in case the predicate fails
        -> a                     -- ^ The value
        -> FailingForm a
ensures ps x | null errors = Success x
             | otherwise   = Failure errors
    where errors = [ err | (p, err) <- ps, not $ p x ]

-- | Helper function for genereting input components based forms.
input' :: Monad m => (String -> String -> xml) -> Maybe String -> Form xml m String
input' i = inputM' (\n v -> return $ i n v)

inputM' :: Monad m => (String -> String -> m xml) -> Maybe String -> Form xml m String
inputM' i defaultValue = Form $ \env -> mkInput env <$> freshName
   where mkInput env name = (return $ (freshName >>= \name -> return $ fromLeft name $ (lookup name env)),
                             i name (value name env), UrlEncoded)
         value name env = maybe (maybe "" id defaultValue) fromLeft' (lookup name env)
         fromLeft' (Left x) = x
         fromLeft' _        = ""
         fromLeft n Nothing         = NotAvailable $ n ++ " is not in the data"
         fromLeft n (Just (Left x)) = Success x
         fromLeft n _               = Failure [n ++ " is a file."]

optionalInput :: Monad m => (String -> xml) -> Form xml m (Maybe String)
optionalInput i = Form $ \env -> mkInput env <$> freshName
   where mkInput env name = (return (freshName >>= \name -> return $ fromLeft name $ (lookup name env)),
                             return (i name), UrlEncoded)
         fromLeft n Nothing         = Success Nothing
         fromLeft n (Just (Left x)) = Success (Just x)
         fromLeft n _               = Failure [n ++ " could not be recognized."]

-- | A File input widget.
inputFile :: Monad m 
          => (String -> xml)  -- ^ Generates the xml for the file-upload widget based on the name
          -> Form xml m File
inputFile i = Form $ \env -> mkInput env <$> freshName
  where  mkInput env name    = (return (freshName >>= \name -> return $ fromRight name (lookup name env)), return (i name), MultiPart)
         fromRight n Nothing          = NotAvailable $ n ++ " is not in the data"
         fromRight n (Just (Right x)) = Success x
         fromRight n _                = Failure [n ++ " is not a file"]

-- | Runs the form state
runFormState :: Monad m 
             => Env               -- ^ A previously filled environment (may be empty)
             -> Form xml m a      -- ^ The form
             -> (m (FailingForm a), m xml, FormContentType)
runFormState e (Form f) = let (coll, xml, typ) = evalState (f e) initialState
                          in (liftM (flip evalState initialState) coll, xml, typ)
                       where initialState = [0]

-- | Check a condition or convert a result
check :: (Monad m) => Form xml m a -> (a -> FailingForm b) -> Form xml m b
check (Form frm) f = Form $ fmap checker frm
 where checker = fmap $ fmapFst3 (liftM $ liftM $ f')
       f' (Failure x)  = Failure x
       f' (NotAvailable x)  = NotAvailable x
       f' (Success x)  = f x

-- | Monadically check a condition or convert a result
checkM :: (Monad m) => Form xml m a -> (a -> m (FailingForm b)) -> Form xml m b
checkM (Form frm) f = Form $ \env -> checker f (frm env)
 where --checker = 'a'-- fmap $ fmapFst3 (myFunc f) -- 'a' -- (liftM f')

       checker :: Monad m 
               => (a -> m (FailingForm b))
               -> State FormState (m (State FormState (FailingForm a)), m xml, FormContentType)
               -> State FormState (m (State FormState (FailingForm b)), m xml, FormContentType)
       checker f frm = do currentState <- get
                          frm'         <- frm
                          return $ fmapFst3 (transform f. liftM (flip evalState currentState)) frm'
       transform :: Monad m => (a -> m (FailingForm b)) -> m (FailingForm a) ->  m (State FormState (FailingForm b))
       transform f source = source >>= \x -> case x of 
                              Success x      -> liftM return (f x)
                              NotAvailable x -> return $ return $ NotAvailable x
                              Failure x      -> return $ return $ Failure x

instance (Functor m, Monad m) => Functor (Form xml m) where
  fmap f (Form a) = Form $ \env -> (fmap . fmapFst3 . liftM . liftM . fmap) f (a env)

fmapFst  f (a, b)    = (f a, b)
fmapFst3 f (a, b, c) = (f a, b, c)

instance (Monad m, Applicative m, Monoid xml) => Applicative (Form xml m) where
   pure = pureF
   (<*>) = applyF

-- | Pure xml
xml :: Monad m => xml -> Form xml m ()
xml x = Form $ \env -> pure (return (return $ Success ()), return x, UrlEncoded)

-- | Transform the XML component
plug :: (Monad m, Monoid xml) => (xml -> xml1) -> Form xml m a -> Form xml1 m a
f `plug` (Form m) = Form $ \env -> pure plugin <*> m env
   where plugin (c, x, t) = (c, liftM f x, t)

-- The return 
massInput :: (Monoid xml, {-, Applicative m, Monad m-}
              Show a -- DEBUG
          )
          => Maybe [a] -- ^ Nothing gives you a single empty item, Just [] no items and Just ls the specified items
          -> Form xml IO a
          -> Form xml IO [a]
massInput defaults single = Form $ \env -> do
  case defaults of
       Nothing -> do 
                     modify (\x -> 0:0:x)
                     st <- get
                     (collector, xml, contentType) <- (deform single) env
                     let newCollector = liftCollector st collector 
                     return (newCollector, xml, contentType)
       Just [] -> error "Not implemented yet."
       Just ls -> error "Not implemented yet."

liftCollector :: (Show a) => -- DEBUG
{-Monad m
              => -}FormState
              -> IO (State FormState (FailingForm a))
              -> IO (State FormState (FailingForm [a]))
liftCollector st coll = do coll' <- coll
                           let first = evalState coll' st
                               st' = nextItem st
                           result <- case first of
                                 Success x      -> do rest <- liftCollector st' coll
                                                      return (fmap (fmap (x:)) rest)
                                 NotAvailable x -> return (return (Success []))
                                 Failure x      -> do rest <- liftCollector st' coll
                                                      let allFailures = combineFailures x rest
                                                      return allFailures
                           return result

nextItem st = flip execState st $ modify tail >> freshName >> modify (0:) >> get

combineFailures :: [String] -> State FormState (FailingForm [a]) -> State FormState (FailingForm [a])
combineFailures msgs s = do x <- s
                            case x of
                                 Success x -> return $ Failure msgs
                                 Failure f -> return $ Failure (msgs ++ f)

-- do repeatCollector coll
--                         -- TODO: fix this, now only reads exactly one value.
--                         return undefined -- return (fmap (fmap pure) first)
                        

-- repeatCollector coll = zipWith undefined (repeat coll) [0..]
                          
-- | Returns Nothing if the result is the empty String.
nothingIfNull :: (Monad m, Functor m) => Form xml m String -> Form xml m (Maybe String)
nothingIfNull frm = nullToMaybe <$> frm
 where nullToMaybe [] = Nothing
       nullToMaybe x  = Just x

-----------------------------------------------
-- Private methods
-----------------------------------------------

freshName :: State FormState String
freshName = do n <- currentName
               modify (changeHead (+1))
               return n

-- TODO: think of a good name
changeHead f []     = error "changeHead: there is no head"
changeHead f (x:xs) = (f x) : xs

currentName :: State FormState String
currentName = gets $ \xs ->  "fval" ++ (intercalate "." $ reverse $ map show xs)

orT UrlEncoded x = x
orT x UrlEncoded = x
orT x y          = x

pureF :: (Monad m, Monoid xml) => a -> Form xml m a
pureF v = Form $ \env -> pure (return (return $ Success v), return mempty, UrlEncoded)

pureM :: (Monad m, Monoid xml) => m a -> Form xml m a
pureM v = Form $ \env -> pure (liftM (return . Success) v, return mempty, UrlEncoded)

applyF :: (Monad m, Applicative m, Monoid xml) => Form xml m (a -> b) -> Form xml m a -> Form xml m b
(Form f) `applyF` (Form v) = Form $ \env -> combine <$> f env <*> v env
  where combine (v1, xml1, t1) (v2, xml2, t2) = (first v1 v2, (mappend <$> xml1 <*> xml2), t1 `orT` t2)
        first :: Monad m
              => m (State FormState (FailingForm (a -> b))) 
              -> m (State FormState (FailingForm (a     ))) 
              -> m (State FormState (FailingForm (b     ))) 
        first v1 v2 = do x <- v1 -- x :: (m FailingForm (a -> b))
                         y <- v2 -- y :: (m FailingForm a)
                         return $ do x'' <- x -- FailingForm (a -> b)
                                     y'' <- y -- FailingForm b
                                     return (x'' <*> y'')
