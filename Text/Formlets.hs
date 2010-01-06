{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}
module Text.Formlets ( input', inputM', optionalInput, generalInput, generalInputMulti, inputFile, fmapFst, nothingIfNull
                     , check, ensure, ensures
                     , ensureM, checkM, pureM
                     , runFormState 
                     , massInput
                     , xml, plug, plug'
                     , Env , Form , Formlet
                     , File (..), ContentType (..), FormContentType (..)
                     )
                     where

import Data.Generics
import Data.Either (partitionEithers)
import Data.Monoid
import Control.Applicative
import Control.Applicative.Error
import Control.Applicative.State
import Data.Maybe (isJust, fromMaybe)
import Data.List (intercalate)
import qualified Text.Formlets.FormResult as FR
import qualified Data.ByteString.Lazy as BS
import qualified Data.Traversable as T

-- Form stuff
type Env = [(String, Either String File)]
type FormState = [Integer]
type Formlet xml m a = Maybe a -> Form xml m a
type Name = String
type S a = State FormState a
type Validator a = S (FR.FormResult a)
data FormContentType = UrlEncoded | MultiPart deriving (Eq, Show, Read)
newtype Form xml m a = Form { deform :: Env -> S (m (Validator a), xml, FormContentType) }
data File = File {content :: BS.ByteString, fileName :: String, contentType :: ContentType} deriving (Eq, Show, Read, Data, Typeable)
data ContentType = ContentType { ctType :: String
                               , ctSubtype :: String
                               , ctParameters :: [(String, String)]
                               }
                               deriving (Eq, Show, Read, Data, Typeable)

-- | Apply a predicate to a value and return FR.Success or FR.Failure as appropriate
ensure :: Show a 
       => (a -> Bool) -- ^ The predicate
       -> String      -- ^ The error message, in case the predicate fails
       -> a           -- ^ The value
       -> Failing a
ensure p msg x | p x = Success x
               | otherwise = Failure [msg]

ensureM :: (Monad m, Show a)
       => (a -> m Bool) -- ^ The predicate
       -> String      -- ^ The error message, in case the predicate fails
       -> a           -- ^ The value
       -> m (Failing a)
ensureM p msg x = do result <- p x
                     return $ if result then Success x else Failure [msg]

-- | Apply multiple predicates to a value, return FR.Success or all the FR.Failure messages
ensures :: Show a
        => [(a -> Bool, String)] -- ^ List of predicate functions and error messages, in case the predicate fails
        -> a                     -- ^ The value
        -> Failing a
ensures ps x | null errors = Success x
             | otherwise   = Failure errors
    where errors = [ err | (p, err) <- ps, not $ p x ]

-- | Helper function for genereting input components based forms.
--          
-- see also 'optionalInput', 'generalInput', and 'generalInputMulti'
input' :: Monad m 
       => (String -> String -> xml) -- ^ function which takes the control name, the initial value, and returns the control markup
       -> Maybe String -- ^ optional default value
       -> Form xml m String
input' i defaultValue = generalInput i' `check` maybe (Failure ["not in the data"]) Success
   where i' n v = i n (fromMaybe (fromMaybe "" defaultValue) v)

{-# DEPRECATED inputM' "You can just use input'"#-}
-- |deprecated. See 'input''
inputM' :: Monad m => (String -> String -> xml) -> Maybe String -> Form xml m String
inputM' = input'

-- | Create a form control which is not required to be successful
--
-- There is no way to provide a default value, because that would
-- result in the control being successful.
-- 
-- For more information on successful controls see:
--
-- <http://www.w3.org/TR/html401/interact/forms.html#successful-controls>
--
-- see also 'input'', 'generalInput', and 'generalInputMulti'
optionalInput :: Monad m 
              => (String -> xml) -- ^ function which takes the form name and produces the control markup
              -> Form xml m (Maybe String)
optionalInput i = generalInput (\n _ -> i n)

-- a combination of lookup and freshName. 
--  1. generate a fresh name
--  2. lookup that name in the environment (returns a Maybe value)
--  3. pass the name and the Maybe value to the function 'f', which returns a value of type 'a'
lookupFreshName :: (Monad m) => (String -> Maybe (Either String File) -> a) -> Env -> m (State FormState a)
lookupFreshName f env = return $ (freshName >>= \name -> return $ f name $ (lookup name env)) 

-- |generate a form control
-- 
-- see also 'input'', 'optionalInput', 'generalInputMulti'.
generalInput :: Monad m =>
                (String -> Maybe String -> xml) -- ^ function which takes the control name, an initial value if one was found in the environment and returns control markup
             -> Form xml m (Maybe String)
generalInput i = Form $ \env -> mkInput env <$> freshName
   where mkInput env name = (lookupFreshName fromLeft env, -- return . result name,
                             i name (value name env), UrlEncoded)
         -- A function to obtain the initial value used to compute the
         -- representation.  The environment is the one passed to
         -- runFormState.  It typically reflects the initial value of
         -- the datatype which the form is meant to represent.
         value name env =
             case lookup name env of
               Just (Left x) -> Just x
               Just (Right _) -> error $ name ++ " is a file."
               Nothing -> Nothing
         -- A function to obtain the form's return value from the
         -- environment returned after the form is run.
         fromLeft n Nothing          = FR.NotAvailable $ n ++ " is not in the data"
         fromLeft n (Just (Left x))  = FR.Success (Just x)
         fromLeft n (Just (Right _)) = FR.Failure [n ++ " is a file, but should not have been."]

-- |generate a form control which can return multiple values
--
-- Useful for controls such as checkboxes and multiple select .
--
-- see also 'input'', 'optionalInput', 'generalInput'.
generalInputMulti :: forall m xml. Monad m =>
                (String -> [String] -> xml)
             -> Form xml m [String]
generalInputMulti i = Form $ \env -> mkInput env <$> freshName
   where mkInput :: Env -> String -> (m (Validator [String]), xml, FormContentType)
         mkInput env name = (return (result env),
                             i name (value name env), UrlEncoded)
         -- A function to obtain the initial value used to compute the
         -- representation.  The environment is the one passed to
         -- runFormState.  It typically reflects the initial value of
         -- the datatype which the form is meanto to represent.
         value :: String -> Env -> [String]
         value name env =
             case partitionEithers $ lookups name env of
               (xs,[]) -> xs
               _ -> error $ name ++ " is a file."
         -- A function to obtain the form's return value from the
         -- environment returned after the form is run.
         result :: Env -> Validator [String]
         result env =
           do name <- freshName
              return $ case partitionEithers $ lookups name env of
                ([],[]) -> FR.NotAvailable $ name ++ " is not in the data."
                (xs,[]) -> FR.Success xs
                _ -> FR.Failure [name ++ " is a file."]
         lookups :: (Eq a) => a -> [(a, b)] -> [b]
         lookups k = map snd . filter ((k ==) . fst)

-- | A File input widget.
inputFile :: Monad m 
          => (String -> xml)  -- ^ Generates the xml for the file-upload widget based on the name
          -> Form xml m File
inputFile i = Form $ \env -> mkInput env <$> freshName
  where  mkInput env name    = (lookupFreshName fromRight env, i name, MultiPart)
         fromRight n Nothing          = FR.NotAvailable $ n ++ " is not in the data"
         fromRight n (Just (Right x)) = FR.Success x
         fromRight n _                = FR.Failure [n ++ " is not a file"]

-- | Runs the form state
runFormState :: Monad m 
             => Env               -- ^ A previously filled environment (may be empty)
             -> Form xml m a      -- ^ The form
             -> (m (Failing a), xml, FormContentType)
runFormState e (Form f) = fmapFst3 (liftM FR.toE . liftM es) (es (f e))
  where es = flip evalState [0]

-- | Check a condition or convert a result
check :: (Monad m) => Form xml m a -> (a -> Failing b) -> Form xml m b
check (Form frm) f = Form $ fmap checker frm
 where checker = fmap $ fmapFst3 (liftM $ liftM $ f')
       f' (FR.Failure x)       = FR.Failure x
       f' (FR.NotAvailable x)  = FR.NotAvailable x
       f' (FR.Success x)       = FR.fromE $ f x

-- | Monadically check a condition or convert a result
checkM :: (Monad m) => Form xml m a -> (a -> m (Failing b)) -> Form xml m b
checkM (Form frm) f = Form $ \env -> checker f (frm env)
 where checker f frm = do currentState <- get
                          (validator, xml, ct) <- frm
                          let validator' = transform f validator currentState
                          return (validator', xml, ct)
                          --return x

       transform :: Monad m => (a -> m (Failing b)) -> m (Validator a) -> FormState -> m (Validator b)
       transform f source st = transform' (makeValidator f) source
        where makeValidator   :: Monad m => (a -> m (Failing b)) -> a -> m (Validator b)
              makeValidator f = fmap (liftM (return . FR.fromE)) f
              transform'  :: Monad m => (a -> m (Validator b)) -> m (Validator a) -> m (Validator b)
              transform' f a = do a' <- a
                                  let (a'', st') = runState a' st
                                  val <- combine f a''
                                  return (changeState st' val)
              changeState :: st -> State st a -> State st a
              changeState st' mComp = do result <- mComp
                                         put st'
                                         return result
       convert :: Monad m => (a -> m (Failing b)) -> (a -> m (FR.FormResult b))
       convert f = fmap (liftM FR.fromE) f
       combine :: Monad m => (a -> m (Validator b)) -> FR.FormResult a -> m (Validator b)
       combine f x = case x of
         (FR.Success x)      -> f x
         (FR.NotAvailable x) -> return . return $ FR.NotAvailable x
         (FR.Failure x)      -> return . return $ FR.Failure x

instance (Functor m, Monad m) => Functor (Form xml m) where
  fmap f (Form a) = Form $ \env -> (fmap . fmapFst3 . liftM . liftM . fmap) f (a env)

fmapFst  f (a, b)    = (f a, b)
fmapFst3 f (a, b, c) = (f a, b, c)

instance (Monad m, Applicative m, Monoid xml) => Applicative (Form xml m) where
   pure = pureF
   (<*>) = applyF

-- | Pure xml
xml :: Monad m => xml -> Form xml m ()
xml x = Form $ \env -> pure (return (return $ FR.Success ()), x, UrlEncoded)

-- | Transform the XML component
plug :: (xml -> xml1) -> Form xml m a -> Form xml1 m a
f `plug` (Form m) = Form $ \env -> pure plugin <*> m env
   where plugin (c, x, t) = (c, f x, t)

plug' :: (xml1 -> xml2) -> Formlet xml1 m a -> Formlet xml2 m a
plug' transformer formlet value = plug transformer (formlet value)

-- | This generates a single (or more) forms for a, and a parser function for a list of a's.
massInput :: (Applicative m, Monad m, Monoid xml)
          => (Formlet xml m a) -- ^ A formlet for a single a
          -> Formlet xml m [a]
massInput single defaults = Form $ \env -> do
  modify (\x -> 0:0:x)
  st <- get
  (collector, xml, contentType) <- (deform $ single Nothing) env
  resetCurrentLevel
  listXml <- generateListXml (single Nothing) env
  let newCollector = liftCollector st collector 
      xml' = case env of
               [] -> xml
               _  -> listXml
  x <- case maybe [] id defaults of
       [] -> return (newCollector, xml', contentType)
       xs -> do resetCurrentLevel
                xmls <- mapM (generateXml single env) xs
                return (newCollector, mconcat xmls, contentType)
  modify (tail.tail)
  return x

generateXml :: Monad m => (Maybe a -> Form xml m a) -> Env -> a -> S xml
generateXml form env value = do (_, xml, _) <- (deform $ form $ Just value) env
                                modify nextItem
                                return xml

resetCurrentLevel :: S ()
resetCurrentLevel = do modify (tail . tail)
                       modify (\x -> 0:0:x)

generateListXml :: (Applicative m, Monad m, Monoid xml) => Form xml m a -> Env -> S xml
generateListXml form env = do n <- currentName
                              case lookup n env of
                                Nothing -> return mempty
                                Just _  -> do (_, xml, _) <- (deform form) env
                                              modify nextItem
                                              rest <- generateListXml form env
                                              return $ mappend xml rest

liftCollector :: (Monad m) => FormState -> m (Validator a) -> m (Validator [a])
liftCollector st coll = do coll' <- coll
                           let st'         = nextItem st
                               computeRest = liftCollector st' coll
                           case evalState coll' st of
                                 FR.Success x      -> do rest <- computeRest
                                                         return (fmap (fmap (x:)) rest)
                                 FR.NotAvailable x -> return (return (FR.Success []))
                                 FR.Failure x      -> do rest <- computeRest
                                                         return $ combineFailures x rest

nextItem st = flip execState st $ modify tail >> freshName >> modify (0:) >> get

combineFailures :: [String] -> Validator [a] -> Validator [a]
combineFailures msgs s = do x <- s
                            case x of
                                 FR.Success x -> return $ FR.Failure msgs
                                 FR.Failure f -> return $ FR.Failure (msgs ++ f)

                          
-- | Returns Nothing if the result is the empty String.
nothingIfNull :: (Monad m, Functor m) => Form xml m String -> Form xml m (Maybe String)
nothingIfNull frm = nullToMaybe <$> frm
 where nullToMaybe [] = Nothing
       nullToMaybe x  = Just x

-----------------------------------------------
-- Private methods
-----------------------------------------------

freshName :: S String
freshName = do n <- currentName
               modify (changeHead (+1))
               return n

-- TODO: think of a good name
changeHead f []     = error "changeHead: there is no head"
changeHead f (x:xs) = (f x) : xs

currentName :: S String
currentName = gets $ \xs ->  "fval[" ++ (intercalate "." $ reverse $ map show xs) ++ "]"

orT UrlEncoded x = x
orT x UrlEncoded = x
orT x y          = x

pureF :: (Monad m, Monoid xml) => a -> Form xml m a
pureF v = Form $ \env -> pure (return (return $ FR.Success v), mempty, UrlEncoded)

pureM :: (Monad m, Monoid xml) => m a -> Form xml m a
pureM v = Form $ \env -> pure (liftM (return . FR.Success) v, mempty, UrlEncoded)

applyF :: (Monad m, Applicative m, Monoid xml) => Form xml m (a -> b) -> Form xml m a -> Form xml m b
(Form f) `applyF` (Form v) = Form $ \env -> combine <$> f env <*> v env
  where combine (v1, xml1, t1) (v2, xml2, t2) = (first v1 v2, (mappend xml1 xml2), t1 `orT` t2)
        first :: Monad m
              => m (Validator (a -> b)) 
              -> m (Validator (a     )) 
              -> m (Validator (b     )) 
        first v1 v2 = do x <- v1
                         y <- v2
                         return $ do x'' <- x
                                     y'' <- y
                                     return (x'' <*> y'')
