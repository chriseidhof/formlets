module Text.Formlets ( input', inputM', optionalInput, inputFile, fmapFst, nothingIfNull
                     , check, ensure, ensures
                     , ensureM, checkM, pureM
                     , runFormState 
                     , massInput
                     , xml, plug
                     , withPrefix
                     , Env , Form
                     , File (..), ContentType (..), FormContentType (..)
                     )
                     where

import Data.Monoid
import Control.Applicative
import Control.Applicative.Error
import Control.Applicative.State
import Data.Maybe (isJust)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Traversable as T

-- Form stuff
type Env = [(String, Either String File)]
type FormState = (Integer, String)
type Name = String
type Collector a = Env -> a
data FormContentType = UrlEncoded | MultiPart deriving (Eq, Show, Read)
newtype Form xml m a = Form { deform :: Env -> State FormState (Collector (m (Failing a)), m xml, FormContentType) }
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

-- | Apply multiple predicates to a value, return Success or all the Failure messages
ensures :: Show a
        => [(a -> Bool, String)] -- ^ List of predicate functions and error messages, in case the predicate fails
        -> a                     -- ^ The value
        -> Failing a
ensures ps x | null errors = Success x
             | otherwise   = Failure errors
    where errors = [ err | (p, err) <- ps, not $ p x ]

-- | Helper function for genereting input components based forms.
input' :: Monad m => (String -> String -> xml) -> Maybe String -> Form xml m String
input' i = inputM' (\n v -> return $ i n v)

inputM' :: Monad m => (String -> String -> m xml) -> Maybe String -> Form xml m String
inputM' i defaultValue = Form $ \env -> mkInput env <$> freshName
   where mkInput env name = (return . fromLeft name . (lookup name),
                             i name (value name env), UrlEncoded)
         value name env = maybe (maybe "" id defaultValue) fromLeft' (lookup name env)
         fromLeft' (Left x) = x
         fromLeft' _        = ""
         fromLeft n Nothing         = Failure [n ++ " is not in the data"]
         fromLeft n (Just (Left x)) = Success x
         fromLeft n _               = Failure [n ++ " is a file."]

optionalInput :: Monad m => (String -> xml) -> Form xml m (Maybe String)
optionalInput i = Form $ \env -> mkInput env <$> freshName
   where mkInput env name = (return . fromLeft name . (lookup name),
                             return (i name), UrlEncoded)
         fromLeft n Nothing         = Success Nothing
         fromLeft n (Just (Left x)) = Success (Just x)
         fromLeft n _               = Failure [n ++ " could not be recognized."]

-- | A File input widget.
inputFile :: Monad m 
          => (String -> xml)  -- ^ Generates the xml for the file-upload widget based on the name
          -> Form xml m File
inputFile i = Form $ \env -> mkInput env <$> freshName
  where  mkInput env name    = (return . fromRight name . (lookup name), return (i name), MultiPart)
         fromRight n Nothing          = Failure [n ++ " is not in the data"]
         fromRight n (Just (Right x)) = Success x
         fromRight n _                = Failure [n ++ " is not a file"]

-- | Runs the form state
runFormState :: Monad m 
             => Env               -- ^ A previously filled environment (may be empty)
             -> String            -- ^ A prefix for the names
             -> Form xml m a      -- ^ The form
             -> (m (Failing a), m xml, FormContentType)
runFormState e prefix (Form f) = let (coll, xml, typ) = evalState (f e) (0, prefix)
                                 in (coll e, xml, typ)

-- | Check a condition or convert a result
check :: (Monad m) => Form xml m a -> (a -> Failing b) -> Form xml m b
check (Form frm) f = Form $ fmap checker frm
 where checker = fmap $ fmapFst3 (fmap . liftM $ f')
       f' (Failure x)  = Failure x
       f' (Success x)  = f x

-- | Monadically check a condition or convert a result
checkM :: (Monad m) => Form xml m a -> (a -> m (Failing b)) -> Form xml m b
checkM (Form frm) f = Form $ fmap checker frm
 where checker = fmap $ fmapFst3 (fmap f')
       f' v' = do v <- v'
                  case v of
                       Failure msg -> return $ Failure msg
                       Success x   -> f x

instance (Functor m, Monad m) => Functor (Form xml m) where
  fmap f (Form a) = Form $ \env -> (fmap . fmapFst3 . liftM . fmap . fmap) f (a env)

fmapFst  f (a, b)    = (f a, b)
fmapFst3 f (a, b, c) = (f a, b, c)

instance (Monad m, Applicative m, Monoid xml) => Applicative (Form xml m) where
   pure = pureF
   (<*>) = applyF

-- | Pure xml
xml :: Monad m => xml -> Form xml m ()
xml x = Form $ \env -> pure (const $ return $ Success (), return x, UrlEncoded)

-- | Transform the XML component
plug :: (Monad m, Monoid xml) => (xml -> xml1) -> Form xml m a -> Form xml1 m a
f `plug` (Form m) = Form $ \env -> pure plugin <*> m env
   where plugin (c, x, t) = (c, liftM f x, t)

-- | Takes a hidden-input field, a form of a and produces a list of a.
-- | 
-- | The hidden input field contains a prefix, which is the pointer to the next form. 
-- | This form has to have the same variable-names as the original form, but prefixed by the prefix.
-- | 
-- | Typically, some client-side code is needed to duplicate the original form and generate a unique prefix.
massInput :: (Monoid xml, Applicative m, Monad m)
          => (Form xml m (Maybe String))
          -> Form xml m a
          -> ([String] -> xml)
          -> Form xml m [a]
massInput h f showErrors = massInputHelper form showErrors
 where form = (,) <$> f <*> h

massInputHelper :: (Monoid xml, Applicative m, Monad m) 
                => Form xml m (a, Maybe String)  -- The form
                -> ([String] -> xml)             -- How to show errors
                -> Form xml m [a]
massInputHelper f showErrors = join f
  where join :: (Monoid xml, Applicative m, Monad m) => Form xml m (a, Maybe String) -> Form xml m [a]
        join (Form f) = Form $ \env -> start (f env) env
        start :: (Monad m)
          => State FormState (Collector (m (Failing (a, Maybe String))), xml, FormContentType)
          -> Env
          -> State FormState (Collector (m (Failing [a])), xml, FormContentType)
        start f e =     do  currentState <- get
                            --todo use v
                            let (a, s) = runState f currentState
                            let (v, xml, t) = a
                            let v' = evalState (combineIt [] f (Just v)) currentState
                            put s
                            return (v', xml, t)
        combineIt p f v = do currentState <- get
                             let x = findLinkedList f currentState
                             return $ \e -> calculate p f e (maybe (x e) (\x -> x e) v) currentState
        calculate p f e v (n,_) = do x <- v
                                     case x of
                                          Success (x, Nothing)    -> return $ Success [x]
                                          Success (v, Just cont)  -> do if cont `elem` p then return $ Failure ["Infinite loop"] else do
                                                                        x <- (evalState (combineIt (cont:p) f Nothing) (n, cont)) e
                                                                        case x of
                                                                             Success ls  -> return $ Success (v:ls)
                                                                             Failure msg -> return $ Failure msg
                                          Failure msg             -> return $ Failure msg
        findLinkedList f = fst3 . evalState f

fst3 (a, b, c) = a

-- | Returns Nothing if the result is the empty String.
nothingIfNull :: (Monad m, Functor m) => Form xml m String -> Form xml m (Maybe String)
nothingIfNull frm = nullToMaybe <$> frm
 where nullToMaybe [] = Nothing
       nullToMaybe x  = Just x

withPrefix :: String -> Form xml m a -> Form xml m a
withPrefix prefix (Form f) = Form $ \env -> (modify (const (0, prefix)) >> f env)

-----------------------------------------------
-- Private methods
-----------------------------------------------

freshName :: State FormState String
freshName = do n <- currentName
               modify (\(n,prefix) -> (n+1, prefix))
               return n

currentName :: State FormState String
currentName = gets $ \(n, prefix) ->  prefix ++ "input" ++ show n

changePrefix :: String -> State FormState ()
changePrefix p = modify (\(n,_) -> (n, p))

orT UrlEncoded x = x
orT x UrlEncoded = x
orT x y          = x

pureF :: (Monad m, Monoid xml) => a -> Form xml m a
pureF v = Form $ \env -> pure (const (return $ Success v), return mempty, UrlEncoded)

pureM :: (Monad m, Monoid xml) => m a -> Form xml m a
pureM v = Form $ \env -> pure (const (liftM Success v), return mempty, UrlEncoded)

applyF :: (Monad m, Applicative m, Monoid xml) => Form xml m (a -> b) -> Form xml m a -> Form xml m b
(Form f) `applyF` (Form v) = Form $ \env -> combine <$> f env <*> v env
  where combine (v1, xml1, t1) (v2, xml2, t2) = (first v1 v2, (mappend <$> xml1 <*> xml2), t1 `orT` t2)
        first v1 v2 e = do x <- v1 e 
                           y <- v2 e
                           return $ x <*> y
