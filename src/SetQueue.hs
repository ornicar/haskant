{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module SetQueue
     ( Q
     , empty
     , enque
     , deque
     , fromList
     , toList
     , len
     , getSet
     ) where

import           Data.List
import qualified Data.Set     as S
import           Data.Word
import Control.Newtype

data Q e t = Q !Word [e] [e] (S.Set t) (e -> t)

newtype Flip f x y = Flip (f y x)

instance Newtype (Flip f x y) (f y x) where
  pack = Flip
  unpack (Flip z) = z

instance Ord t => Monad (Flip Q t) where 
  return x = empty (\_ -> x)
  (Q _ [] _ _ f) >>= k = empty f
  q >>= k = fromList $ concat $ map (toList . k) toList q 

enque :: Ord t => e -> Q e t -> Q e t
enque x (Q _ [] _ _ f) = Q 1 [x] [] (S.singleton $ f x) f
enque x (Q n as zs set f) = Q (n+1) as (x:zs) (S.insert (f x) set) f

deque :: Ord t => Q e t -> (Maybe e, Q e t)
deque (Q _ [] _ set f) = (Nothing, Q 0 [] [] set f)
deque (Q n (a:as) zs set f)
        | null as     = (Just a, Q (n-1) as' [] set f)
        | otherwise   = (Just a, Q (n-1) as zs set f)
            where as' = reverse zs

empty :: Ord t => (e -> t) -> Q e t
empty = Q 0 [] [] S.empty 

getSet :: Ord t => Q e t -> S.Set t
getSet (Q _ _ _ set _) = set

fromList :: Ord t => (e -> t) -> [e] -> Q e t
fromList f as = Q (genericLength as) as [] (S.fromList $ map f as) f

toList :: Q e t -> [e]
toList (Q _ as zs _ _) = as ++ reverse zs

len :: Q e t -> Word
len (Q l _ _ _ _) = l
