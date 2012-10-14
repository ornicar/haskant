{-# LANGUAGE MultiParamTypeClasses #-}

module SetQueue
     ( Q
     , empty
     , enque
     , deque
     , fromList
     , toList
     , getSet
     ) where

import qualified Data.Set as S

data Q e t = Q [e] [e] (S.Set t) (e -> t)

enque :: Ord t => e -> Q e t -> Q e t
enque x (Q [] _ _ f) = Q [x] [] (S.singleton $ f x) f
enque x q@(Q as zs set f) | f x `S.member` set = q
                          | otherwise = Q as (x:zs) (S.insert (f x) set) f

deque :: Ord t => Q e t -> Maybe (e, Q e t)
deque (Q [] _ _ _) = Nothing
deque (Q (a:as) zs set f) | null as = Just (a, Q (reverse zs) [] set f)
                          | otherwise = Just (a, Q as zs set f)

empty :: Ord t => (e -> t) -> Q e t
empty = Q [] [] S.empty

getSet :: Ord t => Q e t -> S.Set t
getSet (Q _ _ set _) = set

fromList :: Ord t => (e -> t) -> [e] -> Q e t
fromList f as = Q as [] (S.fromList $ map f as) f

toList :: Q e t -> [e]
toList (Q as zs _ _) = as ++ reverse zs
