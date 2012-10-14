module SetQueue
     ( Q()
     , empty
     , enque
     , deque
     , fromList
     , toList
     , len
     ) where

import           Data.List
import qualified Data.Set     as S
import           Data.Word

data Q e = Q !Word [e] [e] (S.Set e)

empty :: Ord e => Q e
empty =  Q 0 [] [] S.empty

enque :: Ord e => e -> Q e -> Q e
enque z  (Q _ [] _ _)    = Q 1 [z] [] (S.singleton z)
enque z  (Q n as zs set) = Q (n+1) as (z:zs) (S.insert z set)

deque :: Ord e => Q e -> (Maybe e, Q e)
deque    (Q _ [] _ _)  =   ( Nothing  ,  Q 0     []   []  S.empty)
deque    (Q n (a:as) zs set)
         |  null as       =   ( Just a   ,  Q (n-1) as'  []  (S.delete a set))
         |  otherwise     =   ( Just a   ,  Q (n-1) as   zs  (S.delete a set))
            where as'     = reverse zs

fromList :: Ord e => [e] -> Q e
fromList as = Q (genericLength as) as [] (S.fromList as)

toList :: Q e -> [e]
toList (Q _ as zs _) = as ++ reverse zs

len :: Q e -> Word
len (Q l _ _ _) = l
