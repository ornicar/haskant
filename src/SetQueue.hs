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
import           Data.Word

data Q e = Q !Word [e] [e]

instance Functor Q where
  fmap f (Q n as zs) = Q n (map f as ++ map f (reverse zs)) []

empty :: Q e
empty =  Q 0 [] []

enque :: e -> Q e -> Q e
enque z  (Q _ [] _)  = Q 1     [z]     []
enque z  (Q n as zs)  = Q (n+1) as  (z:zs)

deque :: Q e -> (Maybe e, Q e)
deque    (Q _ [] _)  =   ( Nothing  ,  Q 0     []   []  )
deque    (Q n (a:as) zs)
         |  null as       =   ( Just a   ,  Q (n-1) as'  []  )
         |  otherwise     =   ( Just a   ,  Q (n-1) as   zs  )
            where as'     = reverse zs

fromList :: [e] -> Q e
fromList as = Q (genericLength as) as []

toList :: Q e -> [e]
toList (Q _ as zs) = as ++ reverse zs

len :: Q e -> Word
len (Q l _ _) = l
