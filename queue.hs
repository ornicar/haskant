import           Control.Monad.Queue.Corec

run = showQueue makeQueue3

makeQueue1 :: Q w Int (Maybe Int)
makeQueue1 = enQ 1 >> enQ 2 >> deQ >> deQ >> deQ >> enQ 3 >> deQ

makeQueue2 :: Q w Int ()
-- makeQueue2 = enQ 1 >>= (\_ -> enQ 2)
makeQueue2 = enQ 1 >> enQ 2

makeQueue3 :: Q w Int ()
makeQueue3 = mapM_ enQ [1..10]

showQueue q = show (runResultQueue q)

data  Tree a b
   =  Leaf    a
   |  Branch  b (Tree a b) (Tree a b)
      deriving (Eq,Show)

tree1 = Branch 1 (Branch 2 (Leaf 3) (Leaf 3)) (Leaf 4)

corec1 :: Tree t t -> [Tree t t]
corec1 tree = snd $ runResultQueue (handle tree >> explore) 
  where

    handle t@(Leaf   _      ) = return ()
    handle t@(Branch{}) = enQ t

    explore = do
      branch <- deQ
      case branch of
        (Just (Branch _ l r)) -> handle l >> handle r >> explore
        _ -> return ()
