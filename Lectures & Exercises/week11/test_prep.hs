
data Tree a = EmptyTree | Node {
                            value :: a,
                            left  :: Tree a,
                            right :: Tree a
                          } deriving (Show,Read)


instance Foldable Tree where
    foldr _ nv EmptyTree = nv
    foldr op nv (Node root left right) = foldr op (op root (foldr op nv left)) right 

test :: Tree Int
test = Node 5 (Node 10 EmptyTree EmptyTree) (Node 3 EmptyTree EmptyTree) 


permutations :: [a] -> [[a]]
permutations xs = []
subsequences :: [a] ->[[a]]
subsequences [] = []
subsequences xs = [] : [suffix | prefixes <- inits xs,suffix <-tails prefixes]
    where inits [] = []
          inits xs = xs : inits (init xs)
          tails [] = []
          tails xs = xs : tails (tail xs)

sumLast :: Int -> Int -> [Int]
sumLast initial n = initial : generate [initial]
    where generate memory = sum memory : generate (sum memory : if length memory >= n then init memory else memory)


safeHead :: [a] -> Maybe a
safeHead [] = Nothing 
safeHead (x:xs) = Just x

maxSumPath :: (Num a, Ord a) => Tree a -> a 
maxSumPath EmptyTree = 0
maxSumPath (Node value left right) =  value + max (maxSumPath left) (maxSumPath right)

prune' :: Tree a -> Tree a
prune' (Node value EmptyTree EmptyTree) = EmptyTree
prune' (Node value left right)          = Node value (prune' left) (prune' right)

bloom' :: Tree a -> Tree a
bloom

testTree :: Tree Int
testTree = Node 5
                (Node 6 EmptyTree EmptyTree)
                (Node (-10)
                      (Node 2 EmptyTree EmptyTree)
                      EmptyTree)