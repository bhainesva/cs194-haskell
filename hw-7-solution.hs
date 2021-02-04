import System.Random

-- Exercise 1: more efficient fibs
fib :: Integer -> Integer
fib 1 = 1
fib 2 = 1
fib n = (fib (n-1)) + (fib (n-2))

fibs1 :: [Integer]
fibs1 = map fib [1..]

fibs2 :: [Integer]
fibs2 = 1 : 1 : zipWith (+) (tail fibs2) fibs2

-- Exercise 2: Streams
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons a as) = a : (streamToList as)

instance Show a => Show (Stream a) where
  show a = (show (take 20 (streamToList a))) ++ " ..."

streamRepeat :: a -> Stream a
streamRepeat x = (Cons x (streamRepeat x))

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a as) = (Cons (f a) (streamMap f as))

streamIterate :: (a -> a) -> a -> Stream a
streamIterate f a = (Cons a (streamIterate f (f a)))

streamInterleave :: Stream a -> Stream a -> Stream a
streamInterleave (Cons a as) b = (Cons a (streamInterleave b as))

nats :: Stream Integer
nats = streamIterate succ 0

ruler :: Stream Integer
ruler = streamInterleave (streamRepeat 0) (streamMap (+1) ruler)

-- Exercise 3: Supply monad
data Supply s a = S (Stream s -> (a, Stream s))

get :: Supply s s
get = (S (\(Cons x xs) -> (x, xs)))

pureSupply :: a -> Supply s a
pureSupply a = S (\stream -> (a, stream))

mapSupply :: (a -> b) -> Supply s a -> Supply s b
mapSupply f (S t) = S (func)
  where func stream = let (a, stream2) = t stream
                      in (f a, stream2)

mapSupply2 :: (a -> b -> c) -> Supply s a -> Supply s b -> Supply s c
mapSupply2 f (S s1) (S s2) = S go
  where go stream = let (a, stream') = s1 stream
                        (b, stream'') = s2 stream'
                    in (f a b, stream')

bindSupply :: Supply s a -> (a -> Supply s b) -> Supply s b
bindSupply (S s1) f = S go
  where go stream = let (a, stream') = s1 stream
                        (S s2) = f a
                        (b, stream'') = s2 stream'
                    in (b, stream'')

runSupply :: Stream s -> Supply s a -> a
runSupply stream (S f) = let (a, stream') = f stream
                         in a

instance Functor (Supply s) where
  fmap = mapSupply

instance Applicative (Supply s) where
  pure = pureSupply
  (<*>) = mapSupply2 id

instance Monad (Supply s) where
  return = pureSupply
  (>>=) = bindSupply

data Tree a = Node (Tree a) (Tree a) | Leaf a deriving (Show)

labelTree :: Tree a -> Tree Integer
labelTree t = runSupply nats (go t)
  where
    go :: Tree a -> Supply s (Tree s)
    go (Node t1 t2) = Node <$> go t1 <*> go t2
    go (Leaf _) = Leaf <$> get