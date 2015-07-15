import System.Random

---{{{ Start of 1-10
myLast :: [a] -> a
myLast [x]    = x
myLast (_:xs) = myLast xs

myButLast :: [a] -> a
myButLast [x,_] = x
myButLast (_:xs) = myButLast xs

elementAt :: [a] -> Int -> a
elementAt [] _ = error "IndexOutOfBoundsException (> length)"
elementAt (x:xs) n = if n == 1 then x else elementAt xs (n-1)

myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1+myLength xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs : x

isPalindrome :: [a] -> Bool
isPalindrome xs  = xs == myReverse xs

data NestedList a = Elem a | List [NestedList a]
myflatten :: NestedList a -> [a]
myflatten (List [])     = []
myflatten (Elem x)      = [x]
myflatten (List (x:xs)) = myflatten x ++ myflatten xs

compress :: [a] -> [a]
compress (x:y:xs) = if x == y then compress (y:xs) else x:compress (y:xs)

pack :: [a] -> [[a]]
pack (x:xs) = let (first,rest) = span (==x) xs in
  (x:first) : pack rest

encode :: [a] -> [(Int,a)]
encode xs = map (\x -> (length x,head x)) (group xs)
---}}}

---{{{ Start of 11-20
data MaybeList a = Multiple Int a | Single a deriving (Show)
encodeModified xs = map (\x -> if length x == 1 then Single x else Multiple (length x) x) (group xs)

decodeModified [] = []
decodeModified (Multiple 0 x : xs) =     decodeModified xs
decodeModified (Multiple n x : xs) = x : decodeModified (Multiple (n-1) x : xs)
decodeModified (Single     x : xs) = x : decodeModified xs

encodeDirect :: (Eq a) => [a] -> [MaybeList a]
encodeDirect [] = []
encodeDirect (x:xs)
  | count == 1 = Single x : encodeDirect xs
  | otherwise  = Multiple count x : encodeDirect rest
  where (matched,rest) = span (==x) xs
        count = 1 + length matched

dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x:dupli xs

repli :: [a] -> Int -> [a]
repli xs n = repli' xs n n
  where repli' [] _ _ = []
        repli' (_:xs) 0 a = repli' xs a a
        repli' (x:xs) a b =
          x:repli' (x:xs) (a-1) b

dropEvery :: [a] -> Int -> [a]
dropEvery xs n = dropEvery' xs n n
  where dropEvery' [] _ _ = []
        dropEvery' (_:xs) 1 a = dropEvery' xs a a
        dropEvery' (x:xs) a b =
          x:dropEvery' (x:xs) (a-1) b

split :: [a] -> Int -> ([a],[a])
split xs n = split' [] xs n
  where split' _ _ n | n < 0 = error "Int to split out of range (< 0)"
        split' a xs 0 = (a,xs)
        split' _ [] _ = error "ArrayIndexOutOfBoundsException (> length)"
        split' a (x:xs) n = split' (a++x) xs (n-1)

slice :: [a] -> Int -> Int -> [a]
slice xs s f = fst . split (snd . split xs s) f

rotate :: [a] -> Int -> [a]
rotate xs n
  | n == 0 = xs
  | n < 0  = rotate xs (xs.length+n)
  | n > 0  = s ++ f where (f,s) = split xs n

removeAt :: Int -> [a] -> (a,[a]) -- why is the type of this have Int first?
removeAt n xs = removeAt' n [] xs
  where removeAt' 1 ar (x:xs) = (x,ar++xs)
        removeAt' n ar (x:xs) = removeAt' (n-1) (x:ar) xs
---}}}

---{{{ 21-28
insertAt :: a -> [a] -> Int -> [a]
insertAt c (x:xs) 1 = x:c:xs
insertAt c (x:xs) n = x:insertAt c xs (n-1)

range :: Int -> Int -> [Int]
range a b
  | a > b = range b a
  | otherwise = a:range (a+1) b

rnd_select :: [a] -> Int -> IO [a]
rnd_select xs n = do
  gen <- genStdGen
  return $ take n [ xs !! x | x <- randomRx (0, (length xs) - 1) gen ]

diff_select :: Int -> Int -> IO [Int]
diff_select n max = do
  gen <- genStdGen
  return . take n $ randomRs (1, max) gen

rnd_permu :: [a] -> IO [a]
rnd_permu xs = rnd_select xs (length xs)

combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [[]]
combinations n xs = [y:ys | y:xs' <- tails xs, ys <- combinations (n-1) xs']

group :: [Int] -> [a] -> [[[a]]]
group [] _ = [[]]
group (n:ns) xs = [g:gs | (g,rs) <- combination' n xs, gs <- group ns rs]
  where combination' 0 xs = [([],xs)]
        combination' _ [] = []
        combination' n (x:xs) = ts++ds
          where ts = [(x:ys,zs) | (ys,zs) <- combination (n-1) xs]
                ds = [(ys,x:zs) | (ys,zs) <- combination  n    xs]

lsort :: [[a]] -> [[a]]
lsort list = lsort' 1 list
  where lsort' _ [] = []
        lsort' n xs = [x | x <- xs, x == n] ++ lsort' (n+1) [x | x <- xs, x /= n]
---}}}

---{{{ 31-41
isPrime :: Int -> Bool
isPrime n
  | n < 2     = error "Must be >= 2"
  | otherwise = isPrime' n 2
  where isPrime' n x
          | x^2 <= n = n `rem` x == 0 && isPrime' n (x+1)
          | otherwise = True

gcd' :: Int -> Int -> Int
gcd' x y | x < 0     = gcd' (-x) y
         | y < 0     = gcd' x (-y)
         | y < x     = gcd'' y x
         | otherwise = gcd'' x y
  where gcd'' 0 y = y
        gcd'' x y = gcd'' (y `mod` x) x

-- Now moving on to 54 (<https://wiki.haskell.org/99_questions/54A_to_60>)
