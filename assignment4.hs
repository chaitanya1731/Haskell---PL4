-- Write a Haskell function maxlist lt that computes the maximum element of list lt
-- (Assume that the list lt contains at least one element).
-- E.g. > maxlist [3,1,6,4,2,3] = 6 //the maximum element in the list is 6
maxList :: [Int] -> Int
maxList xs = last (isort xs)

-- Write a Haskell function delete k lt that removes every kth element of a
-- list lt.
-- E.g. > delete 2 [3,4,5,6,7,8,9] //remove the 2nd, 4th, and 6th element
-- [3,5,7,9]
delete:: Int->[Int]->[Int]
delete 0 ys  = ys
delete k [] = []
delete k (ys) = delete2  ys k k
  
delete2 :: [Int]->Int-> Int->[Int]
delete2 [] k n = []
delete2 (y:ys) k n
   | k>1 = y:delete2 ys (k-1) n
   | k==1 =  delete2 ys n n


-- Write a Haskell function isort lt that sorts an integer list lt into ascending
-- order using the insertion sort.
-- E.g. > isort [7,3,9,2] = [2,3,7,9]
switchelement :: Int -> [Int]->[Int]
switchelement y [] = [y]
switchelement y (x:xs) 
   |y >= x = [x] ++ (switchelement y xs) 
   |y < x = [y] ++ [x] ++ xs

isort :: [Int] -> [Int]
isort []=[]
isort (y:ys) = switchelement y (isort ys)


-- Write a Haskell function rotate n lt that rotates a list lt n places to the
-- right. Assume that lt contains at least n elements.
-- E.g. > rotate 3 [1,2,3,4,5,6,7] = [5,6,7,1,2,3,4] // rotate the list 3 places to the right
rotate :: Int -> [a] -> [a]
rotate n xs = drop k xs ++ take k xs
                where k = length xs - n

-- Write a Haskell function single lt that changes a list lt into a list of lists by
-- making each element into a singleton list.
-- E.g. . > single [1,2,3,4] = [[1],[2],[3],[4]]
single :: [a]->[[a]]
single []=[]
single (x:xs)=[x]:single xs

-- Write a Haskell function double lt that doubles every element appearing
-- the odd positions of lt.
-- e.g. > double [1,3,4,5] = [2,3,8,5] //double 1 and 4
double = zipWith ($) (cycle [(*2),id])