module Helpers.Circle (
  Circle,
  fromList,
  toList,
  current,
  rotateRight,
  rotateLeft,
  rotateNRight,
  rotateNLeft,
  update,
  insertAfter,
  removeAfter,
) where

data Circle a = Empty | Circle [a] a [a]

fromList :: [a] -> Circle a
fromList [] = Empty
fromList (x:xs) = Circle [] x xs

toList :: Circle a -> [a]
toList Empty = []
toList (Circle ls c rs) = (c:rs) ++ reverse ls

update :: a -> Circle a -> Circle a
update x (Circle ls c rs) = Circle ls x rs

current :: Circle a -> a
current (Circle _ c _) = c

rotateRight :: Circle a -> Circle a
rotateRight Empty = Empty
rotateRight (Circle [] c []) = Circle [] c []
rotateRight (Circle ls c []) = rotateRight (Circle [] c (reverse ls))
rotateRight (Circle ls c (r:rs)) = Circle (c:ls) r rs

rotateLeft :: Circle a -> Circle a
rotateLeft Empty = Empty
rotateLeft (Circle [] c []) = Circle [] c []
rotateLeft (Circle [] c rs) = rotateLeft (Circle (reverse rs) c [])
rotateLeft (Circle (l:ls) c rs) = Circle ls l (c:rs)

rotateNLeft :: Int -> Circle a -> Circle a
rotateNLeft 0 cc = cc
rotateNLeft n cc = rotateNLeft (n-1) (rotateLeft cc)

rotateNRight :: Int -> Circle a -> Circle a
rotateNRight 0 cc = cc
rotateNRight n cc = rotateNRight (n-1) (rotateLeft cc)

insertAfter :: a -> Circle a -> Circle a
insertAfter x Empty = Circle [] x []
insertAfter x (Circle ls c rs) = Circle (c:ls) x rs

removeAfter :: Circle a -> Circle a
removeAfter (Circle [] c []) = Empty
removeAfter (Circle ls c (r:rs)) = (Circle ls r rs)
removeAfter (Circle (l:ls) c []) = rotateRight (Circle ls l [])
