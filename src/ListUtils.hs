module ListUtils where

updateList :: Int -> (a -> a) -> [a] -> [a]
updateList 0 f (x:xs) = f x : xs
updateList i f (x:xs) = x : updateList (i-1) f xs
updateList _ _ []     = []

extract :: Int -> [a] -> (a,[a])
extract _ []     = error "extract: index too large" 
extract 0 (x:xs) = (x,xs)
extract i (x:xs) =
  let (y,ys) = extract (i-1) xs
  in (y, x:ys)
