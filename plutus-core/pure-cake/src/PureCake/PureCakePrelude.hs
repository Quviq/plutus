-- TODO

raise = Raise Subscript

error e = error e

index a i =
  case a of
    []     -> error "out of bounds"
    x : xs -> if i == 0 then x else index xs (i - 1)

len xs = case xs of
  []      -> 0
  x : xs' -> 1 + len xs'

pure x = Ret x

data Maybe a = Nothing | Just a
