
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

maybe n j m = case m of
  Nothing -> n
  Just x -> j x

x || y = case x of
  True -> True
  False -> y

x && y = case x of
  True -> y
  False -> False

not x = case x of
  True -> False
  False -> True

x >= y = x > y || x == y

f $ x = f x

tryError m = Handle (do { x <- m; pure (Just x) }) (\ x -> pure Nothing)

newArray i = Alloc i 0
readArray arr i = Deref arr i
writeArray arr i v = Update arr i v

s1 ++ s2 = #(__Concat) s1 s2

putStrLn s = do
  Act (#(stdout) (s ++ "\n"))
  pure ()

eqString s s' = #(__StrEq) s s'

mapM_ f xs = case xs of
  [] -> pure ()
  x : xs' -> do
    f x
    mapM_ f xs'
