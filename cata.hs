data Fix f = In (f  (Fix f))

-- In :: f (Fix f) -> Fix f  

inop :: Fix f -> f (Fix f)
inop (In x) = x

data List a k = Empty | Cons a k

cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg k = alg $ fmap (cata alg) (inop k)



instance Functor (List a) where
  fmap f Empty      = Empty
  fmap f (Cons a k) = Cons a (f k)


toList :: [a] -> Fix (List a)
toList = foldr f k where
  k :: Fix (List a)
  k = In Empty

  f :: a -> Fix (List a) -> Fix (List a)
  f x y = In (Cons x y)


fromList :: Fix (List a) -> [a]
fromList = cata alg where
  alg Empty       = []
  alg (Cons x xs) = x : xs


-- inop (In (Cons 5 (In Empty))) -> Cons 5 (In Empty)
--
--                     -> alg $ fmap (cata alg) inop (In (Cons 5 (In Empty)))
--                     -> alg $ fmap (cata alg) Cons 5 (In Empty)
--                     -> alg $ Cons 5 (alg $ fmap (cata alg) inop (In Empty))
--                     -> alg $ Cons 5 (alg $ fmap (cata alg) Empty)
--                     -> alg $ Cons 5 (alg $ Empty)
--                     -> alg $ Cons 5 ([])
--                     -> 5 : []
--
-- cata alg (In (Cons 1 (In (Cons 2 (In Empty))))) -> alg $ fmap (cata alg) inop (In (Cons 1 (In (Cons 2 (In Empty)))))
--                                                 -> alg $ fmap (cata alg) (Cons 1 (In (Cons 2 (In Empty)))
--                                                 -> alg $ Cons 1 (alg $ fmap (cata alg) Cons 2 (In Empty))
--                                                 -> alg $ Cons 1 (alg $ Cons 2 (alg $ fmap (cata alg) Empty))
--                                                 -> alg $ Cons 1 (alg $ Cons 2 (alg $ Empty))
--                                                 -> alg $ Cons 1 (alg $ Cons 2 [])
--                                                 -> alg $ Cons 1 (2 : [])
--                                                 -> 1 : (2 : [])
--                                                 -> [1, 2]
--
