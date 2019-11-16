9.1

> data Nat = Zero | Succ Nat

> int :: Nat -> Int
> int Zero = 0
> int (Succ n) = (int n) + 1

> nat :: Int -> Nat
> nat i	| i == 0	= Zero
> 	| i > 0	= (Succ . nat) (i-1)

> add, mul, pow, tet :: Nat -> Nat -> Nat

> add n Zero = n
> add n (Succ m) = (Succ . (add n)) m

> mul n Zero = Zero
> mul n (Succ m) = ((add n) . (mul n)) m

> pow n Zero = Succ Zero
> pow n (Succ m) = mul n (pow n m)

> tet n (Succ Zero) = n
> tet n (Succ m) = pow n (tet n m)

9.2

> foldNat :: (a -> a) -> a -> Nat -> a
> foldNat cons nil Zero 	= nil
> foldNat cons nil (Succ n) 	= cons (foldNat cons nil n)

> unfoldNat :: Eq a => (a -> Bool) -> (a -> a) -> a -> Nat
> unfoldNat null tail x 	| null x 	= Zero
>		| otherwise	= Succ (unfoldNat null tail (tail x))

> int' = foldNat (+1) 0
> nat' = unfoldNat (\x -> x==0) (+(-1))

> add' n = foldNat Succ n
> mul' n = foldNat (add' n) Zero
> pow' n = foldNat (mul' n) (Succ Zero)
> tet' (Succ n) = foldNat (pow' (Succ n)) (Succ Zero)