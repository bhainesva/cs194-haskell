{-# LANGUAGE TypeFamilies, PolyKinds, UndecidableInstances #-}

type DrawFun = Int -> Int -> Char
type Picture = DrawFun -> DrawFun

-- char :: Char -> DrawFun -> Int -> Int -> Char
char :: Char -> Picture
char c _ 0 0 = c
char _ f x y = f x y

type family Expand (x :: k) :: k where Expand (f x) = Expand f (Expand x); Expand x = x
