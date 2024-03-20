{-# LANGUAGE UnboxedTuples #-}
module SKrIpt.Runtime where
import GHC.Exts (Any)
import Unsafe.Coerce (unsafeCoerce)

s :: Any -> Any -> Any -> Any
s f g x = unsafeCoerce f x (unsafeCoerce g x)

s2 :: (# Any, Any #) -> Any -> Any
s2 (# f, g #) x = unsafeCoerce f x (unsafeCoerce g x)

s3 :: (# Any, Any, Any #) -> Any
s3 (# f, g, x #) = unsafeCoerce f x (unsafeCoerce g x)

s' :: Any -> Any -> Any -> Any -> Any
s' c f g x = unsafeCoerce c (unsafeCoerce f x) (unsafeCoerce g x)

s'2 :: (# Any, Any #) -> Any -> Any -> Any
s'2 (# c, f #) g x = unsafeCoerce c (unsafeCoerce f x) (unsafeCoerce g x)

s'3 :: (# Any, Any, Any #) -> Any -> Any
s'3 (# c, f, g #) x = unsafeCoerce c (unsafeCoerce f x) (unsafeCoerce g x)

s'4 :: (# Any, Any, Any, Any #) -> Any
s'4 (# c, f, g, x #) = unsafeCoerce c (unsafeCoerce f x) (unsafeCoerce g x)

k :: Any -> Any -> Any
k x y = x

k2 :: (# Any, Any #) -> Any
k2 (# x, y #) = x

i :: Any -> Any
i x = x

b :: Any -> Any -> Any -> Any
b f g x = unsafeCoerce f (unsafeCoerce g x)

b2 :: (# Any, Any #) -> Any -> Any
b2 (# f, g #) x = unsafeCoerce f (unsafeCoerce g x)

b3 :: (# Any, Any, Any #) -> Any
b3 (# f, g, x #) = unsafeCoerce f (unsafeCoerce g x)

bStar :: Any -> Any -> Any -> Any -> Any
bStar c f g x = unsafeCoerce c (unsafeCoerce f (unsafeCoerce g x))

bStar2 :: (# Any, Any #) -> Any -> Any -> Any
bStar2 (# c, f #) g x = unsafeCoerce c (unsafeCoerce f (unsafeCoerce g x))

bStar3 :: (# Any, Any, Any #) -> Any -> Any
bStar3 (# c, f, g #) x = unsafeCoerce c (unsafeCoerce f (unsafeCoerce g x))

bStar4 :: (# Any, Any, Any, Any #) -> Any
bStar4 (# c, f, g, x #) = unsafeCoerce c (unsafeCoerce f (unsafeCoerce g x))

c :: Any -> Any -> Any -> Any
c f g x = unsafeCoerce f x g

c2 :: (# Any, Any #) -> Any -> Any
c2 (# f, g #) x = unsafeCoerce f x g

c3 :: (# Any, Any, Any #) -> Any
c3 (# f, g, x #) = unsafeCoerce f x g

c' :: Any -> Any -> Any -> Any -> Any
c' c f g x = unsafeCoerce c (unsafeCoerce f x) g

c'2 :: (# Any, Any #) -> Any -> Any -> Any
c'2 (# c, f #) g x = unsafeCoerce c (unsafeCoerce f x) g

c'3 :: (# Any, Any, Any #) -> Any -> Any
c'3 (# c, f, g #) x = unsafeCoerce c (unsafeCoerce f x) g

c'4 :: (# Any, Any, Any, Any #) -> Any
c'4 (# c, f, g, x #) = unsafeCoerce c (unsafeCoerce f x) g

u :: Any -> Any -> Any
u f t = let ~(x, y) = unsafeCoerce t in unsafeCoerce f x y

y :: Any -> Any
y f = let x = unsafeCoerce f x in x

apair :: Any -> Any -> Any
apair a b = unsafeCoerce (a, b)

acond :: Any -> Any -> Any -> Any
acond c t f = if unsafeCoerce c then t else f

acond2 :: (# Any, Any #) -> Any -> Any
acond2 (# c, t #) f = if unsafeCoerce c then t else f

acond3 :: (# Any, Any, Any #) -> Any
acond3 (# c, t, f #) = if unsafeCoerce c then t else f
