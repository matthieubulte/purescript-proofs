module ProofToys where

--

type NatTrans f g = forall a. f a -> g a
infix 4 type NatTrans as ~>

--

data Id a = Id a

unId :: forall a. Id a -> a
unId (Id x) = x

--

newtype Flip f a b = Flip (f b a)

unFlip :: forall f a b. Flip f a b -> f b a
unFlip (Flip f) = f

newtype Flip2 f a b c = Flip2 (f c b a)

unFlip2 :: forall f a b c. Flip2 f a b c -> f c b a
unFlip2 (Flip2 f) = f

--

newtype Comp f g a = Comp (f (g a))
infix 4 type Comp as ..

unComp :: forall f g a. Comp f g a -> f (g a)
unComp (Comp fga) = fga
