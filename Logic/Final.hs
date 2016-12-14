{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

-- Final embedding of propositional and modal logic in Haskell, using
-- methods from "Typed-tagless final interpretations" by Oleg Kiselyov
-- at http://okmij.org/ftp/tagless-final/course/lecture.pdf

module Logic.Final where

type Atom = Int

-- | Propositional Logic Symantics.
-- Note that `bot` is just an abbreviation but can be overloaded
-- Similarly, only one of `imp`, `dis` and `con` has to be given.
class PropositionalSYM s where
  {-# MINIMAL top, at, neg, (imp | dis | con) #-}
  top :: s
  bot :: s
  bot = neg top
  at :: Atom -> s
  neg :: s -> s
  imp :: s -> s -> s
  imp a b = neg (con a (neg b))
  dis :: s -> s -> s
  dis a = imp (neg a)
  con :: s -> s -> s
  con a b = neg (dis (neg a) (neg b))

-- | We show a formula by making String an instance of the Symantics.
instance PropositionalSYM String where
  top = "⊤"
  at = show
  neg = (++) "¬"
  imp f g = "(" ++ f ++ " → " ++ g ++ ")"
  dis f g = "(" ++ f ++ " OR " ++ g ++ ")"
  con f g = "(" ++ f ++ " AND " ++ g ++ ")"

showForm :: String -> String
showForm = id

-- | Something can be used as a model for propositional
-- logic if we can ask it which propositions are true.
class PropositionalModel m where
  trueProps :: m -> [Atom]
instance PropositionalModel m => PropositionalSYM (m -> Bool) where
  top = const True
  bot = const False
  at a m = a `elem` trueProps m
  neg = (not .)
  imp a b m = not (a m) || b m

-- | Here comes the funny evaluation function.
eval :: a -> (a -> Bool) -> Bool
eval = flip ($)

-- | Finally, something concrete: Lists of Integers are models
instance PropositionalModel [Int] where
  trueProps = id


-- | Modal Logic extends Propositional Logic.
class PropositionalSYM s => ModalSYM s where
  box :: s -> s
  dia :: s -> s
  dia = neg . box . neg

-- We can still use the same `showForm` function from above
-- and only have to add clauses for box and diamond.
instance ModalSYM String where
  box  f = "□(" ++ f ++ ")"
  dia f = "◇(" ++ f ++ ")"

-- | A model for basic modal logic is a propositional model plus reachability.
class PropositionalModel m => ModalModel m where
  reachFrom :: m -> [m]
instance ModalModel m => ModalSYM (m -> Bool) where
  box f m  = all f (reachFrom m)
  dia f m = any f (reachFrom m) -- optional

-- | Simple modal model with a total relation.
type SingleEquivClass = ([[Int]],[Int])
instance PropositionalModel SingleEquivClass where
  trueProps = snd
instance ModalModel SingleEquivClass where
  reachFrom (m,_) = [ (m,w') | w' <- m ]

exampleModalModel :: SingleEquivClass
exampleModalModel = ([[1,2],[3]],[3])

-- Again, we do not have to redefine `eval` now :-)

-- Now we get to proper Kripke Models with arbitrary relations.
type World = Int
data PointedKripkeModel = Krm
  { worlds :: [World]
  , val :: World -> [Atom]
  , rel :: World -> [World]
  , cur :: World }
instance PropositionalModel PointedKripkeModel where
  trueProps m = val m (cur m)
instance ModalModel PointedKripkeModel where
  reachFrom m = [ m {cur = w'} | w' <- rel m (cur m) ]

exampleKrM :: PointedKripkeModel
exampleKrM = Krm [1,2,3] v r 1 where
  v 1 = [1,2,3]; v 2 = [2]; v 3 = [3]; v _ = undefined
  r 1 = [2]; r 2 = [3]; r 3 = [1]; r _ = undefined
