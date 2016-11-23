
.

inc n = S n
map f xs = case xs of [] -> []; y:ys -> (f y):map f ys
root = map inc

h xs = map inc xs
=
case xs of
  [] -> []
  y:ys -> inc y: map inc ys
=
case xs of
  [] -> []
  y:ys -> S y: map inc ys
=
case xs of
  [] -> []
  y:ys -> S y: h ys

This document uses syntax similar to haskell to describe the
supercompilation steps.

The symbol := is used to present a definition.
The symbol ~= is used to shows that this equality follows from a definition.

### Plus using an accumulator.

This example shows the need for generalization.

```haskell
plus x y := case x of
  Zero -> y
  Succ x' -> plus x' (Succ y)  -- Non-accum: Succ (plus x' y)

plus n m ~= case n of
  Zero -> m
  Succ n' ->
    {v1 n' m: plus n' (Succ m) ~= let m'=Succ m in plus n' m'}
```

## Reverse with no accumulator

```haskell
append xs ys := case xs of
  []     -> ys
  (x:xs') -> x:append xs' ys

rev xs := case xs of
  [] -> []
  (x:xs') -> append (rev xs')  [x]

{v0 zs: rev zs ~= case zs of
  [] -> {v1: [] ~= []}
  (x:xs') ->
    {v2 xs' x: append (rev xs') [x] ~=
      case {rev xs' ~~ v0 xs'} of
        [] -> {v4 x: [x] ~= [x]}
        (x':xs'') ->
          {v5 x' xs'' x: x':append xs'' [x] ~= x':
            {v6 xs'' x: append xs'' [x] ~=
              case xs'' of
                [] -> []
                (x'':xs''') -> {x'':append xs''' [x] ~~ v5 x'' xs''' x}
            }
          }
    }
}
```


predLenEqualToLen'' :: [a] -> Bool
predLenEqualToLen'' xs = len xs `eq` len'' xs

predLenEqualToLen' :: [a] -> N -> Bool
predLenEqualToLen' xs n = len' xs n `eq` len xs + n

predLenEqualToLen' xs n
= -- by definition
len' xs n `eq` len xs + n
= -- by definition of eq
case len' xs n of
  0 -> case len xs + n of
    0 -> True
    1+ m' -> False
  1+ n' -> case len xs + n of
    0 -> False
    1+ m' -> n' `eq` m'
= -- by definition of len'
case (case xs of [] -> n; (y:ys) -> len' ys (1+n) ) of
  0 -> case len xs + n of
    0 -> True
    1+ m' -> False
  1+ n' -> case len xs + n of
    0 -> False
    1+ m' -> n' `eq` m'
= -- by distributing case and replacing with case values
case xs of
  [] -> case n of
    0 -> case len [] + 0 of 0 -> True; 1+ m' -> False ~~> True
    1+ n' -> case len [] + n of ~~> case n of 0 -> False; 1+ m' -> n' `eq` m' ~~> n `eq` n ~~> n' `eq` n' ~~> True
  (y:ys) -> case len' ys (1+n) of   --
    0 -> case len xs + n of         --
      0 -> True                     --
      1+ m' -> False                -- == if xs=y:ys => len' ys (1+n) `eq` len (y:ys) + n
    1+ n' -> case len xs + n of     -- Let's continue ...
      0 -> False                    --
      1+ m' -> n' `eq` m'           --
= -- by definition of len
case xs of
  [] -> case n of
    0 -> case len [] + 0 of 0 -> True; 1+ m' -> False ~~> True
    1+ n' -> case len [] + n of ~~> case n of 0 -> False; 1+ m' -> n' `eq` m' ~~> n `eq` n ~~> n' `eq` n' ~~> True
  (y:ys) -> case len' ys (1+n) of
    0 -> case len ys + 1 + n of
      0 -> True
      1+ m' -> False
    1+ n' -> case len ys + 1+ n of
      0 -> False
      1+ m' -> n' `eq` m'
= -- by folding eq
case xs of
  [] -> case n of
    0 -> case len [] + 0 of 0 -> True; 1+ m' -> False ~~> True
    1+ n' -> case len [] + n of ~~> case n of 0 -> False; 1+ m' -> n' `eq` m' ~~> n `eq` n ~~> n' `eq` n' ~~> True
  (y:ys) -> len' ys (1+n) `eq` len ys + (1+n) ~~~~> predLenEqualToLen' ys (n+1)


predRevPreservesLen :: [a] -> Bool
predRevPreservesLen xs
= -- by definition of predRevPreservesLen
  len (rev xs) `eq` len xs
= -- by definition of `eq`
  case len (rev xs) of
    0 -> case len xs of
      0 -> True
      1 + m' -> False
    1 + n' -> case len xs of
      0 -> False
      1 + m' -> n' `eq` m'
= -- by definition of len
  case (case rev xs of [] -> 0; (y:ys) -> 1 + len ys) of
    0 -> case len xs of
      0 -> True
      1 + m' -> False
    1 + n' -> case len xs of
      0 -> False
      1 + m' -> n' `eq` m'
= -- by distributing case
  case rev xs of
    [] -> case 0 of
      0 -> case len xs of
        0 -> True
        1 + m' -> False
      1 + n' -> case len xs of
        0 -> False
        1 + m' -> n' `eq` m'
    (y:ys) -> case 1 + len ys of
      0 -> case len xs of
        0 -> True
        1 + m' -> False
      1 + n' -> case len xs of
        0 -> False
        1 + m' -> n' `eq` m'
= -- by removing and simplifying case 0 and case 1 + len ys ** check `seq`
  case rev xs of
    [] -> case len xs of
      0 -> True
      1 + m' -> False
    (y:ys) -> case len xs of
      0 -> False
      1 + m' -> len ys `eq` m'
= -- by def of rev
  case revAccum xs [] of
    [] -> case len xs of
      0 -> True
      1 + m' -> False
    (y:ys) -> case len xs of
      0 -> False
      1 + m' -> len ys `eq` m'
= -- by def of revAccum
  case (case xs of [] -> []; (y':ys') -> revAccum ys' (y':[])) of
    [] -> case len xs of
      0 -> True
      1 + m' -> False
    (y:ys) -> case len xs of
      0 -> False
      1 + m' -> len ys `eq` m'
= -- by distributing outer case
  case xs of
    [] -> case [] of
      [] -> case len xs of
        0 -> True
        1 + m' -> False
      (y:ys) -> case len xs of
        0 -> False
        1 + m' -> len ys `eq` m'
    (y':ys') -> case revAccum ys' (y':[]) of
      [] -> case len xs of
        0 -> True
        1 + m' -> False
      (y:ys) -> case len xs of
        0 -> False
        1 + m' -> len ys `eq` m'
= -- by removing non matching cases
  case xs of
    [] -> True -- len xs when xs is [] = 0
    (y':ys') -> case revAccum ys' (y':[]) of
      [] -> False
      (y:ys) -> len ys `eq` len ys'


lenRevAccum :: [a] -> [a] -> N
lenRevAccum xs as
= -- by definition of lenrev
  len (revAccum xs as)
= -- by definition of len
  case revAccum xs as of
    [] -> 0
    (y:ys) -> 1 + len ys
= -- by definition of revAccum
  case (case xs of [] -> as; (r:rs) -> revAccum rs (r:as)) of
    [] -> 0
    (y:ys) -> 1 + len ys
= -- by distributing case
  case xs of
    [] -> case as of
      [] -> 0
      (y:ys) -> 1 + len ys
    (r:rs) -> case revAccum rs (r:as) of
      [] -> 0
      (y:ys) -> 1 + len ys
= -- by folding len
  case xs of
    [] -> len as
    (r:rs) -> case revAccum rs (r:as) of
      [] -> 0
      (y:ys) -> 1 + len ys
  = -- by folding lenRevAccum
  case xs of
    [] -> len as
    (r:rs) -> lenRevAccum rs (r:as)

sumLens :: [a] -> [a] -> N
sumLens xs as
= -- by definition
  len xs + len as
= -- by unfolding first len (only for easy of writing)
  (case xs of [] -> 0; (y:ys) -> 1 + len ys) + len as
= -- by definition of plus
  case (case xs of [] -> 0; (y:ys) -> 1 + len ys) of
    0 -> len as
    1 + n' -> 1 + (n' + len as)
= -- by distributing case
  case xs of
    [] -> len as
    (y:ys) -> 1 + (len ys + len as)
= -- by folding sumLens
  case xs of
    [] -> len as
    (y:ys) -> 1 + sumLens ys as



predRevAccumPreservesLen :: [a] -> [a] -> Bool
predRevAccumPreservesLen xs as
= -- by definition
  lenRevAccum xs as `eq` sumLens xs as
= -- by definition of eq
  case lenRevAccum xs as of
    0 -> case sumLens xs as of
      0 -> True
      1 + m' -> False
    1 + n' -> case sumLens xs as of
      0 -> False
      1 + m' -> n' `eq` m'
= -- by unfolding lenRevAccum
  case (case xs of [] -> len as; (r:rs) -> lenRevAccum rs (r:as) ) of
    0 -> case sumLens xs as of
      0 -> True
      1 + m' -> False
    1 + n' -> case sumLens xs as of
      0 -> False
      1 + m' -> n' `eq` m'
= -- by distributing case/case
  case xs of
    [] -> case len as of ~~> case (case as of [] -> 0; (y:ys -> 1 + len ys)) of
      0 -> case sumLens [] as of
        0 -> True
        1 + m' -> False
      1 + n' -> case sumLens xs as of
        0 -> False
        1 + m' -> n' `eq` m'
    (r:rs) -> case lenRevAccum rs (r:as) of
      0 -> case sumLens xs as of
        0 -> True
        1 + m' -> False
      1 + n' -> case sumLens xs as of
        0 -> False
        1 + m' -> n' `eq` m'
= -- by case/case
  case xs of
    [] -> case as of
      [] -> case sumLens [] [] of ~~> sumLens [] [] == 0 ~~> True
      (y:ys) -> case 1 + len ys of
        1 + n' -> case sumLens [] as of ~~> sumLens [] as = len as
          0 -> False
          1 + m' -> n' `eq` m'
    (r:rs) -> case lenRevAccum rs (r:as) of
      0 -> case sumLens xs as of
        0 -> True
        1 + m' -> False
      1 + n' -> case sumLens xs as of
        0 -> False
        1 + m' -> n' `eq` m'
= -- by simplification
case xs of
  [] -> case as of
    [] -> case sumLens [] [] of ~~> sumLens [] [] == 0 ~~> True
    (y:ys) -> len ys `eq` len ys ~~> True
  (r:rs) -> case lenRevAccum rs (r:as) of --
    0 -> case sumLens xs as of            --
      0 -> True                           -- case xs of r:rs ==>
      1 + m' -> False                     -- lenRevAccum rs (r:as) `eq`
    1 + n' -> case sumLens xs as of       --   sumLens (r:rs) as
      0 -> False                          --
      1 + m' -> n' `eq` m'                --
= -- by definition of sumLens
case xs of
  [] -> case as of
    [] -> case sumLens [] [] of ~~> sumLens [] [] == 0 ~~> True
    (y:ys) -> len ys `eq` len ys ~~> True
  (r:rs) -> case lenRevAccum rs (r:as) of --
    0 -> case (1 + sumLens rs as) of      --
      0 -> True                           -- case xs of r:rs ==>
      1 + m' -> False                     -- lenRevAccum rs (r:as) ==
    1 + n' -> case (1 + sumLens rs as) of --  sumLens rs as + 1
      0 -> False                          --
      1 + m' -> n' `eq` m'                --


predPartialLenRevAccum (r:rs) as =
  lenRevAccum rs (r:as) `eq` sumLens rs as + 1
= -- by definition of eq
  case lenRevAccum rs (r:as) of ~~> lenRevAccum rs (a:(r:as))
    0 -> case (1 + sumLens rs as) of ~~> 1 + (1 + sumLens rs as)
      0 -> True
      1 + m' -> False
    1 + n' -> case (1 + sumLens rs as) of
      0 -> False
      1 + m' -> n' `eq` m'
= --
  case lenRevAccum rs (r:as) of --
    0 -> case (1 + sumLens rs as) of ~~>
      0 -> True
      1 + m' -> False
    1 + n' -> case (1 + sumLens rs as) of
      0 -> False
      1 + m' -> n' `eq` m'
