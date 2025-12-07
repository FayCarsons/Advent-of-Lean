def Option.isSomeAnd (o : Option a) (f : a → Bool) : Bool := 
  match o with 
  | some x => f x 
  | none => false

structure Interval where 
  lo : Nat 
  hi : Nat 
deriving Inhabited, DecidableEq, Repr

def Interval.toTuple : Interval → Nat × Nat 
| { lo, hi } => (lo, hi)

def Interval.fromTuple : Nat × Nat → Interval
| (lo, hi) => { lo, hi }

def Interval.contains (int : Interval) (n : Nat) : Bool := 
  n >= int.lo && n <= int.hi

def Interval.overlaps (a b : Interval) : Bool := 
  a.lo <= b.hi && b.lo <= a.hi

def Interval.overlapsOrAdjacent (a b :Interval) : Bool := 
  a.lo <= b.hi.succ && b.lo <= a.hi.succ

def Interval.merge (a b : Interval) : Interval := 
  { lo := min a.lo b.lo, hi := max a.hi b.hi }

def Interval.fromList! : List Nat → Interval 
| lo :: hi :: [] => {lo, hi}
| xs => panic! s!"expected 2 element list, got {repr xs}"

inductive IntervalTree where 
| empty : IntervalTree
| node : Interval → IntervalTree → IntervalTree → IntervalTree
deriving Inhabited, DecidableEq, Repr

def IntervalTree.singleton : Interval → IntervalTree := (.node · .empty .empty)

def IntervalTree.member (self : IntervalTree) (n : Nat) : Bool := 
  lookupFloor n self |>.isSomeAnd (n <= ·.snd)
  where 
    lookupFloor (n : Nat) : (self : IntervalTree) → Option (Nat × Nat) 
    | .empty => none 
    | .node int left right => 
      if n < int.lo then lookupFloor n left else lookupFloor n right <|> some (int.lo, int.hi)

def IntervalTree.insertSimple (self : IntervalTree) (int : Interval) : IntervalTree :=
  match self with
  | .empty => .node int .empty .empty 
  | .node int' left right => 
    if int.lo < int'.lo 
      then .node int' (left.insertSimple int) right  
      else .node int' left (right.insertSimple int)

def IntervalTree.extractMin (self : IntervalTree) : Interval × IntervalTree := 
  match self with 
  | .empty => panic! "fuck! a empty tree man -_-"
  | .node int .empty right => (int, right)
  | .node int left right => 
    let (m, left') := left.extractMin
    (m, .node int left' right)

def IntervalTree.merge : IntervalTree → IntervalTree → IntervalTree
| .empty, other => other 
| self, .empty => self
| left, right => 
  let (int, right') := extractMin right 
  .node int left right'

def IntervalTree.extractTouching (self : IntervalTree) (int : Interval)  : (List (Nat × Nat) × IntervalTree) := go self
  where 
    go self := 
      match self with
      | .empty => ([], .empty)
      | .node int' left right => 
        if int'.hi < int.lo.pred 
          then 
            let (found, right') := right.extractTouching int
            (found, .node int' left right')
        else if int'.lo > int.hi.succ 
          then 
            let (found, left') := left.extractTouching int
            (found, .node int' left' right)
        else
          let (foundL, left') := left.extractTouching int 
          let (foundR, right') := right.extractTouching int 
          ( int'.toTuple :: foundL ++ foundR
          , left'.merge right'
          )

def IntervalTree.insert (self : IntervalTree) (interval : Interval) : IntervalTree :=  
  let (touching, cleaned) := self.extractTouching interval
  let newLo := (interval.lo :: touching.map (·.fst)).min? |>.get!
  let newHi := (interval.hi :: touching.map (·.snd)).max? |>.get!
  cleaned.insertSimple $ Interval.mk newLo newHi

def IntervalTree.toList : IntervalTree → List Interval 
| .empty => [] 
| .node int left right => left.toList ++ [int] ++ right.toList

def IntervalTree.fromList : List (Nat × Nat) → IntervalTree := 
  (·.foldr (fun int t => t.insert $ Interval.fromTuple int) .empty)

def IntervalTree.countMembers : IntervalTree → Nat := 
  List.foldl (fun acc {lo, hi} => acc + (hi - lo + 1)) 0 ∘ IntervalTree.toList

#eval (IntervalTree.fromList [(20, 25)] |>.countMembers)

def parseInput (input : String) : (IntervalTree × Array Nat) := 
  match input.splitOn "\n\n" with 
  | intervals :: ids :: _ => 
    let ids := ids.splitOn "\n" |>.filter (not ∘ String.isEmpty) |>.toArray
    let ids := ids.map String.toNat!
    let tree := parseIntervals intervals
    (tree, ids) 
  | _xs => unreachable!
  where 
    parseIntervals (input : String) : IntervalTree := 
      input.splitOn "\n" 
      |>.map (·.splitOn "-" |> List.map String.toNat! |> Interval.fromList!) 
      |>.mergeSort (·.lo <= ·.lo)
      |>.foldl IntervalTree.insert .empty

def countValidIds (tree : IntervalTree) (ids : Array Nat) : Nat := 
  ids.filter tree.member |>.size 

def solution : IO Nat := do 
  let input <- IO.FS.readFile "day_five.txt"
  let (tree, _) := parseInput input 
  pure tree.countMembers 


#eval solution
