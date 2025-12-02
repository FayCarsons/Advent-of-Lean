structure Range where 
  first : Nat 
  last : Nat
deriving Inhabited, DecidableEq 

def parseRanges (input : String) : Array Range := 
  String.mk (input.foldr (fun c acc => if c.isWhitespace then acc else c::acc) [])
  |>.splitOn ","
  |>.filter (not ∘ String.isEmpty)
  |>.toArray 
  |>.map parseRange!
  where 
    parseRange! s := 
      let xs := String.splitOn s "-";
      match List.map String.toNat? xs with 
      | .some first :: .some last :: [] => {first, last}
      | otherwise => panic! s!"Expected two nats, got {repr otherwise}"

-- Classic leetcode "is substring of" problem
def isInvalidID (n : Nat) : Bool := 
  let s := n.reprFast;
  let doubled := s ++ s ;
  let middle := doubled.extract ⟨1⟩ ⟨doubled.length.pred⟩
  (middle.splitOn s).length > 1

def invalidInRange (rng : Range) := 
  List.range (rng.last.succ - rng.first) 
  |>.map (· + rng.first) 
  |>.filter isInvalidID 
  |>.sum

def countInvalid : Array Range → Nat := 
  Array.sum ∘ Array.map invalidInRange 

def solution : IO Nat := do 
  let input <- IO.FS.readFile "aoc/day_two.txt"
  let ranges := parseRanges input
  pure $ countInvalid ranges

#eval solution 
