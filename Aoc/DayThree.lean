abbrev BatteryArrays := Array $ List Nat

def parseArrays (input : String) : BatteryArrays := 
  input.splitOn "\n"
  |> List.filter (not ∘ String.isEmpty)
  |>.foldl parseArray #[]
  where 
    parseArray (acc : BatteryArrays) (s : String) : BatteryArrays := 
      s.toUTF8 |>.data.map (·.toNat - '0'.toNat) |>.toList |> acc.push

partial def Array.tails  (xs : Array a) : Array (Array a) := 
  go #[] xs 
  where 
    go acc xs := 
      if not xs.isEmpty then 
        go (acc.push xs) (xs.drop 1)
      else 
        acc

def List.tails : List a → List (List a) 
| [] => []
| l@(_ :: tl) => l :: tl.tails

def List.max : List Nat → Nat := List.foldl ifLt 0
  where ifLt a b := if a > b then a else b

def Array.max (xs : Array Nat) : Nat := 
  xs.foldl (fun acc x => if x > acc then x else acc) 0

def batteryArrayIdealConfig_1 (xs : Array Nat) : Nat := 
  xs.tails
  |>.map largestPair? 
  |>.reduceOption 
  |>.max
  where 
    largestPair?  (xs : Array Nat) : Option Nat := 
      if not xs.isEmpty then 
        xs.toSubarray 1 |>.toArray |>.map (10 * xs[0]! + ·) |>.max |> some
      else 
        none

def foldNat : List Nat → Nat := 
  List.foldl (fun acc x => acc * 10 + x) 0

def List.maxIdx  (xs : List Nat) (notEmpty : xs ≠ []) : Nat × Nat := 
  match xs with 
  | [] => absurd rfl notEmpty
  | cons x xs => 
    xs.zipIdx (n := 1) |>.foldl (fun acc x => 
      if x.fst > acc.fst 
        then x 
        else acc
    ) (x, 0)

def pickBest (remaining : Nat) (xs : List Nat) : Nat × List Nat := 
  let window := (xs.length - remaining).succ 
  let pref := xs.take window 
  if h : pref ≠ [] then 
    let (val, idx) := pref.maxIdx h 
    let rest := xs.drop idx.succ 
    (val, rest)
  else 
    (0, [])

def largestK : Nat → List Nat → List Nat
| .zero, _ => []
| _, [] => [] 
| .succ k, xs => 
  let (best, rest) := pickBest k.succ xs 
  best :: largestK k rest

def bestJoltage : Nat → List Nat → Nat := fun N xs => 
  largestK N xs |> foldNat

#eval bestJoltage 12 [
  2,4,1,2,1,2,2,3,
  2,2,3,2,1,2,2,2,
  2,5,2,2,2,2,2,2,
  1,6,2,2,3,3,2,2,
  2,2,2,2,1,5,2,1,
  2,6,1,4,3,1,7,2,
  3,1,1,2,6,3,4,2,
  1,1,2,3,2,2,2,3,
  2,3,2,1,3,2,1,5,
  4,2,2,2,2,1,4,2,
  2,2,2,1,1,3,3,2,
  2,2,9,2,2,2,2,3,
  1,2,2,2
]

def solve (N : Nat) : IO Unit := do 
  let input <- IO.FS.readFile "day_three.txt"
  let answer := parseArrays input |>.map (bestJoltage N) |>.sum
  println! s!"For arrays of size {repr N}:\n{repr answer}"

public def solution : IO Unit := 
  solve 12

#eval solution

