

inductive Dir where 
| left 
| right
deriving DecidableEq , Inhabited

structure Step where 
  dir : Dir
  ticks : Nat

def parse_steps (input : String) : IO (Array Step) :=   
  let parsed := 
    String.splitOn input "\n" 
    |> List.filter (not ∘ String.isEmpty) 
    |>.mapM parseStep
  List.toArray <$> parsed 
  where 
    parseStep s := do
      let dir : Dir := match String.front s with 
        | 'L' => .left 
        | 'R' => .right 
        | c => panic! s!"Impossible char {c}"
      let ticks := String.drop s 1 |> String.toNat!; 
      pure $ Step.mk dir ticks

structure CountTicks where 
  current_val : Nat 
  crossed_zero : Nat
deriving DecidableEq , Inhabited

def sub_cyclic (crossed : Nat) (acc : Nat) : Nat → Nat × Nat 
| .zero => (acc, crossed) 
| .succ n => 
  match acc with 
  | .zero => sub_cyclic crossed.succ 99 n 
  | .succ acc => sub_cyclic crossed acc n

def add_cyclic (crossed : Nat) (acc : Nat) : Nat → Nat × Nat 
| .zero => (acc, crossed)
| .succ n => 
  match acc with 
  | 99 => add_cyclic crossed 0 n 
  | 0 => add_cyclic crossed.succ (.succ 0) n 
  | acc => add_cyclic crossed acc.succ n 

def count_ticks (acc : CountTicks)  (step : Step) : CountTicks :=
  match step with
  | { dir := .left, ticks } => 
    let (current_val, crossed_zero) := sub_cyclic acc.crossed_zero acc.current_val ticks;
    { current_val, crossed_zero }
  | { dir := .right , ticks } => 
    let (current_val, crossed_zero) := add_cyclic acc.crossed_zero acc.current_val ticks;
    { current_val, crossed_zero }

def count_zero_crossings (steps : Array Step) : Nat := 
  steps.foldl count_ticks (CountTicks.mk 50 0) |>.crossed_zero

def solution : IO Unit := do 
  let input <- IO.FS.readFile "aoc/day_one.txt"
  let steps <- parse_steps input
  let num_zero_crossings := count_zero_crossings steps
  println! s!"{num_zero_crossings}"

#eval solution
