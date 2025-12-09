inductive Space 
| nothing 
| splitter
deriving Inhabited, DecidableEq, Repr
open Space (nothing splitter)

structure State where 
  agents : Array Bool
  splits : Nat
deriving Inhabited, DecidableEq, Repr

def Space.parse! : Char → Space 
  | '.' => nothing 
  | '^' => splitter 
  | c => panic! s!"Unexpected symbol: {repr c}"

def parseInput (input : String) : IO (Nat × Array (Array Space)) := do
  let lines  := input.splitOn "\n" |> List.filter (not ∘ String.isEmpty)
  println! "Init lines: {repr lines.length}"

  let lines := lines.map (List.toArray ∘ String.toList) |>.toArray 

  if h : lines.size > 0 then 
    let initPos := lines[0]!.findIdx (· = 'S')
    let lines := lines.set! 0 (lines[0].map (fun x => if x = 'S' then '.' else x))
    let lines := lines.map (·.map Space.parse!)
    pure (initPos, lines)
  else 
    panic! "Invalid grid"

def solve_1 (init : Nat) (grid : Array (Array Space)) : IO Nat := do
  let width := grid[0]!.size 
  let mut agents : Array Bool := Array.replicate width false 
  agents := agents.set! init true

  let mut totalSplits := 0

  for row in grid do 
    let mut newAgents := Array.replicate width false

    for i in [:width] do 
      match agents[i]? with 
      | some true => 
        match row[i]! with 
        | nothing => newAgents := newAgents.set! i true 
        | splitter => 
          totalSplits := totalSplits.succ
          newAgents := newAgents.set! i.pred true
          newAgents := newAgents.set! i.succ true
      | _ => continue

    agents := newAgents
  pure totalSplits


def solve_2 (init : Nat) (grid : Array (Array Space)) : IO Nat := do
  let width := grid[0]!.size 
  let mut paths : Array Nat := Array.replicate width 0 
  paths := paths.set! init 1

  for row in grid do 
    let mut newPaths := Array.replicate width 0

    for i in [:width] do 
      let count := paths[i]!

      if count = .zero then continue
      else 
        match row[i]! with 
        | nothing => 
          newPaths := newPaths.modify i (· + count)
        | splitter => 
          newPaths := newPaths.modify i.pred (· + count)
          newPaths := newPaths.modify i.succ (· + count)

    paths := newPaths
  paths.foldl Nat.add 0 |> pure

public def solution : IO Nat := do
    IO.FS.readFile "day_seven.txt" 
    >>= parseInput 
    >>= Function.uncurry solve_2

#eval solution
