
def Roll := '@'.toUInt8 

inductive Space | Nothing | Roll 
  deriving Inhabited, DecidableEq

def Roll.fromUInt8 : UInt8 → Space := 
  (if · = Roll then .Roll else .Nothing)

abbrev Grid := Array $ Array Space

def Offsets : List (Int × Int) := 
  [ (-1, 0)
  , (1, 0)
  , (0, -1)
  , (0, 1)
  , (-1, 1)
  , (1, -1)
  , (-1, -1)
  , (1, 1)
  ]

def parseGrid (input : String) : Array (Array Space) := 
  input.splitOn "\n" 
  |>.toArray 
  |>.map (·.toUTF8.data.map Roll.fromUInt8) 

def lookupNeighbor (grid : Array (Array Space)) (x y : Nat) (offset : (Int × Int)) : Option Nat := do 
  let (dx, dy) := offset
  let x <- x + dx |>.toNat?
  let y <- y + dy |>.toNat?

  let row <- grid[y]?
  let space <- row[x]?
  match space with 
  | .Roll => some 1
  | _ => none

def removeLegalRolls (grid : Grid) : Nat × Grid := Id.run $ do
  let mut count := 0
  let mut grid := grid

  for (row, y) in grid.zipIdx do 
    for (space, x) in row.zipIdx do 
      match space with 
      | .Roll => 
        let neighbors := Offsets.map (lookupNeighbor grid x y) |>.reduceOption |>.sum
        if neighbors < 4 
          then do
            count := count.succ 
            grid := grid.set! y (grid[y]!.set! x .Nothing)
      | .Nothing => ()

  (count, grid)


partial def countAdjacentRolls (grid : Array (Array Space)) : Nat := go 0 grid 
  where 
    go acc grid := 
      match removeLegalRolls grid with 
      | (.zero, _) => acc 
      | (n, grid) => go (acc + n) grid

def solution : IO Nat := do
  let input <- IO.FS.readFile "day_four.txt"
  let grid := parseGrid input 
  pure $ countAdjacentRolls grid

#eval solution
