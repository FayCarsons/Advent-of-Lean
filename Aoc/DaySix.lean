inductive Op | Add | Mul 
deriving Inhabited, DecidableEq

def testInput := "123 328  51 64 
 45 64  387 23 
  6 98  215 314
*   +   *   +"

instance : Repr Op where 
  reprPrec op _ := 
    match op with 
    | .Add => s!"+"
    | .Mul => s!"*"

def Op.parse! : String → Op 
  | "+" => .Add
  | "*" => .Mul 
  | s => panic! s!"Expected op, got {repr s}"

def Op.parse? : Char → Option Op 
  | '+' => some .Add 
  | '*' => some .Mul 
  | _ => none

def Op.apply : Op → Nat → Nat → Nat
  | .Add, x, y => x + y 
  | .Mul, x, y => x * y

def Op.applies : Op → List Nat → Nat 
  | .Add, x :: xs => xs.foldl Nat.add x
  | .Mul, x :: xs => xs.foldl Nat.mul x 
  | _,_ => panic! "Expected nonempty list in 'applies'"

def Nat.parseEither (s : String) : Except String Nat := 
  match s.toNat? with 
  | some n => .ok n 
  | none => .error s

def Except.get! {e a : Type} [Repr e] [Inhabited a] : Except e a → a
  | .ok x => x 
  | .error e => panic! s!"{repr e}"

def parseInput_1 (input : String) : IO (List (List Char) × List Char) := do
  let lines := input.splitOn "\n"
  let (numbers, ops) := lines.splitAt 4
  let numbers := 
    numbers.map (List.reverse ∘ String.toList)
  let ops := ops.head! |>.toList |>.reverse
  assert! numbers.all (·.length = ops.length) 
  pure (numbers, ops)

def solve_1 : List Nat → List Nat → List Nat → List Nat → List Op → Nat := go 0
  where 
    go acc
    | a :: as, b :: bs, c :: cs, d :: ds, op :: ops => 
      let result := op.apply a (op.apply b (op.apply c d))
      go (acc + result) as bs cs ds ops
    | [], [], [], [], [] => acc 
    | _,_,_,_,_ => panic! "List empty"

-- Cannibalized the first half to do the second, apologies to the reader :/

def atoi (xs : List Char) : Nat := List.foldl (fun a x => 10 * a + x.toNat - '0'.toNat) 0 $ xs.filter Char.isDigit

def solve_2 : List Char → List Char → List Char → List Char → List Char → Nat := go 0 []
  where 
    go acc (temps : List Nat)
    | ' ' :: as, ' ' :: bs, ' ' :: cs, ' ' :: ds, ' ' :: ops => 
      go acc [] as bs cs ds ops
    | a :: as, b :: bs, c :: cs, d :: ds, op :: ops => 
      let num := atoi [a, b, c, d]

      match Op.parse? op with 
      | some op => 
        go (acc + op.applies (num :: temps)) [] as bs cs ds ops 
      | none => 
        go acc (num :: temps) as bs cs ds ops
    | [], [], [], [], [] => acc 
    | _, _, _, _, _ => panic! "idk man"

def doit : IO Nat := do 
  let (nums, ops) <- IO.FS.readFile "day_six.txt" >>= parseInput_1
  match nums with 
  | [a, b, c, d] => pure $ solve_2 a b c d ops 
  | _ => panic! "Not enough nums"

#eval doit
