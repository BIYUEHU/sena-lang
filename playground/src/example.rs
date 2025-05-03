pub const DEMO_CODE: &'static str = r#"import { Show, Eq } from "prelude"
import as std from "stdlib"

type Nat = Z | S(Nat)
type Pair = <A, B> MkPair(A, B)
type Maybe = <A> Just(A) | Nothing
type Result = <A, E> Ok(A) | Err(E)
export abstract type Option = <A> Some(A) | None
export type List = <A> Nil | Cons(A, List(A))

let a = (x) => c + 1
let c = 22

print("Hello, world!")

let fibonaci: Int -> Int = (n) =>
  match n then
  | n if n <= 2 => 1
  | n => fibonaci(n - 1) + fibonaci(n - 2)

let start = get_timestrap()
print(fibonaci(5))
print(get_timestrap() - start)
"#;

pub const FIBONACI_CODE: &'static str = r#"let fibonaci = (n: Int): Int => {
  if n < 2 then 1 else fibonaci(n - 1) + fibonaci(n - 2)
}

let fibonaci2: Int -> Int -> Int -> Int = (n, first, second) =>
  if n == 1 then first else
  if n == 2 then second
  else fibonaci(n - 1, second, first + second)

fibonaci(5)
"#;

pub const MATCH_CODE: &'static str = r#"type Nat = Z | S(Nat)

let to_number = (x) =>
  match x then
    | S(n) => 1 + to_number(n)
    | Z => 0


let bar = S(S(S(Z)))
print("Result: ", bar, " <===> ", to_number(bar))

let add = (x, y) =>
  match x then
    | S(n) => S(add(n, y))
    | Z => y

let foo = add(S(S(Z)), bar)
print("Result: ", foo, " <===> ", to_number(foo))

"#;

pub const LIST_CODE: &'static str = r#"type Maybe = <A> Just(A) | Nothing
type List = <A> Cons(A, List(A)) | Nil

let intList = List(Int)

print(intList)

let #:# = (el, list) => Cons(el, list)
let #$# = (f, x) => f(x)
let #.# = (g, f) => (x) => f(g(x))

let get = (list, index) =>
  match list then
    | Cons(a, b) =>
      if index == 0 then Just(a) else get(b, index - 1)
    | Nil =>
      Nothing

let map = (f, list) =>
  match list then
    | Cons(a, b) =>
      Cons(f(a), map(f, b))
    | Nil => Nil

let #++# = (list1, list2) =>
  match list1 then
    | Cons(a, b) =>
      Cons(a, b ++ list2)
    | Nil => list2

let #+++# = (str1, str2) => connect(str1, str2)

let show = (a) =>
  let f = (a) =>
    match a then
      | Cons(a, b) =>
        a +++ let c = f(b) in if c == "" then "" else ", " +++ c
      | Nil => ""
  in "[" +++ f(a) +++ "]"

let arr = 1 : 2 : 3 : 4 : Nil

show . print $ arr

map (print, get(arr, 2) : get(arr, 6) : Nil)

{()}
"#;

pub const INFIX_CODE: &'static str = r#"let f: Int -> Int -> Int = (x, y) => x + (y + 1)

let g: Int -> Int = (x) => x * 2

let #-=# = f

print("Custom infix:\n f(2, 3) =", f(2, 3), "\n 2 `f` 3 =", 2 `f` 3, "\n 2 -= 3 =", 2 -= 3, "\n")

let #$# = (f, x) => f(x)

let #.# = (g, f) => (x) => f(g(x))

let h: Int -> Int = (x) => x ^ 3

print("Apply infix:\n g(33 + 44) =", g(33 + 44), "\n g $ 33 + 44 =", g $ 33 + 44, "\n")

print("Compose infix:\n g(h(3)) =", g(h(3)), "\n (g . h)(3) =", (h . g)(3), "\n g . h $ 3 =", h . g $ 3, "\n")

let foo =
  let x = 10 in
  let y = 20 in
  let z = 30 in
  x + y + z

print("Let expression:\n foo =", foo, "\n")
"#;
