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
print(a(1))

let fibonaci: Int -> Int = (n) => {
  if n < 2 then 1 else fibonaci(n - 1) + fibonaci(n - 2)
}

let start = get_timestrap()
print(fibonaci(7))
print(get_timestrap() - start)
"#;

pub const FIBONACII_CODE: &'static str = r#"let fibonaci = (n: Int): Int => {
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
