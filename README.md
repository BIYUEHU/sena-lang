<!-- markdownlint-disable-next-line -->
<div align="center">

# Sena Language

<!-- markdownlint-disable-next-line -->
<img src="./extension/icons/icon.png" width="230" />

 [![wakatime](https://wakatime.com/badge/user/018dc603-712a-4205-a226-d4c9ccd0d02b/project/5a7b91ab-6432-4729-8b56-bc6b70c9b93e.svg)](https://wakatime.com/badge/user/018dc603-712a-4205-a226-d4c9ccd0d02b/project/5a7b91ab-6432-4729-8b56-bc6b70c9b93e) [![Build](https://github.com/BIYUEHU/sena-lang/actions/workflows/build.yml/badge.svg)](https://github.com/BIYUEHU/sena-lang/actions/workflows/build.yml)

<!-- markdownlint-disable-next-line -->
### [**Playground 👉**](https://l.himeno-sena.com)

</div>

> Developing...

- **Sena** 是一门支持一等类型与依赖类型、多后端、多范式、现代化的函数式编程语言。
- **Sena** is a functional programming language, supporting first-class types and dependent types, multiple backends, multiple paradigms, and modern features.

## Advantages

- **现代化语言**：摒弃迂腐的传统 C 系语言的经典概念和糟粕语法，保持语法简洁与语义统一（如不区分普通函数与匿名函数）
- **强大类型系统**：引入并推广一等类型、类型类、高阶类型等特性到工业界，并提升语言的表达能力与可靠性
- **工业友好**：基于 Haskell、Idris、OCaml 等 ML 系语言特性，保持工业开发者熟悉语法（`{}` 块、`f(x)` 函数调用、JS 风格语法）以降低门槛
- **多后端**：希望支持多种动态语言后端，包括 JavaScript、Python、Ruby、Lua、Common Lisp，并支持 LLVM、Wasm、Native C 等后端，但绝不会考虑虚拟机
- **多范式**：在保持语法简洁性和语义统一性前提下效仿 Scala 将 OOP 与 FP 高度融合

## Anchoring

经过几个月的痛定思痛，秉持着打造一门有其独特亮点的现代函数式语言，决定放弃引入完整的依赖类型。

具体地说，引入一等类型但不包括依赖类型以及类型宇宙等复杂特性，延续类似于 Haskell 中的 `Kind` 系统。首先明确 `Kind` 包含什么以及它和类型宇宙的区别（Haskell-Like 伪代码）：

```haskell
1 :: Int
Int :: *
Maybe :: * -> *
```

此处的 `*` 和 `* -> *` 即分别为 `Kind` 中的 `Star`（相当于 `Type`） 和 `Arrow`。当然 Haskell 中的 `Kind` 还包括 `Constraint` 乃至 `DataKinds` 扩展可以提升类型为 `Kind`，不过这不在讨论范围内了。类型宇宙则提供了一个更纯粹统一的视角：

```haskell
1 :: Int
Int :: Type
Type :: Type 1
Type 1 :: Type 2
-- ...
```

无限地累积下去就会变成一个垂直上升的结构，Lean4 中即是如此，而 Idris2 现阶段未实现类型宇宙，只能 `Type : Type` 这意味着可以直接构造出罗素悖论。实现依赖类型本身就很复杂，而实现依赖类型也自然会带来类型宇宙，有了类型宇宙也必须一并引入宇宙多态，如此工程量是极不划算的。

做这个项目的初衷一方面是一等类型确实有趣，当然依赖类型也很有趣，但对于工业导向还是太无必要了，也无必须可以定理证明的硬性需求。另一方面是哀于 TypeScript 它有着极强类型编程能力，可类型层面和值层面是完全隔离的两个世界和两套语法，实在遗憾。Zig 的编译时计算类型的特性也很不错，至少语法统一了，但碍于 Zig 的类型系统比 TypeScript 弱太多无法玩出花样，其次运行时依旧不存在令人遗憾。对比下来，Sena 的定位及目标也就明确了。

总的来说，一等类型意味着 `Type` 具有一等性，但 `Kind` 不具有一等性，如有这样的行为：

```ts
let foo = 1 // foo : Int
let Foo = Int // foo : Type
let FOO = Type // Error!
```

通俗地说，`Type`、`Type -> Int`、`Type -> Type` 等只要出现了 `Type` 的就无法作为值表达式传递，但若是 `Int`或 `Int -> Int` 等则可以。虽然不支持依赖类型（即类型依赖于值），但对于类型算子允许在编译时做一些静态字面量值计算，如：

```ts
let F: Int -> Type = (x) => match x
  | 0 => Int
  | 1 => String
  | _ => Bool

let x: F(1) = "hi"
```

这看着会像 Scala3 的 `TypeLambda`，因为其也支持在类型层面做 `match`，但本质上截然不同，首先 Scala3 中的 `0`、`hi` 等是字面量类型而非值，`match` 也只是其类型层面的特殊语法。而 Sena 中的这一切均来自一等类型，与普通的值操作别无二致，只是说在参与编译时的类型计算里一切都必须纯且可停机（这将涉及后续设计细节了），在 Idris2 等语言亦是如此。

## Features

### 当前实现

- 基本语法
- 代数数据类型（ADT）、模式匹配
- 自定义中缀运算符
- 一等类型（First-class-type）
- 宏（文本替换与预处理宏）、条件执行、模块导入
- JS 后端
- 直接解释
- 较完善的 REPL 支持

### 长期目标

- 语法层面上的 Record 与 Tuple 类型
- 副作用 IO
- 广义代数数据类型（Generalized ADT）
- RankNTypes 与完善的 System F
- 列表推导式
- 函数式循环
- 完善的系统 F（System F）
- 类型类（Trait/Typeclass）
- 高阶类型（Higher-order type）
<!-- - 依赖类型（分阶段引入） -->
- 更完善的宏与模块系统
- LSP 与更友好的错误提示
- 更多的动态语言后端支持：Python、Ruby、Lua、Common Lisp
- LLVM、Wasm、Native C 后端
- 工业级生态（库、包管理、互操作）

---

## Docs

### 开始

可在 REPL 中交互，也可运行以 `.mh` 为后缀名的源文件，或者在 [Playground](https://l..icu) 中在线编辑并运行。

### CLI 使用

```bash
A modern functional programming language with dependent types

Usage: senas.exe [OPTIONS] [COMMAND]

Commands:
  repl
  run
  trans
  check
  parse
  lex
  format
  help    Print this message or the help of the given subcommand(s)

Options:

  -v, --verbose
  -h, --help     Print help
  -V, --version  Print version
```

### REPL 使用

```bash
> :h
Available commands:
:help, :h          - Show this help
:exit, :quit, :q   - Exit the REPL
:mode, :m <mode>   - Switch REPL mode (l|p|c|e|u)
:read, :r <file>   - Read and execute file
:clear, :c         - Reset environments
:trans, :t <src> <dst> - Transpile source to destination
.t <code>           - Evaluate and show types
```

### 绑定

Rust 将其称之为不可变变量，但它与传统编程中变量的概念有实质区别，也绝不等同常量，更接近于数学中的变量概念，在 FP 中将其称之为**绑定**（binding）。具体可参考 `demo/` 下的示例代码。

```ts
let x = 1
// x = 2 // Parser error
let bar: Float = 3.1415926
let name: String = "Hello, FP!"
```

`let x = 1` 可以理解为数学中的 “令 x 等于 1” 。`x = 2` 不会是运行错误，而是解析器层面的语法错误，因为没有 `=` 也不允许这种中缀运算符。

### 函数

不区分普通函数与匿名函数，其语法参考自 JavaScript 的箭头函数（Lambda 表达式）。

```ts
let add: Int -> Int -> Int = (x) => (y) => x + y
let add1: Int -> Int -> Int = (x, y) => x + y
let add2 = (x: Int, y: Int): Int => x + y
// let foo = () => 1 // Parser error
```

支持柯里化，不支持无参数函数。

### 原语类型

原则上提供了 `String`、`Char`、`Int`、`Float`、`Bool`、`Unit`、`Kind` 作为原语类型。

```ts
let s: String = "Hello, world!"
let c: Char = 'a'
let i: Int = 123
let f: Float = 3.1415926
let b: Bool = true
let u: Unit = {()}
let k: Kind = Int
```

### 运算符

前缀运算符：

```ts
-1
!true
```

算术运算符：

```ts
1 + 2
1 - 2
1 * 2
1 / 2
1 ^ 2
1 % 2
```

逻辑运算符：

```ts
true && false
true || false
```

比较运算符：

```ts
1 == 2
1!= 2
1 < 2
1 <= 2
1 > 2
1 >= 2
```

字符串相关：

```ts
"Hello, " +: "World!"
```

数组相关：

```ts
1 : 2 : [] // [1, 2]
[1, 2, 3] ++ [4, 5, 6] // [1, 2, 3, 4, 5, 6]
```

Haskell 风格运算符：

```ts
let g: Int -> Int = (x) => x * 2
let h: Int -> Int = (x) => x ^ 3

// ($) : (a -> b) -> a -> b
// (.) : (b -> c) -> (a -> b) -> a -> c

g $ 33 + 44 // Equivalent to g(33 + 44)
(h . g)(3) // Equivalent to g(h(3))
```

函数类型 `->` 是一种类型到类型到类型（`Kind -> Kind -> Kind`） 的运算符，与其他运算符无本质区别，此时一等类型的特性逐渐显现：

```ts
Int -> Int
```

### Let-in 表达式

对于 ML 系语言用户会非常熟悉，既能进行新绑定也保证其仍是一个纯粹的表达式而非语句。

```ts
let result =
  let x = 10 in
  let y = 20 in
  x + y
```

每个 `Let-in` 表达式只支持一个绑定，但可以嵌套，作用域向 in 内延申。

### 块语句

上面的代码对于工业语言用户可能比较陌生，于是也提供了典型的块语句，可借此编写 C-Like 风格代码：

```ts
let block = {
  let x = 20
  let y = 30
  x + y ^ 2 - 2 / 5 - 5 % 1
}
```

### 条件表达式

```ts
let result = if true then 1 else 0
let handleScore = (x: Int) =>
  if x > 100 || x < 0 then "Fake"
  else if x == 100 then "Best"
  else if x > 90 then "Excellent"
  else if x > 70 then "Good"
  else if x > 60 then "Not bad"
  else if x > 40 then "Bad"
  else "Shit"
handleScore(80) // "Good"
```

### 代数数据类型

语法类似 Haskell 系语言，但方便理解使用 `type` 语句进行专门定义 ADT，但并非只有 `type` 定义的东西才是类型。

```ts
type Color = Red | Green | Blue
let color: Color = Red
```

无类型参数的代数数据类型：

```ts
type Nat = Z | S(Nat)
let one = S(Z)
let two = S(one)
```

不同于 ML 系语言，带有参数的代数数据类型需要显示标注类型参数：

```ts
type List = <A> Nil | Cons(A, List(A))
let xs: List(Int) = Cons(1, Cons(2, Cons(3, Nil)))
```

常见的代数数据类型：

```ts
// ...
type Maybe = <A> Just(A) | Nothing
type Either = <A, B> Left(A) | Right(B)
type Tree = <A> Node(A, List(Tree(A)))
```

### 类型别名

Sena 是一等类型的语言，类型别名本质就是与值一样的新绑定（不可变变量）：

```ts
// ...
let Option = Maybe
let Result = Either

let IntList = List(Int)
```

也可以定义新的类型算子（类型到类型的函数）：

```ts
// ...
let StringResult: Kind -> Kind = (T) => Result(String, T)
```

### 模式匹配

对原语类型使用模式匹配时效果类似于传统 C 系语言的 `switch` 语句：

```ts
let handleScore = (x: Int) =>
  match x then
  | 100 => "Best"
  | 90 => "Excellent"
  | 70 => "Good"
  | 60 => "Not bad"
  | 40 => "Bad"
  | _ => "Shit" // Other cases

handleScore(80) // "Good"
```

对于 ADT 类型：

```ts
// ...
let nat_to_int = (n: Nat) =>
  match n then
  | Z => 0
  | S(n) => 1 + nat_to_int(n)
```

条件守卫：

```ts
// ...
let int_to_nat = (n: Int) =>
  match n then
  | 0 => Z
  | n if n > 0 => S(int_to_nat(n - 1))
  | _ => error("Negative number")
```

### 一等类型和类型的类型

值能赋值给变量（通俗的说法，即绑定），类型也能赋值给变量，此即为一等类型。但要强调一点，一等类型下会使类型与值的绑定共享同一作用域：

```ts
let x: Int = 10
// let Int = 233 // Error
let MyInt = Int
```

那么 `MyInt` 是什么类型？显然你已经知道了答案，在 `REPL` 中输入 `.t <expr>` 即可查看类型：

```bash
> .t MyInt
MyInt : Kind
> .t Result(Int, String)
Result(Int, String) : Kind
> .t Result(Int)
Result(Int) : Kind -> Kind
> .t Result
Result : Kind -> Kind -> Kind
```

`Kind` 即是类型的类型，不过其他大部分语言表示类型的类型选用的是 `Type`，但 Sena 认为其太过宽泛。那么 `Kind` 的类型又是什么了？如果是 Agda 或者 Lean4 之类的语言你会看到（以 Lean4 为例）：

```lean
#check Type -- Type : Type 1
#check Type 1 -- Type 1 : Type 2
#check Type 2 -- Type 2 : Type 3
-- ...
```

这被称作为 **类型宇宙（Type Universe）**，但对于目标是工业语言的 Sena 而言太没必要，于是借鉴了 Idris2 的做法，`Kind` 的类型仍是 `Kind`：

```bash
> .t Kind
Kind : Kind
```

### 自定义中缀运算符

类似于 Haskell，对于普通函数可通过反引号作为中缀运算符：

```ts
// ...
1 `add` 2 // Equivalent to add(1, 2)
```

特殊符号名字的函数可直接作为中缀：

```ts
// ...
let #++# = (list1, list2) =>
  match list1 then
    | Cons(a, b) =>
      Cons(a, b ++ list2)
    | Nil => list2

let #+:# = (str1, str2) => concat(str1, str2)

[2] ++ [3] // [2, 3]
"Hello, " +: "World!" // "Hello, World!"
#++#([1, 2], [3, 4]) // Normal function call
```

其中 `#<...>#` 的语法表示保留为原始文本，类似于 Haskell 中定义中缀需要加括号 `(++)`，但对于偏 C-Like 的 Sena 而言，括号已有太多语义，解析起来会很复杂，故选用了 `#`。

### 模块

`@import` 语句后接 `.mh` 文件路径，后缀名可省略。

```ts
@import "a"
// @import "a" // Error
@import "b.mh"
@import "../b"
```

### 宏

目前宏均基于预处理器，类似于 C。定义常量：

```ts
@define PI 3.14159
```

简单函数宏：

```ts
@macro max(a, b) => if a > b then a else b

```

条件宏：

```ts
@ifdef MAIN_FILE
   print("In main file")
@endif
@ifndef MAIN_FILE
   print("Not in main file")
@endif
```

基于条件宏的条件导入：

```ts
@ifdef DEBUG
  @import "../debug_module"
@endif
```

## License

Under the BCU License.
