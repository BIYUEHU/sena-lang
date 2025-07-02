class MihamaError extends Error { }

const mihamaCurry = (f) => {
  const curried = (expectedLength, args) => ((...rest) => {
    if (rest.length === 0 || expectedLength === 0) {
      throw new MihamaError(`Arguments cannot be empty for function ${f.name}`);
    }

    if (rest.length === expectedLength) {
      return f(...args, ...rest);
    }

    if (rest.length > expectedLength) {
      throw new MihamaError(`Too many arguments for function ${f.name}`);
    }

    return curried(expectedLength - rest.length, [...args, ...rest])
  });

  return curried(f.length, []);
}

// type MihamaType = {
//   type_tag: string;
// }
// type MihamaValue = {
//   value_tag: string;
//   value?: any[];
// }

const createMihamaType = (tag) => ({ type_tag: tag } )
const createMihamaValue = (tag, value) => ({ value_tag: tag, value })

const [MihamaKind, MihamaString, MihamaInt, MihamaBool, MihamaChar] = ["Kind", "String", "Int", "Bool", "Char"].map(createMihamaType)

const foo = 1;
// foo = 2 // Error
const foo1 = "Hello, FP!";
const foo2 = 'X';
const Bool2 = createMihamaType("Bool2");
const True = createMihamaValue("True");
const False = createMihamaValue("False");
const foo3 = False
const foo4 = 3.1415926

const List = mihamaCurry((x) => createMihamaType(`List<${x.type_tag}>`))
const Cons = mihamaCurry((x, xs) => createMihamaValue("Cons", [createMihamaValue('Cons', [x, xs])]))
const Nil = createMihamaValue("Nil")
const String1 = List(MihamaChar)
const String2 = List(MihamaChar)

const add = mihamaCurry((x, y) =>  x + y)

const fibonacii = mihamaCurry((x) =>
  x === 1 ? 1 :
  x === 2 ? 1 :
  fibonacii(x - 1) + fibonacii(x - 2))

const Nat = createMihamaType("Nat")
const Z = createMihamaValue("Z")
const S = mihamaCurry((x) => createMihamaValue("S", [x]))

const toNumber = mihamaCurry((x) =>
  x.value_tag === "S" && x.value[0] === Z ? 1 :
  x.value_tag === "S" ? toNumber(x.value[0]) + 1 :
  x === Z ? 0 :
  (() => { throw new MihamaError("Match pattern not exhaustive at function toNumber") })())

const num = S(S(S(Z)))
console.log(add(toNumber(num))(3))

const bar = ((x) =>
  ((y) =>
    x + y ^ 2 * 2 / 5 - 5 % 1
  )(30)
)(20)

const bar2 = (() => {
  const x = 20
  const y = 30
  return x + y ^ 2 * 2 / 5 - 5 % 1
})()

const colon = (el, list) => Cons(el, list)
const dollor = (f, x) => f(x)
const dot = (g, f) => (x) => f(g(x))

const handleScore = (x) =>
  x > 100 || x < 0 ? "Fake" :
  x === 100 ? "Best" :
  x > 90 ? "Excellent" :
  x > 70 ? "Good" :
  x > 60 ? "Not bad" :
  x > 40 ? "Bad" :
  "Shit"