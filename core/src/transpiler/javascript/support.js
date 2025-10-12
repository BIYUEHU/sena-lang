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

const createMihamaType = (tag) => ({ type_tag: tag })
const createMihamaValue = (tag, value) => ({ value_tag: tag, value })

const [MihamaKind, MihamaString, MihamaInt, MihamaBool, MihamaChar] = ["Kind", "String", "Int", "Bool", "Char"].map(createMihamaType)

const print = console.log
const get_timestamp = () => new Date().getTime() / 1000
const concat = (a, b) => `${a}${b}`
