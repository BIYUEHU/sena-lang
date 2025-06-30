// ===== 核心运行时类型系统 =====

// 运行时类型表示
interface RuntimeType {
  kind: string;
  name?: string;
  params?: RuntimeType[];
  constraints?: any;
}

// 基础类型定义
const RT_Int: RuntimeType = { kind: "primitive", name: "Int" };
const RT_Float: RuntimeType = { kind: "primitive", name: "Float" };
const RT_String: RuntimeType = { kind: "primitive", name: "String" };
const RT_Char: RuntimeType = { kind: "primitive", name: "Char" };
const RT_Bool: RuntimeType = { kind: "primitive", name: "Bool" };

// 函数类型构造器
const RT_Function = (from: RuntimeType, to: RuntimeType): RuntimeType => ({
  kind: "function",
  params: [from, to],
});

// 列表类型构造器
const RT_List = (element: RuntimeType): RuntimeType => ({
  kind: "list",
  params: [element],
});

// ADT 类型构造器
const RT_ADT = (
  name: string,
  constructors: Record<string, RuntimeType[]>
): RuntimeType => ({
  kind: "adt",
  name,
  constraints: constructors,
});

// ===== 运行时值包装 =====

interface RuntimeValue<T = any> {
  value: T;
  type: RuntimeType;
  sourceInfo: SourceInfo;
}

interface SourceInfo {
  line: number;
  column: number;
  file: string;
  originalName?: string;
  sourceExpression?: string;
}

// 值构造器
function createValue<T>(
  value: T,
  type: RuntimeType,
  sourceInfo: SourceInfo
): RuntimeValue<T> {
  return { value, type, sourceInfo };
}

// ===== 运行时类型检查器 =====

class TypeChecker {
  static check(value: RuntimeValue, expectedType: RuntimeType): boolean {
    return this.isCompatible(value.type, expectedType);
  }

  static isCompatible(actual: RuntimeType, expected: RuntimeType): boolean {
    if (actual.kind !== expected.kind) return false;

    switch (actual.kind) {
      case "primitive":
        return actual.name === expected.name;

      case "function":
        return (
          actual.params?.length === 2 &&
          expected.params?.length === 2 &&
          this.isCompatible(actual.params[0], expected.params[0]) &&
          this.isCompatible(actual.params[1], expected.params[1])
        );

      case "list":
        return (
          actual.params?.length === 1 &&
          expected.params?.length === 1 &&
          this.isCompatible(actual.params[0], expected.params[0])
        );

      case "adt":
        return actual.name === expected.name;

      default:
        return false;
    }
  }

  static assert(value: RuntimeValue, expectedType: RuntimeType): void {
    if (!this.check(value, expectedType)) {
      throw new FPRuntimeError(
        `Type mismatch: expected ${this.typeToString(expectedType)}, ` +
          `got ${this.typeToString(value.type)}`,
        value.sourceInfo
      );
    }
  }

  static typeToString(type: RuntimeType): string {
    switch (type.kind) {
      case "primitive":
        return type.name || "Unknown";
      case "function":
        return `${this.typeToString(type.params![0])} -> ${this.typeToString(
          type.params![1]
        )}`;
      case "list":
        return `[${this.typeToString(type.params![0])}]`;
      case "adt":
        return type.name || "ADT";
      default:
        return "Unknown";
    }
  }
}

// ===== 自定义错误系统 =====

class FPRuntimeError extends Error {
  constructor(
    message: string,
    public sourceInfo: SourceInfo,
    public originalError?: Error
  ) {
    super(message);
    this.name = "FPRuntimeError";
  }

  toString(): string {
    return (
      `Error at ${this.sourceInfo.file}:${this.sourceInfo.line}:${this.sourceInfo.column}\n` +
      `${this.message}\n` +
      (this.sourceInfo.sourceExpression
        ? `Expression: ${this.sourceInfo.sourceExpression}\n`
        : "") +
      (this.sourceInfo.originalName
        ? `Variable: ${this.sourceInfo.originalName}\n`
        : "")
    );
  }
}

// 错误处理包装器
function wrapOperation<T>(
  operation: () => T,
  sourceInfo: SourceInfo,
  operationName: string
): T {
  try {
    return operation();
  } catch (error) {
    if (error instanceof FPRuntimeError) {
      throw error;
    }

    // 将 JS 原生错误转换为我们的错误
    throw new FPRuntimeError(
      `Runtime error in ${operationName}: ${
        error instanceof Error ? error.message : String(error)
      }`,
      sourceInfo,
      error instanceof Error ? error : undefined
    );
  }
}

// ===== ADT 实现 =====

// Union 类型的基类
abstract class ADTValue {
  abstract readonly tag: string;
  abstract readonly type: RuntimeType;
  abstract readonly sourceInfo: SourceInfo;
}

// Bool2 = True | False 的实现
class Bool2 extends ADTValue {
  readonly type = RT_ADT("Bool2", { True: [], False: [] });

  constructor(
    public readonly tag: "True" | "False",
    public readonly sourceInfo: SourceInfo
  ) {
    super();
  }
}

// List 的实现
class FPList<T> extends ADTValue {
  readonly tag: "Cons" | "Nil";
  readonly type: RuntimeType;

  constructor(
    tag: "Cons" | "Nil",
    public readonly elementType: RuntimeType,
    public readonly sourceInfo: SourceInfo,
    public readonly head?: RuntimeValue<T>,
    public readonly tail?: FPList<T>
  ) {
    super();
    this.tag = tag;
    this.type = RT_List(elementType);
  }

  static cons<T>(
    head: RuntimeValue<T>,
    tail: FPList<T>,
    sourceInfo: SourceInfo
  ): FPList<T> {
    return new FPList("Cons", head.type, sourceInfo, head, tail);
  }

  static nil<T>(elementType: RuntimeType, sourceInfo: SourceInfo): FPList<T> {
    return new FPList("Nil", elementType, sourceInfo);
  }
}

// ===== 模式匹配实现 =====

interface MatchCase<T, R> {
  pattern: Pattern;
  handler: (bindings: Record<string, RuntimeValue>) => RuntimeValue<R>;
}

type Pattern =
  | { kind: "literal"; value: any }
  | { kind: "variable"; name: string }
  | { kind: "constructor"; name: string; args: Pattern[] }
  | { kind: "wildcard" };

class PatternMatcher {
  static match<T, R>(
    value: RuntimeValue<T>,
    cases: MatchCase<T, R>[],
    sourceInfo: SourceInfo
  ): RuntimeValue<R> {
    for (const matchCase of cases) {
      const bindings: Record<string, RuntimeValue> = {};
      if (this.matchPattern(value, matchCase.pattern, bindings)) {
        return wrapOperation(
          () => matchCase.handler(bindings),
          sourceInfo,
          "pattern match"
        );
      }
    }

    throw new FPRuntimeError("Non-exhaustive pattern match", sourceInfo);
  }

  private static matchPattern(
    value: RuntimeValue,
    pattern: Pattern,
    bindings: Record<string, RuntimeValue>
  ): boolean {
    switch (pattern.kind) {
      case "literal":
        return value.value === pattern.value;

      case "variable":
        bindings[pattern.name] = value;
        return true;

      case "wildcard":
        return true;

      case "constructor":
        if (!(value.value instanceof ADTValue)) return false;
        if (value.value.tag !== pattern.name) return false;

        // 这里需要根据具体的 ADT 结构来匹配参数
        // 简化实现，实际需要更复杂的逻辑
        return true;

      default:
        return false;
    }
  }
}

// ===== 函数包装和柯里化 =====

class FPFunction {
  constructor(
    public readonly fn: (...args: RuntimeValue[]) => RuntimeValue | FPFunction,
    public readonly type: RuntimeType,
    public readonly sourceInfo: SourceInfo,
    public readonly arity: number = 1
  ) {}

  // 柯里化调用
  call(arg: RuntimeValue): RuntimeValue | FPFunction {
    if (this.arity === 1) {
      return wrapOperation(
        () => this.fn(arg),
        this.sourceInfo,
        "function call"
      );
    } else {
      // 返回部分应用的函数
      return new FPFunction(
        (...args) => this.fn(arg, ...args),
        this.type.params![1], // 返回类型
        this.sourceInfo,
        this.arity - 1
      );
    }
  }
}

// ===== 转译示例 =====

// 原始代码: let add: Int -> Int -> Int = (x) => (y) => x + y
const add = createValue(
  new FPFunction(
    (x: RuntimeValue<number>) =>
      new FPFunction(
        (y: RuntimeValue<number>) => {
          TypeChecker.assert(x, RT_Int);
          TypeChecker.assert(y, RT_Int);

          return createValue(x.value + y.value, RT_Int, {
            line: 15,
            column: 45,
            file: "main.fp",
            sourceExpression: "x + y",
          });
        },
        RT_Function(RT_Int, RT_Int),
        { line: 15, column: 40, file: "main.fp", originalName: "y" }
      ),
    RT_Function(RT_Int, RT_Function(RT_Int, RT_Int)),
    { line: 15, column: 35, file: "main.fp", originalName: "x" },
    2
  ),
  RT_Function(RT_Int, RT_Function(RT_Int, RT_Int)),
  { line: 15, column: 5, file: "main.fp", originalName: "add" }
);

// 原始代码: let fibonacii: Int -> Int = (x) => match x then ...
const fibonacci = createValue(
  new FPFunction(
    (x: RuntimeValue<number>) => {
      TypeChecker.assert(x, RT_Int);

      return PatternMatcher.match(
        x,
        [
          {
            pattern: { kind: "literal", value: 1 },
            handler: () =>
              createValue(1, RT_Int, {
                line: 22,
                column: 10,
                file: "main.fp",
                sourceExpression: "1",
              }),
          },
          {
            pattern: { kind: "literal", value: 2 },
            handler: () =>
              createValue(1, RT_Int, {
                line: 23,
                column: 10,
                file: "main.fp",
                sourceExpression: "1",
              }),
          },
          {
            pattern: { kind: "variable", name: "x" },
            handler: (bindings) => {
              const xVal = bindings.x;
              // fibonacci(x - 1) + fibonacci(x - 2) 的实现
              // 这里需要递归调用，简化示例
              return createValue(42, RT_Int, {
                line: 24,
                column: 10,
                file: "main.fp",
                sourceExpression: "fibonacci(x - 1) + fibonacci(x - 2)",
              });
            },
          },
        ],
        {
          line: 21,
          column: 3,
          file: "main.fp",
          sourceExpression: "match x then ...",
        }
      );
    },
    RT_Function(RT_Int, RT_Int),
    { line: 20, column: 36, file: "main.fp", originalName: "x" }
  ),
  RT_Function(RT_Int, RT_Int),
  { line: 20, column: 5, file: "main.fp", originalName: "fibonacci" }
);

// ===== 辅助工具 =====

// 创建数组字面量的辅助函数
function createArrayLiteral<T>(
  elements: RuntimeValue<T>[],
  elementType: RuntimeType,
  sourceInfo: SourceInfo
): RuntimeValue<FPList<T>> {
  let result = FPList.nil<T>(elementType, sourceInfo);

  // 从右到左构建列表
  for (let i = elements.length - 1; i >= 0; i--) {
    result = FPList.cons(elements[i], result, sourceInfo);
  }

  return createValue(result, RT_List(elementType), sourceInfo);
}

// Let 表达式的辅助函数
function withLetBinding<T>(
  bindings: Record<string, RuntimeValue>,
  body: (env: Record<string, RuntimeValue>) => RuntimeValue<T>
): RuntimeValue<T> {
  return body(bindings);
}

// ===== 转译策略核心思路 =====

/**
 * 1. 所有值都被包装在 RuntimeValue 中，保持类型信息和源码位置
 * 2. 函数调用时进行运行时类型检查
 * 3. 错误时抛出包含源码信息的自定义错误
 * 4. ADT 通过类继承实现，保持结构化信息
 * 5. 模式匹配编译为 switch-case 或 if-else 链
 */

// ===== 具体转译示例 =====

// 原代码: let fruits2: [String] = ["Apple", "Banana", "Pear"]
const fruits2 = (() => {
  const elements = [
    createValue("Apple", RT_String, {
      line: 13,
      column: 30,
      file: "main.fp",
      sourceExpression: '"Apple"',
    }),
    createValue("Banana", RT_String, {
      line: 13,
      column: 39,
      file: "main.fp",
      sourceExpression: '"Banana"',
    }),
    createValue("Pear", RT_String, {
      line: 13,
      column: 49,
      file: "main.fp",
      sourceExpression: '"Pear"',
    }),
  ];

  return createArrayLiteral(elements, RT_String, {
    line: 13,
    column: 28,
    file: "main.fp",
    originalName: "fruits2",
  });
})();

// 原代码: let bar2 = { let x = 20; let y = 30; x + y ^ 2 * 2 / 5 - 5 % 1 }
const bar2 = (() => {
  return withLetBinding(
    {
      x: createValue(20, RT_Int, {
        line: 45,
        column: 13,
        file: "main.fp",
        originalName: "x",
      }),
      y: createValue(30, RT_Int, {
        line: 46,
        column: 13,
        file: "main.fp",
        originalName: "y",
      }),
    },
    (env) => {
      // x + y ^ 2 * 2 / 5 - 5 % 1 的计算
      return wrapOperation(
        () => {
          const x = env.x;
          const y = env.y;

          // 每个操作都有类型检查和错误处理
          TypeChecker.assert(x, RT_Int);
          TypeChecker.assert(y, RT_Int);

          const result = x.value + (Math.pow(y.value, 2) * 2) / 5 - (5 % 1);
          return createValue(result, RT_Int, {
            line: 47,
            column: 3,
            file: "main.fp",
            sourceExpression: "x + y ^ 2 * 2 / 5 - 5 % 1",
          });
        },
        {
          line: 47,
          column: 3,
          file: "main.fp",
          sourceExpression: "x + y ^ 2 * 2 / 5 - 5 % 1",
        },
        "arithmetic expression"
      );
    }
  );
})();

// ===== 编译器生成的辅助代码 =====

// 类型声明的运行时表示
const RUNTIME_TYPES = {
  Bool2: RT_ADT("Bool2", { True: [], False: [] }),
  // 其他用户定义类型...
} as const;

// 构造器函数
const CONSTRUCTORS = {
  True: (sourceInfo: SourceInfo) =>
    createValue(new Bool2("True", sourceInfo), RUNTIME_TYPES.Bool2, sourceInfo),

  False: (sourceInfo: SourceInfo) =>
    createValue(
      new Bool2("False", sourceInfo),
      RUNTIME_TYPES.Bool2,
      sourceInfo
    ),

  Cons: <T>(
    head: RuntimeValue<T>,
    tail: RuntimeValue<FPList<T>>,
    sourceInfo: SourceInfo
  ) =>
    createValue(
      FPList.cons(head, tail.value, sourceInfo),
      tail.type,
      sourceInfo
    ),

  Nil: <T>(elementType: RuntimeType, sourceInfo: SourceInfo) =>
    createValue(
      FPList.nil<T>(elementType, sourceInfo),
      RT_List(elementType),
      sourceInfo
    ),
} as const;

// ===== 依赖类型和一等类型的处理 =====

// Kind 系统的运行时表示
interface Kind {
  kind: "type" | "kind" | "higher_kind";
  params?: Kind[];
}

const KIND_TYPE: Kind = { kind: "type" };
const KIND_KIND: Kind = { kind: "kind" };

// 类型构造器的运行时表示
interface TypeConstructor {
  name: string;
  kind: Kind;
  arity: number;
  construct: (...args: RuntimeType[]) => RuntimeType;
}

// List 类型构造器
const LIST_CONSTRUCTOR: TypeConstructor = {
  name: "List",
  kind: { kind: "higher_kind", params: [KIND_TYPE, KIND_TYPE] },
  arity: 1,
  construct: (elementType: RuntimeType) => RT_List(elementType),
};

// 类型级函数的实现
function applyTypeConstructor(
  constructor: TypeConstructor,
  args: RuntimeType[],
  sourceInfo: SourceInfo
): RuntimeType {
  if (args.length !== constructor.arity) {
    throw new FPRuntimeError(
      `Type constructor ${constructor.name} expects ${constructor.arity} arguments, got ${args.length}`,
      sourceInfo
    );
  }

  return constructor.construct(...args);
}

// ===== 性能优化提示 =====

/**
 * 编译时优化策略：
 *
 * 1. 类型推导：尽可能在编译时推导类型，减少运行时检查
 * 2. 内联简单函数：对于简单的数学运算等，可以直接内联
 * 3. 去除死代码：未使用的类型检查代码可以移除
 * 4. 缓存类型信息：重复的类型可以预计算和缓存
 * 5. 懒加载：只有在需要时才创建完整的运行时类型信息
 */

// 编译时标记，指示是否需要运行时检查
interface CompileTimeInfo {
  typeChecked: boolean;
  inlined: boolean;
  optimized: boolean;
}

// 带优化信息的值创建
function createOptimizedValue<T>(
  value: T,
  type: RuntimeType,
  sourceInfo: SourceInfo,
  compileInfo: CompileTimeInfo
): RuntimeValue<T> {
  const result = createValue(value, type, sourceInfo);

  // 如果编译时已经类型检查，可以跳过运行时检查
  if (compileInfo.typeChecked) {
    (result as any).__typeChecked = true;
  }

  return result;
}
