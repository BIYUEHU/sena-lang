type BuildTuple<
  T,
  N extends number,
  R extends unknown[] = []
> =
  // 当累积的 R 长度等于目标 N 时，终止递归，R 即为结果
  R['length'] extends N
    ? R
    // 否则在 R 前面再“押入”一个 T，继续递归
    : BuildTuple<T, N, [T, ...R]>;

    // FixedArray<T, N>：只读版本的长度 N 元组
type FixedArray<T, N extends number> = BuildTuple<T, N>;

// 1) 构造器：用剩余参数保证参数个数正好是 N
function createFixedArray<T, N extends number>(
  ...args: BuildTuple<T, N>
): FixedArray<T, N> {
  return args;
}

// 2) 索引安全的 head/tail
function head<T, N extends number>(arr: FixedArray<T, N>): T {
  return arr[0];
}

// tail：利用条件类型把元组“拆头”
type Tail<Arr extends any[]> = Arr extends [any, ...infer R] ? R : never;
function tail<T, N extends number>(
  arr: FixedArray<T, N>
): Readonly<Tail<BuildTuple<T, N>>> {
  const [, ...rest] = arr;
  return rest;
}
