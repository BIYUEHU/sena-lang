
/* Rank 1 type parameter */
function bar(f: <T>(a: T) => T): number {
    return f(1);
}

/* No type parameter */
function baz(f: (a: number) => number): number {
    return f(1);
}

/* Rank 2 type parameter */
function foo(f: <T, U>(a: T, b: U) => T): number {
    return f(1, "hello");
}