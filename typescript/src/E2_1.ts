// yarn run build && node build/src/E2_1.js

// note: this is not tail recursive and will overflow the stack
const fib = (n: number): number => {
  const fibR = (n: number, a: number, b: number): number =>
    n <= 0 ? a : fibR(n - 1, b, a + b);
  return fibR(n, 0, 1);
};

console.log([0, 1, 2, 3, 4, 5].map(fib));
// console.log(fib(10000)); // stack overflow

// we can use a trampoline to avoid a stack overflow
type Rec<T> = Ret<T> | Delay<T>;

type Ret<T> = { tag: 'Ret', val: T };
const Ret = <T>(val: T): Rec<T> => ({ tag: 'Ret', val });
type Delay<T> = { tag: 'Delay', fn: () => Rec<T> };
const Delay = <T>(fn: () => Rec<T>): Rec<T> => ({ tag: 'Delay', fn });

const runRec = <T>(rec: Rec<T>): T => {
  let c: Rec<T> = rec;
  while (c.tag === 'Delay') c = c.fn();
  return c.val;
};

const fibT = (n: number): number => {
  const fibR = (n: number, a: number, b: number): Rec<number> =>
    n <= 0 ? Ret(a) : Delay(() => fibR(n - 1, b, a + b));
  return runRec(fibR(n, 0, 1));
};

console.log([0, 1, 2, 3, 4, 5].map(fibT));
console.log(fibT(10000)); // no stack overflow
