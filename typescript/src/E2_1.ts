// yarn run build && node build/src/E2_1.js

// note: this is not tail recursive and will overflow the stack
const fib = (n: number): number => {
  const fibR = (n: number, a: number, b: number): number =>
    n <= 0 ? a : fibR(n - 1, b, a + b);
  return fibR(n, 0, 1);
};

console.log([0, 1, 2, 3, 4, 5].map(fib));
// console.log(fib(10000)); // stack overflow

import { Rec, Ret, Delay, runRec } from './Trampoline';

const fibT = (n: number): number => {
  const fibR = (n: number, a: number, b: number): Rec<number> =>
    n <= 0 ? Ret(a) : Delay(() => fibR(n - 1, b, a + b));
  return runRec(fibR(n, 0, 1));
};

console.log([0, 1, 2, 3, 4, 5].map(fibT));
console.log(fibT(10000)); // no stack overflow
