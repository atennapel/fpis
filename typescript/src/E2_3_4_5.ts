// yarn run build && node build/src/E2_3_4_5.js

const curry = <A, B, C>(f: (a: A, b: B) => C): ((a: A) => (b: B) => C) =>
  a => b => f(a, b);

const uncurry = <A, B, C>(f: (a: A) => (b: B) => C): ((a: A, b: B) => C) =>
  (a, b) => f(a)(b);

const compose = <A, B, C>(f: (b: B) => C, g: (a: A) => B): ((a: A) => C) =>
  x => f(g(x));
