// yarn run build && node build/src/E2_2.js

const isSorted = <A>(as: A[], ordered: (a: A, b: A) => boolean): boolean => {
  const end = as.length - 1;
  const loop = (i: number): boolean =>
    i >= end ? true : ordered(as[i], as[i + 1]) && loop(i + 1);
  return loop(0);
};

console.log(isSorted([1, 2, 3, 4, 5], (a, b) => a <= b));
console.log(isSorted([1, 3, 2, 5, 4], (a, b) => a <= b));
