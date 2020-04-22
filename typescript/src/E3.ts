// yarn run build && node build/src/E3.js

type List<T> = Nil | Cons<T>;

type Nil = { tag: 'Nil' };
const Nil: List<never> = { tag: 'Nil' };
type Cons<T> = { tag: 'Cons', head: T, tail: List<T> };
const Cons = <T>(head: T, tail: List<T>): List<T> => ({ tag: 'Cons', head, tail });

const tail = <T>(l: List<T>): List<T> =>
  l.tag === 'Nil' ? Nil : l.tail;

const setHead = <T>(l: List<T>, h: T): List<T> =>
  l.tag === 'Nil' ? Nil : Cons(h, l.tail);

const drop = <T>(l: List<T>, n: number): List<T> =>
  l.tag === 'Nil' ? Nil : n == 0 ? l : drop(l.tail, n - 1);

const dropWhile = <T>(l: List<T>, f: (x: T) => boolean): List<T> =>
  l.tag === 'Nil' ? Nil : f(l.head) ? dropWhile(l.tail, f) : l;

const init = <T>(l: List<T>): List<T> =>
  l.tag === 'Nil' ? Nil : l.tail.tag === 'Nil' ? Nil : Cons(l.head, init(l.tail));
