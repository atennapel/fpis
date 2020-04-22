// yarn run build && node build/src/E3.js

import { Rec, Ret, Delay, runRec } from './Trampoline';

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

const foldRight = <A, B>(l: List<A>, x: B, f: (h: A, a: B) => B): B =>
  l.tag === 'Nil' ? x : f(l.head, foldRight(l.tail, x, f));

const length2 = <T>(l: List<T>): number => foldRight(l, 0, (_, b) => b + 1);

// use trampoline to make it tail recursive
const foldLeft = <A, B>(l: List<A>, x: B, f: (a: B, h: A) => B) => {
  const loop = (l: List<A>, a: B): Rec<B> =>
    l.tag === 'Nil' ? Ret(a) : Delay(() => loop(l.tail, f(a, l.head)));
  return runRec(loop(l, x));
};

const sum = (l: List<number>): number => foldLeft(l, 0, (a, b) => a + b);
const product = (l: List<number>): number => foldLeft(l, 1, (a, b) => a * b);
const length3 = <T>(l: List<T>): number => foldLeft(l, 0, (a, _) => a + 1)
