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
  l.tag === 'Cons' && f(l.head) ? dropWhile(l.tail, f) : l;

const init = <T>(l: List<T>): List<T> =>
  l.tag === 'Nil' ? Nil : l.tail.tag === 'Nil' ? Nil : Cons(l.head, init(l.tail));

const foldRight = <A, B>(l: List<A>, x: B, f: (h: A, a: B) => B): B =>
  l.tag === 'Nil' ? x : f(l.head, foldRight(l.tail, x, f));

const length2 = <T>(l: List<T>): number => foldRight(l, 0, (_, b) => b + 1);

// use trampoline to make it tail recursive
const foldLeft = <A, B>(l: List<A>, x: B, f: (a: B, h: A) => B): B => {
  const loop = (l: List<A>, a: B): Rec<B> =>
    l.tag === 'Nil' ? Ret(a) : Delay(() => loop(l.tail, f(a, l.head)));
  return runRec(loop(l, x));
};

const sum = (l: List<number>): number => foldLeft(l, 0, (a, b) => a + b);
const product = (l: List<number>): number => foldLeft(l, 1, (a, b) => a * b);
const length3 = <T>(l: List<T>): number => foldLeft(l, 0, (a, _) => a + 1)

const reverse = <T>(l: List<T>): List<T> => foldLeft(l, Nil as List<T>, (a, h) => Cons(h, a));

const foldLeft2 = <A, B>(l: List<A>, x: B, f: (a: B, h: A) => B): B =>
  foldRight(l, (x: B) => x, (b, g) => x => g(f(x, b)))(x);
const foldRight2 = <A, B>(l: List<A>, x: B, f: (h: A, a: B) => B): B =>
  foldLeft(l, (x: B) => x, (g, b) => x => g(f(b, x)))(x);

const append = <T>(a: List<T>, b: List<T>): List<T> =>
  foldRight(a, b, Cons);

const flatten = <T>(xs: List<List<T>>): List<T> =>
  foldLeft(xs, Nil as List<T>, append);

const add1toEach = (l: List<number>): List<number> =>
  foldLeft(l, Nil as List<number>, (a, h) => Cons(h + 1, a));
const convertToString = (l: List<number>): List<string> =>
  foldLeft(l, Nil as List<string>, (a, h) => Cons(`${h}`, a));

const map = <A, B>(l: List<A>, f: (x: A) => B): List<B> =>
  foldLeft(l, Nil as List<B>, (a, h) => Cons(f(h), a));

const filter = <A>(l: List<A>, f: (x: A) => boolean): List<A> =>
  foldLeft(l, Nil as List<A>, (a, h) => f(h) ? Cons(h, a) : a);

const flatMap = <A, B>(l: List<A>, f: (x: A) => List<B>): List<B> =>
  flatten(map(l, f));

const filter2 = <A>(l: List<A>, f: (x: A) => boolean): List<A> =>
  flatMap(l, x => f(x) ? Cons(x, Nil) : Nil);

const zipAdd = (a: List<number>, b: List<number>): List<number> =>
  a.tag === 'Cons' && b.tag === 'Cons' ? Cons(a.head + b.head, zipAdd(a, b)) : Nil;

const zipWith = <A, B, C>(f: (x: A, y: B) => C, a: List<A>, b: List<B>): List<C> =>
  a.tag === 'Cons' && b.tag === 'Cons' ? Cons(f(a.head, b.head), zipWith(f, a, b)) : Nil;
