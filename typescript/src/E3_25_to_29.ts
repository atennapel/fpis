// yarn run build && node build/src/E3_25_to_29.js

type Tree<T> = Leaf<T> | Branch<T>;

type Leaf<T> = { tag: 'Leaf', value: T };
const Leaf = <T>(value: T): Tree<T> => ({ tag: 'Leaf', value });
type Branch<T> = { tag: 'Branch', left: Tree<T>, right: Tree<T> };
const Branch = <T>(left: Tree<T>, right: Tree<T>): Tree<T> => ({ tag: 'Branch', left, right });

const size = <T>(t: Tree<T>): number =>
  t.tag === 'Leaf' ? 1 : 1 + size(t.left) + size(t.right);

const maximum = (t: Tree<number>): number =>
  t.tag === 'Leaf' ? t.value : Math.max(maximum(t.left), maximum(t.right));

const depth = <T>(t: Tree<T>): number =>
  t.tag === 'Leaf' ? 0 : 1 + Math.max(depth(t.left), depth(t.right));

const map = <A, B>(t: Tree<A>, f: (x: A) => B): Tree<B> =>
  t.tag === 'Leaf' ? Leaf(f(t.value)) : Branch(map(t.left, f), map(t.right, f));

const fold = <A, B>(t: Tree<A>, i: (x: A) => B, f: (x: B, y: B) => B): B =>
  t.tag === 'Leaf' ? i(t.value) : f(fold(t.left, i, f), fold(t.right, i, f));

const size2 = <T>(t: Tree<T>): number =>
  fold(t, _ => 1, (a, b) => 1 + a + b);
const maximum2 = (t: Tree<number>): number =>
  fold(t, x => x, Math.max);
const depth2 = <T>(t: Tree<T>): number =>
  fold(t, _ => 0, (a, b) => 1 + Math.max(a, b));
const map2 = <A, B>(t: Tree<A>, f: (x: A) => B): Tree<B> =>
  fold(t, x => Leaf(f(x)), Branch);
