// yarn run build && node build/src/E4_1_to_5.js

type Option<T> = None | Some<T>;

type None = { tag: 'None' };
const None: Option<never> = { tag: 'None' };
type Some<T> = { tag: 'Some', value: T };
const Some = <T>(value: T): Option<T> => ({ tag: 'Some', value });

// export or else typescript will complain
export const map = <A, B>(f: (x: A) => B, o: Option<A>): Option<B> =>
  o.tag === 'None' ? o : Some(f(o.value));

const getOrElse = <T>(x: T, o: Option<T>): T =>
  o.tag === 'None' ? x : o.value;

const orElse = <T>(a: Option<T>, b: Option<T>) =>
  a.tag === 'Some' ? a : b;

const flatMap = <A, B>(f: (x: A) => Option<B>, o: Option<A>): Option<B> =>
  o.tag === 'None' ? o : f(o.value);

const map2 = <A, B, C>(f: (x: A, y: B) => C, a: Option<A>, b: Option<B>): Option<C> =>
  a.tag === 'Some' && b.tag === 'Some' ? Some(f(a.value, b.value)) : None;

const filter = <T>(f: (x: T) => boolean, a: Option<T>): Option<T> =>
  a.tag === 'Some' && f(a.value) ? a : None;

const mean = (xs: number[]): Option<number> =>
  xs.length === 0 ? None : Some(xs.reduce((x, y) => x + y) / xs.length);

const variance = (xs: number[]): Option<number> =>
  flatMap(m => mean(xs.map(x => Math.pow(x - m, 2))), mean(xs));

const sequence = <T>(xs: Option<T>[]): Option<T[]> =>
  xs.length === 0 ? Some([]) : flatMap(y => flatMap(ys => Some([y].concat(ys)), sequence(xs.slice(1))), xs[0]);

const traverse = <A, B>(f: (x: A) => Option<B>, xs: A[]): Option<B[]> =>
  xs.length === 0 ? Some([]) : flatMap(y => flatMap(ys => Some([y].concat(ys)), traverse(f, xs.slice(1))), f(xs[0]));

const sequence2 = <T>(xs: Option<T>[]): Option<T[]> => traverse(x => x, xs);
