// yarn run build && node build/src/E4_6_7.js

type Either<E, T> = Left<E> | Right<T>;

type Left<E> = { tag: 'Left', value: E };
const Left = <E, T>(value: E): Either<E, T> => ({ tag: 'Left', value });
type Right<T> = { tag: 'Right', value: T };
const Right = <E, T>(value: T): Either<E, T> => ({ tag: 'Right', value });

export const map = <E, A, B>(f: (x: A) => B, e: Either<E, A>): Either<E, B> =>
  e.tag === 'Right' ? Right(f(e.value)) : e;

const flatMap = <E, A, B>(f: (x: A) => Either<E, B>, e: Either<E, A>): Either<E, B> =>
  e.tag === 'Right' ? f(e.value) : e;

const orElse = <E, T>(a: Either<E, T>, b: Either<E, T>): Either<E, T> =>
  a.tag === 'Right' ? a : b;

const impossible = () => { throw new Error('impossible') };

const map2 = <E, A, B, C>(f: (x: A, y: B) => C, a: Either<E, A>, b: Either<E, B>): Either<E, C> =>
  a.tag === 'Right' && b.tag === 'Right' ? Right(f(a.value, b.value)) :
  a.tag === 'Left' ? Left(a.value) :
  b.tag === 'Left' ? Left(b.value) :
  impossible();

const traverse = <E, A, B>(f: (x: A) => Either<E, B>, a: A[]): Either<E, B[]> =>
  a.length === 0 ? Right([]) :
    flatMap(hd => flatMap(tl => Right([hd].concat(tl)), traverse(f, a.slice(1))), f(a[0]));

const sequence = <E, T>(e: Either<E, T>[]): Either<E, T[]> =>
  traverse(x => x, e);
