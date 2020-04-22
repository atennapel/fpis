// we can use a trampoline to avoid a stack overflow
export type Rec<T> = Ret<T> | Delay<T>;

export type Ret<T> = { tag: 'Ret', val: T };
export const Ret = <T>(val: T): Rec<T> => ({ tag: 'Ret', val });
export type Delay<T> = { tag: 'Delay', fn: () => Rec<T> };
export const Delay = <T>(fn: () => Rec<T>): Rec<T> => ({ tag: 'Delay', fn });

export const runRec = <T>(rec: Rec<T>): T => {
  let c: Rec<T> = rec;
  while (c.tag === 'Delay') c = c.fn();
  return c.val;
};
