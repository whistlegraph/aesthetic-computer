/// <reference types="node" />
import { Data } from './types.js';
/** create a promise with a timeout */
export declare const promiseWithTimeout: <T>(promise: Promise<T>, ms: number, timeoutError?: Error) => Promise<T>;
/** make a small promise-based pause */
export declare const pause: (ms?: number) => Promise<void>;
/** creates a new Task using setTimeout() */
export declare const task: (task: () => void) => NodeJS.Timeout;
/** creates a new Microtask using Promise() */
export declare const tick: (<TResult1 = any, TResult2 = never>(onfulfilled?: ((value: any) => TResult1 | PromiseLike<TResult1>) | null | undefined, onrejected?: ((reason: any) => TResult2 | PromiseLike<TResult2>) | null | undefined) => Promise<TResult1 | TResult2>) | typeof setTimeout;
declare const isStringMessage: (data: any) => boolean;
declare const isBufferMessage: (data: any) => boolean;
declare const isJSONMessage: (data: Data) => boolean;
export { isStringMessage, isBufferMessage, isJSONMessage };
//# sourceMappingURL=helpers.d.ts.map