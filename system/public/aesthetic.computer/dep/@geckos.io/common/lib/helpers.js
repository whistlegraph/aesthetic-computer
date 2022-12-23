import { ArrayBufferView } from './types.js';
// const isRawMessage = (data: Data | RawMessage) => {
//   return typeof data === 'string' || isBufferMessage(data)
// }
// https://dev.to/nikosanif/create-promises-with-timeout-error-in-typescript-fmm
/** create a promise with a timeout */
export const promiseWithTimeout = (promise, ms, timeoutError = new Error('Promise timed out')) => {
    // create a promise that rejects in milliseconds
    const timeout = new Promise((_, reject) => {
        setTimeout(() => {
            reject(timeout);
        }, ms);
    });
    // returns a race between timeout and the passed promise
    return Promise.race([promise, timeout]);
};
/** make a small promise-based pause */
export const pause = (ms = 0) => {
    return new Promise(resolve => {
        setTimeout(() => {
            resolve();
        }, ms);
    });
};
/** creates a new Task using setTimeout() */
export const task = (task) => setTimeout(task, 0);
/** creates a new Microtask using Promise() */
export const tick = typeof Promise == 'function' ? Promise.prototype.then.bind(Promise.resolve()) : setTimeout;
const isStringMessage = (data) => {
    return typeof data === 'string';
};
const isBufferMessage = (data) => {
    return data instanceof ArrayBuffer || data instanceof ArrayBufferView;
};
const isJSONMessage = (data) => {
    try {
        // check if it is a string
        if (typeof data !== 'string')
            return false;
        // check if it is a number as a string
        if (!isNaN(parseInt(data)))
            return false;
        // check if it is a JSON object
        JSON.parse(data);
        return true;
    }
    catch (error) {
        return false;
    }
};
export { isStringMessage, isBufferMessage, isJSONMessage };
//# sourceMappingURL=helpers.js.map