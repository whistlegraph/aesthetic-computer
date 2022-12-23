import makeRandomId from './makeRandomId.js';
import runInterval from './runInterval.js';
const makeReliable = (options, cb) => {
    const { interval = 150, runs = 10 } = options;
    const id = makeRandomId(24);
    runInterval(interval, runs, () => {
        cb(id);
    });
};
export { makeReliable };
//# sourceMappingURL=reliableMessage.js.map