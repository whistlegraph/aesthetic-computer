import { Events } from '@yandeu/events';
export class Bridge {
    constructor() {
        this.eventEmitter = new Events();
    }
    emit(eventName, data, connection = {}) {
        this.eventEmitter.emit(eventName, data, connection);
    }
    on(eventName, cb) {
        return this.eventEmitter.on(eventName, (data, options) => {
            cb(data, options);
        });
    }
    removeAllListeners() {
        this.eventEmitter.removeAllListeners();
    }
}
const bridge = new Bridge();
export default bridge;
//# sourceMappingURL=bridge.js.map