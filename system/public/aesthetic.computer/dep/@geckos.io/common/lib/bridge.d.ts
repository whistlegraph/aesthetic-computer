import { ChannelId, Data, EventName, EventOptions, RoomId } from './types.js';
import { Events } from '@yandeu/events';
interface BridgeEventMap {
    [key: string]: (data?: Data, options?: EventOptions) => void;
}
export declare class Bridge {
    eventEmitter: Events<BridgeEventMap>;
    emit(eventName: EventName, data?: Data, connection?: {
        id?: ChannelId;
        roomId?: RoomId;
        senderId?: ChannelId;
    }): void;
    on(eventName: EventName, cb: Function): Events<any>;
    removeAllListeners(): void;
}
declare const bridge: Bridge;
export default bridge;
//# sourceMappingURL=bridge.d.ts.map