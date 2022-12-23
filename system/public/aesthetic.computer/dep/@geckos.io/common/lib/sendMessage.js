import { isBufferMessage, isStringMessage } from './helpers.js';
import { EVENTS } from './constants.js';
const SendMessage = (dataChannel, maxMessageSize, eventName, data = null) => {
    var _a;
    const send = (data, isBuffer) => {
        var _a;
        const bytes = (_a = data.byteLength) !== null && _a !== void 0 ? _a : data.length * 2; // (times 2 for characters that uses 2 bytes per char)
        if (typeof maxMessageSize === 'number' && bytes > maxMessageSize) {
            throw new Error(`maxMessageSize of ${maxMessageSize} exceeded`);
        }
        else {
            Promise.resolve()
                .then(() => {
                // server-side (send() does not exist on the server side)
                if (dataChannel.send)
                    dataChannel.send(data);
                else {
                    if (!isBuffer)
                        dataChannel.sendMessage(data);
                    else
                        dataChannel.sendMessageBinary(Buffer.from(data));
                }
            })
                .catch(error => {
                console.log('error', error);
            });
        }
    };
    if (!dataChannel)
        return;
    if (dataChannel.readyState === 'open' || ((_a = dataChannel.isOpen) === null || _a === void 0 ? void 0 : _a.call(dataChannel))) {
        try {
            if (eventName === EVENTS.RAW_MESSAGE && data !== null && (isStringMessage(data) || isBufferMessage(data))) {
                send(data, isBufferMessage(data));
            }
            else {
                send(JSON.stringify({ [eventName]: data }), false);
            }
        }
        catch (error) {
            console.error('Error in sendMessage.ts: ', error.message);
            return error;
        }
    }
};
export default SendMessage;
//# sourceMappingURL=sendMessage.js.map