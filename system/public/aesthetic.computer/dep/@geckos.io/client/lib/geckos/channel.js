import ConnectionsManagerClient from '../wrtc/connectionsManager.js';
import { EVENTS } from '@geckos.io/common/lib/constants.js';
import PeerConnection from '../wrtc/peerConnection.js';
import { makeReliable } from '@geckos.io/common/lib/reliableMessage.js';
export class ClientChannel {
    constructor(url, authorization, port, label, rtcConfiguration // eslint-disable-line no-undef
    ) {
        this.userData = {};
        // stores all reliable messages for about 15 seconds
        this.receivedReliableMessages = [];
        this.url = port ? `${url}:${port}` : url;
        this.connectionsManager = new ConnectionsManagerClient(this.url, authorization, label, rtcConfiguration);
        this.bridge = this.connectionsManager.bridge;
        // remove all event listeners on disconnect
        this.bridge.on(EVENTS.DISCONNECTED, () => this.bridge.removeAllListeners());
    }
    onconnectionstatechange() {
        const lpc = this.peerConnection.localPeerConnection;
        lpc.onconnectionstatechange = () => {
            if (lpc.connectionState === 'disconnected' || lpc.connectionState === 'closed')
                this.bridge.emit(EVENTS.DISCONNECTED);
        };
    }
    /** Get the channel's id. */
    get id() {
        return this.peerConnection.id;
    }
    /** Close the WebRTC connection */
    close() {
        this.peerConnection.localPeerConnection.close();
        // fire the DISCONNECTED event manually
        this.bridge.emit(EVENTS.DISCONNECTED);
        try {
            const host = `${this.url}/.wrtc/v2`;
            fetch(`${host}/connections/${this.id}/close`, {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/json'
                }
            });
        }
        catch (error) {
            console.error(error.message);
        }
    }
    /** Emit a message to the server. */
    emit(eventName, data = null, options) {
        if (options && options.reliable) {
            makeReliable(options, (id) => this.connectionsManager.emit(eventName, {
                MESSAGE: data,
                RELIABLE: 1,
                ID: id
            }));
        }
        else {
            this.connectionsManager.emit(eventName, data);
        }
    }
    /** Emit a raw message to the server */
    get raw() {
        return {
            /**
             * Emit a raw message.
             * @param rawMessage The raw message. Can be of type 'USVString | ArrayBuffer | ArrayBufferView'
             */
            emit: (rawMessage) => this.emit(EVENTS.RAW_MESSAGE, rawMessage)
        };
    }
    /**
     * Listen for a raw message from the server.
     * @param callback The event callback.
     */
    onRaw(callback) {
        this.bridge.on(EVENTS.RAW_MESSAGE, (rawMessage) => {
            const cb = (rawMessage) => callback(rawMessage);
            cb(rawMessage);
        });
    }
    /**
     * Listen for the connect event.
     * @param callback The event callback.
     */
    async onConnect(callback) {
        var _a;
        this.peerConnection = new PeerConnection();
        const response = await this.peerConnection.connect(this.connectionsManager);
        if (response.error)
            callback(response.error);
        else {
            // set the userData
            if (response.userData)
                this.userData = response.userData;
            // keep track of the maxMessageSize
            this.maxMessageSize = this.connectionsManager.maxMessageSize = (_a = this.peerConnection.localPeerConnection.sctp) === null || _a === void 0 ? void 0 : _a.maxMessageSize;
            // init onConnectionStateChange event
            this.onconnectionstatechange();
            // we are now ready
            callback();
        }
    }
    /**
     * Listen for the disconnect event.
     * @param callback The event callback.
     */
    onDisconnect(callback) {
        this.bridge.on(EVENTS.DISCONNECTED, callback);
    }
    /**
     * Listen for a message from the server.
     * @param eventName The event name.
     * @param callback The event callback.
     */
    on(eventName, callback) {
        this.bridge.on(eventName, (data) => {
            // check if message is reliable
            // and reject it if it has already been submitted
            const isReliableMessage = data && data.RELIABLE === 1 && data.ID !== 'undefined';
            const expireTime = 15000; // 15 seconds
            const deleteExpiredReliableMessages = () => {
                const currentTime = new Date().getTime();
                this.receivedReliableMessages.forEach((msg, index, object) => {
                    if (msg.expire <= currentTime) {
                        object.splice(index, 1);
                    }
                });
            };
            if (isReliableMessage) {
                deleteExpiredReliableMessages();
                if (this.receivedReliableMessages.filter(obj => obj.id === data.ID).length === 0) {
                    this.receivedReliableMessages.push({
                        id: data.ID,
                        timestamp: new Date(),
                        expire: new Date().getTime() + expireTime
                    });
                    callback(data.MESSAGE);
                }
                else {
                    // reject message
                }
            }
            else {
                callback(data);
            }
        });
    }
}
/**
 * The geckos.io client library.
 * @param options.iceServers An array of RTCIceServers. See https://developer.mozilla.org/en-US/docs/Web/API/RTCIceServer.
 * @param options.iceTransportPolicy RTCIceTransportPolicy enum defines string constants which can be used to limit the transport policies of the ICE candidates to be considered during the connection process.
 * @param options.label The label of the DataChannel. Default: 'geckos.io'.
 * @param options.port The port of the server. Default: 9208.
 * @param options.url The url of the server. Default: \`${location.protocol}//${location.hostname}\`.
 */
const geckosClient = (options = {}) => {
    const { authorization = undefined, iceServers = [], iceTransportPolicy = 'all', label = 'geckos.io', port = 9208, url = `${location.protocol}//${location.hostname}` } = options;
    return new ClientChannel(url, authorization, port, label, { iceServers, iceTransportPolicy });
};
export default geckosClient;
//# sourceMappingURL=channel.js.map