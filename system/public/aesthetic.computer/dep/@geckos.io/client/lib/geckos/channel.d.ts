import * as Types from '@geckos.io/common/lib/types.js';
export declare class ClientChannel {
    maxMessageSize: number | undefined;
    userData: {};
    private bridge;
    private connectionsManager;
    private peerConnection;
    private receivedReliableMessages;
    private url;
    constructor(url: string, authorization: string | undefined, port: number | null, label: string, rtcConfiguration: RTCConfiguration);
    private onconnectionstatechange;
    /** Get the channel's id. */
    get id(): Types.ChannelId;
    /** Close the WebRTC connection */
    close(): void;
    /** Emit a message to the server. */
    emit(eventName: Types.EventName, data?: Types.Data | null, options?: Types.EmitOptions): void;
    /** Emit a raw message to the server */
    get raw(): {
        /**
         * Emit a raw message.
         * @param rawMessage The raw message. Can be of type 'USVString | ArrayBuffer | ArrayBufferView'
         */
        emit: (rawMessage: Types.RawMessage) => void;
    };
    /**
     * Listen for a raw message from the server.
     * @param callback The event callback.
     */
    onRaw(callback: Types.EventCallbackRawMessage): void;
    /**
     * Listen for the connect event.
     * @param callback The event callback.
     */
    onConnect(callback: Types.ConnectionEventCallbackClient): Promise<void>;
    /**
     * Listen for the disconnect event.
     * @param callback The event callback.
     */
    onDisconnect(callback: Types.ConnectionEventCallbackClient): void;
    /**
     * Listen for a message from the server.
     * @param eventName The event name.
     * @param callback The event callback.
     */
    on(eventName: Types.EventName, callback: Types.EventCallbackClient): void;
}
/**
 * The geckos.io client library.
 * @param options.iceServers An array of RTCIceServers. See https://developer.mozilla.org/en-US/docs/Web/API/RTCIceServer.
 * @param options.iceTransportPolicy RTCIceTransportPolicy enum defines string constants which can be used to limit the transport policies of the ICE candidates to be considered during the connection process.
 * @param options.label The label of the DataChannel. Default: 'geckos.io'.
 * @param options.port The port of the server. Default: 9208.
 * @param options.url The url of the server. Default: \`${location.protocol}//${location.hostname}\`.
 */
declare const geckosClient: (options?: Types.ClientOptions) => ClientChannel;
export default geckosClient;
//# sourceMappingURL=channel.d.ts.map