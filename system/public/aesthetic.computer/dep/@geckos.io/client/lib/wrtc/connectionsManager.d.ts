import { ChannelId, Data, EventName, RawMessage } from '@geckos.io/common/lib/types.js';
import { Bridge } from '@geckos.io/common/lib/bridge.js';
interface RTCRemotePeerConnection {
    id: ChannelId;
    localDescription: RTCSessionDescriptionInit;
}
export default class ConnectionsManagerClient {
    url: string;
    authorization: string | undefined;
    label: string;
    rtcConfiguration: RTCConfiguration;
    bridge: Bridge;
    dataChannel: RTCDataChannel;
    id: ChannelId;
    localPeerConnection: RTCPeerConnection;
    maxMessageSize: number | undefined;
    remotePeerConnection: RTCRemotePeerConnection;
    emit(eventName: EventName, data?: Data | RawMessage | null): void;
    constructor(url: string, authorization: string | undefined, label: string, rtcConfiguration: RTCConfiguration);
    onDataChannel: (ev: RTCDataChannelEvent) => void;
    fetchAdditionalCandidates(host: string, id: ChannelId): Promise<void>;
    connect(): Promise<{
        error: any;
        userData?: undefined;
        localPeerConnection?: undefined;
        dataChannel?: undefined;
        id?: undefined;
    } | {
        userData: {};
        localPeerConnection: RTCPeerConnection;
        dataChannel: RTCDataChannel;
        id: ChannelId;
        error?: undefined;
    }>;
}
export {};
//# sourceMappingURL=connectionsManager.d.ts.map