import { ChannelId } from '@geckos.io/common/lib/types.js';
import ConnectionsManagerClient from './connectionsManager.js';
export default class PeerConnection {
    dataChannel: RTCDataChannel;
    id: ChannelId;
    localPeerConnection: RTCPeerConnection;
    connect(connectionsManager: ConnectionsManagerClient): Promise<{
        error: any;
        userData?: undefined;
    } | {
        userData: {};
        error?: undefined;
    }>;
}
//# sourceMappingURL=peerConnection.d.ts.map