import { Bridge } from '@geckos.io/common/lib/bridge.js';
import ParseMessage from '@geckos.io/common/lib/parseMessage.js';
import SendMessage from '@geckos.io/common/lib/sendMessage.js';
export default class ConnectionsManagerClient {
    constructor(url, authorization, label, rtcConfiguration // eslint-disable-line no-undef
    ) {
        this.url = url;
        this.authorization = authorization;
        this.label = label;
        this.rtcConfiguration = rtcConfiguration;
        this.bridge = new Bridge();
        this.onDataChannel = (ev) => {
            const { channel } = ev;
            if (channel.label !== this.label)
                return;
            this.dataChannel = channel;
            // set default binaryType to arraybuffer
            // https://github.com/node-webrtc/node-webrtc/issues/441
            this.dataChannel.binaryType = 'arraybuffer';
            this.dataChannel.onmessage = (ev) => {
                const { key, data } = ParseMessage(ev);
                this.bridge.emit(key, data);
            };
        };
    }
    emit(eventName, data = null) {
        SendMessage(this.dataChannel, this.maxMessageSize, eventName, data);
    }
    // fetch additional candidates
    async fetchAdditionalCandidates(host, id) {
        var _a;
        if (((_a = this.dataChannel) === null || _a === void 0 ? void 0 : _a.readyState) === 'closed')
            return;
        const res = await fetch(`${host}/connections/${id}/additional-candidates`, {
            method: 'GET',
            headers: {
                'Content-Type': 'application/json'
            }
        });
        if (res.ok) {
            const candidates = await res.json();
            // eslint-disable-next-line no-undef
            candidates.forEach((c) => {
                // eslint-disable-line no-undef
                this.localPeerConnection.addIceCandidate(c);
            });
        }
    }
    async connect() {
        const host = `${this.url}/.wrtc/v2`;
        let headers = { 'Content-Type': 'application/json' };
        if (this.authorization)
            headers = { ...headers, ['Authorization']: this.authorization };
        let userData = {};
        try {
            const res = await fetch(`${host}/connections`, {
                method: 'POST',
                headers
            });
            if (res.status >= 300) {
                throw {
                    name: 'Error',
                    message: `Connection failed with status code ${res.status}.`,
                    status: res.status,
                    statusText: res.statusText
                };
            }
            const json = await res.json();
            userData = json.userData;
            this.remotePeerConnection = json;
        }
        catch (error) {
            console.error(error.message);
            return { error };
        }
        const { id, localDescription } = this.remotePeerConnection;
        /**
         * testing
         */
        // console.log(localDescription.sdp?.split('\n'))
        // remove all host type candidates (for testing)
        // let removedHostCandidates: any[] = []
        // localDescription.sdp = localDescription.sdp
        //   ?.split('\n')
        //   .filter(line => {
        //     if (/typ host/.test(line)) {
        //       console.log('removing', line)
        //       removedHostCandidates.push(line.replace('a=', '').trim())
        //     }
        //     return !/typ host/.test(line)
        //   })
        //   .join('\n')
        // console.log(localDescription.sdp)
        // add all (host) candidates manually
        // setTimeout(() => {
        //   removedHostCandidates.forEach(candidate => {
        //     console.log('try to add candidate: ', candidate)
        //     this.localPeerConnection.addIceCandidate({ candidate, sdpMid: '0', sdpMLineIndex: 0 })
        //   })
        // }, 2000)
        // eslint-disable-next-line no-undef
        const configuration = {
            // @ts-ignore
            sdpSemantics: 'unified-plan',
            ...this.rtcConfiguration
        };
        // @ts-ignore
        const RTCPc = RTCPeerConnection || webkitRTCPeerConnection; // eslint-disable-line no-undef
        // create rtc peer connection
        this.localPeerConnection = new RTCPc(configuration);
        // get additional ice candidates
        // we do still continue to gather candidates even if the connection is established,
        // maybe we get a better connection.
        // So the server is still gathering candidates and we ask for them frequently.
        const showBackOffIntervals = (attempts = 10, initial = 50, factor = 1.8, jitter = 20) => Array(attempts)
            .fill(0)
            .map((_, index) => parseInt((initial * factor ** index).toString()) + parseInt((Math.random() * jitter).toString()));
        showBackOffIntervals().forEach(ms => {
            setTimeout(() => {
                this.fetchAdditionalCandidates(host, id).catch(() => { });
            }, ms);
        });
        try {
            await this.localPeerConnection.setRemoteDescription(localDescription);
            this.localPeerConnection.addEventListener('datachannel', this.onDataChannel, { once: true });
            const originalAnswer = await this.localPeerConnection.createAnswer();
            const updatedAnswer = new RTCSessionDescription({
                type: 'answer',
                sdp: originalAnswer.sdp
            });
            await this.localPeerConnection.setLocalDescription(updatedAnswer);
            try {
                await fetch(`${host}/connections/${id}/remote-description`, {
                    method: 'POST',
                    body: JSON.stringify(this.localPeerConnection.localDescription),
                    headers: {
                        'Content-Type': 'application/json'
                    }
                });
            }
            catch (error) {
                console.error(error.message);
                return { error };
            }
            const waitForDataChannel = () => {
                return new Promise(resolve => {
                    this.localPeerConnection.addEventListener('datachannel', () => {
                        resolve();
                    }, { once: true });
                });
            };
            if (!this.dataChannel)
                await waitForDataChannel();
            return {
                userData,
                localPeerConnection: this.localPeerConnection,
                dataChannel: this.dataChannel,
                id: id
            };
        }
        catch (error) {
            console.error(error.message);
            this.localPeerConnection.close();
            return { error };
        }
    }
}
//# sourceMappingURL=connectionsManager.js.map