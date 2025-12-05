import Geckos from "../dep/geckos.io-client.3.0.2.min.js";
import { logs } from "./logs.mjs";

/* #region 🏁 todo
  + Later
  - [] Use better production ICE / TURN Servers (once things are scalable).
  - [] Set up room system.
#endregion */

// 📓 geckos docs: https://github.com/geckosio/geckos.io

const DEFAULT_RECONNECT_IN = 500; // Gets doubled on subsequent attempts.
const MAX_RECONNECT_TIME = 5000; // Maximum retry time of 5 seconds
let reconnectIn = DEFAULT_RECONNECT_IN; // Gets doubled on subsequent attempts.
let channel, reconnectingTimeout;
let reconnectTime = reconnectIn;
// const debug = window.acDEBUG;

let connected = false;
let reconnect;
let dontreconnect = false;

function connect(port = 8889, url = undefined, send) {
  if (connected) {
    if (logs.udp) console.log("🩰 Connection already exists:", channel);
    return;
  }

  console.log("🩰 Connecting to UDP:", url, "on:", port);

  dontreconnect = false;

  try {
    channel = Geckos({ url, port }); // default port is 9208
    console.log("🩰 Geckos channel created:", channel);
    console.log("🩰 Calling channel.onConnect...");
    
    // Debug: Log if onConnect doesn't fire within 5 seconds
    const connectTimeout = setTimeout(() => {
      console.error("🩰 ⚠️ onConnect callback never fired after 5 seconds!");
      console.log("🩰 Channel state:", channel);
      console.log("🩰 connectionsManager:", channel.connectionsManager);
      // Try to check the peerConnection state
      if (channel.peerConnection) {
        console.log("🩰 peerConnection:", channel.peerConnection);
        console.log("🩰 peerConnection.localPeerConnection:", channel.peerConnection?.localPeerConnection);
      }
    }, 5000);
    
    // Manually trigger the connection to see what happens
    console.log("🩰 Manually checking connectionsManager.connect()...");
    if (channel.connectionsManager && channel.connectionsManager.connect) {
      channel.connectionsManager.connect().then(result => {
        console.log("🩰 connectionsManager.connect() result:", result);
        if (result.error) {
          console.error("🩰 Connection error:", result.error);
        }
      }).catch(err => {
        console.error("🩰 connectionsManager.connect() threw:", err);
      });
    }
    
    // Store timeout so we can clear it
    channel._debugTimeout = connectTimeout;
  } catch (e) {
    console.error("🩰 Failed to create Geckos channel:", e);
    return;
  }

  reconnect = () => {
    reconnectingTimeout = setTimeout(() => {
      connect(port, url, send);
    }, reconnectTime);
    reconnectTime = Math.min(reconnectTime * 2, MAX_RECONNECT_TIME);
  };

  channel.onConnect((error) => {
    // Clear the debug timeout
    if (channel._debugTimeout) clearTimeout(channel._debugTimeout);
    
    console.log("🩰 onConnect callback fired! error:", error);
    if (error) {
      console.log('%cUDP connection failed: ' + (error.message || error) + ', retrying in ' + (reconnectTime / 1000) + 's...', 'color: orange; background: black; padding: 2px;');
      reconnect();
      return;
    }

    console.log("🩰 UDP onConnect SUCCESS! Sending udp:connected to disk...");
    if (logs.udp) console.log("🩰 Connected to UDP:", channel.url);
    reconnectIn = DEFAULT_RECONNECT_IN;
    reconnectTime = reconnectIn;
    send({ type: "udp:connected" });
    connected = true;
    
    // Send user/handle info if available
    if (window.acUSER || window.acHANDLE) {
      channel.emit("udp:identity", JSON.stringify({
        user: window.acUSER,
        handle: window.acHANDLE
      }));
    }

    function respond(name, content) {
      content = JSON.parse(content);
      if (logs.udp) console.log(`🩰 UDP Received:`, content);
      send({
        type: "udp:receive",
        content: { type: name, content },
      });
    }

    // 💎 TODO: Make these channel names programmable somehow? 24.12.08.04.12

    channel.on("tv", (content) => {
      respond("tv", content);
    });

    channel.on("fairy:point", (content) => {
      respond("fairy:point", content);
    });

    // 🎮 1v1 FPS game position updates
    channel.on("1v1:move", (content) => {
      respond("1v1:move", content);
    });
  });

  channel.onDisconnect((error) => {
    if (logs.udp) console.log("🩰 Disconnected from UDP");
    // console.log("Don't reconnect:", dontreconnect);
    connected = false;
    send({ type: "udp:disconnected" });
    if (error || !dontreconnect) {
      console.log('%cUDP connection failed, retrying in ' + (reconnectTime / 1000) + 's...', 'color: orange; background: black; padding: 2px;');
      reconnect();
    }
  });
}

export const UDP = {
  connect,
  disconnect: (connectIn) => {
    if (!connectIn) {
      dontreconnect = true;
    } else {
      reconnectIn = connectIn * 1000;
    }
    if (connected) channel.close();
  },
  send: ({ type, content }) => {
    if (connected) {
      if (logs.udp) console.log(`🩰 UDP Sent:`, { type, content });
      channel.emit(type, JSON.stringify(content));
    } else {
      if (logs.udp) console.log(`🩰 UDP NOT sent (not connected):`, { type, content });
    }
  },
};
