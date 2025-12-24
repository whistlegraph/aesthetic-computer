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

  if (logs.udp) console.log("🩰 Connecting to UDP:", url, "on:", port);

  dontreconnect = false;

  // Ensure port is a number
  const portNum = typeof port === 'string' ? parseInt(port, 10) : port;

  // Detect if we're in local dev (localhost or LAN IP)
  const isLocalDev = url?.includes('localhost') || url?.includes('192.168.') || url?.includes('127.0.0.1');
  
  // Get the hostname from URL for TURN server (use same host as session server)
  const turnHost = url ? new URL(url).hostname : 'localhost';
  
  try {
    if (logs.udp) console.log("🩰 Creating Geckos channel with:", { url, port: portNum, isLocalDev, turnHost });
    // For local dev, use local TURN server for relay (required in Docker/devcontainer)
    // TURN server relays traffic when direct P2P isn't possible
    // Use the same hostname as the session server for TURN
    const localIceServers = [
      { urls: `stun:${turnHost}:3478` },
      { 
        urls: `turn:${turnHost}:3478`,
        username: 'aesthetic',
        credential: 'computer123'
      },
    ];
    const prodIceServers = [
      { urls: 'stun:stun.l.google.com:19302' },
    ];
    channel = Geckos({ 
      url, 
      port: portNum,
      iceServers: isLocalDev ? localIceServers : prodIceServers,
    });
    if (logs.udp) console.log("🩰 Geckos channel created:", channel);
    if (logs.udp) console.log("🩰 Channel URL will be:", `${url}:${portNum}`);
  } catch (e) {
    console.error("🩰 Failed to create Geckos channel:", e);
    return;
  }

  // Add timeout to detect if onConnect never fires
  const connectTimeout = setTimeout(() => {
    if (logs.udp) console.warn("🩰 ⚠️ UDP connection timeout - onConnect never fired after 10s");
    if (logs.udp) console.warn("🩰 This usually means WebRTC ICE gathering failed or signaling endpoint issues");
  }, 10000);

  reconnect = () => {
    reconnectingTimeout = setTimeout(() => {
      connect(port, url, send);
    }, reconnectTime);
    reconnectTime = Math.min(reconnectTime * 2, MAX_RECONNECT_TIME);
  };

  channel.onConnect((error) => {
    clearTimeout(connectTimeout); // Clear the timeout since onConnect fired
    if (logs.udp) console.log("🩰 onConnect callback fired! error:", error);
    if (error) {
      if (logs.udp) console.log('%cUDP connection failed: ' + (error.message || error) + ', retrying in ' + (reconnectTime / 1000) + 's...', 'color: orange; background: black; padding: 2px;');
      reconnect();
      return;
    }

    if (logs.udp) console.log("🩰 UDP onConnect SUCCESS! Sending udp:connected to disk...");
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
      if (logs.udp) console.log('%cUDP connection failed, retrying in ' + (reconnectTime / 1000) + 's...', 'color: orange; background: black; padding: 2px;');
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
