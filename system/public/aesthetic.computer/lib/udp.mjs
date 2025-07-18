import Geckos from "../dep/geckos.io-client.2.3.2.min.js";
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

  channel = Geckos({ url, port }); // default port is 9208

  reconnect = () => {
    reconnectingTimeout = setTimeout(() => {
      connect(port, url, send);
    }, reconnectTime);
    reconnectTime = Math.min(reconnectTime * 2, MAX_RECONNECT_TIME);
  };

  channel.onConnect((error) => {
    if (error) {
      console.log('%cUDP connection failed, retrying in ' + (reconnectTime / 1000) + 's...', 'color: orange; background: black; padding: 2px;');
      reconnect();
      return;
    }

    if (logs.udp) console.log("🩰 Connected to UDP:", channel.url);
    reconnectIn = DEFAULT_RECONNECT_IN;
    reconnectTime = reconnectIn;
    send({ type: "udp:connected" });
    connected = true;

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
    if (logs.udp) console.log(`🩰 UDP Sent:`, { type, content });
    if (connected) channel.emit(type, JSON.stringify(content));
  },
};
