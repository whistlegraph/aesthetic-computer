import Geckos from "../dep/geckos.io-client.2.3.2.min.js";
import { logs } from "./logs.mjs";

/* #region 🏁 todo
  + Later
  - [] Use better production ICE / TURN Servers (once things are scalable).
  - [] Set up room system.
#endregion */

// 📓 geckos docs: https://github.com/geckosio/geckos.io

const RECONNECT_START_TIME = 500; // Gets doubled on subsequent attempts.
let channel, reconnectingTimeout;
let reconnectTime = RECONNECT_START_TIME;
// const debug = window.acDEBUG;

let connected = false;
let reconnect;
let dontreconnect = false;

function connect(port = 8889, url = undefined, send) {
  if (connected) {
    console.log("🩰 Connection already exists:", channel);
    return;
  }

  // console.log("🩰 Connecting to UDP:", url, "on:", port);

  dontreconnect = false;

  channel = Geckos({ url, port }); // default port is 9208

  reconnect = () => {
    reconnectingTimeout = setTimeout(() => {
      connect(port, url, send);
    }, reconnectTime);
    reconnectTime *= 2;
  };

  channel.onConnect((error) => {
    if (error) {
      console.error("🩰", error.message);
      reconnect();
      return;
    }

    // console.log("🩰 Connected:", channel.url);
    reconnectTime = RECONNECT_START_TIME;
    send({ type: "udp:connected" });
    connected = true;

    channel.on("fairy:point", (content) => {
      content = JSON.parse(content);
      if (logs.udp) console.log(`🩰 UDP Received:`, content);
      send({
        type: "udp:receive",
        content: { type: "fairy:point", content },
      });
    });
  });

  channel.onDisconnect((error) => {
    // console.log("🩰 Disconnected from UDP");
    // console.log("Don't reconnect:", dontreconnect);
    connected = false;
    send({ type: "udp:disconnected" });
    if (error || !dontreconnect) {
      console.warn("🩰 Reconnecting...");
      reconnect();
    }
  });
}

export const UDP = {
  connect,
  disconnect: () => {
    dontreconnect = true;
    if (connected) channel.close();
  },
  send: ({ type, content }) => {
    if (logs.udp) console.log(`🩰 UDP Sent:`, { type, content });
    if (connected) channel.emit(type, JSON.stringify(content));
  },
};
