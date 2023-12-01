import Geckos from "../dep/geckos.io-client.2.3.2.min.js";
import { logs } from "./logs.mjs";

/* #region 🏁 todo
  - [] Production ICE / TURN Servers.
  - [] Set up room system.
#endregion */

const RECONNECT_START_TIME = 500; // Gets doubled on subsequent attempts.
let channel, reconnectingTimeout;
let reconnectTime = RECONNECT_START_TIME;
// const debug = window.acDEBUG;

let reconnect;

function connect(port = 8889, url = undefined, send) {
  console.log("🩰 Connecting to UDP:", url, "on:", port);
  if (channel) {
    console.log("🩰 UDP Channel already exists:", channel);
    return;
  }

  channel = Geckos({ url, port }); // default port is 9208

  reconnect = () => {
    reconnectingTimeout = setTimeout(() => {
      connect(port, url, send);
    }, reconnectTime);
    reconnectTime *= 2;
  };

  channel.onConnect((error) => {
    clearTimeout(reconnectingTimeout);

    if (error) {
      console.error("🩰", error.message);
      channel = null;
      reconnect();
      return;
    }

    console.log("🩰 Connected to UDP:", channel.url);
    reconnectTime = RECONNECT_START_TIME;
    send({ type: "udp:connected" });

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
    console.log("🩰 Disconnected from UDP");
    if (error) {
      console.warn("🩰 Reconnecting:", error);
      channel = null;
      reconnect();
    }
  });
}

export const UDP = {
  connect,
  disconnect: () => {
    if (channel) channel?.close?.();
    channel = null;
  },
  send: ({ type, content }) => {
    if (logs.udp) console.log(`🩰 UDP Sent:`, { type, content });
    channel?.emit(type, JSON.stringify(content));
  },
};
