// Socket
// Manages clientside WebSocket connections.
// This includes `session` and `chat` servvices.

/* #region 🏁 todo
#endregion */

import { checkObjktMode } from "./objkt-mode.mjs";

const logs = { socket: false };
const { min } = Math;

export class Socket {
  id; // Will be filled in with the user identifier after the first message.
  connected = false;
  #sendToBIOS;
  #debug;
  #killSocket = false;
  #ws;
  #reconnectTime = 1000;
  #reconnectTimeout;
  #queue = [];

  constructor(debug, sendToBIOS) {
    this.#debug = debug;
    this.#sendToBIOS = sendToBIOS;
  }

  // Connects a WebSocket object and takes a handler for messages.
  connect(
    host,
    receive,
    reload,
    protocol = "wss",
    connectCallback,
    disconnectCallback,
  ) {
    // Don't connect sockets in OBJKT mode
    if (typeof window !== 'undefined' && window.acOBJKT_MODE) {
      if (this.#debug && logs.socket) console.log("🧦 Sockets disabled in OBJKT mode.");
      return;
    }

    if (this.connected) {
      console.warn("🧦 Already connected...");
      return;
    }

    if (this.#debug && logs.socket) console.log("🧦 Connecting...", host);

    try {
      this.#ws = new WebSocket(`${protocol}://${host}`);
    } catch {
      // Only log and retry if not in OBJKT mode
      if (!checkObjktMode()) {
        console.log('%cconnection failed, retrying in ' + (this.#reconnectTime / 1000) + 's...', 'color: orange; background: black; padding: 2px;');
      }
      return;
    }

    const socket = this;
    const ws = this.#ws;

    // Send a message to the console after the first connection.
    ws.onopen = (e) => {
      // if (this.#debug && logs.socket) console.log("🧦 Connected."); // Redundant log given an initial message from the server.
      socket.#queue.forEach((q) => socket.send(...q)); // Send any held messages.
      socket.connected = true;
      socket.#queue.length = 0; // Clear out the full queue.
      socket.#reconnectTime = 1; // 1ms reconnect on drop, doubled each time.
      connectCallback?.(); // Run any post-connection logic, like setting codeChannel for example.
    };

    // Respond to incoming messages and assume `e.data` is a JSON String.
    ws.onmessage = (e) => {
      const msg = JSON.parse(e.data);
      socket.#preReceive(msg, receive, reload, this.#sendToBIOS);
    };

    // Recursively re-connect after every second upon close or failed connection.
    ws.onclose = (e) => {
      if (logs.socket)
        console.warn("🧦 Disconnected...", e.currentTarget?.url);
      clearTimeout(this.pingTimeout);

      socket.connected = false;
      // Only reconnect if we are not killing the socket and not in development mode and not in OBJKT mode.
      if (socket.#killSocket === false && !checkObjktMode()) {
        console.log('%cconnection failed, retrying in ' + (socket.#reconnectTime / 1000) + 's...', 'color: orange; background: black; padding: 2px;');
        this.#reconnectTimeout = setTimeout(() => {
          socket.connect(host, receive, reload, protocol, connectCallback);
        }, socket.#reconnectTime);
        socket.#reconnectTime = min(socket.#reconnectTime, 16000);
        socket.#reconnectTime *= 2;
      }
      disconnectCallback?.();
    };

    // Close on error.
    ws.onerror = (err) => {
      if (checkObjktMode()) {
        // In OBJKT mode, just close without retrying
        ws.close();
      } else {
        console.log('%cconnection failed, retrying...', 'color: orange; background: black; padding: 2px;');
        ws.close();
      }
    };
  }

  // Send a formatted message to the connected WebSocket server.
  // Passes silently on no connection.
  send(type, content) {
    if (this.#ws?.readyState === WebSocket.OPEN) {
      // if (logs.socket) console.log("🧦 Sent:", type, content);
      this.#ws.send(JSON.stringify({ type, content }));
    } else {
      // if (logs.socket) console.log("⌛ Queued:", type, content);
      this.#queue.push([type, content]);
    }
  }

  // Kills the socket permanently.
  kill(reconnectIn) {
    if (!reconnectIn) {
      this.#killSocket = true;
      clearTimeout(this.#reconnectTimeout);
    } else {
      this.#reconnectTime = reconnectIn * 1000;
      if (logs.socket) {
        console.log("🧦 Reconnecting in:", this.#reconnectTime, "seconds.");
      }
    }
    this.#ws?.close();
  }

  // Before passing messages to disk code, handle some system messages here.
  // Note: "reload" should only be defined when in development / debug mode.
  #preReceive({ id, type, content }, receive, reload, sendToBIOS) {
    if (this.#killSocket) return;
    if (type === "connected") {
      const c = JSON.parse(content);
      this.id = id; // Set the user identifier.
      // Send a self-connection message here. (You are connected as...)
      if (logs.socket) {
        console.log(
          `🧦 You joined: ${c.ip} id: ${c.id} 🤹 Connections open: ${c.playerCount}`,
        );
      }
      receive?.(id, type, c);
    } else if (type === "joined") {
      const c = JSON.parse(content);
      if (logs.socket) console.log(`🧦 ${c.text || c}`); // Someone else has connected as...
      receive?.(id, type, c);
    } else if (type === "vscode-extension:reload") {
      // TODO: This is only for `session` and not relevant to all WebSocket
      //       connections like in `chat`.
      sendToBIOS?.({
        type: "post-to-parent",
        content: { type: "vscode-extension:reload" },
      });
    } else if (type === "reload" && reload && this.#debug) {
      // Only respond to global `reload` signals when in debug mode.
      let c;
      if (typeof content === "object") {
        c = content;
      } else {
        c = JSON.parse(content);
      }
      // this.kill(); // Don't immmediately kill the socket on reload.
      reload(c);
    } else if (type === "code") {
      const parsed = JSON.parse(content);

      if (id === "development") {
        reload?.({
          name: parsed.piece,
          source: parsed.source,
          codeChannel: parsed.codeChannel,
        });
      }
    } else if (type === "left") {
      const c = typeof content === "string" ? JSON.parse(content) : content;
      if (logs.socket)
        console.log(`🧦 ${id} has left. Connections open: ${content.count}`);
      receive?.(id, type, c);
    } else {
      try {
        receive?.(id, type, content); // Finally send the message to the client.
      } catch (err) {
        console.error("🧦 Socket message error:", err);
      }
    }
  }
}
