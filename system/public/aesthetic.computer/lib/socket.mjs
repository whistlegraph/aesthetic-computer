// Socket
// Manages clientside WebSocket connections.

/* #region 🏁 todo
+ Done
- [x] Queue any sent messages to be received when the connection opens?
#endregion */

export class Socket {
  id; // Will be filled in with the user identifier after the first message.
  #debug;
  #killSocket = false;
  #ws;
  #reconnectTime = 1000;
  #queue = [];

  constructor(debug) {
    this.#debug = debug;
  }

  // Connects a WebSocket object and takes a handler for messages.
  connect(host, receive, reload, protocol = "wss", connectCallback) {
    try {
      this.#ws = new WebSocket(`${protocol}://${host}`);
    } catch {
      console.warn("📡 Connection failed.");
      return;
    }

    const socket = this;
    const ws = this.#ws;

    // Send a message to the console after the first connection.
    ws.onopen = (e) => {
      /*if (this.#debug)*/ console.log("📡 Connected."); // Redundant log given an initial message from the server.
      socket.#queue.forEach((q) => socket.send(...q)); // Send any held messages.
      socket.#reconnectTime = 1000;
      connectCallback?.(); // Run any post-connection logic, like setting codeChannel for example.
    };

    // Respond to incoming messages and assume `e.data` is a JSON String.
    ws.onmessage = (e) => {
      const msg = JSON.parse(e.data);
      socket.#preReceive(msg, receive, reload);
    };

    // Recursively re-connect after every second upon close or failed connection.
    ws.onclose = (e) => {
      console.warn("📡 Disconnected...", e.currentTarget?.url);
      // Only reconnect if we are not killing the socket and not in development mode.
      if (socket.#killSocket === false) {
        console.log("📡 Reconnecting in:", socket.#reconnectTime, "ms");
        setTimeout(() => {
          socket.connect(host, receive, reload, protocol);
        }, socket.#reconnectTime);
        socket.#reconnectTime = Math.min(socket.#reconnectTime * 2, 32000);
      }
    };

    // Close on error.
    ws.onerror = (err) => {
      console.error("📡 Error:", err);
      ws.close();
    };
  }

  // Send a formatted message to the connected WebSocket server.
  // Passes silently on no connection.
  send(type, content) {
    if (this.#ws?.readyState === WebSocket.OPEN) {
      this.#ws.send(JSON.stringify({ type, content }));
    } else {
      this.#queue.push([type, content]);
    }
  }

  // Kills the socket permanently.
  kill() {
    this.#killSocket = true;
    this.#ws?.close();
  }

  // Before passing messages to disk code, handle some system messages here.
  // Note: "reload" should only be defined when in development / debug mode.
  #preReceive({ id, type, content }, receive, reload) {
    if (type === "message") {
      // 🔴 TODO: Catch this JSON.parse error.
      const c = JSON.parse(content);
      if (c.text) {
        console.log(`📡 ${c.text}`); // Someone else has connected as...
      } else {
        // Send a self-connection message here. (You are connected as...)
        console.log(`📡 ${c.ip} → 🤹 ${c.playerCount} : @${c.id}`);
        this.id = c.id; // Set the user identifier.
      }
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
      console.log(
        `📡 ${content.id} has left. Connections open: ${content.count}`
      );
      receive?.(id, type, content);
    } else {
      receive?.(id, type, content); // Finally send the message to the client.
    }
  }
}
