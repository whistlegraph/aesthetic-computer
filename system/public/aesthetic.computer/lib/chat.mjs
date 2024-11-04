// Chat, 24.11.02.00.03
// A client library for connecting to and relaying
// chat server messages. Used both in `disk` and `sotce-net`.

import { Socket } from "./socket.mjs";
import { logs } from "./logs.mjs";

export class Chat {
  system;
  $commonApi; // Set by `disk` after `$commonApi` is defined.
  #debug = false;

  constructor(debug, send) {
    this.system = {
      server: new Socket(debug, send),
      chatterCount: 0,
      messages: [],
      // receiver: // A custom receiver that can be defined in a piece.
      //              like `chat` to get the events.
      // disconnect: // A custom disconnection that triggers below.
      connecting: true,
    };
    this.#debug = debug;
  }

  // Connect to a chat instance.

  // Instance options are `system` for AC users and `sotce` for Sotce Net
  // as of 24.11.02.00.31
  connect(instanceName) {
    instanceName = "chat-" + instanceName;
    if (instanceName !== "chat-system" && instanceName !== "chat-sotce") {
      console.warn(
        "🪫 Chat connection aborted. Invalid instance name:",
        instanceName,
      );
      return;
    }

    let chatUrl;
    if (this.#debug) {
      if (location.hostname === "local.aesthetic.computer") {
        chatUrl = `${instanceName}.${location.hostname}`;
      } else {
        let port;
        if (instanceName === "chat-system") {
          port = 8083;
        } else if (instanceName === "chat-sotce") {
          port = 8084;
        }
        chatUrl = `${location.hostname}:${port}`;
      }
    } else {
      if (instanceName === "chat-system") {
        chatUrl = "chat-system.aesthetic.computer";
      } else if (instanceName === "chat-sotce") {
        chatUrl = "chat.sotce.net";
      }
    }

    this.system.server.connect(
      chatUrl, // host
      (id, type, content) => {
        // receive
        if (type === "connected") {
          this.system.connecting = false;
          this.system.chatterCount =
            content?.chatters || this.system.chatterCount;
          this.system.messages.length = 0;
          this.system.messages.push(...content.messages);
          if (logs.chat) {
            console.log(
              `💬 %c${content.message}`,
              `color: cyan; background: rgba(10, 20, 40);`,
            );
          }
        }

        if (type === "unauthorized") {
          if (logs.chat) console.log("🔴 Chat message unauthorized!", content);
          this.$commonApi?.notice("Unauthorized", ["red", "yellow"]);
        }

        if (type === "message") {
          const msg = JSON.parse(content);
          if (logs.chat) console.log("💬 Chat message received:", msg);

          this.system.messages.push(msg);
          content = msg; // Pass the transformed message.
        }

        // Auto parse handle updates.
        if (type === "handle:update" || type === "handle:strip") {
          const msg = JSON.parse(content);
          content = msg;

          if (type === "handle:strip" && msg.user === USER?.sub) {
            console.log("🩹 Your handle has been stripped!");
            this.$commonApi?.net.refresh();
          }
        }

        if (type === "left") {
          // console.log("️✌️ Goodbye:", id);
          this.system.chatterCount -= 1;
        }

        if (type === "joined") {
          // console.log("️👋 Hello:", id, type, content);
          this.system.chatterCount += 1;
        }

        this.system.receiver?.(id, type, content); // Run the piece receiver.
        // if (logs.chat) console.log("🌠 Message received:", id, type, content);
      },
      undefined, // reload
      "wss", // protocol
      undefined, // connectionCallback
      () => {
        // disconnectCallback
        if (logs.chat) console.log("💬🚫 Chat disconnected.");
        this.system.chatterCount = 0;
        this.system.connecting = true;
        this.system.disconnect?.();
      },
    );
  }
}
