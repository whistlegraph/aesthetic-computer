// Chat, 24.11.02.00.03
// A client library for connecting to and relaying
// chat server messages. Used both in `disk` and `sotce-net`.

import { Socket } from "./socket.mjs";
import { logs } from "./logs.mjs";
import { redact, unredact } from "./redact.mjs";

/* #region 🏁 TODO
  + Done
  - [x] Fix chatter count number. 
#endregion */

const validInstances = ["chat-system", "chat-sotce", "chat-clock"];

export class Chat {
  system;
  $commonApi; // Set by `disk` after `$commonApi` is defined.
  #debug = false;

  constructor(debug, send, disconnect) {
    this.system = {
      server: new Socket(debug, send),
      chatterCount: 0,
      messages: [],
      // receiver: // A custom receiver that can be defined in a piece.
      //              like `chat` to get the events.
      disconnect, // A custom disconnection that triggers below.
      connecting: true,
    };
    this.#debug = debug;
  }

  // Connect to a chat instance.

  // Instance options are `system` for AC users and `sotce` for Sotce Net
  // as of 24.11.02.00.31
  connect(instanceName) {
    instanceName = "chat-" + instanceName;
    if (validInstances.indexOf(instanceName) === -1) {
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
        } else if (instanceName === "chat-clock") {
          port = 8085;
        }
        chatUrl = `${location.hostname}:${port}`;
      }
    } else {
      if (instanceName === "chat-system") {
        chatUrl = "chat-system.aesthetic.computer";
      } else if (instanceName === "chat-sotce") {
        chatUrl = "chat.sotce.net";
      } else if (instanceName === "chat-clock") {
        chatUrl = "chat-clock.aesthetic.computer";
      }
    }
    console.log("🗨️ Chat url:", chatUrl);

    this.system.server.connect(
      chatUrl, // host
      (id, type, content) => {
        const extra = {};

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
          console.log("MESSSSSSSSSSSSSSSSSSAGE!");
          const msg = JSON.parse(content);
          if (logs.chat) console.log("💬 Chat message received:", msg);
          this.system.messages.push(msg);
          console.log(this.system.messages);
          // Only keep the last 100 messages in this array.
          if (this.system.messages.length > 100) this.system.messages.shift();
          content = msg; // Pass the transformed message.
          extra.layoutChanged = true;
        }

        // Auto parse handle updates.
        if (type === "handle:update" || type === "handle:strip") {
          const msg = JSON.parse(content);
          content = msg;

          console.log("👱️‍ `handle` edit received:", type, content);

          this.system.messages.forEach((message) => {
            if (message.sub === content.user) {
              message.from = content.handle;
              extra.layoutChanged = true;
            }
          });

          // console.log("👱 `handle` edit completed for:", content.handle);

          if (
            type === "handle:strip" &&
            msg.user === this.$commonApi?.user?.sub
          ) {
            console.log("🩹 Your handle has been stripped!");
            this.$commonApi?.notice("HANDLE STRIPPED", ["red", "yellow"]);
            setTimeout(() => {
              this.$commonApi?.net.refresh();
            }, 750);
          }
        }

        if (type === "chat-system:mute" || type === "chat-system:unmute") {
          const msg = JSON.parse(content);
          content = msg;
          if (this.$commonApi?.user?.sub === content.user) {
            if (type === "chat-system:mute")
              this.$commonApi.notice("MUTED", ["red", "yellow"]);
            if (type === "chat-system:unmute")
              this.$commonApi.notice("UNMUTED");
          }
          this.system.messages.forEach((message) => {
            if (message.sub === content.user) {
              if (type === "chat-system:mute") redact(message); // Modify the text content of each message.
              if (type === "chat-system:unmute") unredact(message);
              extra.layoutChanged = true; // Re-paint them if necessary.
            }
          });
        }

        if (type === "left") {
          if (logs.chat) console.log("️✌️ Goodbye:", id, type, content);
          this.system.chatterCount = content.chatters;
        }

        if (type === "joined") {
          if (logs.chat) console.log("️👋 Hello:", id, type, content);
          this.system.chatterCount = content.chatters;
        }

        this.system.receiver?.(id, type, content, extra); // Run the piece receiver.
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
