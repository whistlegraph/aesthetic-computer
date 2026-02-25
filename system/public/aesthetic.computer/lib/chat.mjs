// Chat, 24.11.02.00.03
// A client library for connecting to and relaying
// chat server messages. Used both in `disk` and `sotce-net`.

import { Socket } from "./socket.mjs";
import { logs } from "./logs.mjs";
import { redact, unredact } from "./redact.mjs";
import { FUNDING_MODE } from "../disks/prompt.mjs";

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
      onlineHandles: [], // Realtime list of online user handles (all connected)
      hereHandles: [],   // Users actually viewing the chat piece right now
      messages: [],
      hearts: new Map(), // Map<messageId, { count, heartedByMe }>
      // receiver: // A custom receiver that can be defined in a piece.
      //              like `chat` to get the events.
      disconnect, // A custom disconnection that triggers below.
      connecting: true, // Start in connecting state, set to false when connected
    };
    // sendHeart is on system so pieces can call client.sendHeart(id, token)
    this.system.sendHeart = (id, token) => {
      if (!id || !token) return;
      const existing = this.system.hearts.get(id) || { count: 0, heartedByMe: false };
      const heartedByMe = !existing.heartedByMe;
      const count = existing.count + (heartedByMe ? 1 : -1);
      this.system.hearts.set(id, { count: Math.max(0, count), heartedByMe });
      this.system.server.send("chat:heart", { for: id, token });
    };
    this.#debug = debug;
  }

  // Connect to a chat instance.

  // Instance options are `system` for AC users and `sotce` for Sotce Net
  // as of 24.11.02.00.31
  connect(instanceName) {
    // 💸 FUNDING MODE: Never connect to chat servers, just show ransom message
    if (FUNDING_MODE) {
      console.log("💬 Chat connection skipped - FUNDING_MODE active");
      return;
    }
    
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
    console.log("🗨️ Chat connect:", instanceName, "→", chatUrl, "debug:", this.#debug);

    this.system.server.connect(
      chatUrl, // host
      (id, type, content) => {
        const extra = {};

        // receive
        if (type === "connected") {
          this.system.connecting = false;
          this.system.chatterCount =
            content?.chatters || this.system.chatterCount;
          this.system.onlineHandles = content?.handles || [];
          this.system.messages.length = 0;
          this.system.messages.push(...content.messages);
          // Seed heart counts from initial payload
          this.system.hearts.clear();
          const hc = content?.heartCounts || {};
          for (const [id, count] of Object.entries(hc)) {
            this.system.hearts.set(id, { count, heartedByMe: false });
          }
          // Also seed counts from message objects (loaded via REST)
          for (const msg of content.messages) {
            if (msg.id && msg.hearts > 0 && !this.system.hearts.has(msg.id)) {
              this.system.hearts.set(msg.id, { count: msg.hearts, heartedByMe: false });
            }
          }
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
          // console.log(this.system.messages);
          // Only keep the last 500 messages in this array.
          if (this.system.messages.length > 500) this.system.messages.shift();
          content = msg; // Pass the transformed message.
          extra.layoutChanged = true;
        }

        if (type === "message:update") {
          const updateData = JSON.parse(content);
          if (logs.chat) console.log("💬 Chat message updated:", updateData);
          // Update the count on the existing message
          if (this.system.messages[updateData.index]) {
            this.system.messages[updateData.index].count = updateData.count;
            extra.layoutChanged = true;
          }
        }

        if (type === "message:delete") {
          const deleteData = JSON.parse(content);
          if (logs.chat) console.log("💬 Chat message deleted:", deleteData);
          const msg = this.system.messages.find((m) => m.id === deleteData.id);
          if (msg) {
            msg.text = "[deleted]";
            msg.deleted = true;
            delete msg.redactedText;
            extra.layoutChanged = true;
          }
        }

        if (type === "message:hearts") {
          const heartsData = JSON.parse(content);
          if (logs.chat) console.log("💬 Hearts update:", heartsData);
          const existing = this.system.hearts.get(heartsData.for) || { heartedByMe: false };
          this.system.hearts.set(heartsData.for, {
            count: heartsData.count,
            heartedByMe: existing.heartedByMe,
          });
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

        // Handle color updates from other users.
        if (type === "handle:colors") {
          const msg = JSON.parse(content);
          console.log("🎨 Handle colors update received:", msg.handle);
          const cleanHandle = msg.handle.startsWith("@") ? msg.handle.slice(1) : msg.handle;
          handleColorsCache.set(cleanHandle, msg.colors);
          // Re-colorize all messages from this handle.
          this.system.messages.forEach((message) => {
            const msgHandle = message.from?.startsWith("@") ? message.from.slice(1) : message.from;
            if (msgHandle?.toLowerCase() === cleanHandle.toLowerCase()) {
              extra.layoutChanged = true;
            }
          });
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
          if (content.handles) this.system.onlineHandles = content.handles;
        }

        if (type === "joined") {
          if (logs.chat) console.log("️👋 Hello:", id, type, content);
          this.system.chatterCount = content.chatters;
          if (content.handles) this.system.onlineHandles = content.handles;
        }

        if (type === "online-handles") {
          if (logs.chat) console.log("👥 Online handles:", content.handles);
          this.system.onlineHandles = content.handles || [];
        }

        // New presence message with both online and here handles
        if (type === "presence") {
          if (logs.chat) console.log("👥 Presence update - online:", content.online, "here:", content.here);
          this.system.onlineHandles = content.online || content.handles || [];
          this.system.hereHandles = content.here || [];
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
        this.system.onlineHandles = [];
        this.system.connecting = true;
        this.system.disconnect?.();
      },
    );
  }
}
