// Chat, 2024.3.23.16.35.00.043
// Chat with other handles.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  - [🍁] Use the TextInput implementation from `world` for input.
  - [...] Prototype a scrollback output on the main screen.
#endregion */

let input,
  inputBtn,
  server,
  chat,
  token,
  chatterCount = 0;

let messages = [];

import { Socket } from "../lib/socket.mjs";

async function boot({
  api,
  ui,
  send,
  net: { socket },
  handle,
  debug,
  notice,
  authorize,
}) {
  // 🗨️ Chat Networking
  const chatUrl = debug ? "localhost:8083" : "chat-system.aesthetic.computer";

  try {
    token = await authorize(); // Get user token.
    console.log("🔐 Authorized token:", token);
  } catch (err) {
    console.error("🟥 Unauthorized.");
  }

  chat = new Socket(debug, send);
  chat.connect(
    chatUrl,
    (id, type, content) => {
      if (type === "connected") {
        console.log("🔌 Connected:", content);
        chatterCount = content?.chatters || chatterCount;
        return;
      }

      if (type === "unauthorized") {
        console.log("🔴 Chat message unauthorized!", content);
        notice("Unauthorized", ["red", "yellow"]);
        return;
      }

      if (type === "message") {
        const msg = JSON.parse(content);
        console.log("💬 Chat message received:", msg);
        notice("RECEIVED");
        messages.push(msg);
        return;
      }

      console.log("🌠 Message received:", id, type, content);
    },
    undefined,
    "wss",
    undefined,
    () => {
      console.log("🔌 Disconnected!");
      chatterCount = 0;
    },
  );

  // 🧦 Socket Networking
  server = socket((id, type, content) => {
    if (type === "left") {
      console.log("️✌️ Goodbye:", id);
      return;
    }

    if (type === "joined") {
      console.log("️👋 Hello:", id, type, content);
      return;
    }

    if (type.startsWith("connected")) {
      console.log("🪴 Welcome:", id);
      return;
    }
  });

  // ✍️️️ Text Input
  input = new ui.TextInput(
    api,
    "...",
    async (text) => {
      const currentHandle = handle();

      if (!currentHandle) {
        notice("NO HANDLE", ["red", "yellow"]);
      } else {
        chat.send(`chat:message`, { text, handle: currentHandle, token }); // Send the chat message.
        notice("SENT");
      }

      // Clear text, hide cursor block, and close keyboard.
      input.text = "";
      input.showBlink = false;
      input.mute = true;
      send({ type: "keyboard:close" });
    },
    {
      // autolock: false,
      // wrap,
      scheme: {
        text: 255,
        background: [0, 100],
        block: 255,
        highlight: 0,
        guideline: 255,
      },
      // copied,
      // activated,
      // didReset: () => {
      // messageComplete = true;
      // },
      // gutterMax,
      // lineSpacing,
      hideGutter: true,
      closeOnEmptyEnter: true,
    },
  );

  inputBtn = new ui.Button(64, 64, 32, 32);
  send({ type: "keyboard:soft-lock" });
}

function paint({ api, ink, wipe, screen, leaving, typeface }) {
  wipe("brown");

  // Messages
  messages.forEach((message, i) => {
    const x = 6;
    const y = 6 + (i + 1) * 12;
    ink("yellow").write(message.handle, { x, y });
    ink("white").write(message.text, {
      x: x + message.handle.length * typeface.blockWidth,
      y,
    });
  });

  // Interface
  inputBtn.paint((btn) => {
    ink("white", btn.down && btn.over ? 128 : 64).box(btn.box);
  });

  if (input.canType && !leaving()) {
    input.paint(api, false, {
      x: 0,
      y: 18,
      width: screen.width,
      height: screen.height - 18,
    });
  }

  ink("yellow").write("Chatters: " + chatterCount, { left: 6, bottom: 6 });
}

function act({ api, event: e, hud, piece, send }) {
  if (!input.canType) {
    // me.act(api);

    inputBtn.act(e, {
      down: () => {
        send({ type: "keyboard:soft-unlock" });
      },
      push: () => {
        send({ type: "keyboard:soft-lock" });
      },
      cancel: () => {
        send({ type: "keyboard:soft-lock" });
      },
      rollout: () => {
        send({ type: "keyboard:soft-lock" });
      },
      rollover: () => {
        if (inputBtn.down) send({ type: "keyboard:soft-unlock" });
      },
    });

    if (
      !input.canType &&
      e.is("keyboard:down:enter") // ||
      // e.is("keyboard:down:escape") ||
      // e.is("keyboard:down:`")
    ) {
      send({ type: "keyboard:open" });
    }

    if (e.is("keyboard:down:escape") || e.is("keyboard:down:`")) jump("prompt");

    // Backspace back to `prompt`.
    if (e.is("keyboard:down:backspace")) {
      jump(`prompt~${hud.currentLabel.text || piece}`)(() => {
        send({ type: "keyboard:open" });
      });
    }
  }

  if (
    input.canType &&
    (e.is("keyboard:down:`") ||
      e.is("keyboard:down:escape") ||
      (input.text.trim().length === 0 &&
        e.is("keyboard:down:enter") &&
        !e.shift))
  ) {
    send({ type: "keyboard:close" });
  }

  if (input.canType && e.is("lift") && !input.shifting && !input.paste.down) {
    send({ type: "keyboard:close" });
  }

  if (
    e.is("keyboard:open") ||
    e.is("keyboard:close") ||
    (input.canType && !e.is("keyboard:down:escape"))
  ) {
    input.act(api);
  }
}

function sim({ api }) {
  input.sim(api); // 💬 Chat
}

// function beat() {
//   // Runs once per metronomic BPM.
// }

function leave() {
  chat?.kill();
}

export { boot, paint, act, sim, leave };

// 📚 Library
//   (Useful functions used throughout the piece)
