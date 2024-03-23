// Chat, 2024.3.23.16.35.00.043
// Chat with other handles.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  - [🍁] Use the TextInput implementation from `world` for input.
  - [...] Prototype a scrollback output on the main screen.
#endregion */

let input, inputBtn, server;

// 🥾 Boot
function boot({ api, ui, send }) {

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

    if (type === "chat:message") {
      console.log("💬 Chat Received:", id, type, content);
      return;
    }
  });


  input = new ui.TextInput(
    api,
    "...",
    async (text) => {
      server.send(`chat:message`, text); // Send the chat message.
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

  inputBtn = new ui.Button(0, 0, 32, 32);
  send({ type: "keyboard:soft-lock" });
}

// 🎨 Paint
function paint({ wipe, screen, leaving }) {
  wipe("brown");

  inputBtn.paint((btn) => {
    ink("white", btn.down && btn.over ? 128 : 64).circle(
      btn.box.x,
      btn.box.y,
      btn.box.w / 2,
      true,
    );
  });

  if (input.canType && !leaving()) {
    input.paint(api, false, {
      x: 0,
      y: 18,
      width: screen.width,
      height: screen.height - 18,
    });
  }
}

// 🎪 Act
 function act({ event: e, hud, piece, send }) {
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

// 🧮 Sim
function sim() {
  input.sim(api); // 💬 Chat
}

// 🥁 Beat
// function beat() {
//   // Runs once per metronomic BPM.
// }

// 👋 Leave
// function leave() {
//  // Runs once before the piece is unloaded.
// }

export { boot, paint, act, sim, meta };

// 📚 Library
//   (Useful functions used throughout the piece)
