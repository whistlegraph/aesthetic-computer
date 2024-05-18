// Chat, 2024.3.23.16.35.00.043
// Chat with other handles.

/* #region 📚 README
#endregion */

/* #region 🏁 TODO
  - [-] Search this file for `ChatToDisk` references and do them.
  - [] Add custom sound to iOS notifications.
  + Future
  + Done
  - [x] Add custom message received sound.
  - [x] Include this file as an interface module inside of disk, so it can 
       conditionally be run inside of any piece so chat can be everywhere!
  - [x] Play a sound when chats come in by others.
  - [x] Move connection so that updates appear in every piece.
  - [x] Words longer than the width should be character-wrapped in word wrap
         mode of `write`.
  - [x] Don't push messages to the database on local / have an option for that.
  - [x] Add scrolling.
    - [x] Only calculate the height of each existing message one time.
    - [x] Add a better reframing on scroll.
    - [x] Add colored overlays for hiding the fold.
    - [x] Prevent scrolling past top.
    - [x] Don't render lines that are under the bottomMargin.
    - [x] Can't scroll past newest message.
  - [x] Line breaks.
  - [x] Live chatter updates.
  - [x] Show better connectivity.
#endregion */

const { max, floor, ceil } = Math;

let input, inputBtn, handleBtn, token;

const lineHeight = 12,
  topMargin = 22,
  bottomMargin = 33,
  leftMargin = 6;

let messageSfx;

let scroll = 0,
  totalScrollHeight,
  chatHeight;

async function boot({
  api,
  ui,
  send,
  handle,
  text,
  notice,
  authorize,
  user,
  screen,
  chat,
  sound,
  net,
}) {
  // console.log("💬 Chat booting...");

  // TODO: Now make it so that you see the last chat message
  //       as a button under the piece name.

  // 🥅 Preload messageReceived sound.
  net
    .preload("chat_1")
    .then((sfx) => (messageSfx = sfx))
    .catch((err) => console.warn("Could not preload:", err)); // and key sounds.

  // 🗨️ Chat Networking

  // Get the user token for sending authorized messages.
  try {
    token = await authorize();
    // console.log("🔐 Authorized token:", token);
  } catch (err) {
    // console.error("🟥 Unauthorized.");
  }

  // 🟢 Connected...
  chat.initialized(() => {
    ({ totalScrollHeight, chatHeight } = computeScrollbar(api));
  });

  // 🤖 Runs on every message...
  chat.receiver = (id, type, content) => {
    if (type === "too-long") {
      notice("TOO LONG", ["red", "yellow"]);
      return;
    }

    if (type === "unauthorized") {
      notice("Unauthorized", ["red", "yellow"]);
      return;
    }

    if (type === "message") {
      const msg = content; // Pre-transformed and stored.
      msg.fullMessage = msg.from + " " + msg.text;
      msg.tb = text.box(
        msg.fullMessage,
        { x: leftMargin, y: 0 },
        screen.width - leftMargin,
        1,
        true,
      );
      totalScrollHeight += msg.tb.lines.length * lineHeight;
      sound.play(messageSfx);
      return;
    }

    console.log("🌠 Message received:", id, type, content);
  };

  // chat.disconnect = () => {}; // This is also part of the API.

  // ✍️️️ Text Input
  input = new ui.TextInput(
    api,
    "...", // This empty call-to-action prompt will not be shown.
    async (text) => {
      const currentHandle = handle();

      if (!currentHandle) {
        notice("NO HANDLE", ["red", "yellow"]);
      } else {
        // Send the chat message.
        chat.server.send(`chat:message`, { text, token, sub: user.sub });
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
        background: [0, 180],
        block: 255,
        highlight: 0,
        guideline: [255, 128],
      },
      // copied,
      // activated,
      // didReset: () => {
      // messageComplete = true;
      // },
      // gutterMax,
      // lineSpacing,
      hideGutter: false,
      closeOnEmptyEnter: true,
    },
  );

  inputBtn = new ui.Button(
    0,
    screen.height - bottomMargin + 2,
    screen.width / 2 - 1,
    bottomMargin - 2,
  );

  handleBtn = new ui.Button(
    screen.width / 2,
    screen.height - bottomMargin + 2,
    screen.width / 2 - 1,
    bottomMargin - 2,
  );
  send({ type: "keyboard:soft-lock" });
}

function paint(
  { api, ink, wipe, screen, leaving, chat, mask, unmask, geo: { Box }, handle },
  options,
) {
  if (!options?.embedded) wipe(100, 100, 145);

  const x = 42;
  const y = 10;
  const gap = 8;
  ink(50, 50, 100)
    .write("back to prompt", { x: x + gap, y: 6 })
    .line(x, y, x - gap, y) // Base of arrow.
    .line(x - gap, y, x - gap + 3, y - 3) // Top of arrow.
    .line(x - gap, y, x - gap + 3, y + 3); // Bottom of arrow.

  if (chat.connecting) ink("pink").write("Connecting...", { center: "xy" });

  // Messages
  // Start from the bottom of the screen
  if (!chat.connecting) {
    let y = screen.height - lineHeight - bottomMargin + scroll;

    // Iterate through the messages array backwards, calculating their
    // height and painting them if they are within the boundaries.

    mask({
      x: 0,
      y: topMargin,
      width: screen.width,
      height: screen.height - bottomMargin + 3,
    });

    for (let i = chat.messages.length - 1; i >= 0; i--) {
      const message = chat.messages[i];

      if (!message.tb) continue; // If `tb` is not defined then kill this. 👾

      const x = leftMargin;

      // ❇️ These are precomputed in computeScrollbar for each message.
      const fullMessage = message.fullMessage;
      const tb = message.tb;

      y -= lineHeight * (tb.lines.length - 1);

      if (y > screen.height - bottomMargin) {
        y -= lineHeight;
        continue;
      }

      ink("white").write(fullMessage, { x, y }, undefined, screen.width - x);
      ink(message.from === "system" ? "cyan" : "pink").write(message.from, {
        x,
        y,
      });

      y -= lineHeight; // Move up one line for the next message.
      if (y < topMargin - lineHeight) break; // Break if y is below top line.
    }

    unmask();

    // 📜 Scroll bar.

    ink("gray").box(0, topMargin + 1, 3, chatHeight - 1); // Backdrop.

    const segHeight = max(
      1,
      floor((chatHeight / totalScrollHeight) * chatHeight) - 1,
    );
    ink("pink").box(
      0,
      ceil(
        chatHeight +
          topMargin -
          segHeight -
          (scroll / totalScrollHeight) * chatHeight,
      ),
      3,
      segHeight,
    ); // Backdrop.
  }

  // Interface

  inputBtn.box = new Box(
    0,
    screen.height - bottomMargin + 2,
    screen.width / 2 - 1,
    bottomMargin - 2,
  );

  handleBtn.box = new Box(
    screen.width / 2,
    screen.height - bottomMargin + 2,
    screen.width / 2 - 1,
    bottomMargin - 2,
  );

  ink(90, 200, 150, 48)
    .line(0, topMargin, screen.width, topMargin)
    .line(
      0,
      screen.height - bottomMargin + 2,
      screen.width,
      screen.height - bottomMargin + 2,
    );

  inputBtn.paint((btn) => {
    if (btn.down) {
      ink("lime", btn.down && btn.over ? 128 : 64).box(btn.box);
    }
  });

  handleBtn.paint((btn) => {
    if (btn.down) {
      ink("yellow", btn.down && btn.over ? 128 : 64).box(btn.box);
    }
  });

  if (!chat.connecting) {
    ink(220).write("Enter message...", {
      left: leftMargin,
      bottom: 10,
    });

    if (!input.canType) {
      ink(160).write("Online: " + chat.chatterCount, {
        right: leftMargin,
        top: 6,
      });
    }
  }

  if (input.canType && !leaving()) {
    input.paint(api, false, {
      x: 0,
      y: topMargin,
      width: screen.width,
      height: screen.height / 2, // - 18
    });

    // Character limit.
    const len = 64;
    ink(input.text.length > len ? "red" : "gray").write(
      `${input.text.length}/${len}`,
      { right: 6, top: 6 },
    );
  }

  const currentHandle = handle();
  const msg = currentHandle || "no handle";
  ink(0).write(msg, { bottom: 10 - 1, right: 6 - 1 });
  ink(currentHandle ? "lime" : "red").write(msg, { bottom: 10, right: 6 });
}

function act({ api, event: e, hud, piece, send, handle, store, jump }) {
  if (e.is("viewport-height:changed")) {
    console.log("✨ New keyboard cutoff would be:", e.y, "?");
    // notice(e.y);
  }

  if (e.is("reframed")) {
    const lastScrollHeight = totalScrollHeight;
    const lastScroll = scroll;
    ({ totalScrollHeight, chatHeight } = computeScrollbar(api));
    scroll = (lastScroll / lastScrollHeight) * totalScrollHeight;
    boundScroll();
  }

  // TODO: ChatToDisk: The chat module will need some kind of "focus" in order
  //                   to enable scroll and such.. maybe.

  if (!input.canType) {
    // 📜 Scrolling
    if (e.is("draw")) {
      scroll += e.delta.y;
      boundScroll();
    }

    if (e.is("keyboard:down:arrowdown")) {
      scroll -= 10;
      boundScroll();
    }

    if (e.is("keyboard:down:arrowup")) {
      scroll += 10;
      boundScroll();
    }

    if (e.is("scroll")) {
      scroll -= e.y;
      boundScroll();
    }

    // 🖥️ Keyboard / Text Input
    inputBtn.act(e, {
      down: () => {
        send({ type: "keyboard:enabled" }); // In case corner was pressed,
        //                                     which can disable the keyboard.
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

    handleBtn.act(e, () => {
      const hand = handle();
      if (!hand) store["prompt:splash"] = true;
      jump(hand || "prompt");
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

// function leave() {
// chat?.kill();
// }

export { boot, paint, act, sim };

// 📚 Library
//   (Useful functions used throughout the piece)

function boundScroll() {
  if (scroll < 0) scroll = 0;
  if (scroll > totalScrollHeight - chatHeight + 5) {
    scroll = totalScrollHeight - chatHeight + 5;
  }
}

function computeScrollbar({ text, screen, chat }) {
  let height = 0;
  // Iterate through the messages array.
  for (let i = 0; i < chat.messages.length; i += 1) {
    const message = chat.messages[i];
    const fullMessage = message.from + " " + message.text;
    const tb = text.box(
      fullMessage,
      { x: leftMargin, y: 0 },
      screen.width - leftMargin,
      1,
      true,
    );
    message.tb = tb;
    message.fullMessage = fullMessage;
    height += tb.lines.length * lineHeight;
  }
  // console.log("Messages:", chat.messages.length, chat.messages);
  // console.log("📜 Computed scroll height:", height);
  // console.log("💻 Screen height:", screen.height);
  const chatHeight = screen.height - bottomMargin - topMargin + 2;
  return { totalScrollHeight: height, chatHeight };
}
