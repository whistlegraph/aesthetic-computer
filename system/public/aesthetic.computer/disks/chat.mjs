// Chat, 2024.3.23.16.35.00.043
// Chat with other handles.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
  - [🟠] Words longer than the width should be character-wrapped in word wrap
       mode of `write`.
  - [] Add basic sounds.
  - [] Move connection so that updates appear in every piece?
  + Done
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

let input,
  inputBtn,
  // server,
  chat,
  token,
  chatterCount = 0;

let messages = [];

const lineHeight = 12; // Height of each line
const topMargin = 38; // Space from the top of the screen
const bottomMargin = 33;
const leftMargin = 6;

let scroll = 0;
let totalScrollHeight;
let chatHeight;

let connecting = true;

import { Socket } from "../lib/socket.mjs";

async function boot({
  api,
  ui,
  send,
  net: { socket },
  handle,
  debug,
  text,
  notice,
  authorize,
  user,
  screen,
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
        connecting = false;
        console.log("🔌 Connected:", content);
        chatterCount = content?.chatters || chatterCount;
        // console.log("💬 Messages so far:", content.messages);
        messages.push(...content.messages);
        ({ totalScrollHeight, chatHeight } = computeScrollbar(api));
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
        // notice("RECEIVED");

        // TODO: Compute this message's tb and fullMessage.
        msg.fullMessage = msg.handle + " " + msg.text;
        msg.tb = text.box(
          msg.fullMessage,
          { x: leftMargin, y: 0 },
          screen.width - leftMargin,
          1,
          true,
        );

        console.log("BOX:", msg.tb);

        totalScrollHeight += msg.tb.lines.length * lineHeight;

        messages.push(msg);
        return;
      }

      if (type === "left") {
        console.log("️✌️ Goodbye:", id);
        chatterCount -= 1;
        return;
      }

      if (type === "joined") {
        console.log("️👋 Hello:", id, type, content);
        chatterCount += 1;
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
      connecting = true;
    },
  );

  // 🧦 Socket Networking
  /*
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
  */

  // ✍️️️ Text Input
  input = new ui.TextInput(
    api,
    "...",
    async (text) => {
      const currentHandle = handle();

      if (!currentHandle) {
        notice("NO HANDLE", ["red", "yellow"]);
      } else {
        chat.send(`chat:message`, {
          text,
          // handle: currentHandle,
          token,
          sub: user.sub,
        }); // Send the chat message.
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
      hideGutter: false,
      closeOnEmptyEnter: true,
    },
  );

  inputBtn = new ui.Button(
    0,
    screen.height - bottomMargin + 2,
    screen.width,
    bottomMargin - 2,
  );
  send({ type: "keyboard:soft-lock" });
}

function paint({
  api,
  ink,
  wipe,
  screen,
  leaving,
  text,
  typeface,
  geo: { Box },
}) {
  wipe(100, 100, 145);
  if (connecting) ink("pink").write("Connecting...", { center: "xy" });

  // Messages
  // Start from the bottom of the screen
  if (!connecting) {
    let y = screen.height - lineHeight - bottomMargin + scroll;

    // Iterate through the messages array backwards, calculating their
    // height and painting them if they are within the boundaries.
    for (let i = messages.length - 1; i >= 0; i--) {
      const message = messages[i];
      const x = leftMargin;

      // ❇️ These are precomputed in computeScrollbar for each message.
      const fullMessage = message.fullMessage;
      const tb = message.tb; //text.box(fullMessage, { x, y }, screen.width - x, 1, true);

      y -= lineHeight * (tb.lines.length - 1);

      if (y > screen.height - bottomMargin) {
        y -= lineHeight;
        continue;
      }

      ink("white").write(fullMessage, { x: x, y }, undefined, screen.width - x);
      ink("pink").write(message.handle, { x, y });

      y -= lineHeight; // Move up one line for the next message.
      if (y < topMargin - lineHeight) break; // Break if y is below top line.
    }

    // 📜 Scroll bar.

    ink("gray").box(0, topMargin, 3, chatHeight); // Backdrop.

    const segHeight = (chatHeight / totalScrollHeight) * chatHeight;
    ink("pink").box(
      0,
      chatHeight +
        topMargin -
        segHeight -
        (scroll / totalScrollHeight) * chatHeight,
      3,
      segHeight,
    ); // Backdrop.
  }

  // Interface

  inputBtn.box = new Box(
    0,
    screen.height - bottomMargin + 2,
    screen.width,
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

  ink(100, 100, 145).box(0, 0, screen.width, topMargin);
  ink(100, 100, 145).box(
    0,
    screen.height - bottomMargin + 3,
    screen.width,
    screen.height,
  );

  inputBtn.paint((btn) => {
    if (btn.down) {
      ink("white", btn.down && btn.over ? 128 : 64).box(btn.box);
    }
  });

  if (!connecting)
    ink(160).write("Chatters: " + chatterCount, {
      left: leftMargin,
      bottom: 10,
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

function act({ api, event: e, hud, piece, send, screen }) {
  if (e.is("reframed")) {
    const lastScrollHeight = totalScrollHeight;
    const lastScroll = scroll;
    // const lastChatHeight = chatHeight;
    ({ totalScrollHeight, chatHeight } = computeScrollbar(api));
    scroll = (lastScroll / lastScrollHeight) * totalScrollHeight;
    boundScroll();
    console.log("📜 Reframed scroll:", scroll);
  }

  if (!input.canType) {
    // me.act(api);

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

function boundScroll() {
  if (scroll < 0) scroll = 0;
  if (scroll > totalScrollHeight - chatHeight + 5) {
    scroll = totalScrollHeight - chatHeight + 5;
  }
}

function computeScrollbar({ text, screen }) {
  let height = 0;
  console.log("🤩 New width:", screen.width);
  // Iterate through the messages array.
  for (let i = 0; i < messages.length; i += 1) {
    const message = messages[i];
    const fullMessage = message.handle + " " + message.text;
    const tb = text.box(
      fullMessage,
      { x: leftMargin, y: 0 },
      screen.width - leftMargin,
      1,
      true,
    );
    message.tb = tb;
    message.fullMessage = fullMessage;
    // TODO: ^ These can be memoized per reframe.

    height += tb.lines.length * lineHeight;
  }

  console.log("📜 Computed scroll height:", height);
  console.log("💻 Screen height:", screen.height);

  const chatHeight = screen.height - bottomMargin - topMargin + 2;
  return { totalScrollHeight: height, chatHeight };
}
