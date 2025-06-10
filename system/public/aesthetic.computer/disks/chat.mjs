// Chat, 2024.3.23.16.35.00.043
// Chat with other handles.

/* #region 📚 README
#endregion */

/* #region 🏁 TODO
  - [] Add custom sound to iOS notifications.
  + Future
  + Done
  - [x] Make a 'renderableMessages' object model with action support. 
  - [x] Search this file for `ChatToDisk` references and do them.
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
let messagesNeedLayout = true;
let tapState = null;

let rowHeight;
const lineGap = 1,
  topMargin = 22,
  bottomMargin = 33,
  leftMargin = 6;

let messageSfx;
let ellipsisTicker;

let scroll = 0,
  totalScrollHeight,
  chatHeight;

async function boot(
  {
    api,
    ui,
    send,
    handle,
    gizmo,
    notice,
    authorize,
    user,
    screen,
    chat,
    sound,
    net,
    store,
    typeface,
  },
  otherChat,
) {
  rowHeight = typeface.blockHeight + 1;

  const client = otherChat || chat;

  // console.log("💬 Chat booting...");
  scroll = store["chat:scroll"] || 0; // Memoize scroll.

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
  // chat.connected(() => {
  //   console.log("💬 Connected... computing layout!");
  // });

  // 🤖 Runs on every message...
  client.receiver = (id, type, content, extra) => {
    if (type === "connected") {
      messagesNeedLayout = true;
      return;
    }

    if (type === "muted") {
      notice("MUTED", ["red", "yellow"]);
      return;
    }

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
      sound.play(messageSfx);
      // delete store["chat:scroll"]; // Reset scroll on new message?
      // return;
    }

    if (extra?.layoutChanged) messagesNeedLayout = true;
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
        text = text.replace(/\s+$/, ""); // Trim trailing whitespace.
        // Send the chat message.
        client.server.send(`chat:message`, { text, token, sub: user.sub });
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

  ellipsisTicker = new gizmo.EllipsisTicker();

  send({ type: "keyboard:soft-lock" });
}

function paint(
  {
    api,
    ink,
    wipe,
    text,
    screen,
    leaving,
    chat,
    mask,
    unmask,
    geo: { Box },
    handle,
    help,
    typeface,
    pen,
    num,
    piece,
  },
  options,
) {
  const client = options?.otherChat || chat;
  if (!options?.embedded) wipe(100, 100, 145);

  // Interface
  {
    const x = 42;
    const y = 10;
    const gap = 8;

    if (screen.width > 200 && piece === "chat") {
      ink(50, 50, 100)
        .write("back to prompt", { x: x + gap, y: 6 })
        .line(x, y, x - gap, y) // Base of arrow.
        .line(x - gap, y, x - gap + 3, y - 3) // Top of arrow.
        .line(x - gap, y, x - gap + 3, y + 3); // Bottom of arrow.
    }
  }

  ink(90, 200, 150, 48)
    .line(0, topMargin, screen.width, topMargin)
    .line(
      0,
      screen.height - bottomMargin + 2,
      screen.width,
      screen.height - bottomMargin + 2,
    );

  if (client.connecting) {
    ink("pink").write("Connecting" + ellipsisTicker?.text(help.repeat), {
      center: "xy",
    });
    return;
  }

  // Messages
  // Start from the bottom of the screen
  // if (!client.connecting) {
  if (messagesNeedLayout) {
    totalScrollHeight = computeMessagesHeight(api, client);
    computeMessagesLayout(api, client);
    chatHeight = computeScrollbarHeight(api, client);
    messagesNeedLayout = false;
  }

  // Mask off the area of renderable messages.
  mask({
    x: 0,
    y: topMargin,
    width: screen.width,
    height: screen.height - bottomMargin + 3,
  });

  let lastAgo; // Keep track of last rendered timestamp.

  // Iterate through the messages array backwards, calculating their
  // height and painting them if they are within the boundaries.

  // console.log(client.messages.length);

  for (let i = client.messages.length - 1; i >= 0; i--) {
    const message = client.messages[i];

    if (!message.tb || !message.layout) {
      continue; // If `tb` is not defined then kill this. 👾
      // console.log("No message layout found for:", message);
    }

    const x = leftMargin;
    const tb = message.tb; // Precomputed in `computeScrollbar` for each message.

    const layout = message.layout;
    const y = layout.y;

    // 🪧 Paint the message and its contents.
    ink("white", layout.inBox ? 64 : 32).box(x, y, tb.box.width, tb.box.height);
    ink(layout.msgColor).write(
      layout.paintableMessage,
      { x, y },
      undefined,
      screen.width - x,
    );

    layout.handles.forEach((handle) => {
      ink(handle.over ? "yellow" : handle.color).write(
        handle.word,
        x + handle.x,
        handle.y,
      );
    });

    layout.prompts.forEach((prompt) => {
      // if (handle.over) handle.color = pen?.drawing ? "yellow" : [190, 180, 255];
      prompt.lines.forEach((line) => {
        ink(prompt.over ? "yellow" : "lime").write(
          line.text,
          x + line.x,
          line.y,
        );
      });
    });

    layout.urls.forEach((url) => {
      // if (handle.over) handle.color = pen?.drawing ? "yellow" : [190, 180, 255];
      url.lines.forEach((line) => {
        ink(url.over ? "yellow" : "orange").write(
          line.text,
          x + line.x,
          line.y,
        );
      });
    });

    const ago = timeAgo(message.when);
    let overTimestamp = false;

    if (ago !== lastAgo || layout.timestamp.over || layout.inBox) {
      ink(layout.timestamp.over ? "yellow" : layout.timestampColor).write(
        ago,
        x + layout.timestamp.x,
        layout.timestamp.y,
      );
    }

    lastAgo = ago;

    let nameColor = message.from === "log" ? "cyan" : "pink";

    // 🖋️ Check if the pen is inside the user of this message.
    /*
    if (pen && !input.canType) {
      if (
        pen.x > x &&
        pen.x < x + text.width(message.from) &&
        pen.y > y &&
        pen.y < y + rowHeight
      ) {
        nameColor = pen.drawing ? "yellow" : [200, 255, 200];
      }
    }
    */

    // Message byline.
    /*
    ink(nameColor).write(message.from, {
      x,
      y,
    });
    */

    /*
    y -= rowHeight; // Move up one line for the next message.
    if (y < topMargin - rowHeight) break; // Break if y is below top line.
    */
  }

  unmask();

  // 📜 Scroll bar.

  ink("gray").box(0, topMargin + 1, 3, chatHeight - 1); // Backdrop.

  const segHeight = max(
    1,
    floor((chatHeight / totalScrollHeight) * chatHeight) - 1,
  );

  const boxY =
    ceil(
      chatHeight +
        topMargin -
        segHeight -
        (scroll / totalScrollHeight) * chatHeight,
    ) || 0;

  ink("pink").box(0, boxY, 3, segHeight); // Backdrop.
  // }

  const currentHandle = handle();
  const msg = currentHandle || "nohandle";

  ink(0).write(msg, { bottom: 10 - 1, left: leftMargin - 1 });

  const color = currentHandle ? "lime" : "red";

  ink(handleBtn.down ? "white" : color).write(msg, {
    bottom: 10,
    left: leftMargin,
  });

  const charW = typeface.glyphs[0].resolution[0];
  const handleWidth = msg.length * charW;

  ink(inputBtn.down ? "yellow" : 220).write(
    "Enter message" + ellipsisTicker.text(help.repeat),
    {
      left: leftMargin + handleWidth + charW,
      bottom: 10,
    },
  );

  handleBtn.btn.box = new Box(
    0,
    screen.height - bottomMargin + 2,
    handleWidth + charW - 1,
    bottomMargin - 2,
  );

  inputBtn.btn.box = new Box(
    leftMargin + handleWidth,
    screen.height - bottomMargin + 2,
    screen.width - leftMargin - handleWidth,
    bottomMargin - 2,
  );

  if (!input.canType) {
    ink(160).write("Online: " + client.chatterCount, {
      right: leftMargin,
      top: 6,
    });
  }

  if (input.canType && !leaving()) {
    input.paint(api, false, {
      x: 0,
      y: topMargin,
      width: screen.width,
      height: screen.height / 2, // - 18
    });

    // Character limit.
    const len = 128;
    ink(input.text.length > len ? "red" : "gray").write(
      `${input.text.length}/${len}`,
      { right: 6, top: 6 },
    );
  }
}

function act(
  {
    api,
    chat,
    pen,
    event: e,
    hud,
    piece,
    send,
    handle,
    store,
    beep,
    text,
    jump,
  },
  otherChat,
) {
  const client = otherChat || chat;
  // if (e.is("viewport-height:changed")) {
  // console.log("✨ New keyboard cutoff would be:", e.y, "?");
  // notice(e.y);
  // }

  if (e.is("reframed")) {
    const lastScrollHeight = totalScrollHeight;
    const lastScroll = scroll;

    totalScrollHeight = computeMessagesHeight(api, client);
    computeMessagesLayout(api, client);
    chatHeight = computeScrollbarHeight(api);

    scroll = (lastScroll / lastScrollHeight) * totalScrollHeight;
    store["chat:scroll"] = scroll;
    boundScroll();
  }

  // TODO: ChatToDisk: The chat module will need some kind of "focus" in order
  //                   to enable scroll and such.. maybe.

  if (!input.canType) {
    // 👇 Message Picking

    if (e.is("lift")) {
      tapState = null;
    }

    // TODO: Scrolling should cancel out the tapping.

    // 👈 Tapping
    if (e.is("touch")) {
      // Detect if we are inside a message or not.
      for (let i = client.messages.length - 1; i >= 0; i--) {
        const message = client.messages[i];
        if (!message.tb || !message.layout) {
          continue; // If `tb` is not defined then kill this. 👾
          // console.log("No message layout found for:", message);
        }
        if (
          e.x > message.layout.x &&
          e.x < message.layout.x + message.layout.width &&
          e.y > message.layout.y &&
          e.y < message.layout.y + message.layout.height
        ) {
          message.layout.inBox = true;

          // 📆 `timestamp` hover and activate.
          //    (Will always only be on one line)
          // for (let h = 0; h < message.layout.timestamp.length; h += 1) {
          //  const handle = message.layout.handles[h];
          {
            const timestamp = message.layout.timestamp;
            const startX = message.layout.x + timestamp.x;
            if (
              e.x > startX &&
              e.x < startX + timestamp.width &&
              e.y > timestamp.y &&
              e.y < timestamp.y + timestamp.height
            ) {
              timestamp.over = true;
              break;
            }
          }
          // }

          // 👱 `handle` hover and activate.
          //    (Will always only be on one line)
          for (let h = 0; h < message.layout.handles.length; h += 1) {
            const handle = message.layout.handles[h];
            const startX = message.layout.x + handle.x;

            if (
              e.x > startX &&
              e.x < startX + handle.width &&
              e.y > handle.y &&
              e.y < handle.y + handle.height
            ) {
              handle.over = true;
              break;
            }
          }

          // 🖥️ `prompt` hover and activate.
          //     (With line break support)
          for (let p = 0; p < message.layout.prompts.length; p += 1) {
            const prompt = message.layout.prompts[p];
            for (let l = 0; l < prompt.lines.length; l += 1) {
              const line = prompt.lines[l];
              const startX = message.layout.x + line.x;

              if (
                e.x > startX &&
                e.x < startX + line.width &&
                e.y > line.y &&
                e.y < line.y + line.height
              ) {
                prompt.over = true;
                break;
              }
            }
          }

          // 🕸️ `url` hover and activate.
          //     (With line break support)
          for (let u = 0; u < message.layout.urls.length; u += 1) {
            const url = message.layout.urls[u];
            for (let l = 0; l < url.lines.length; l += 1) {
              const line = url.lines[l];
              const startX = message.layout.x + line.x;

              if (
                e.x > startX &&
                e.x < startX + line.width &&
                e.y > line.y &&
                e.y < line.y + line.height
              ) {
                url.over = true;
                break;
              }
            }
          }

          // TODO: ⏰ Add timestamp hover and activate per message.
          //            - With 'delete' action.
        }
      }
    }

    if (e.is("lift")) {
      for (let i = client.messages.length - 1; i >= 0; i--) {
        const message = client.messages[i];
        if (!message.tb || !message.layout) {
          continue; // If `tb` is not defined then kill this. 👾
        }
        // 🟠 TODO: Reset all the other potential tapped handles and prompts.
        message.layout.inBox = false;

        // Handles
        for (let h = 0; h < message.layout.handles.length; h += 1) {
          const handle = message.layout.handles[h];
          if (handle.over) {
            beep();
            hud.labelBack();
            jump(handle.word);
            handle.over = false;
            break;
          }
        }

        // Prompts
        for (let p = 0; p < message.layout.prompts.length; p += 1) {
          const prompt = message.layout.prompts[p];
          if (prompt.over) {
            beep();
            hud.labelBack();
            const innerPrompt = prompt.text.slice(1, -1); // Unquote prompt text.
            if (innerPrompt.startsWith(">")) {
              jump("prompt " + innerPrompt.slice(1));
            } else {
              jump(innerPrompt);
            }
            prompt.over = false;
            break;
          }
        }

        // URLs
        for (let u = 0; u < message.layout.urls.length; u += 1) {
          const url = message.layout.urls[u];
          if (url.over) {
            beep();
            jump("out:", url.text);
            console.log("🟨 Visiting...", url);
            url.over = false;
            break;
          }
        }

        // Timestamps (Moderation Menu)
        const timestamp = message.layout.timestamp;
        if (timestamp.over) {
          timestamp.over = false;
        }
      }
    }

    // 📜 Scrolling
    if (e.is("draw")) {
      scroll += e.delta.y;
      store["chat:scroll"] = scroll;
      boundScroll();
      // computeMessagesLayout(api);
      messagesNeedLayout = true;
    }

    if (e.is("keyboard:down:arrowdown")) {
      scroll -= 10;
      store["chat:scroll"] = scroll;
      boundScroll();
      // computeMessagesLayout(api);
      messagesNeedLayout = true;
    }

    if (e.is("keyboard:down:arrowup")) {
      scroll += 10;
      store["chat:scroll"] = scroll;
      boundScroll();
      // computeMessagesLayout(api);
      messagesNeedLayout = true;
    }

    if (e.is("scroll")) {
      scroll -= e.y;
      store["chat:scroll"] = scroll;
      boundScroll();
      // computeMessagesLayout(api);
      messagesNeedLayout = true;
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
      jump(`prompt~${hud.currentLabel().text || piece}`)(() => {
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
  ellipsisTicker?.sim();
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

function computeMessagesHeight({ text, screen }, chat) {
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
    height += tb.lines.length * rowHeight + lineGap;
  }
  return height;
}

// Build a display graph for the messages.
// (From the bottom to the top.)
function computeMessagesLayout({ screen, text }, chat) {
  let y = screen.height - rowHeight - bottomMargin + scroll;

  // Delete all layouts.
  for (let i = chat.messages.length - 1; i >= 0; i--) {
    const msg = chat.messages[i];
    msg.lastLayout = msg.layout;
    delete msg.layout;
  }

  for (let i = chat.messages.length - 1; i >= 0; i--) {
    const msg = chat.messages[i];

    y -= rowHeight * (msg.tb.lines.length - 1) + lineGap;

    if (y > screen.height - bottomMargin) {
      y -= rowHeight;
      continue;
    } // Outline a 🪧 renderable message and its geometry.
    let msgColor = "white",
      inBox = msg.lastLayout?.inBox || false; // A flag that determines if we are in the message box or not.

    const handles = []; // Parse '@handle' names.
    const prompts = []; // Parse `prompts`.
    const urls = []; // Parse any 'urls'.
    const totalHeight = msg.tb.lines.length * rowHeight;    const paintableMessage = msg.fullMessage;

    // Parse the original message to find complete URLs and prompts
    const parsedElements = parseMessageElements(paintableMessage);

    // Build word-level element map instead of character-level
    // This approach reconstructs which words belong to which elements
    const wordElementMap = new Map();
    
    // Split the original message into words to match how text.box breaks it
    const originalWords = paintableMessage.split(/(\s+)/);
    let charPos = 0;
    
    for (let wordIdx = 0; wordIdx < originalWords.length; wordIdx++) {
      const word = originalWords[wordIdx];
      const wordStart = charPos;
      const wordEnd = charPos + word.length;
      
      // Check if this word overlaps with any parsed elements
      for (const element of parsedElements) {
        if (wordStart < element.end && wordEnd > element.start) {
          wordElementMap.set(wordIdx, element);
          break;
        }
      }
      
      charPos += word.length;
    }

    // Track active elements for multi-line spanning
    const activeElements = new Map(); // elementText -> { element, container }

    // Track word position in flattened word array
    let flatWordIndex = 0;for (let rowIndex = 0; rowIndex < msg.tb.lines.length; rowIndex += 1) {
      let rowY = y + rowIndex * rowHeight;
      let cursorX = 0;      for (
        let wordIndex = 0;
        wordIndex < msg.tb.lines[rowIndex].length;
        wordIndex += 1
      ) {
        const word = msg.tb.lines[rowIndex][wordIndex];
        const wordWidth = text.width(word);

        // Check if this word belongs to any parsed element
        const element = wordElementMap.get(flatWordIndex);
        
        if (element) {
          // This word belongs to a URL or prompt
          let container = activeElements.get(element.text);

          if (!container) {
            // Create new container for this element
            container = {
              text: element.text,
              lines: [],
            };

            if (element.type === "url") {
              urls.push(container);
            } else if (element.type === "prompt") {
              prompts.push(container);
            }

            activeElements.set(element.text, container);
          }

          // Add this word to the container
          container.lines.push({
            text: word,
            x: cursorX,
            y: rowY,
            width: wordWidth,
            height: rowHeight,
          });
        }

        // Handle @mentions (always single words)
        if (word.startsWith("@")) {
          handles.push({
            word,
            color: [255, 230, 220],
            x: cursorX,
            y: rowY,
            width: wordWidth,
            height: rowHeight,
          });
        }

        flatWordIndex++;
        cursorX += wordWidth;        // Add space character and width, except for last word in line
        if (wordIndex < msg.tb.lines[rowIndex].length - 1) {
          cursorX += text.width(" ");
        }
      }
    }

    const timestamp = {
      x: text.width(msg.tb.lines[msg.tb.lines.length - 1]) + 6,
      y: y + (msg.tb.lines.length - 1) * rowHeight,
      height: rowHeight,
      width: text.width(timeAgo(msg.when)),
    };

    let timestampColor = [100 / 1.3, 100 / 1.3, 145 / 1.3];
    msg.layout = {
      x: leftMargin,
      y,
      width: msg.tb.box.width,
      height: msg.tb.box.height,
      timestamp,
      timestampColor,
      msgColor,
      inBox,
      handles,
      prompts,
      urls,
      paintableMessage,
    };

    delete msg.lastLayout;

    y -= rowHeight; // Move up one line for the next message.
    if (y < topMargin - rowHeight) {
      break; // Break if y is below top line.
    }
  }
}

function computeScrollbarHeight(api) {
  return api.screen.height - bottomMargin - topMargin + 2;
}

function timeAgo(timestamp) {
  const now = new Date();
  const past = new Date(timestamp);
  const seconds = floor((now - past) / 1000);

  const units = [
    { name: "year", seconds: 31536000 },
    { name: "month", seconds: 2592000 },
    { name: "week", seconds: 604800 },
    { name: "day", seconds: 86400 },
    { name: "hour", seconds: 3600 },
    { name: "minute", seconds: 60 },
    { name: "second", seconds: 1 },
  ];

  for (const unit of units) {
    const count = floor(seconds / unit.seconds);
    if (count >= 1) {
      return `${count} ${unit.name}${count > 1 ? "s" : ""} ago`;
    }
  }
  return "just now";
}

// Parse prompts and URLs from the original message string
function parseMessageElements(message) {
  const elements = [];

  // Parse prompts (text within single quotes)
  const promptRegex = /'[^']*'/g;
  let match;
  while ((match = promptRegex.exec(message)) !== null) {
    elements.push({
      type: "prompt",
      text: match[0],
      start: match.index,
      end: match.index + match[0].length,
    });
  }  // Parse URLs (starting with http://, https://, or www.)
  const urlRegex = /(https?:\/\/[^\s]+|www\.[^\s]+)/g;
  while ((match = urlRegex.exec(message)) !== null) {
    elements.push({
      type: "url",
      text: match[0],
      start: match.index,
      end: match.index + match[0].length,
    });
  }

  return elements;
}
