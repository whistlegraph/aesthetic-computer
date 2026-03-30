// Table, 2026.03.30
// A multiplayer card table — drag cards on a shared surface.
// Fixed table geography (like a pool table). One card per player at a time.

const SUITS = ["hearts", "diamonds", "clubs", "spades"];
const RANKS = ["A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K"];
const SYM = { hearts: "\u2665", diamonds: "\u2666", clubs: "\u2663", spades: "\u2660" };
const RED = [220, 60, 80];
const BLK = [40, 40, 50];

const CARD_W = 28;
const CARD_H = 38;
const BTN_W = 36;
const BTN_H = 13;

// Table size (fixed geography, larger than screen)
const TABLE_W = 480;
const TABLE_H = 320;

// Full 52-card reference
const ALL = [];
for (const s of SUITS) for (const r of RANKS) ALL.push({ rank: r, suit: s });

// -- State --
let server, udpChannel;
let myHandle = "guest";
let myId = null;
let players = {}; // id -> { handle, dragging: cardIdx|null, dx, dy }
let frameCount = 0;

// Cards on the table: { idx, x, y, faceUp, dragger: handle|null }
let tableCards = [];
let deck = []; // remaining card indices
let deckSeed = 0;

// Deck position on table (fixed)
const DECK_X = Math.floor(TABLE_W / 2 - CARD_W / 2);
const DECK_Y = Math.floor(TABLE_H / 2 - CARD_H / 2);

// Camera (viewport offset into table)
let camX = 0, camY = 0;
let sw = 0, sh = 0;

// Local drag state
let dragging = null; // index into tableCards
let dragOffX = 0, dragOffY = 0;
let hoverIdx = -1; // index into tableCards

// Button rects (screen space)
let shuffleBtn = null, drawBtn = null;

// Seeded shuffle
function seededShuffle(arr, seed) {
  const a = [...arr];
  let s = seed;
  for (let i = a.length - 1; i > 0; i--) {
    s = (s * 1664525 + 1013904223) & 0xffffffff;
    const j = (s >>> 0) % (i + 1);
    [a[i], a[j]] = [a[j], a[i]];
  }
  return a;
}

function doShuffle(seed) {
  deckSeed = seed;
  deck = seededShuffle(Array.from({ length: 52 }, (_, i) => i), seed);
  tableCards = [];
  dragging = null;
  hoverIdx = -1;
}

// Convert screen coords to table coords
function toTable(sx, sy) {
  return { tx: sx + camX, ty: sy + camY };
}

// Convert table coords to screen coords
function toScreen(tx, ty) {
  return { sx: tx - camX, sy: ty - camY };
}

function btnHit(btn, x, y) {
  return btn && x >= btn.x && x < btn.x + btn.w && y >= btn.y && y < btn.y + btn.h;
}

// Find topmost card at table position (last in array = top)
function cardAtPos(tx, ty) {
  for (let i = tableCards.length - 1; i >= 0; i--) {
    const c = tableCards[i];
    if (tx >= c.x && tx < c.x + CARD_W && ty >= c.y && ty < c.y + CARD_H) {
      return i;
    }
  }
  return -1;
}

// Bring card to top of stack
function bringToTop(idx) {
  const card = tableCards.splice(idx, 1)[0];
  tableCards.push(card);
  return tableCards.length - 1;
}

function boot({ wipe, screen, net: { socket, udp }, handle }) {
  sw = screen.width;
  sh = screen.height;
  myHandle = handle?.() || "guest_" + Math.floor(Math.random() * 9999);

  // Center camera on table
  camX = Math.floor(TABLE_W / 2 - sw / 2);
  camY = Math.floor(TABLE_H / 2 - sh / 2);

  doShuffle(Date.now());

  // UDP for low-latency drag position sync
  udpChannel = udp((type, content) => {
    if (type === "table:drag") {
      const d = typeof content === "string" ? JSON.parse(content) : content;
      if (d.handle === myHandle) return;
      // Find card on table and update its position
      const tc = tableCards.find((c) => c.idx === d.cardIdx);
      if (tc) {
        tc.x = d.x;
        tc.y = d.y;
        tc.dragger = d.handle;
      }
    }
  });

  server = socket((id, type, content) => {
    if (type.startsWith("connected")) {
      myId = id;
      server.send("table:join", { handle: myHandle });
      return;
    }

    if (type === "left") {
      // Release any card they were dragging
      for (const c of tableCards) {
        if (players[id] && c.dragger === players[id].handle) {
          c.dragger = null;
        }
      }
      delete players[id];
      return;
    }

    const msg = typeof content === "string" ? JSON.parse(content) : content;

    if (type === "table:join") {
      players[id] = { handle: msg.handle };
      // Send full state to newcomer
      server.send("table:state", {
        handle: myHandle,
        seed: deckSeed,
        deckLen: deck.length,
        cards: tableCards.map((c) => ({
          idx: c.idx, x: c.x, y: c.y, faceUp: c.faceUp, dragger: c.dragger,
        })),
      });
    }

    if (type === "table:state") {
      if (!players[id]) players[id] = { handle: msg.handle };
      // Sync table state from existing player
      if (msg.cards && msg.cards.length > 0 && tableCards.length === 0) {
        // Only accept if we have no cards yet (fresh join)
        doShuffle(msg.seed);
        // Remove cards that are on the table from the deck
        for (const c of msg.cards) {
          const di = deck.indexOf(c.idx);
          if (di >= 0) deck.splice(di, 1);
          tableCards.push({
            idx: c.idx, x: c.x, y: c.y,
            faceUp: c.faceUp, dragger: c.dragger,
          });
        }
      }
    }

    if (type === "table:shuffle") {
      doShuffle(msg.seed);
    }

    if (type === "table:draw") {
      // Another player drew a card onto the table
      const di = deck.indexOf(msg.cardIdx);
      if (di >= 0) deck.splice(di, 1);
      tableCards.push({
        idx: msg.cardIdx, x: msg.x, y: msg.y,
        faceUp: true, dragger: null,
      });
    }

    if (type === "table:grab") {
      const tc = tableCards.find((c) => c.idx === msg.cardIdx);
      if (tc) {
        tc.dragger = msg.handle;
        // Bring to top
        const i = tableCards.indexOf(tc);
        if (i >= 0) bringToTop(i);
      }
    }

    if (type === "table:drop") {
      const tc = tableCards.find((c) => c.idx === msg.cardIdx);
      if (tc) {
        tc.x = msg.x;
        tc.y = msg.y;
        tc.dragger = null;
      }
    }

    if (type === "table:flip") {
      const tc = tableCards.find((c) => c.idx === msg.cardIdx);
      if (tc) tc.faceUp = !tc.faceUp;
    }
  });

  wipe(245, 240, 230);
}

function sim() {
  frameCount++;

  // Send drag position via UDP every 2 frames
  if (dragging !== null && frameCount % 2 === 0 && udpChannel?.connected) {
    const c = tableCards[dragging];
    if (c) {
      udpChannel.send("table:drag", {
        handle: myHandle,
        cardIdx: c.idx,
        x: c.x,
        y: c.y,
      });
    }
  }
}

function act({ event: e, screen }) {
  sw = screen.width;
  sh = screen.height;

  // Button positions (screen space, near bottom-left)
  shuffleBtn = { x: 4, y: sh - BTN_H - 4, w: BTN_W, h: BTN_H };
  drawBtn = { x: 4 + BTN_W + 4, y: sh - BTN_H - 4, w: BTN_W, h: BTN_H };

  // Hover detection
  if (e.is("move")) {
    const { tx, ty } = toTable(e.x, e.y);
    hoverIdx = cardAtPos(tx, ty);
  }

  // Touch: start dragging a card or tap buttons
  if (e.is("touch")) {
    // Check buttons first (screen space)
    if (btnHit(shuffleBtn, e.x, e.y)) {
      const seed = Date.now();
      doShuffle(seed);
      server?.send("table:shuffle", { seed, handle: myHandle });
      return;
    }

    if (btnHit(drawBtn, e.x, e.y) && deck.length > 0) {
      const cardIdx = deck.pop();
      // Place near deck position with slight random offset
      const ox = (Math.random() - 0.5) * 40;
      const oy = (Math.random() - 0.5) * 20 + CARD_H + 8;
      const x = DECK_X + ox;
      const y = DECK_Y + oy;
      tableCards.push({ idx: cardIdx, x, y, faceUp: true, dragger: null });
      server?.send("table:draw", {
        handle: myHandle, cardIdx, x, y,
      });
      return;
    }

    // Try to grab a card
    const { tx, ty } = toTable(e.x, e.y);
    const idx = cardAtPos(tx, ty);
    if (idx >= 0) {
      const c = tableCards[idx];
      // Can only grab if no one else is dragging it
      if (!c.dragger || c.dragger === myHandle) {
        dragging = bringToTop(idx);
        const nc = tableCards[dragging];
        nc.dragger = myHandle;
        dragOffX = tx - nc.x;
        dragOffY = ty - nc.y;
        server?.send("table:grab", {
          handle: myHandle, cardIdx: nc.idx,
        });
      }
    }
  }

  // Drag: move the card
  if (e.is("draw") && dragging !== null) {
    const { tx, ty } = toTable(e.x, e.y);
    const c = tableCards[dragging];
    if (c) {
      c.x = tx - dragOffX;
      c.y = ty - dragOffY;
    }
  }

  // Lift: drop the card
  if (e.is("lift")) {
    if (dragging !== null) {
      const c = tableCards[dragging];
      if (c) {
        c.dragger = null;
        server?.send("table:drop", {
          handle: myHandle, cardIdx: c.idx, x: c.x, y: c.y,
        });
      }
      dragging = null;
    }
  }

  // Double-tap to flip (keyboard shortcut: f)
  if (e.is("keyboard:down:f")) {
    if (hoverIdx >= 0 && hoverIdx < tableCards.length) {
      const c = tableCards[hoverIdx];
      c.faceUp = !c.faceUp;
      server?.send("table:flip", { handle: myHandle, cardIdx: c.idx });
    }
  }
}

function paint({ wipe, ink, box, write, line, screen }) {
  sw = screen.width;
  sh = screen.height;
  wipe(60, 85, 55); // Green felt base

  // -- Table surface --
  const tl = toScreen(0, 0);
  const tw = TABLE_W;
  const th = TABLE_H;

  // Table felt
  ink(70, 100, 65).box(tl.sx, tl.sy, tw, th);
  // Table border
  ink(90, 65, 40).box(tl.sx, tl.sy, tw, th, "outline");
  ink(80, 58, 35).box(tl.sx - 1, tl.sy - 1, tw + 2, th + 2, "outline");

  // -- Deck (table space, rendered in screen space) --
  const ds = toScreen(DECK_X, DECK_Y);
  if (deck.length > 0) {
    const n = Math.min(deck.length, 3);
    for (let i = n - 1; i >= 0; i--) {
      ink(205, 200, 190).box(ds.sx + i, ds.sy - i, CARD_W, CARD_H);
      ink(180, 175, 165).box(ds.sx + i, ds.sy - i, CARD_W, CARD_H, "outline");
    }
    ink(130, 100, 150).box(ds.sx + 2, ds.sy + 2, CARD_W - 4, CARD_H - 4);
    ink(150, 120, 170).box(ds.sx + 4, ds.sy + 4, CARD_W - 8, CARD_H - 8, "outline");
    const ct = "" + deck.length;
    ink(200, 195, 180).write(ct, {
      x: ds.sx + Math.floor(CARD_W / 2 - ct.length * 3),
      y: ds.sy + CARD_H + 2,
    });
  } else {
    ink(80, 110, 75).box(ds.sx, ds.sy, CARD_W, CARD_H, "outline");
  }

  // -- Cards on table --
  for (let i = 0; i < tableCards.length; i++) {
    const c = tableCards[i];
    const s = toScreen(c.x, c.y);
    const isHover = i === hoverIdx && dragging === null;
    const isDragging = i === dragging;
    const isRemoteDrag = c.dragger && c.dragger !== myHandle;

    // Shadow when dragging
    if (isDragging || isRemoteDrag) {
      ink(0, 0, 0, 30).box(s.sx + 2, s.sy + 2, CARD_W, CARD_H);
    }

    if (c.faceUp) {
      const card = ALL[c.idx];
      const col = card.suit === "hearts" || card.suit === "diamonds" ? RED : BLK;

      // Card bg
      if (isHover) {
        ink(255, 255, 252).box(s.sx, s.sy, CARD_W, CARD_H);
      } else {
        ink(255, 253, 249).box(s.sx, s.sy, CARD_W, CARD_H);
      }

      // Border — highlight if hovered or being dragged
      if (isDragging) {
        ink(120, 160, 200).box(s.sx, s.sy, CARD_W, CARD_H, "outline");
      } else if (isRemoteDrag) {
        ink(200, 140, 100).box(s.sx, s.sy, CARD_W, CARD_H, "outline");
      } else if (isHover) {
        ink(140, 135, 130).box(s.sx, s.sy, CARD_W, CARD_H, "outline");
      } else {
        ink(185, 180, 175).box(s.sx, s.sy, CARD_W, CARD_H, "outline");
      }

      // Rank + suit
      ink(...col).write(c.faceUp ? card.rank : "", { x: s.sx + 2, y: s.sy + 2 });
      ink(...col).write(SYM[card.suit], {
        x: s.sx + Math.floor(CARD_W / 2 - 3),
        y: s.sy + Math.floor(CARD_H / 2 - 3),
      });
      ink(...col).write(card.rank, {
        x: s.sx + CARD_W - card.rank.length * 6 - 2,
        y: s.sy + CARD_H - 9,
      });
    } else {
      // Face down
      ink(130, 100, 150).box(s.sx, s.sy, CARD_W, CARD_H);
      ink(150, 120, 170).box(s.sx + 2, s.sy + 2, CARD_W - 4, CARD_H - 4, "outline");
      if (isHover) {
        ink(170, 145, 190).box(s.sx, s.sy, CARD_W, CARD_H, "outline");
      }
    }

    // Handle label for whoever is dragging this card
    if (c.dragger) {
      ink(255, 255, 255, 200).write(c.dragger, {
        x: s.sx,
        y: s.sy - 8,
      });
    }
  }

  // -- HUD (screen space, on top) --

  // Players list (top right)
  const pList = [myHandle];
  for (const p of Object.values(players)) pList.push(p.handle);
  let py = 4;
  for (let i = 0; i < pList.length; i++) {
    const h = pList[i];
    const label = i === 0 ? h + " (you)" : h;
    ink(220, 230, 215).write(label, { x: sw - label.length * 6 - 4, y: py });
    py += 10;
  }

  // Buttons (bottom left)
  paintBtn(ink, box, write, shuffleBtn, "shuf", true);
  paintBtn(ink, box, write, drawBtn, "draw", deck.length > 0);

  // Deck count near draw button
  if (deck.length > 0) {
    const dl = "" + deck.length;
    ink(200, 210, 195).write(dl, {
      x: drawBtn.x + BTN_W + 4,
      y: drawBtn.y + 3,
    });
  }

  // Hint
  const hint = dragging !== null
    ? "dragging..."
    : hoverIdx >= 0
      ? "drag to move / f to flip"
      : "tap deck or draw";
  ink(180, 195, 175).write(hint, { x: 4, y: 4 });
}

function paintBtn(ink, box, write, btn, label, active) {
  if (!btn) return;
  if (active) {
    ink(55, 75, 50).box(btn.x, btn.y, btn.w, btn.h);
    ink(80, 110, 72).box(btn.x, btn.y, btn.w, btn.h, "outline");
    ink(200, 210, 195).write(label, { x: btn.x + 6, y: btn.y + 3 });
  } else {
    ink(50, 65, 45).box(btn.x, btn.y, btn.w, btn.h);
    ink(65, 80, 58).box(btn.x, btn.y, btn.w, btn.h, "outline");
    ink(100, 115, 95).write(label, { x: btn.x + 6, y: btn.y + 3 });
  }
}

function meta() {
  return {
    title: "Table",
    desc: "Multiplayer card table. Drag cards on a shared surface.",
  };
}

export { boot, sim, act, paint, meta };
export const desc = "Multiplayer card table.";
