// Table, 2026.03.30
// A multiplayer card table with a shared deck.
// Shuffle, draw, see who's seated.

const SUITS = ["hearts", "diamonds", "clubs", "spades"];
const RANKS = ["A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K"];
const SYM = { hearts: "\u2665", diamonds: "\u2666", clubs: "\u2663", spades: "\u2660" };
const RED = [220, 60, 80];
const BLK = [40, 40, 50];

const CARD_W = 28;
const CARD_H = 38;
const CARD_GAP = 2;
const BTN_W = 36;
const BTN_H = 13;

// Full 52-card reference
const ALL = [];
for (const s of SUITS) for (const r of RANKS) ALL.push({ rank: r, suit: s });

// -- State --
let server;
let myHandle = "guest";
let myId = null;
let players = {}; // id -> { handle, handSize }
let deck = []; // indices into ALL
let myHand = []; // indices I hold
let deckSeed = 0;
let sw = 0, sh = 0;
let hoverCard = -1;
let shuffleBtn, drawBtn;

// Seeded shuffle so all clients get same order from same seed
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
  myHand = [];
  for (const p of Object.values(players)) p.handSize = 0;
}

function btnHit(btn, x, y) {
  return btn && x >= btn.x && x < btn.x + btn.w && y >= btn.y && y < btn.y + btn.h;
}

function boot({ wipe, screen, net: { socket }, handle }) {
  sw = screen.width;
  sh = screen.height;
  myHandle = handle?.() || "guest_" + Math.floor(Math.random() * 9999);
  doShuffle(Date.now());

  server = socket((id, type, content) => {
    if (type.startsWith("connected")) {
      myId = id;
      server.send("cards:join", { handle: myHandle, seed: deckSeed });
      return;
    }

    if (type === "left") {
      delete players[id];
      return;
    }

    const msg = typeof content === "string" ? JSON.parse(content) : content;

    if (type === "cards:join") {
      players[id] = { handle: msg.handle, handSize: 0 };
      // Tell newcomer current deck state
      server.send("cards:sync", {
        handle: myHandle,
        seed: deckSeed,
        deckLen: deck.length,
        handSize: myHand.length,
      });
    }

    if (type === "cards:sync") {
      if (!players[id]) players[id] = { handle: msg.handle, handSize: 0 };
      players[id].handSize = msg.handSize || 0;
    }

    if (type === "cards:shuffle") {
      doShuffle(msg.seed);
    }

    if (type === "cards:draw") {
      // Another player drew — pop from shared deck
      if (deck.length > 0) deck.pop();
      if (!players[id]) players[id] = { handle: msg.handle, handSize: 0 };
      players[id].handSize = msg.handSize;
    }

    if (type === "cards:discard") {
      if (!players[id]) players[id] = { handle: msg.handle, handSize: 0 };
      players[id].handSize = msg.handSize;
    }
  });

  wipe(245, 240, 230);
}

function act({ event: e, screen }) {
  sw = screen.width;
  sh = screen.height;

  const deckX = Math.floor(sw / 2 - CARD_W / 2);
  const deckY = Math.floor(sh / 2 - CARD_H / 2 - 14);
  shuffleBtn = { x: deckX - BTN_W - 4, y: deckY + CARD_H + 6, w: BTN_W, h: BTN_H };
  drawBtn = { x: deckX + CARD_W + 4, y: deckY + CARD_H + 6, w: BTN_W, h: BTN_H };

  // Hover cards in hand
  hoverCard = -1;
  if (e.is("move") || e.is("draw")) {
    const handY = sh - CARD_H - 10;
    const totalW = myHand.length * (CARD_W + CARD_GAP) - CARD_GAP;
    const startX = Math.floor(sw / 2 - totalW / 2);
    for (let i = myHand.length - 1; i >= 0; i--) {
      const cx = startX + i * (CARD_W + CARD_GAP);
      if (e.x >= cx && e.x < cx + CARD_W && e.y >= handY - 4 && e.y < handY + CARD_H) {
        hoverCard = i;
        break;
      }
    }
  }

  if (e.is("touch")) {
    if (btnHit(shuffleBtn, e.x, e.y)) {
      const seed = Date.now();
      doShuffle(seed);
      server?.send("cards:shuffle", { seed, handle: myHandle });
      return;
    }

    if (btnHit(drawBtn, e.x, e.y) && deck.length > 0) {
      const idx = deck.pop();
      myHand.push(idx);
      server?.send("cards:draw", { handle: myHandle, handSize: myHand.length });
      return;
    }

    // Tap card in hand to discard
    if (hoverCard >= 0) {
      myHand.splice(hoverCard, 1);
      server?.send("cards:discard", { handle: myHandle, handSize: myHand.length });
      hoverCard = -1;
    }
  }
}

function paint({ wipe, ink, box, write, screen }) {
  sw = screen.width;
  sh = screen.height;
  wipe(245, 240, 230);

  // Felt area
  const pad = 10;
  ink(236, 230, 218).box(pad, pad, sw - pad * 2, sh - pad * 2);

  // Title
  ink(90, 80, 70).write("table", { x: 4, y: 4 });

  // Players list (top right)
  const pList = [myHandle + " (you) " + myHand.length];
  for (const p of Object.values(players)) {
    pList.push(p.handle + " " + (p.handSize || 0));
  }
  let py = 4;
  for (const h of pList) {
    ink(150, 140, 130).write(h, { x: sw - h.length * 6 - 4, y: py });
    py += 10;
  }

  // -- Deck (center) --
  const deckX = Math.floor(sw / 2 - CARD_W / 2);
  const deckY = Math.floor(sh / 2 - CARD_H / 2 - 14);

  if (deck.length > 0) {
    // Stack shadow
    const n = Math.min(deck.length, 3);
    for (let i = n - 1; i >= 0; i--) {
      ink(205, 200, 190).box(deckX + i, deckY - i, CARD_W, CARD_H);
      ink(180, 175, 165).box(deckX + i, deckY - i, CARD_W, CARD_H, "outline");
    }
    // Face-down pattern
    ink(130, 100, 150).box(deckX + 2, deckY + 2, CARD_W - 4, CARD_H - 4);
    ink(150, 120, 170).box(deckX + 4, deckY + 4, CARD_W - 8, CARD_H - 8, "outline");

    const ct = "" + deck.length;
    ink(110, 100, 90).write(ct, {
      x: deckX + Math.floor(CARD_W / 2 - ct.length * 3),
      y: deckY + CARD_H + 2,
    });
  } else {
    ink(225, 220, 210).box(deckX, deckY, CARD_W, CARD_H, "outline");
    ink(205, 200, 190).write("--", {
      x: deckX + Math.floor(CARD_W / 2 - 6),
      y: deckY + Math.floor(CARD_H / 2 - 3),
    });
  }

  // -- Buttons --
  paintBtn(ink, box, write, shuffleBtn, "shuf", true);
  paintBtn(ink, box, write, drawBtn, "draw", deck.length > 0);

  // -- My hand --
  if (myHand.length > 0) {
    const handY = sh - CARD_H - 10;
    const totalW = myHand.length * (CARD_W + CARD_GAP) - CARD_GAP;
    const startX = Math.floor(sw / 2 - totalW / 2);

    for (let i = 0; i < myHand.length; i++) {
      const c = ALL[myHand[i]];
      const cx = startX + i * (CARD_W + CARD_GAP);
      const cy = hoverCard === i ? handY - 4 : handY;
      const col = c.suit === "hearts" || c.suit === "diamonds" ? RED : BLK;

      // Card bg
      ink(255, 253, 249).box(cx, cy, CARD_W, CARD_H);
      ink(185, 180, 175).box(cx, cy, CARD_W, CARD_H, "outline");

      // Rank top-left
      ink(...col).write(c.rank, { x: cx + 2, y: cy + 2 });
      // Suit center
      ink(...col).write(SYM[c.suit], {
        x: cx + Math.floor(CARD_W / 2 - 3),
        y: cy + Math.floor(CARD_H / 2 - 3),
      });
      // Rank bottom-right
      ink(...col).write(c.rank, {
        x: cx + CARD_W - c.rank.length * 6 - 2,
        y: cy + CARD_H - 9,
      });
    }
  }

  // Hint
  const hint = myHand.length > 0 ? "tap card to discard" : "draw from the deck";
  ink(195, 185, 175).write(hint, {
    x: Math.floor(sw / 2 - hint.length * 3),
    y: sh - 6,
  });
}

function paintBtn(ink, box, write, btn, label, active) {
  if (!btn) return;
  if (active) {
    ink(215, 208, 198).box(btn.x, btn.y, btn.w, btn.h);
    ink(170, 163, 153).box(btn.x, btn.y, btn.w, btn.h, "outline");
    ink(100, 90, 80).write(label, { x: btn.x + 6, y: btn.y + 3 });
  } else {
    ink(232, 228, 222).box(btn.x, btn.y, btn.w, btn.h);
    ink(210, 205, 200).box(btn.x, btn.y, btn.w, btn.h, "outline");
    ink(205, 200, 195).write(label, { x: btn.x + 6, y: btn.y + 3 });
  }
}

function meta() {
  return {
    title: "Table",
    desc: "Multiplayer card table. Shuffle, draw, and see who's at the table.",
  };
}

export { boot, act, paint, meta };
export const desc = "Multiplayer card table.";
