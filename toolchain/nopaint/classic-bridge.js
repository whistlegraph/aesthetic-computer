// No Paint classic bridge, 2026.11.18
// Appended to scripts/c3runtime.js by toolchain/nopaint/patch-classic.mjs.
// It runs where the game runs (a worker when the browser has OffscreenCanvas)
// and speaks to the page over the BroadcastChannel "nopaint-classic".
//
// The game says:   {to:"page", event, ...state}, where event is one of
//   ready, layout             the game started; it changed layout
//   proposed {step, tool}     a brush began to show a proposal
//   painting {step, tool}               Paint was pressed
//   painted {step, tool, options}       the proposal is over and is in the painting
//   refused {step, tool, options}       the proposal was left without Paint (No)
//   ai {id, note, settings}             an AI turn began (the AI brush); answer with aiResult
//   failed {step, tool, turn}           an AI turn got no picture; not a step
//   saving {code}                       Save was pressed
// The page asks:   {to:"game", id, op, ...}  →  {to:"page", id, result} or {to:"page", id, error}
//
//   state                        where the game is
//   prompt {text}                type a line at the prompt and run it: "Flip:v"
//   call {name, args}            call an event-sheet function: "Paint", "No", "Clear"
//   set {name, value}            write a global variable
//   goto {layout}                go to a layout by name: "Index"
//   flags {flags}                raise or lower the tuning's conditions: {ai: true}
//   odds                         each brush's chance in 100, as the game has it now
//   sample {n}                   draw n proposals with the game's picker and count them
//   aiResult {turn, png | error} the picture of an AI turn, or why there is none
//   look {uid}                   one placed thing on this layout: place, size, text, box on the page
//   write {uid, text, visible, x, y, width, height, fit}   set a sprite font
//   create {like | type, layer, x, y, width, height, text, under}   a new sprite font or sprite, drawn like another; under: a UID to sit just behind
//   code {code}                  the #code Save should show, as prompt.ac/#code (null: a classic code)
//   settings                     the tuning's settings for AI turns
//   texts                        every text on this layout: UID, type, words, place
//   stage                        every placed thing on this layout, back to front, with its box
//   place {uid, x, y, width, height, visible, under}   move, size, show or hide any placed thing; under: a UID to sit just behind
//   tint {uid, color, opacity}   tint a placed thing, or set how solid it is
//   pointer {as}                 the cursor: "hand", "arrow", or null to leave it to the game
//   address                      the prompt.ac/# beside Save's code: where it is, how much ink
//   force {name}                 make the next proposal this brush, once
//   pixels {object, data}        read a canvas: hash and ink count, bytes if data is true
//   load {object, width, height, data}   write RGBA bytes into a canvas
//
// The brush test (tests/browser/brushbook.e2e.mjs) drives the game with
// these, and the page module that stores each step listens to "painted".

// ---- the picker hook --------------------------------------------------------
// The game rolls between its two lists and then asks its random plugin for a
// weighted pick. From here that pick is made by classic-picker.js with the
// numbers in classic-tuning.json (both appended above this file). This runs
// when the script loads, before the runtime looks the plugin's functions up.
// A brush's own tables (Stamp, Softy and Build keep some) are left alone, and
// so is everything if the picker or the tuning is missing.
const classic = (self.nopaintClassic = { tuning: self.nopaintTuning || null, flags: {}, run: {}, resolved: null, force: null,
  turns: new Map(), turn: 0, runtime: null, onTurn: null, code: null, onSaving: null, pointer: null, picked: null, brush: false,
  // The game's Save asks the save server for a code. This is added to that
  // request (below): it names the AC painting these steps belong to, and the
  // server hands its #code back as the code.
  codeQuery() { this.onSaving?.(); return this.code ? `?as=${encodeURIComponent(this.code)}` : ""; } });
{
  // The request is wrapped, not its URL: the game works a constant URL out
  // once, when it loads, so text added to the URL would never run at Save.
  const ajax = self.C3?.Plugins?.AJAX?.Acts, request = ajax?.Request;
  if (request) ajax.Request = function (tag, url) {
    return request.call(this, tag, url === "https://server.nopaint.art/get-code" ? url + classic.codeQuery() : url);
  };
}
// The game's cursor is a sprite: an arrow ("Normal"), and a hand over its
// buttons ("Over Button"), which it works out from where its own buttons are.
// The page has a button of its own on the title screen (Sign In) and shuts the
// game's for a guest, so while the page says which the pointer is over
// (op "pointer"), that is the cursor, whatever the game asks for.
{
  const acts = self.C3?.Plugins?.Sprite?.Acts, setAnim = acts?.SetAnim;
  if (setAnim) acts.SetAnim = function (name, from) {
    let want = name;
    try { if (classic.pointer && (name === "Normal" || name === "Over Button") && this.GetObjectClass().GetName() === "Cursor") want = classic.pointer === "hand" ? "Over Button" : "Normal"; } catch {}
    return setAnim.call(this, want, from);
  };
}
// Which list a brush is in, and its entry there.
const entryOf = (name) => { for (const list of Object.keys(classic.tuning?.split || {})) if (classic.tuning[list]?.[name] !== undefined) return classic.tuning[list][name]; return null; };
{
  const plugin = self.C3?.Plugins?.AdvancedRandom, picker = self.nopaintPicker;
  if (plugin?.Exps?.Weighted && picker && classic.tuning) {
    const rolled = new Set(["Brushes", "Filters"]); // the lists the 2021 game rolls between
    const weighted = plugin.Exps.Weighted;
    plugin.Exps.Weighted = function () {
      // The plugin knows which table is current, however it was made current:
      // Stamp picks a painter from a table it creates and never "sets".
      if (!rolled.has(this._currentProbabilityTableName)) return weighted.call(this);
      try {
        classic.resolved ||= picker.resolve(classic.tuning, classic.flags);
        const name = classic.force || picker.next(classic.run, classic.tuning, classic.resolved);
        classic.force = null;
        // An AI turn is the AI brush (layout and sheet "AI", added to data.json
        // by patch-classic.mjs). It takes its turn when it starts (below).
        if (name && entryOf(name)?.ai) {
          if (!classic.brush) return weighted.call(this); // a data.json without the AI brush
          classic.picked = name;
          return "AI";
        }
        if (name) return name;
      } catch (error) { console.warn("[nopaint] the tuned pick failed; the game picks", error); }
      return weighted.call(this);
    };
    classic.hooked = true;
  }
  // The picture of an AI turn, when the AI brush asks for it by the turn's
  // code. Only the AI brush gets it: "Load: ai3" asks the save server.
  const TURN = /^https:\/\/server\.nopaint\.art\/painting\/(ai\d+)\.png$/, fetchBlob = self.C3?.FetchBlob;
  if (fetchBlob) self.C3.FetchBlob = function (url) {
    let on = null; try { on = classic.runtime?.layout.name; } catch {}
    const turn = on === "AI" ? classic.turns.get(TURN.exec(String(url))?.[1]) : null;
    if (!turn) return fetchBlob.call(this, url);
    return new Promise((resolve, reject) => {
      const timer = setTimeout(() => turn.settle?.(null, "the AI turn took too long"), 120000);
      turn.settle = (blob, error) => {
        clearTimeout(timer); turn.settle = null;
        if (blob) return resolve(blob);
        turn.error = error || "the AI turn failed";
        // The brush goes back to the index, which shows this as its error.
        try { classic.runtime.globalVars.CurrentError = "AI Failed"; } catch {}
        reject(new Error(turn.error));
      };
      if (turn.blob) turn.settle(turn.blob); else if (turn.error) turn.settle(null, turn.error);
    });
  };
}

runOnStartup(async (runtime) => {
  classic.runtime = runtime;
  // The AI brush takes its turn as it starts, before its sheet runs: the note
  // the picker chose, or one typed after it ("AI: add rain"), or one of the
  // tuning's. Its Params is the turn's code, which its sheet fetches, and its
  // hint is the note, which its sheet prints: "AI: add rain".
  try {
    runtime.getLayout("AI").addEventListener("beforelayoutstart", () => {
      const vars = runtime.globalVars, notes = classic.tuning?.ai?.notes || [];
      const typed = classic.picked ? "" : String(vars.Params || "").trim();
      const note = typed || notes[Math.floor(Math.random() * notes.length)] || "surprise me";
      const turn = { id: `ai${++classic.turn}`, name: classic.picked || "AI", note, blob: null, error: null, settle: null };
      classic.picked = null;
      classic.turns.set(turn.id, turn);
      vars.Params = turn.id;
      vars.parameterHint = ` ${note}`;
      vars.promptText = `AI: ${note}`; // what the keyboard opens on, as for any brush
      classic.onTurn?.(turn);
    });
    classic.brush = true;
  } catch (error) { console.warn("[nopaint] no AI brush in this data.json; no AI turns", error); }
  if (typeof BroadcastChannel !== "function") return;
  const channel = new BroadcastChannel("nopaint-classic");
  const vars = runtime.globalVars;
  const say = (message) => channel.postMessage({ to: "page", ...message });
  const canvas = (name) => {
    const instance = runtime.objects[name]?.getFirstInstance();
    if (!instance || typeof instance.getImagePixelData !== "function") throw new Error(`no canvas named ${name}`);
    return instance;
  };
  // Between two layouts the game is on none, and asking it which throws.
  const where = () => { try { return runtime.layout.name; } catch { return null; } };
  const state = () => ({
    layout: where(), tool: vars.CurrentTool, params: vars.Params,
    prompt: vars.promptText, hint: vars.parameterHint, painting: !!vars.Painting, desktop: !!vars.DesktopMode, paused: !!vars.Paused,
    paintDate: vars.PaintDate, code: vars.LoadCode, name: vars.Name, time: runtime.gameTime,
  });
  // FNV-1a over the RGBA bytes; ink counts pixels that are not opaque white
  // and not fully clear, so a blank canvas reads 0 either way.
  function measure(bytes) {
    let hash = 0x811c9dc5, ink = 0;
    for (let i = 0; i < bytes.length; i += 4) {
      const r = bytes[i], g = bytes[i + 1], b = bytes[i + 2], a = bytes[i + 3];
      hash = Math.imul(hash ^ r, 0x01000193); hash = Math.imul(hash ^ g, 0x01000193);
      hash = Math.imul(hash ^ b, 0x01000193); hash = Math.imul(hash ^ a, 0x01000193);
      if (a !== 0 && !(r === 255 && g === 255 && b === 255 && a === 255)) ink++;
    }
    return { hash: (hash >>> 0).toString(16).padStart(8, "0"), ink };
  }
  const ops = {
    state,
    prompt({ text }) {
      vars.promptText = String(text);
      runtime.callFunction("ProcessPromptText");
      runtime.callFunction("RunBrushCommand");
      return state();
    },
    call({ name, args = [] }) {
      const value = runtime.callFunction(String(name), ...args);
      return { value: value ?? null, ...state() };
    },
    set({ name, value }) {
      if (!(name in vars)) throw new Error(`no global named ${name}`);
      vars[name] = value;
      return state();
    },
    // Raise or lower the conditions the tuning names (when: "ai").
    flags({ flags = {} }) {
      classic.flags = { ...classic.flags, ...flags };
      classic.resolved = null;
      return { flags: classic.flags, hooked: !!classic.hooked };
    },
    // The odds as the game has them now: each brush's chance in 100.
    odds() {
      if (!classic.hooked) return { hooked: false, odds: null };
      const table = self.nopaintPicker.table(self.nopaintPicker.resolve(classic.tuning, classic.flags));
      return { hooked: true, flags: classic.flags, odds: Object.fromEntries(table.map((row) => [row.name, +(row.chance * 100).toFixed(3)])) };
    },
    // Draw n proposals with the game's own picker, on a run of their own, and count them.
    sample({ n = 10000 }) {
      if (!classic.hooked) return { hooked: false, counts: null };
      const resolved = self.nopaintPicker.resolve(classic.tuning, classic.flags), run = {}, counts = {};
      let first = null;
      for (let i = 0; i < Math.min(n, 1_000_000); i++) { const name = self.nopaintPicker.next(run, classic.tuning, resolved); first ??= name; counts[name] = (counts[name] || 0) + 1; }
      return { hooked: true, n: Math.min(n, 1_000_000), first, counts };
    },
    // The page's answer to "ai": the picture as PNG bytes, or why there is none.
    // (The turn is named "turn": "id" is the question's own id.)
    aiResult({ turn: name, png, error }) {
      const turn = classic.turns.get(name);
      if (!turn) throw new Error(`no AI turn ${name}`);
      if (png) turn.blob = new Blob([png], { type: "image/png" }); else turn.error = String(error || "the AI turn failed");
      turn.settle?.(turn.blob, turn.error);
      return { turn: name, ok: !!turn.blob };
    },
    // One placed thing, by the UID the Construct source carries: where it is,
    // what it says, and its box on the page in CSS pixels (for taps).
    look({ uid }) {
      const thing = runtime.getInstanceByUid(Number(uid));
      if (!thing) throw new Error(`nothing with UID ${uid} on this layout`);
      const corner = (x, y) => thing.layer.layerToCssPx(x, y);
      return { uid: Number(uid), type: thing.objectType?.name, x: thing.x, y: thing.y, width: thing.width, height: thing.height,
        visible: thing.isVisible, layer: thing.layer?.name, ...("text" in thing ? { text: thing.text } : {}),
        // Can a player see it now? Its own switch, and its layer's.
        shown: Boolean(thing.isVisible && thing.layer?.isVisible && thing.layer.opacity > 0 && thing.opacity > 0),
        css: [corner(thing.x, thing.y), corner(thing.x + thing.width, thing.y + thing.height)] };
    },
    // Write a sprite font on this layout: its text, whether it shows, where it
    // sits. fit: make it as wide as its text, keeping its right edge.
    write({ uid, text, visible, x, y, width, height, fit }) {
      const thing = runtime.getInstanceByUid(Number(uid));
      if (!thing || !("text" in thing)) throw new Error(`no text with UID ${uid} on this layout`);
      if (text !== undefined) thing.text = String(text);
      if (visible !== undefined) thing.isVisible = Boolean(visible);
      if (fit) {
        // As wide as the text. A label on the right half keeps its right edge.
        // (Colour, background and scale tags take no room of their own.)
        const letters = String(thing.text).replace(/\[[^\]]*\]/g, "").length;
        const right = thing.x + thing.width, wide = Math.max(8, Math.ceil(letters * Number(fit)));
        thing.width = wide;
        if (right > 128) thing.x = right - wide;
      }
      for (const [key, value] of Object.entries({ x, y, width, height })) if (Number.isFinite(value)) thing[key] = value;
      return ops.look({ uid });
    },
    // Put a new sprite font on this layout, drawn like an existing one. It
    // lasts until the layout ends, as everything on a layout does.
    // (like: a UID to draw like; or type: any one of that object type here.)
    create({ like, type, layer, x, y, width, height, text = "", under }) {
      const model = like !== undefined ? runtime.getInstanceByUid(Number(like)) : runtime.objects[type]?.getFirstInstance();
      if (!model) throw new Error(`nothing here to draw like (${like ?? type})`);
      const made = model.objectType.createInstance(layer || model.layer.name, Number.isFinite(x) ? x : model.x, Number.isFinite(y) ? y : model.y);
      made.width = Number.isFinite(width) ? width : model.width; made.height = Number.isFinite(height) ? height : model.height;
      if ("text" in model) {
        for (const key of ["characterScale", "characterSpacing", "lineHeight", "horizontalAlign", "verticalAlign", "wordWrapMode"]) try { made[key] = model[key]; } catch {}
        made.text = String(text);
      }
      // Just behind another placed thing: a frame behind a plate.
      const over = under !== undefined ? runtime.getInstanceByUid(Number(under)) : null;
      if (over) made.moveAdjacentToInstance(over, false);
      return ops.look({ uid: made.uid });
    },
    // The stage: every placed thing on this layout, back to front, with its
    // box in layout pixels and on the page. toolchain/nopaint/stage-map.mjs draws it.
    stage() {
      const things = [];
      for (const type of Object.values(runtime.objects)) for (const thing of type.getAllInstances?.() || []) {
        if (!thing.layer || !Number.isFinite(thing.width)) continue;
        const corner = (x, y) => thing.layer.layerToCssPx(x, y);
        let box = null; try { const b = thing.getBoundingBox(); box = [b.left, b.top, b.right, b.bottom]; } catch {}
        box ||= [thing.x, thing.y, thing.x + thing.width, thing.y + thing.height];
        things.push({ uid: thing.uid, type: type.name, layer: thing.layer.name, layerIndex: thing.layer.index, z: thing.zIndex ?? 0,
          x: thing.x, y: thing.y, width: thing.width, height: thing.height, angle: thing.angleDegrees || 0, box,
          css: [corner(box[0], box[1]), corner(box[2], box[3])], opacity: thing.opacity, tint: thing.colorRgb ? [...thing.colorRgb] : null,
          shown: Boolean(thing.isVisible && thing.layer.isVisible && thing.layer.opacity > 0 && thing.opacity > 0),
          ...("text" in thing && typeof thing.text === "string" ? { text: thing.text } : {}),
          ...(thing.animationName !== undefined ? { animation: thing.animationName, frame: thing.animationFrame } : {}) });
      }
      things.sort((a, b) => a.layerIndex - b.layerIndex || a.z - b.z);
      const layers = runtime.layout.getAllLayers().map((layer) => ({ name: layer.name, index: layer.index, visible: layer.isVisible, opacity: layer.opacity }));
      return { layout: where(), size: [runtime.layout.width, runtime.layout.height], layers, things };
    },
    // Move or size any placed thing, or show and hide it.
    place({ uid, x, y, width, height, visible, under }) {
      const thing = runtime.getInstanceByUid(Number(uid));
      if (!thing) throw new Error(`nothing with UID ${uid} on this layout`);
      for (const [key, value] of Object.entries({ x, y, width, height })) if (Number.isFinite(value)) thing[key] = value;
      if (visible !== undefined) thing.isVisible = Boolean(visible);
      const over = under !== undefined ? runtime.getInstanceByUid(Number(under)) : null;
      if (over) thing.moveAdjacentToInstance(over, false);
      return { uid: thing.uid, x: thing.x, y: thing.y, width: thing.width, height: thing.height, visible: thing.isVisible };
    },
    // Tint a placed thing (1, 1, 1 is its own colours), or set how solid it is.
    tint({ uid, color, opacity }) {
      const thing = runtime.getInstanceByUid(Number(uid));
      if (!thing) throw new Error(`nothing with UID ${uid} on this layout`);
      const was = [...thing.colorRgb];
      if (color) thing.colorRgb = color.map(Number);
      if (Number.isFinite(opacity)) thing.opacity = opacity;
      return { uid: thing.uid, tint: [...thing.colorRgb], was, opacity: thing.opacity };
    },
    // Say what the cursor is over: "hand" (a button), "arrow" (nothing), or null to leave it to the game.
    pointer({ as = null }) {
      classic.pointer = as === "hand" || as === "arrow" ? as : null;
      const cursor = runtime.objects.Cursor?.getFirstInstance();
      if (classic.pointer && cursor) try { cursor.setAnimation(classic.pointer === "hand" ? "Over Button" : "Normal"); } catch {}
      return { pointer: classic.pointer, cursor: cursor?.animationName ?? null };
    },
    // Every text on this layout: for finding one whose UID is not known.
    texts() {
      const found = [];
      for (const type of Object.values(runtime.objects)) for (const thing of type.getAllInstances?.() || []) {
        if (!("text" in thing) || typeof thing.text !== "string") continue;
        found.push({ uid: thing.uid, type: type.name, text: thing.text, x: thing.x, y: thing.y, width: thing.width, height: thing.height, layer: thing.layer?.name,
          shown: Boolean(thing.isVisible && thing.layer?.isVisible && thing.layer.opacity > 0 && thing.opacity > 0) });
      }
      return { layout: where(), texts: found };
    },
    // The prompt.ac/# drawn beside Save's code, if it is up: where, and its pixels.
    async address() {
      const made = mark && !mark.making ? runtime.getInstanceByUid(mark.uid) : null;
      if (!made) return { shown: false };
      const image = await made.getImagePixelData();
      let ink = 0; for (let i = 3; i < image.data.length; i += 4) if (image.data[i]) ink++;
      return { shown: Boolean(made.isVisible), code: mark.code, x: made.x, y: made.y, width: made.width, height: made.height, layer: made.layer?.name, ink, pixels: [image.width, image.height] };
    },
    // The #code of the AC painting on the canvas, or null: what Save will show.
    code({ code }) { classic.code = code ? String(code) : null; return { code: classic.code }; },
    // The tuning's settings for AI turns, for the page.
    settings() { return { ai: classic.tuning?.ai || null, hooked: !!classic.hooked }; },
    // Make the next proposal this brush, once. For tests, and for asking.
    force({ name }) { classic.force = String(name); return { force: classic.force }; },
    goto({ layout }) {
      runtime.goToLayout(String(layout));
      return state();
    },
    async pixels({ object = "Painting", data = false }) {
      const image = await canvas(object).getImagePixelData();
      return { object, width: image.width, height: image.height, ...measure(image.data), ...(data ? { data: image.data } : {}) };
    },
    async load({ object = "Painting", width, height, data }) {
      const bytes = new Uint8ClampedArray(data);
      if (bytes.length !== width * height * 4) throw new Error("pixel data does not match width × height");
      canvas(object).loadImagePixelData(new ImageData(bytes, width, height));
      return { object, width, height, ...measure(bytes) };
    },
  };
  channel.onmessage = async ({ data }) => {
    if (!data || data.to !== "game" || !Object.hasOwn(ops, data.op)) return;
    try { say({ id: data.id, result: await ops[data.op](data) }); }
    catch (error) { say({ id: data.id, error: String(error?.message || error) }); }
  };
  // Every proposal is a step, and every step ends one of two ways. The game
  // raises its Painting flag while a brush commits. A proposal that is left
  // after the flag was raised was painted; one that is left with the flag
  // never raised was answered No (or abandoned) and is "refused".
  // Both carry the brush and the options it ran with.
  const NOT_BRUSHES = /^(Index|Splash|Load|Playlist)$|Profile|PainterStories/;
  const hint = () => String(vars.parameterHint || "").replace(/\[\/?[a-z]+(?:=[^\]]*)?\]/gi, "").trim();
  classic.onSaving = () => say({ event: "saving", code: classic.code });
  classic.onTurn = (turn) => say({ event: "ai", id: turn.id, note: turn.note, settings: classic.tuning.ai || {} });
  // Save writes the code at the right end of the pause bar, a colour to each
  // letter. An AC painting's code is a #code and has an address, so the line
  // reads prompt.ac/#code. The game's font cannot say that: its # is a carrot
  // and it has no letters small enough. So the address and the # are pixels,
  // put on a small canvas of the game's that sits against the code's left
  // edge: prompt.ac/ three pixels wide a letter, then a # as tall as the code.
  // A long code that leaves the address no room gets the # alone.
  const CODE_LINE = 718, CODE_LEFT = 172; // the line's UID in the Construct source; where the date and time end
  const SMALL = { p: ["000", "110", "101", "110", "100"], r: ["000", "101", "110", "100", "100"], o: ["000", "010", "101", "101", "010"],
    m: ["00000", "11010", "10101", "10101", "10101"], t: ["010", "111", "010", "010", "011"], ".": ["0", "0", "0", "0", "1"],
    a: ["000", "110", "011", "101", "111"], c: ["000", "011", "100", "100", "011"], "/": ["001", "001", "010", "100", "100"] };
  const HASH = ["01010", "01010", "11111", "01010", "11111", "01010", "01010"];
  const plain = (text) => String(text).replace(/\[[^\]]*\]/g, "");
  // The mark's pixels, one layout pixel each: [x, y, grey].
  function markPixels(withSite) {
    const dots = [];
    let x = 0;
    if (withSite) { for (const letter of "prompt.ac/") { SMALL[letter].forEach((row, y) => [...row].forEach((on, i) => { if (on === "1") dots.push([x + i, y + 2, 154]); })); x += SMALL[letter][0].length + 1; } x += 1; }
    HASH.forEach((row, y) => [...row].forEach((on, i) => { if (on === "1") dots.push([x + i, y, 255]); }));
    return { dots, width: x + HASH[0].length + 1 };
  }
  let mark = null; // { uid, code }: the canvas now showing, and the code it stands beside
  async function address() {
    const line = classic.code && vars.LoadCode === classic.code ? runtime.getInstanceByUid(CODE_LINE) : null;
    const showing = Boolean(line && line.isVisible && plain(line.text) === classic.code);
    let made = mark && runtime.getInstanceByUid(mark.uid);
    if (made && (!showing || mark.code !== classic.code)) { made.destroy(); made = mark = null; }
    if (!showing || made || mark?.making) return;
    const letters = classic.code.length * 8, right = line.x + line.width;
    const full = markPixels(true), { dots, width } = right - letters - full.width >= CODE_LEFT ? full : markPixels(false);
    const type = runtime.objects.WorkAroundCanvas;
    if (!type) return;
    mark = { making: true };
    try {
      made = type.createInstance(line.layer.name, right - letters - width, line.y + 5); // the letters sit five pixels down their line
      made.width = width; made.height = 8;
      mark = { uid: made.uid, code: classic.code, making: true };
      // The canvas has as many pixels as the screen gives it; ask, and fill each layout pixel.
      const image = await made.getImagePixelData(), scale = Math.max(1, Math.round(image.width / width));
      const data = new Uint8ClampedArray(image.width * image.height * 4);
      for (const [x, y, grey] of dots) for (let v = 0; v < scale; v++) for (let u = 0; u < scale; u++) {
        const at = ((y * scale + v) * image.width + x * scale + u) * 4;
        if (at + 3 < data.length && x * scale + u < image.width) data.set([grey, grey, grey, 255], at);
      }
      made.loadImagePixelData(new ImageData(data, image.width, image.height));
      mark.making = false;
    } catch (error) { console.warn("[nopaint] the address was not drawn", error); mark = null; }
  }
  let layout = null, painting = false, proposal = null, steps = 0;
  runtime.addEventListener("tick", () => {
    const now = where();
    if (now === null) return;
    address();
    const flag = !!vars.Painting;
    // The options are chosen a moment after the layout starts; keep the latest.
    if (proposal && !proposal.turn && now === proposal.layout && !flag) proposal.options = hint() || vars.Params || proposal.options;
    if (flag !== painting) {
      painting = flag;
      if (flag && proposal) proposal.painted = true;
      if (flag) say({ event: "painting", ...state(), ...(proposal ? { step: proposal.step, tool: proposal.tool } : {}) });
    }
    if (now !== layout) {
      // The proposal is over. The painting holds a Paint only once the game
      // has left the brush, so "painted" is said here and not when the flag falls.
      // An AI turn that never got its picture was not answered: it "failed".
      if (proposal) say({ event: proposal.painted ? "painted" : proposal.turn?.error ? "failed" : "refused", ...state(),
        step: proposal.step, tool: proposal.tool, options: proposal.options, ...(proposal.turn ? { turn: proposal.turn.id } : {}) });
      layout = now;
      // The AI brush's proposal is its turn.
      const turn = now === "AI" ? classic.turns.get(String(vars.Params)) : null;
      proposal = turn ? { step: ++steps, layout: now, tool: turn.name, options: turn.note, painted: false, turn }
        : NOT_BRUSHES.test(now) ? null : { step: ++steps, layout: now, tool: now, options: "", painted: false };
      say({ event: "layout", ...state() });
      if (proposal) say({ event: "proposed", step: proposal.step, tool: proposal.tool });
    }
  });
  runtime.addEventListener("afterprojectstart", () => say({ event: "ready", ...state() }));
});
