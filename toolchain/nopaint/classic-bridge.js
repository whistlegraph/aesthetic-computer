// No Paint classic bridge, 2026.11.18
// Appended to scripts/c3runtime.js by toolchain/nopaint/patch-classic.mjs.
// It runs where the game runs (a worker when the browser has OffscreenCanvas)
// and speaks to the page over the BroadcastChannel "nopaint-classic".
//
// The game says:   {to:"page", event:"ready" | "layout" | "painting" | "painted", ...state}
// The page asks:   {to:"game", id, op, ...}  →  {to:"page", id, result} or {to:"page", id, error}
//
//   state                        where the game is
//   prompt {text}                type a line at the prompt and run it: "Flip:v"
//   call {name, args}            call an event-sheet function: "Paint", "No", "Clear"
//   set {name, value}            write a global variable
//   goto {layout}                go to a layout by name: "Index"
//   pixels {object, data}        read a canvas: hash and ink count, bytes if data is true
//   load {object, width, height, data}   write RGBA bytes into a canvas
//
// The brush test (tests/browser/brushbook.e2e.mjs) drives the game with
// these, and the page module that stores each step listens to "painted".
runOnStartup(async (runtime) => {
  if (typeof BroadcastChannel !== "function") return;
  const channel = new BroadcastChannel("nopaint-classic");
  const vars = runtime.globalVars;
  const say = (message) => channel.postMessage({ to: "page", ...message });
  const canvas = (name) => {
    const instance = runtime.objects[name]?.getFirstInstance();
    if (!instance || typeof instance.getImagePixelData !== "function") throw new Error(`no canvas named ${name}`);
    return instance;
  };
  const state = () => ({
    layout: runtime.layout.name, tool: vars.CurrentTool, params: vars.Params,
    prompt: vars.promptText, hint: vars.parameterHint, painting: !!vars.Painting, paused: !!vars.Paused,
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
  // One "painted" per Paint: the game raises its Painting flag while a brush
  // commits and lowers it when the painting is stored.
  let layout = null, painting = false;
  runtime.addEventListener("tick", () => {
    const now = runtime.layout.name;
    if (now !== layout) { layout = now; say({ event: "layout", ...state() }); }
    const flag = !!vars.Painting;
    if (flag !== painting) { painting = flag; say({ event: flag ? "painting" : "painted", ...state() }); }
  });
  runtime.addEventListener("afterprojectstart", () => say({ event: "ready", ...state() }));
});
