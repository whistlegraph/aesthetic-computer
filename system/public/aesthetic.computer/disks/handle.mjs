// Handle, 2026.02.13.09.00
// Per-character color editing with phone-sized touch controls.

const { floor, max, min } = Math;

let handleButtons = {}, handleLayout, handleMusic = true, handleScroll = 0, handleScrollMax = 0;
let handle; // User's handle
let colors = []; // RGB colors for @ sign + each character [{r, g, b}, ...]
let defaultColors = []; // Store defaults for reset
let selectedCharIndex = 0; // Currently selected character for editing
let saveBtn, resetBtn; // Buttons
let loading = true;
let saveStatus = null; // "saving", "saved", "error"
let lastSaveTime = 0;
let saving = false; // Guard against concurrent saves
let supporter = null, supportChecking = false, supportSource = "handle-colors", lifecycle = 0;
const DRAFT_KEY = "handle:color-preview";
const SAVE_COOLDOWN = 3000; // Minimum ms between save attempts
async function boot({ get, net, store, query, help, send }) {
  handleButtons = {}; handleLayout = null; handleMusic = true; handleScroll = 0; handleScrollMax = 0;
  send?.({ type: "onboarding:audio", content: { action: "enter" } });
  const mine = ++lifecycle;
  loading = true; saving = false; saveStatus = null; lastSaveTime = 0;
  supporter = null; supportChecking = false; saveBtn = null; resetBtn = null;
  selectedCharIndex = 0;
  supportSource = query?.source === "signup" ? "signup" : "handle-colors";
  // Load user's handle
  handle = store["handle"];

  if (!handle) {
    // No handle set - jump to prompt to set one
    return {
      jump: "prompt",
    };
  }
  void checkSupport(net, help);
  let draft;
  try { draft = await store.retrieve?.(DRAFT_KEY); } catch { /* Preview storage is optional. */ }
  if (mine !== lifecycle) return;

  // Initialize default colors (white for all characters) or load from API
  const handleWithAt = "@" + handle;

  // Fetch existing color data from server
  console.log(`🎨 Loading colors for handle: ${handle}`);
  fetch(`/.netlify/functions/handle-colors?handle=${encodeURIComponent(handle)}`, { cache: "no-store" })
    .then((response) => response.json())
    .then((data) => {
      if (mine !== lifecycle) return;
      console.log(`🎨 Received color data:`, data);
      if (data.colors && data.colors.length === handleWithAt.length) {
        // Load existing colors
        colors = data.colors.map((c) => ({
          r: c.r ?? 255,
          g: c.g ?? 255,
          b: c.b ?? 255,
        }));
        console.log(`🎨 Loaded ${colors.length} custom colors`);
      } else {
        // Initialize with default magenta (matching prompt button theme)
        colors = handleWithAt.split("").map(() => ({ r: 255, g: 100, b: 255 }));
      }
      // Store defaults for reset functionality
      defaultColors = colors.map((c) => ({ ...c }));
      if (draft?.handle === handle && Date.now() >= draft.at && Date.now() - draft.at < 86400000 &&
          draft.colors?.length === handleWithAt.length && draft.colors.every(c =>
            c && [c.r, c.g, c.b].every(v => Number.isInteger(v) && v >= 0 && v <= 255)))
        colors = draft.colors;
      loading = false;
      help.repeat();
    })
    .catch((err) => {
      if (mine !== lifecycle) return;
      console.warn("⚠️ Failed to load handle colors, using defaults:", err);
      colors = handleWithAt.split("").map(() => ({ r: 255, g: 100, b: 255 }));
      defaultColors = colors.map((c) => ({ ...c }));
      loading = false;
      help.repeat();
    });
}

async function checkSupport(net, help) {
  const mine = lifecycle;
  if (supportChecking) return;
  supportChecking = true;
  try {
    const result = await net.userRequest("GET", "/api/give-status");
    if (mine === lifecycle) supporter = result.status === 200 && typeof result.monthlySupporter === "boolean" ? result.monthlySupporter : null;
  } catch { if (mine === lifecycle) supporter = null; }
  finally { if (mine === lifecycle) { supportChecking = false; saveBtn = null; help.repeat(); } }
}

function paint($) {
  const { ink, wipe, screen, ui } = $;
  wipe(15, 15, 20);
  if (loading) { ink(255).write('Loading...', { center: 'xy', screen }); return; }
  const w = Math.min(280, screen.width - 24), x = floor((screen.width - w) / 2);
  const compact = screen.height < 300;
  const text = (value, tx, ty, color = [230, 220, 241]) => ink(color).write(value, { x: tx, y: ty }, undefined, undefined, false, 'MatrixChunky8');
  text('Handle colors', x, 36 - handleScroll, [255, 113, 191]);
  const chars = ('@' + handle).split(''), cell = 22;
  const columns = Math.max(1, floor(w / cell)), rows = Math.ceil(chars.length / columns);
  handleLayout = { chars: [], sliders: [], buttons: [] };
  chars.forEach((char, i) => {
    const bx = x + (i % columns) * cell, by = 57 + floor(i / columns) * 27 - handleScroll;
    const box = { x: bx, y: by, w: cell, h: 25 };
    handleLayout.chars.push(box);
    if (i === selectedCharIndex) ink(80, 62, 92).box(box, 'fill');
    const c = colors[i]; text(char, bx + floor((cell - 6) / 2), by + 8, [c.r, c.g, c.b]);
    if (i === selectedCharIndex) ink(135, 233, 191).box(bx + 2, by + 23, cell - 4, 1, 'fill');
  });
  let y = 63 + rows * 27 - handleScroll;
  if (!compact) { text('Tap a letter. Play with its color.', x, y); y += 19; }
  const sliderW = w - 54, c = colors[selectedCharIndex];
  ['r', 'g', 'b'].forEach((key, i) => {
    const box = { x: x + 13, y, w: sliderW, h: 24 };
    handleLayout.sliders.push({ ...box, key });
    text(key.toUpperCase(), x, y + 8);
    ink(35, 30, 43).box(box, 'fill');
    const color = [[255, 90, 113], [120, 235, 140], [100, 155, 255]][i];
    const filled = floor(c[key] / 255 * (sliderW - 1));
    ink(color).box(box.x, y + 8, filled, 8, 'fill');
    ink(255).box(box.x + filled - 1, y + 3, 3, 18, 'fill');
    text(String(c[key]).padStart(3, ' '), x + w - 27, y + 8); y += 31;
  });
  const button = (id, label, bx, by, bw) => {
    const b = handleButtons[id] ||= new ui.Button(); Object.assign(b.box, { x: bx, y: by, w: bw, h: 26 });
    b.disabled = saving || supportChecking || by < 34 || by + 26 > screen.height; handleLayout.buttons.push({ id, b });
    ink(id === 'save' ? [60, 99, 68] : [47, 35, 62]).box(b.box, 'fill');
    ink(160, 150, 175).box(b.box, 'outline'); text(label, bx + 5, by + 9);
  };
  const status = saveStatus === 'saving' ? 'Saving...' : saveStatus === 'saved' ? 'Saved!' : saveStatus === 'error' ? 'Could not save. Try again.' : supportChecking ? 'Checking support...' : supporter === false ? 'Monthly support to save.' : supporter === null ? 'Support check unavailable.' : 'Your colors travel across AC.';
  text(status, x, y + 5, [135, 233, 191]); y += 25;
  button('reset', 'Reset', x, y, 43);
  button('save', supporter === true ? 'Save colors' : supporter === false ? 'Support monthly' : 'Check support', x + 49, y, w - 49);
  handleScrollMax = max(0, y + handleScroll + 34 - screen.height);
  ink(15, 15, 20).box(0, 0, screen.width, 32, 'fill');
  button('music', handleMusic ? 'Music on' : 'Music off', screen.width - 80, 5, 72);
  handleButtons.music.disabled = false;
}
function act($) {
  const { event: e, screen, net, help, jump, sound, store, send } = $;
  if (loading || !handleLayout) return;
  if (e.is('scroll') || (e.is('draw:1') && !handleLayout.sliders.some(b => e.x >= b.x && e.x < b.x + b.w))) {
    handleScroll = max(0, min(handleScrollMax, handleScroll - (e.is('scroll') ? e.y : e.delta.y))); help.repeat(); return;
  }
  const hit = b => b.y >= 32 && e.x >= b.x && e.x < b.x + b.w && e.y >= b.y && e.y < b.y + b.h;
  const tap = () => send?.({ type: 'onboarding:audio', content: { action: 'tap' } });
  if (e.is('touch')) {
    const i = handleLayout.chars.findIndex(hit);
    if (i >= 0) { selectedCharIndex = i; tap(); help.repeat(); return; }
  }
  for (const { id, b } of handleLayout.buttons) b.act(e, { push: () => {
    tap();
    if (id === 'music') send?.({ type: 'onboarding:audio', content: { action: 'toggle' } });
    if (id === 'reset') { colors = defaultColors.map(c => ({ ...c })); help.repeat(); }
    if (id === 'save') {
      if (supporter === null) { void checkSupport(net, help); return; }
      if (supporter === false) {
        store[DRAFT_KEY] = { handle, colors, at: Date.now() }; store.persist(DRAFT_KEY);
        jump(`support?source=${supportSource}`); return;
      }
      saveColors(net, help, sound, store);
    }
  } });
  if (e.is('touch') || e.is('draw')) {
    const slider = handleLayout.sliders.find(hit);
    if (slider) {
      colors[selectedCharIndex][slider.key] = max(0, min(255, floor((e.x - slider.x) / (slider.w - 1) * 255)));
      if (e.is('touch')) tap();
      help.repeat();
    }
  }
}
function receive({ type, content }, $) { if (type === 'onboarding:audio-state') { handleMusic = content.enabled; $.needsPaint(); } }

async function saveColors(net, help, sound, store) {
  const mine = lifecycle;
  // Rate limit: prevent concurrent saves and enforce cooldown
  const now = Date.now();
  if (saving || (now - lastSaveTime < SAVE_COOLDOWN)) return;
  saving = true;

  saveStatus = "saving";
  lastSaveTime = now;
  help.repeat();

  try {
    const payload = {
      handle,
      colors: colors.map((c) => ({ r: c.r, g: c.g, b: c.b })),
    };
    const response = await net.userRequest("POST", "/.netlify/functions/handle-colors", payload);
    if (mine !== lifecycle) return;

    if (response.status === 200) {
      saveStatus = "saved";
      // Update defaults to current colors after successful save
      defaultColors = payload.colors.map((c) => ({ ...c }));
      void store.delete?.(DRAFT_KEY);
      sound.synth({
        type: "sine",
        tone: 600,
        duration: 0.1,
        volume: 0.3,
      });
    } else if (response.code === "monthly_support_required") {
      supporter = false; saveBtn = null; saveStatus = null;
    } else {
      saveStatus = "error";
      console.error("❌ Failed to save handle colors. Status:", response.status, "Response:", response);
    }
  } catch (err) {
    if (mine !== lifecycle) return;
    saveStatus = "error";
    console.error("❌ Error saving handle colors:", err);
  }

  saving = false;
  lastSaveTime = Date.now();
  help.repeat();

  // Clear status after 2.5 seconds
  setTimeout(() => {
    if (mine === lifecycle && Date.now() - lastSaveTime >= 2500) {
      saveStatus = null;
      help.repeat();
    }
  }, 2500);
}

function leave($) { lifecycle++; $?.send?.({ type: "onboarding:audio", content: { action: "leave" } }); }
export { boot, paint, act, receive, leave };

function meta() {
  return {
    title: "Handle",
    desc: "Customize your @handle colors.",
  };
}

export { meta };
