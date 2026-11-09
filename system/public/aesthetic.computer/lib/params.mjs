// Params, 2026.11.07
// The query-parameter ledger: every `?name` the runtime knows, in one table,
// read through the same helpers by boot, bios, parse and the disk worker.
// `node system/scripts/params-doc.mjs` renders it to docs/query-params.md.
//
// scope
//   view    presentation. bios re-applies `carry: bar` names on every history
//           write; the worker re-appends `carry: jump` names when a piece jumps
//   boot    consumed on the main thread (boot/bios) at start
//   auth    session hand-off; stripped before the worker ever sees it
//   piece   reaches the piece as `$.query`; gone from the bar after load
//   server  read by the HTML shell (netlify/functions/index.mjs)
// kind (how `parseQuery` types the value)
//   flag    present → true; `=false` does not turn it off, drop the name
//   number  Number(value); bare → true; a non-numeric value stays a string
//   size    bare or `true` → true; `WxH` → "WxH"; anything else stays a string
//   string  the value as written (bare → "")
//   token   opaque, as written; never logged, never remembered
// status  live = read somewhere today; unused = read nowhere, kept on purpose

const BAR = ["bar"];
const JUMP = ["jump"];
const BOTH = ["bar", "jump"];

// p(kind, scope, readers, doc, extra?) keeps each row to one line.
const p = (kind, scope, readers, doc, extra = {}) => ({
  kind,
  scope,
  readers,
  doc,
  status: "live",
  ...extra,
});

export const PARAMS = Object.freeze({
  // — view —
  nogap: p("flag", "view", ["boot", "bios", "disk", "server"], "Zero gap: the canvas fills its viewport, no rounded corners or checkerboard.", { carry: BOTH }),
  nolabel: p("flag", "view", ["boot", "bios", "disk", "kidlisp", "piece:clock"], "Hide the corner label / HUD.", { carry: BOTH }),
  nosource: p("flag", "view", ["boot", "bios", "disk"], "A KidLisp $code labels itself by name, not source.", { carry: BOTH }),
  autoreload: p("flag", "view", ["boot", "bios", "disk"], "Take piece updates automatically instead of showing the badge.", { carry: BAR }),
  shellhtml: p("flag", "view", ["boot", "bios", "disk"], "An HTML shell (prompt.ac) owns the prompt and corner chrome in DOM.", { carry: BAR }),
  density: p("number", "view", ["boot", "bios"], "Rendering density; remembered in localStorage outside device mode.", { carry: BAR }),
  zoom: p("number", "view", ["boot"], "Navigator zoom level; stored in localStorage and handed to bios, not applied.", { carry: BAR }),
  duration: p("number", "view", ["boot", "disk"], "Seconds for a timed piece's progress bar.", { carry: BAR }),
  daw: p("string", "view", ["bios", "piece:notepat", "piece:metronome", "piece:pedal"], "`daw=1`: hosted by Max/Ableton; pedal effects and Live sync come up.", { carry: BAR }),
  width: p("number", "view", [], "Preserved on history writes; read nowhere.", { carry: BAR, status: "unused" }),
  height: p("number", "view", [], "Preserved on history writes; read nowhere.", { carry: BAR, status: "unused" }),
  desktop: p("flag", "view", ["boot", "bios"], "Electron desktop pane: implies nogap, adds the `desktop` body class.", { carry: BAR }),
  device: p("flag", "view", ["boot", "bios", "disk"], "Display device (FF1): tv + nogap + noauth, density defaults to 8, no HUD overlays.", { carry: BOTH }),
  tv: p("flag", "view", ["boot", "bios", "disk"], "TV display: touch and keyboard input off, fonts awaited before first paint.", { carry: BOTH }),
  perf: p("flag", "view", ["boot", "disk"], "KidLisp performance / FPS HUD.", { carry: JUMP }),
  "auto-scale": p("flag", "view", ["boot", "disk"], "Scale density automatically to hold the target FPS.", { carry: JUMP, aliases: ["autoScale"] }),
  solo: p("flag", "view", ["boot", "bios", "disk"], "Lock the piece in place; navigation away is refused.", { carry: BOTH }),
  highlight: p("string", "view", ["boot", "bios", "disk"], "HUD background highlight; bare for the default grey or a color (`#rrggbb`, `r,g,b`, name).", { carry: BOTH }),
  maxfps: p("number", "view", ["boot", "bios"], "Cap the render frame rate, e.g. `maxfps=60`.", { carry: BAR }),
  spoofaudio: p("flag", "view", ["boot", "disk"], "Synthetic audio amplitude for headless captures.", { carry: JUMP }),
  noauth: p("flag", "view", ["boot", "bios"], "Skip authentication entirely (embedded editors, previews).", { carry: BAR }),
  preview: p("size", "view", ["boot", "bios", "disk"], "Render the piece's `preview()` thumbnail; bare, `true`, or `WxH`. `preview=walkieware` only hushes telemetry.", { carry: BAR }),
  icon: p("size", "view", ["boot", "bios", "disk"], "Render the piece's `icon()`; bare, `true`, or `WxH`.", { carry: BAR }),
  mime: p("string", "view", ["bios"], "`mime=1`: a mime card's player; starts muted, piece changes are not Back steps.", { carry: BAR }),
  reel: p("string", "view", ["bios"], "`reel=1` (with mime=1 and noauth): the whole mime feed in one runtime.", { carry: BAR }),

  // — boot —
  vscode: p("string", "boot", ["boot"], "`vscode=true`: running inside the VS Code extension; remembered in localStorage."),
  signup: p("flag", "boot", ["boot"], "Open the sign-up dialog once auth is ready."),
  supportSignUp: p("string", "boot", ["boot"], "Auth0 email-verified return (with success=true and code=success) → `notice=email-verified`."),
  supportForgotPassword: p("string", "boot", ["boot"], "Auth0 password-reset return; removed from the bar."),
  success: p("string", "boot", ["boot"], "Auth0 return flag; see supportSignUp."),
  message: p("string", "boot", ["boot"], "Auth0 return message; removed from the bar."),
  menuband: p("string", "boot", ["boot", "server"], "`menuband=true`: the Menu Band webview; no service worker, no update cache."),
  modulews: p("string", "boot", ["boot", "server"], "WebSocket module-loader host for local hot reload."),
  login: p("string", "boot", ["boot"], "`login=password`: open the password door instead of the social dialog."),
  nocache: p("flag", "boot", ["boot", "kidlisp"], "Sets window.acNOCACHE; KidLisp skips caching source (kidlisp.com editor)."),
  vst: p("string", "boot", ["bios"], "`vst=true`: running inside the VST plugin."),
  camrot: p("string", "boot", ["bios"], "Camera rotation: `cw`, `ccw`, `90`, `-90`, `180`, `0`, `none`."),
  camforce: p("string", "boot", ["bios"], "`camforce=1`: apply camrot even when the device says it is upright."),
  workerbundle: p("string", "boot", ["bios"], "`workerbundle=0|false`: run the unbundled disk.mjs worker."),
  crossfade: p("string", "boot", [], "Listed by boot; read nowhere.", { status: "unused" }),
  ac_campaign: p("string", "boot", ["bios"], "Campaign tag on aesel.app / notepat.com visits (analytics)."),
  "social-preview": p("flag", "boot", ["bios"], "Marks the visit as automated in analytics (social card render)."),
  "offline-render": p("flag", "boot", ["bios"], "Marks the visit as automated in analytics (offline render)."),
  "jev-vs-jev": p("flag", "boot", ["bios"], "Marks the visit as automated in analytics (jev demo)."),
  "ac-automation": p("flag", "boot", ["bios"], "Marks the visit as automated in analytics (puppet / frame fleet)."),

  // — auth —
  code: p("token", "auth", ["boot"], "Auth0 authorization code (with state), or `success` on the email-verified return."),
  state: p("token", "auth", ["boot"], "Auth0 callback state."),
  error: p("token", "auth", ["boot"], "Auth0 callback error."),
  error_description: p("token", "auth", ["boot"], "Auth0 callback error text."),
  "session-aesthetic": p("token", "auth", ["boot"], "A session handed in by a host (desktop app, VS Code); stored, then removed."),
  "session-sotce": p("token", "auth", ["boot"], "A sotce session handed in by the VS Code extension; stored, then removed."),

  // — piece —
  notice: p("string", "piece", ["disk", "piece:painting"], "A notice to flash on load: `success`, `cancel`, `email-verified`, or free text."),
  dool: p("flag", "piece", ["piece:prompt"], "Boot the prompt with the dool room already up (a minted object's doorway links `?dool` bare)."),
  test: p("string", "piece", ["piece:chat", "piece:laklok", "piece:mail"], "Test hooks (BroadcastChannel) for browser e2e runs."),
  ticket: p("string", "piece", ["piece:botce"], "A botce ticket key."),
  rom: p("string", "piece", ["piece:gameboy"], "A ROM link to load."),
  source: p("string", "piece", ["piece:give", "piece:handle", "piece:support"], "Where the visitor came from: `signup`, `homepage`."),
  thanks: p("string", "piece", ["piece:give"], "`thanks=1`: show the thank-you state."),
  amount: p("string", "piece", ["piece:support"], "Preset support amount (digits, optional two decimals)."),
  letter: p("string", "piece", ["piece:mail"], "A 24-hex letter id to open."),
  renderer: p("string", "piece", ["piece:notepat"], "`renderer=cpu`: force the CPU painter."),
  overlay: p("string", "piece", ["piece:notepat"], "`overlay=true|1`: overlay mode."),
  relayHandle: p("string", "piece", ["piece:notepat"], "Relay source handle."),
  relayMachine: p("string", "piece", ["piece:notepat"], "Relay source machine id."),
  relayAll: p("string", "piece", ["piece:notepat"], "`relayAll=1`: subscribe to every relay."),
  relay: p("string", "piece", ["piece:notepat"], "`relay=all`: subscribe to every relay."),
  amxd: p("string", "piece", ["piece:notepat"], "Hosted by the Max for Live device."),
  effect: p("string", "piece", ["piece:pedal"], "`effect=1`: effect mode."),
  novec: p("flag", "piece", ["piece:cancelok"], "Pixel-baked D-pad instead of the vector overlay."),
  objectPreview: p("string", "piece", ["piece:object-viewer"], "`objectPreview=1`: preview an object."),

  // — server —
  boot: p("string", "server", ["server"], "Boot screen theme for the HTML shell (default `empty`)."),
  noboot: p("flag", "server", ["server"], "No boot screen (theme `empty`)."),
  give: p("string", "server", ["server"], "`give=true|false`: force the give sheet on or off."),
  abtest: p("string", "server", ["server"], "`abtest=give`: the give A/B arm."),
});

// alias → canonical name, e.g. autoScale → auto-scale.
const ALIASES = Object.freeze(
  Object.fromEntries(
    Object.entries(PARAMS).flatMap(([name, entry]) =>
      (entry.aliases || []).map((alias) => [alias, name]),
    ),
  ),
);

export function canonical(name) {
  return ALIASES[name] || name;
}

export function entry(name) {
  return PARAMS[canonical(name)];
}

export function legitimate() {
  return [...Object.keys(PARAMS), ...Object.keys(ALIASES)];
}

export function names(scope) {
  return Object.keys(PARAMS).filter((name) => PARAMS[name].scope === scope);
}

// carry: "bar" | "jump" | undefined (every view name).
export function viewParams(carry) {
  return names("view").filter(
    (name) => !carry || (PARAMS[name].carry || []).includes(carry),
  );
}

export function authParams() {
  return names("auth");
}

// Type one raw value by its kind. Unknown names stay strings.
function typed(kind, raw) {
  if (kind === "flag") return true;
  if (kind === "number") {
    if (raw === "") return true;
    const n = Number(raw);
    return Number.isFinite(n) ? n : raw;
  }
  if (kind === "size") {
    if (raw === "" || raw === "true") return true;
    return raw; // "WxH", or a string that is not a size (preview=walkieware)
  }
  return raw; // string, token
}

// Split a search string into [name, rawValue, verbatimPart] without the
// re-encoding URLSearchParams does, so `cleanSearch` can hand parts back as
// they were written. `+` and percent-escapes in names are decoded.
function parts(search) {
  const s = (search || "").replace(/^\?/, "");
  if (!s) return [];
  return s.split("&").filter(Boolean).map((part) => {
    const eq = part.indexOf("=");
    const rawName = eq >= 0 ? part.slice(0, eq) : part;
    const rawValue = eq >= 0 ? part.slice(eq + 1) : "";
    return [decode(rawName), decode(rawValue), part];
  });
}

function decode(s) {
  try {
    return decodeURIComponent(s.replace(/\+/g, " "));
  } catch {
    return s;
  }
}

// parseQuery("?tv&density=8&autoScale=true&foo=bar")
//   → { tv: true, density: 8, "auto-scale": true, foo: "bar" }   (+ __unknown: ["foo"])
// Last write wins on a repeated name, like Object.fromEntries(URLSearchParams).
export function parseQuery(search) {
  const out = {};
  const unknown = [];
  for (const [rawName, rawValue] of parts(search)) {
    const name = canonical(rawName);
    const known = PARAMS[name];
    if (known) {
      out[name] = typed(known.kind, rawValue);
    } else {
      out[rawName] = rawValue;
      if (!unknown.includes(rawName)) unknown.push(rawName);
    }
  }
  Object.defineProperty(out, "__unknown", { value: unknown, enumerable: false });
  return out;
}

// Rebuild a search string keeping only some scopes. Parts are kept verbatim.
// Unknown names have the scope "unknown".
//   cleanSearch(location.search, { drop: ["auth"] })
//   cleanSearch(location.search, { keep: ["view", "piece", "unknown"] })
export function cleanSearch(search, { keep, drop } = {}) {
  const kept = parts(search)
    .filter(([rawName]) => {
      const scope = entry(rawName)?.scope || "unknown";
      if (drop && drop.includes(scope)) return false;
      if (keep && !keep.includes(scope)) return false;
      return true;
    })
    .map(([, , part]) => part);
  return kept.length ? "?" + kept.join("&") : "";
}

// serialize({ tv: true, highlight: "0,0,0", perf: false }) → "tv=true&highlight=0%2C0%2C0"
// false / null / undefined drop the name; true writes `name=true` (the
// runtime's convention, so `?nogap=true` round-trips).
export function serialize(obj) {
  const out = [];
  for (const [name, value] of Object.entries(obj || {})) {
    if (value === false || value === null || value === undefined) continue;
    out.push(`${name}=${value === true ? "true" : encodeURIComponent(String(value))}`);
  }
  return out.join("&");
}

// The names present in a search, canonical, for telemetry. Names only — never values.
export function present(search) {
  return Object.keys(parseQuery(search));
}

// A RAM store for the worker's life: what the view asked for, remembered past
// the search string it arrived in, so a jump can carry it along.
export function createParamStore() {
  const remembered = new Map();
  return {
    remember(name, value = true) {
      name = canonical(name);
      if (entry(name)?.kind === "token") return; // never hold a token
      if (value === false || value === null || value === undefined) {
        remembered.delete(name);
      } else {
        remembered.set(name, value);
      }
    },
    forget(name) {
      remembered.delete(canonical(name));
    },
    get(name) {
      return remembered.get(canonical(name));
    },
    has(name) {
      return remembered.has(canonical(name));
    },
    all() {
      return Object.fromEntries(remembered);
    },
    // Remembered view names with this carry, as an object for `serialize`.
    carried(carry) {
      const out = {};
      for (const name of viewParams(carry)) {
        if (remembered.has(name)) out[name] = remembered.get(name);
      }
      return out;
    },
    clear() {
      remembered.clear();
    },
  };
}

// Warn once per unknown name, at most `limit` names per runtime, so a piece
// with its own bespoke parameters keeps working and we still learn the name.
const warned = new Set();
export function warnUnknown(names, { warn = console.warn, limit = 20 } = {}) {
  for (const name of names || []) {
    if (warned.has(name) || warned.size >= limit) continue;
    warned.add(name);
    warn(`🧾 Unknown query parameter "${name}" — not in lib/params.mjs; the piece sees it as a string.`);
  }
}

// The markdown table for docs/query-params.md.
export function renderDoc() {
  const row = (cells) => `| ${cells.join(" | ")} |`;
  const lines = [
    row(["Parameter", "Kind", "Scope", "Carry", "Readers", "Status", "Effect"]),
    row(["---", "---", "---", "---", "---", "---", "---"]),
  ];
  for (const [name, e] of Object.entries(PARAMS)) {
    const label = e.aliases ? `\`${name}\` (\`${e.aliases.join("`, `")}\`)` : `\`${name}\``;
    lines.push(
      row([
        label,
        e.kind,
        e.scope,
        (e.carry || []).join(", ") || "—",
        e.readers.length ? e.readers.join(", ") : "—",
        e.status,
        e.doc.replace(/\|/g, "\\|"),
      ]),
    );
  }
  return lines.join("\n");
}
