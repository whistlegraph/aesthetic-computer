// Slab AppDelegate.statusDecor's Aesel palette (16-bit RGB), without blinking.
// The same phase language is used by MIME's native Whistlegraph workspace.
const colors = {
  blank: { light: [[65535,65535,65535],[8000,8000,8000],[20000,20000,20000]], dark: [[0,0,0],[60000,60000,60000],[50000,50000,50000]] },
  working: { light: [[39000,64000,45000],[1200,22000,6000],[1000,53000,11000]], dark: [[600,12000,3200],[40000,64000,47000],[14000,62000,30000]] },
  rendering: { light: [[65535,54500,60000],[26000,2500,15000],[54000,6000,31000]], dark: [[9000,1800,5800],[62000,44000,54000],[58000,18000,40000]] },
  awaiting: { light: [[65535,59000,45000],[26000,13000,0],[65535,35000,0]], dark: [[19000,10500,900],[65535,58000,38000],[65535,46000,10000]] },
  complete: { light: [[65535,45500,44000],[30000,1200,1800],[62000,5000,5000]], dark: [[17000,1900,3000],[65535,45000,43000],[65535,11000,10000]] },
  interrupted: { light: [[58000,52000,65535],[22000,8000,34000],[34000,12000,60000]], dark: [[8500,2800,13500],[54000,46000,64000],[40000,22000,60000]] },
  stale: { light: [[65535,54000,54000],[26000,1500,4000],[60000,4000,7000]], dark: [[17000,1400,2600],[65535,42000,42000],[65535,20000,20000]] },
};
const clamp = value => Math.min(1, Math.max(0, value));
const mix = (a, b, t) => a.map((v, i) => v * (1 - t) + b[i] * t);
const hex = rgb => "#" + rgb.map(v => Math.round(v).toString(16).padStart(2, "0")).join("");
const luminance = rgb => rgb.map(v => v / 255).map(v => v <= .04045 ? v / 12.92 : ((v + .055) / 1.055) ** 2.4)
  .reduce((sum, v, i) => sum + v * [.2126, .7152, .0722][i], 0);
const contrast = (a, b) => (Math.max(luminance(a), luminance(b)) + .05) / (Math.min(luminance(a), luminance(b)) + .05);
function readable(ink, background, minimum) {
  const black = [0, 0, 0], white = [255, 255, 255];
  const target = contrast(black, background) > contrast(white, background) ? black : white;
  for (let i = 0; i <= 100; i++) {
    const candidate = mix(ink, target, i / 100).map(Math.round);
    if (contrast(candidate, background) >= minimum) return candidate;
  }
  return target;
}

// MIME's Los Angeles sun: one clock for the web surfaces, including dusk/dawn.
export function workspaceDaylight(now = Date.now()) {
  const deg = Math.PI / 180, days = Number(now) / 86400000 + 2440587.5 - 2451545;
  const longitude = (280.46 + .9856474 * days) % 360;
  const anomaly = (357.528 + .9856003 * days) % 360;
  const ecliptic = longitude + 1.915 * Math.sin(anomaly * deg) + .02 * Math.sin(2 * anomaly * deg);
  const obliquity = 23.439 - .0000004 * days;
  const ascension = Math.atan2(Math.cos(obliquity * deg) * Math.sin(ecliptic * deg), Math.cos(ecliptic * deg)) / deg;
  const declination = Math.asin(Math.sin(obliquity * deg) * Math.sin(ecliptic * deg));
  const sidereal = 18.697374558 + 24.06570982441908 * days;
  const hour = ((sidereal * 15 - 118.2437 - ascension + 540) % 360) - 180;
  const latitude = 34.0522 * deg;
  const altitude = Math.asin(Math.sin(latitude) * Math.sin(declination) + Math.cos(latitude) * Math.cos(declination) * Math.cos(hour * deg)) / deg;
  const light = clamp((altitude + 6) / 14);
  return light * light * (3 - 2 * light);
}

export function workspacePalette(state = "blank", day = workspaceDaylight()) {
  if (!Object.hasOwn(colors, state)) state = "blank";
  day = Number.isFinite(day) ? clamp(day) : 1;
  const palette = colors[state];
  const rgb = value => {
    let [r, g, b] = value;
    if (state !== "complete") { r = Math.round(r * .86); b = Math.min(65535, Math.round(b * 1.14) + 2600); }
    return [r, g, b].map(v => Math.round(v / 257));
  };
  const background = mix(rgb(palette.dark[0]), rgb(palette.light[0]), day).map(Math.round);
  const inks = palette[day >= .5 ? "light" : "dark"];
  return {
    state, day, dark: luminance(background) < .18,
    background: hex(background),
    ink: hex(readable(rgb(inks[1]), background, 4.5)),
    accent: hex(readable(rgb(inks[2]), background, 3)),
    canvas: hex(mix(background, day >= .5 ? [0,0,0] : [255,255,255], .08)),
  };
}

export const WORKSPACE_THEME_CACHE = "ac-workspace-theme";

export function followWorkspaceTheme({ root = document.documentElement, meta = document.querySelector('meta[name="theme-color"]'), now = Date.now } = {}) {
  let state = "blank", signature;
  const update = () => {
    const palette = workspacePalette(state, workspaceDaylight(now()));
    const next = JSON.stringify(palette);
    if (next === signature) return;
    signature = next;
    root.dataset.phase = state;
    root.dataset.theme = palette.dark ? "dark" : "light";
    root.style.colorScheme = palette.dark ? "dark" : "light";
    for (const name of ["background", "ink", "accent", "canvas"]) root.style.setProperty(`--${name}`, palette[name]);
    meta?.setAttribute("content", palette.background);
    // A page's inline boot script repaints from this before its modules load.
    try { localStorage.setItem(WORKSPACE_THEME_CACHE, JSON.stringify({ at: now(), palette })); } catch {}
  };
  update();
  // Colour transitions start once the first palette has painted, so the
  // switch from the stylesheet's default never fades in as a flash.
  const raf = globalThis.requestAnimationFrame;
  if (raf) raf(() => raf(() => { root.dataset.themeReady = ""; }));
  else root.dataset.themeReady = "";
  const interval = setInterval(update, 60000);
  const doc = root.ownerDocument;
  doc?.addEventListener("visibilitychange", update);
  return {
    set(next) { state = Object.hasOwn(colors, next) ? next : "blank"; update(); },
    dispose() { clearInterval(interval); doc?.removeEventListener("visibilitychange", update); },
  };
}
