export const MODE_BAR_HEIGHT = 21;
export function notepatModeBar(width, { mode = "main", sound = "sine", octave = 4, bpm = 180 } = {}) {
  const groups = {
    main: [["sound", sound], ["octave", String(octave)], ["fx", "FX"], ["tempo", String(bpm)]],
    fx: [["back", "FX"], ["slide", "slide"], ["room", "room"], ["glitch", "glitch"], ["quick", "quick"]],
    tempo: [["back", "<"], ["minus", "-"], ["metro", String(bpm)], ["plus", "+"]],
  };
  const labels = (groups[mode] || groups.main).map(([id, label]) => ({ id, label }));
  const total = () => labels.reduce((n, b) => n + b.label.length * 6 + 6, 0) + (labels.length - 1) * 2;
  if (total() > width - 4) for (const b of labels) {
    if (["slide", "room", "glitch", "quick"].includes(b.id)) b.label = {slide:"sld", room:"rm", glitch:"glt", quick:"qk"}[b.id];
    if (b.id === "sound") b.label = b.label.slice(0, 3);
  }
  if (total() > width - 4) for (const b of labels) if (["slide", "room", "glitch", "quick"].includes(b.id)) b.label = b.label[0];
  const logo = width - total() >= 84 ? "notepat.com" : "";
  let x = logo ? width - total() - 2 : Math.max(0, Math.floor((width - total()) / 2));
  const controls = labels.map(b => { const w = b.label.length * 6 + 6; const item = { ...b, x, y: 2, w, h: 17 }; x += w + 2; return item; });
  return { height: MODE_BAR_HEIGHT, logo, controls };
}
