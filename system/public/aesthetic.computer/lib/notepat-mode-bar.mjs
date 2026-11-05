export const MODE_BAR_HEIGHT = 21;
export function notepatModeBar(width, { mode = "main", sound = "piano", octave = 4, bpm = 180, dark = true, layout = "piano" } = {}) {
  const groups = {
    main: [["sound", sound], ["octave", String(octave)], ["fx", "FX"], ["tempo", String(bpm)]],
    fx: [["back", "FX"], ["slide", "slide"], ["room", "room"], ["glitch", "glitch"], ["quick", "quick"], ["theme", dark ? "light" : "dark"], ["layout", layout === "piano" ? "grid" : "keys"]],
    tempo: [["back", "<"], ["minus", "-"], ["metro", String(bpm)], ["plus", "+"]],
  };
  const labels = (groups[mode] || groups.main).map(([id, label]) => ({ id, label }));
  let padding=6,gap=2;
  const total = () => labels.reduce((n, b) => n + b.label.length * 6 + padding, 0) + (labels.length - 1) * gap;
  if (total() > width - 4) for (const b of labels) {
    if (["slide", "room", "glitch", "quick"].includes(b.id)) b.label = {slide:"sld", room:"rm", glitch:"glt", quick:"qk"}[b.id];
    if (b.id === "sound") b.label = b.label.slice(0, 3);
  }
  if (total() > width - 4) for (const b of labels) if (["slide", "room", "glitch", "quick", "theme", "layout"].includes(b.id)) b.label = b.label[0];
  if(total()>width-4) { padding=4; gap=1; }
  let x = Math.max(0,width-total()-2);
  const controls = labels.map(b => { const w = b.label.length * 6 + padding; const item = { ...b, x, y: 2, w, h: 17 }; x += w + gap; return item; });
  const header = notepatHeader(controls[0].x);
  return { height: MODE_BAR_HEIGHT, logo:header.logo, header, controls };
}
export function notepatHeader(right) {
  const logo=right>=84?'notepat.com':right>=60?'notepat':'';
  const x=logo==='notepat.com'?75:logo?50:3;
  return {logo,piano:null,viz:{x,y:3,w:Math.max(0,right-x-3),h:15}};
}

const WHITE_KEYS=['C','D','E','F','G','A','B'];
const BLACK_KEYS=[{note:'C#',afterWhite:0},{note:'D#',afterWhite:1},{note:'F#',afterWhite:3},{note:'G#',afterWhite:4},{note:'A#',afterWhite:5}];
const PIANO_KEYS={
  7:{whiteKeys:WHITE_KEYS,blackKeys:BLACK_KEYS},
  14:{whiteKeys:[...WHITE_KEYS,...WHITE_KEYS.map(n=>'+'+n)],blackKeys:[...BLACK_KEYS,...BLACK_KEYS.map(k=>({note:'+'+k.note,afterWhite:k.afterWhite+7}))]},
};
export const notepatPianoKeys=whiteCount=>PIANO_KEYS[whiteCount===14?14:7];
