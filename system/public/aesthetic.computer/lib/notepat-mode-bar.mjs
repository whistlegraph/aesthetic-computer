export const MODE_BAR_HEIGHT = 21;
export function notepatModeBar(width, { sound = "piano", octave = 4, dark = true, layout = "piano" } = {}) {
  const labels = [["sound", sound], ["octave", String(octave)],
    ["layout", "  "]]
    .map(([id, label]) => ({ id, label }));
  let padding=6,gap=2;
  const total = () => labels.reduce((n, b) => n + b.label.length * 6 + padding, 0) + (labels.length - 1) * gap;
  // Keep the corner wordmark on phone widths; use initials only when needed.
  if (total() > width - (width >= 140 ? 90 : 4)) for (const b of labels) {
    if (b.id === "sound") b.label = b.label.slice(0, 3);
  }
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
