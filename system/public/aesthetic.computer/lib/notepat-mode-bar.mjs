export const MODE_BAR_HEIGHT = 21;
export function notepatModeBar(width, { mode = "main", sound = "piano", octave = 4, bpm = 180, dark = true } = {}) {
  const groups = {
    main: [["sound", sound], ["octave", String(octave)], ["fx", "FX"], ["tempo", String(bpm)]],
    fx: [["back", "FX"], ["slide", "slide"], ["room", "room"], ["glitch", "glitch"], ["quick", "quick"], ["theme", dark ? "light" : "dark"]],
    tempo: [["back", "<"], ["minus", "-"], ["metro", String(bpm)], ["plus", "+"]],
  };
  const labels = (groups[mode] || groups.main).map(([id, label]) => ({ id, label }));
  const total = () => labels.reduce((n, b) => n + b.label.length * 6 + 6, 0) + (labels.length - 1) * 2;
  if (total() > width - 4) for (const b of labels) {
    if (["slide", "room", "glitch", "quick"].includes(b.id)) b.label = {slide:"sld", room:"rm", glitch:"glt", quick:"qk"}[b.id];
    if (b.id === "sound") b.label = b.label.slice(0, 3);
    if (b.id === "theme") b.label = dark ? "day" : "dim";
  }
  if (total() > width - 4) for (const b of labels) if (["slide", "room", "glitch", "quick", "theme"].includes(b.id)) b.label = b.label[0];
  let x = Math.max(0,width-total()-2);
  const controls = labels.map(b => { const w = b.label.length * 6 + 6; const item = { ...b, x, y: 2, w, h: 17 }; x += w + 2; return item; });
  const header = notepatHeader(controls[0].x);
  return { height: MODE_BAR_HEIGHT, logo:header.logo, header, controls };
}
export function notepatHeader(right) {
  // Reserve a playable octave and a scope before spending space on the wordmark.
  const logo = right >= 119 ? 'notepat.com' : right >= 92 ? 'notepat' : '';
  let x=logo==='notepat.com'?75:logo?50:3;
  const room=right-x-3;
  const whiteCount=room>=98?14:7;
  const keyWidth=Math.min(6,Math.floor((room-14)/whiteCount));
  const piano=keyWidth>=3?{x,y:3,w:whiteCount*keyWidth,h:15,keyWidth,whiteCount}:null;
  if(piano) x+=piano.w+3;
  return {logo,piano,viz:{x,y:3,w:Math.max(0,right-x-3),h:15}};
}
