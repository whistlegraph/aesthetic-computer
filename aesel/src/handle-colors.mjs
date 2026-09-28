// AC's character palette (nom.mjs), overridden by saved @handle colors.
const colors=[[255,105,97],[255,170,80],[255,225,90],[150,225,90],[90,220,140],[90,215,210],[95,180,255],[150,150,255],[200,140,255],[255,140,210]];
export function handleCharacterColors(handle,saved){return Array.from(handle).map((character,index)=>{const custom=saved?.[index];if(custom&&[custom.r,custom.g,custom.b].every(n=>Number.isFinite(n)&&n>=0&&n<=255))return[custom.r,custom.g,custom.b].map(Math.round);const ch=character.toLowerCase();return ch>='a'&&ch<='z'?colors[(ch.charCodeAt(0)-97)%10]:ch>='0'&&ch<='9'?colors[Number(ch)]:[220,180,255];});}
export async function fetchHandleColors(handle,{fetch=globalThis.fetch}={}){const r=await fetch('https://aesthetic.computer/api/handle-colors?handle='+encodeURIComponent(handle.replace(/^@/,'')),{signal:AbortSignal.timeout(5000)});if(!r.ok)throw new Error('Handle colors unavailable');const data=await r.json();return handleCharacterColors(handle,data.colors);}

// Colour words a person would type, as the site stores them.
const NAMED = {
  red: [230, 50, 50], orange: [255, 140, 0], yellow: [250, 210, 40], lime: [150, 220, 50],
  green: [40, 180, 80], teal: [0, 160, 160], cyan: [40, 200, 230], blue: [50, 110, 230],
  purple: [140, 80, 210], magenta: [220, 60, 200], pink: [255, 120, 180], white: [245, 245, 245],
  gray: [150, 150, 150], grey: [150, 150, 150], black: [20, 20, 20], brown: [140, 90, 50], gold: [230, 180, 40],
};
function parseColor(word) {
  const w = String(word).trim().toLowerCase();
  if (NAMED[w]) return NAMED[w];
  const hex = /^#?([0-9a-f]{3}|[0-9a-f]{6})$/.exec(w)?.[1];
  if (!hex) throw new Error(`not a colour: ${word} (try a name like orange or a hex like #ff8800)`);
  const full = hex.length === 3 ? [...hex].map((c) => c + c).join("") : hex;
  return [0, 2, 4].map((i) => parseInt(full.slice(i, i + 2), 16));
}
// One colour per character of `@handle`, cycling through the words given.
export function handleColorPlan(handle, words) {
  const palette = words.map(parseColor);
  return Array.from(handle).map((_, i) => { const [r, g, b] = palette[i % palette.length]; return { r, g, b }; });
}
