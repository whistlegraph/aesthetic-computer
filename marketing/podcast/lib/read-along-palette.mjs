// Stable topic palettes: no model call or new randomness during playback.
const palettes = [
  { name: 'paper', words: /\b(paper|pages?|books?|catalog|notepad|legal pad|letters?|writing|essay|library|print)\b/gi,
    background: [246, 235, 196], foreground: [48, 43, 63], highlight: [151, 39, 77], link: [37, 76, 124] },
  { name: 'music', words: /\b(music|songs?|singing|dance|beats?|melody|audio|sound|voice|semitones?|distrokid|sidechain)\b/gi,
    background: [230, 222, 246], foreground: [46, 37, 65], highlight: [116, 42, 114], link: [32, 76, 117] },
  { name: 'play', words: /\b(games?|doom|wad|wads|playable|players?|keyboard|controls?|candy|lollipop|health|match)\b/gi,
    background: [213, 238, 223], foreground: [30, 57, 51], highlight: [145, 48, 48], link: [54, 60, 131] },
  { name: 'network', words: /\b(network|server|browser|protocol|database|account|posts?|feed|handle|web|chat|community)\b/gi,
    background: [215, 235, 247], foreground: [34, 48, 68], highlight: [136, 39, 101], link: [43, 67, 126] },
  { name: 'garden', words: /\b(flowers?|garden|trees?|sun|sea|rain|walk|outside|nature|daisy|city|room)\b/gi,
    background: [249, 223, 211], foreground: [60, 43, 42], highlight: [44, 92, 61], link: [95, 49, 111] },
];
export function contentPalette(text) {
  const scores = palettes.map(p => [...text.matchAll(p.words)].length +
    (p.name === 'paper' && /\b(legal pad|card catalog|notepad)\b/i.test(text) ? 3 : 0));
  const best = Math.max(...scores);
  let hash = 2166136261;
  for (const ch of text.toLowerCase()) hash = Math.imul(hash ^ ch.codePointAt(0), 16777619) >>> 0;
  const { words, ...palette } = palettes[best ? scores.indexOf(best) : hash % palettes.length];
  return palette;
}
export function readAlongThemes(body, title = '') {
  let start = 0;
  return body.split('\n\n').map((text, i) => {
    const theme = { start, end: start + text.length, ...contentPalette(i ? text : `${title} ${text}`) };
    start += text.length + 2;
    return theme;
  });
}
