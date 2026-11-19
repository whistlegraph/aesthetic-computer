// A guest's name, from their server-issued identity: the same owner reads
// the same name in every room, and two words beat "guest 1".
const FIRST = ['moss', 'cobalt', 'amber', 'velvet', 'quartz', 'ember', 'dusk', 'mint',
  'tidal', 'paper', 'lunar', 'copper', 'neon', 'fern', 'opal', 'static'];
const SECOND = ['newt', 'heron', 'comet', 'fox', 'moth', 'otter', 'lark', 'pike',
  'lynx', 'wren', 'crab', 'eel', 'ibis', 'mole', 'vole', 'gull'];
export function guestName(owner) {
  if (typeof owner !== 'string' || !owner) return null;
  let h = 2166136261;
  for (const ch of owner) h = Math.imul(h ^ ch.charCodeAt(0), 16777619) >>> 0;
  return `${FIRST[h & 15]}-${SECOND[(h >>> 4) & 15]}`;
}
