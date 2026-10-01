import {PieceVersions} from './piece-versions.mjs';
// Pick once per new workspace; the saved v0 keeps its color across launches.
const color = '#' + Math.floor(Math.random() * 0x1000000).toString(16).padStart(6, '0');
export const BASE_PIECE_SOURCE = `// Walkieware v0 — base color.
export function paint({wipe}) {
  wipe("${color}");
}
`;
export const isBasePiece = source => /^\/\/ Walkieware v0 — (?:base color|TV static)\./.test(source || '');

// Only untouched blank histories migrate. Archive their old identity instead
// of rewriting an already-synchronized immutable v0.
export function initializeBasePiece(storage, key) {
  const saved = storage.getItem(key + '-versions');
  const ledger = saved ? JSON.parse(saved) : null;
  if (storage.getItem(key)?.trim()) return false;
  if (ledger && !(ledger.head === 0 && ledger.versions?.length === 1 && !ledger.versions[0].source.trim())) return false;
  const identity = storage.getItem(key + '-thread');
  if (identity) {
    const parsed = JSON.parse(identity);
    storage.setItem('walkieware-archive-' + parsed.id, JSON.stringify({identity: parsed, ledger, source: ''}));
  }
  for (const suffix of ['-versions', '-thread', '-cloud-revision', '-cloud-ledger', '-attempt']) storage.removeItem(key + suffix);
  new PieceVersions(storage, key + '-versions', BASE_PIECE_SOURCE);
  storage.setItem(key, BASE_PIECE_SOURCE);
  return true;
}
