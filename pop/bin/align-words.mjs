// align-words.mjs — SHIM. The canonical implementation moved to
// spinging/lib/align-words.mjs (the spinging stack: text → spoken TTS →
// metadata → singing). This file stays so every existing importer
// (score-pitch.mjs, melody-bells.mjs, menuband/bin/sing-jingle.mjs,
// jungle/bin/karaoke-words.mjs) keeps resolving. New code should import
// from spinging/lib directly.
export { alignWords } from "../../spinging/lib/align-words.mjs";
