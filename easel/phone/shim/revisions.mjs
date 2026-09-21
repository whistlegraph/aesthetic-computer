// Parse modules without evaluating user code in the session web view.
import {parse} from '../../src/vendor/acorn.mjs';
export async function validatePieceSource(source, file) {
  if (typeof source !== 'string' || !source.trim()) throw new Error('The piece is empty.');
  if (!String(file).endsWith('.mjs')) return;
  try { parse(source, {ecmaVersion:'latest',sourceType:'module'}); }
  catch(error) { throw new Error(`Invalid JavaScript; previous preview kept. ${error.message}`); }
}

// The desktop module also exports `PieceRevisions`, a local snapshot store under
// ~/.local/share/easel/history. The phone keeps its history somewhere else, so
// this is a stub that fails loudly rather than a half-implementation that looks
// like it is saving and is not.
export class PieceRevisions {
  constructor() {
    throw new Error("PieceRevisions is not available in the phone client.");
  }
}
