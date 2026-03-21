// Piece in Piece, 22.10.13.01.27 
// A test that imports multiple pieces and runs them simultaneously.

// TODO: How could each piece get its own buffer?
// TODO: How could each piece better share an existing buffer? 

// TODO: Smear and Rect cannot combine because there is no shared intermediate
//       layer / painting for them to both use that gets autocleared?

import * as line from './line.mjs';
import * as rect from './rect.mjs';
import * as smear from './smear.mjs';

let painting;

function boot($api) {

  // `line` and `rect` both use the `nopaint` system which means
  // we need to supply them with a painting, but we won't implement
  // any other core system hooks like writing to the system buffer
  // or should system.painting always be available?

  painting = $api.painting(256, 256, p => p.noise16())
  $api.system.painting = painting;
  line?.boot?.($api);
  rect?.boot?.($api);
}

function paint($api) {
  $api.system = { painting };
  line?.paint?.($api);
  rect?.paint?.($api);
  smear?.paint?.($api);
}

function act($api) {
  $api.system = { painting };
  line?.act?.($api);
  rect?.act?.($api);
}

function sim($api) {
  line?.sim?.($api);
  rect?.sim?.($api);
}

function beat($api) {
  line?.beat?.($api);
  rect?.beat?.($api);
}

function leave($api) {
  line?.leave?.($api);
  rect?.leave?.($api);
}

export { boot, sim, paint, act, beat, leave };