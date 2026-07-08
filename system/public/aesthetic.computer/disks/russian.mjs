// Russian, 2026.07.07
// Translate any language to Russian.

import * as T from "../disks/common/translate.mjs";

const LANG = "russian";

function boot($) { T.boot(LANG, $); }
function paint($) { T.paint(LANG, $); }
function act($) { T.act(LANG, $); }
function meta() { return T.meta(LANG); }

export { boot, paint, act, meta };
