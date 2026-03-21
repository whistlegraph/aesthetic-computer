// Spanish, 2026.01.19
// Translate any language to Spanish.

import * as T from "../disks/common/translate.mjs";

const LANG = "spanish";

function boot($) { T.boot(LANG, $); }
function paint($) { T.paint(LANG, $); }
function act($) { T.act(LANG, $); }
function meta() { return T.meta(LANG); }

export { boot, paint, act, meta };
