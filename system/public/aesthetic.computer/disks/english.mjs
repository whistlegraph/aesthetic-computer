// English, 2026.01.19
// Translate any language to English.

import * as T from "../disks/common/translate.mjs";

const LANG = "english";

function boot($) { T.boot(LANG, $); }
function paint($) { T.paint(LANG, $); }
function act($) { T.act(LANG, $); }
function meta() { return T.meta(LANG); }

export { boot, paint, act, meta };

