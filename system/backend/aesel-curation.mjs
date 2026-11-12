// aesel-curation.mjs — a curator's locked model menu for one account.
//
// A curator (staff, by hand: aesel/scripts/curate-models.mjs) picks which
// hosted models an account may run and which one it opens on. Aesel reads the
// menu from /api/easel-credits and shows only those models; /api/easel-inference
// refuses any other, so the lock holds whatever client asks. No document means
// no curation: the account sees every hosted model, as before.
import { connect } from "./database.mjs";
import { EASEL_MODELS } from "./easel-policy.mjs";

export const CURATION_COLLECTION = "aesel-curations";

// A stored menu, checked: known models, a default among them, a curator.
export function curationMenu(value) {
  if (!value || !Array.isArray(value.models) || !value.models.length) return null;
  const models = [...new Set(value.models)].filter(m => typeof m === "string" && Object.hasOwn(EASEL_MODELS, m));
  if (!models.length) return null;
  const fallback = models.includes(value.default) ? value.default : models[0];
  return { models, default: fallback, by: typeof value.by === "string" ? value.by : "", note: typeof value.note === "string" ? value.note : "" };
}

// The model a curated account runs for a request: an unnamed request takes the
// curator's default; a named one must be on the menu. Throws when it is not.
export function curatedModel(menu, requested) {
  if (!menu) return requested;
  if (requested === undefined || requested === "") return menu.default;
  if (menu.models.includes(requested)) return requested;
  const names = menu.models.map(m => EASEL_MODELS[m]?.label || m).join(", ");
  throw new Error(`Your models are curated${menu.by ? ` by ${menu.by}` : ""}: ${names}.`);
}

export async function readCuration(user, { connectTo = connect } = {}) {
  if (!user) return null;
  const connection = await connectTo();
  try { return curationMenu(await connection.db.collection(CURATION_COLLECTION).findOne({ _id: user })); }
  finally { await connection.disconnect(); }
}
