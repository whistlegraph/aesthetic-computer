// curation.mjs — the curator's locked model menu, as the AC site reports it.
//
// /api/easel-credits carries `curation: {models, default, by}` for an account a
// curator has set up (aesel/scripts/curate-models.mjs). While it holds, AC
// hosted shows only those models and opens on the default; the site refuses
// any other model too, so this is the picker's half of a lock the server keeps.
// An account no curator has set up gets the house menu, shown the same way.
let menu = null;

export function setCuration(value) {
  const models = Array.isArray(value?.models) ? value.models.filter(m => typeof m === "string" && m) : [];
  menu = models.length ? { models, default: models.includes(value.default) ? value.default : models[0], by: String(value.by || "") } : null;
  return menu;
}
export const curation = () => menu;
// The model to run on AC under the menu: kept when allowed, else the default.
export const curatedChoice = (model) => !menu || menu.models.includes(model) ? model : menu.default;
