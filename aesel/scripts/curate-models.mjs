// curate-models.mjs — lock an account's hosted Aesel models to a curated menu.
//
// Staff use, on Lith with its environment, like grant-braincells.mjs. The
// first model is the one the account opens on; Aesel shows only these, and
// /api/easel-inference refuses any other. --clear lifts the lock.
//
//   node aesel/scripts/curate-models.mjs @dreamdealer sonnet,deepseek --by @jeffrey [--note "coding trial"]
//   node aesel/scripts/curate-models.mjs @dreamdealer --show
//   node aesel/scripts/curate-models.mjs @dreamdealer --clear --by @jeffrey
import { connect } from "../../system/backend/database.mjs";
import { CURATION_COLLECTION, curationMenu } from "../../system/backend/aesel-curation.mjs";
import { HOSTED_MODELS } from "../src/open-models.mjs";

const usage = "usage: node aesel/scripts/curate-models.mjs @handle <model,model…> --by @curator [--note text] | --show | --clear --by @curator";
const args = process.argv.slice(2);
const flag = (name) => { const i = args.indexOf(name); return i >= 0 ? args.splice(i, 2)[1] : undefined; };
const toggle = (name) => { const i = args.indexOf(name); return i >= 0 ? (args.splice(i, 1), true) : false; };
const by = (flag("--by") || "").trim(), note = (flag("--note") || "").trim();
const show = toggle("--show"), clear = toggle("--clear");
const [who = "", list = ""] = args;
const stop = (message) => { console.error(message); process.exit(1); };
const handle = who.replace(/^@/, "").toLowerCase();
if (!handle) stop(usage);
if (!show && !by.startsWith("@")) stop(`--by needs the curator's @handle\n${usage}`);
// Short names from Aesel's picker (sonnet, deepseek, flash…) or full ids.
const models = list.split(",").map(s => s.trim()).filter(Boolean).map(name => HOSTED_MODELS[name] || name);
if (!show && !clear && !models.length) stop(usage);

const connection = await connect();
try {
  const user = (await connection.db.collection("@handles").findOne({ handle }, { projection: { _id: 1 } }))?._id;
  if (!user) stop(`no such handle: @${handle}`);
  const curations = connection.db.collection(CURATION_COLLECTION);
  if (show) {
    console.log(JSON.stringify({ handle: `@${handle}`, curation: curationMenu(await curations.findOne({ _id: user })) }));
  } else if (clear) {
    await curations.deleteOne({ _id: user });
    console.log(JSON.stringify({ handle: `@${handle}`, curation: null, by }));
  } else {
    const menu = curationMenu({ models, default: models[0], by, note });
    if (!menu || menu.models.length !== models.length) stop(`unknown model in ${list}; use ${Object.keys(HOSTED_MODELS).join(", ")} or a hosted id`);
    await curations.replaceOne({ _id: user }, { _id: user, ...menu, at: new Date() }, { upsert: true });
    console.log(JSON.stringify({ handle: `@${handle}`, curation: menu }));
  }
} finally { await connection.disconnect(); }
process.exit(0);
