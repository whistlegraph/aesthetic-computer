// Artnom, 2026.07.21
// The art-history edition of nom: every answer is a museum thumbnail and every
// round is identified by a Getty AAT style URI. Artwork metadata and rights
// come from the generated assets/artnom/manifest.json catalog snapshot.

import { boot as nomBoot, sim, paint, act, makeMeta } from "../lib/nom.mjs";

async function boot(api) {
  const artnom = await api.net.preload(
    "/aesthetic.computer/assets/artnom/manifest.json",
  );
  nomBoot({ ...api, params: ["art"], artnom });
}

function meta() {
  return makeMeta(["art"]);
}

export { boot, sim, paint, act, meta };
