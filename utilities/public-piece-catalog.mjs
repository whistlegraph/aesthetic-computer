import { readdir } from "node:fs/promises";
import { handler } from "../system/netlify/functions/docs.js";

// Reuse the published docs' curated overrides and meta() opt-in without ever
// importing or executing a piece. Both human and machine indexes use this list.
export async function publicPieceCatalog() {
  const response = await handler({ httpMethod: "GET", path: "/docs.json", headers: { host: "aesthetic.computer" } }, {});
  if (response.statusCode !== 200) throw new Error("Could not load the public piece catalog");
  const { pieces } = JSON.parse(response.body);
  return (await readdir(new URL("../system/public/aesthetic.computer/disks/", import.meta.url)))
    .filter(file => {
      const name = file.replace(/\.(mjs|lisp)$/, "");
      return name !== file && pieces[name] && !pieces[name].hidden;
    }).sort().map(file => {
      const name = file.replace(/\.(mjs|lisp)$/, "");
      return { file, name, description: pieces[name].desc };
    });
}

export async function publicPieceFiles() {
  return (await publicPieceCatalog()).map(piece => piece.file);
}
