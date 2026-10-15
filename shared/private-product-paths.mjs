import { readFileSync } from "node:fs";

const manifest = JSON.parse(readFileSync(new URL("../private-products.json", import.meta.url), "utf8"));
const glob = (pattern) => new RegExp("^" + pattern.split("**").map(part =>
  part.split("*").map(text => text.replace(/[.*+?^${}()|[\]\\]/g, "\\$&")).join("[^/]*")
).join(".*") + "$");
const rules = Object.values(manifest.products).map(({ include, exclude = [] }) => ({
  include: include.map(glob), exclude: exclude.map(glob),
}));

// Works in a public checkout and with a private bundle installed as real files.
export function isPrivateProductPath(path) {
  return rules.some(rule => rule.include.some(re => re.test(path)) && !rule.exclude.some(re => re.test(path)));
}

export function isPrivateProductPiece(name) {
  return name === "sotce-net" || ["mjs", "lisp"].some(ext =>
    isPrivateProductPath(`system/public/aesthetic.computer/disks/${name}.${ext}`));
}
