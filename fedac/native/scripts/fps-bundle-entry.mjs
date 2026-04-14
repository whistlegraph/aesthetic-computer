// fps-bundle-entry.mjs — esbuild entry for the FPS system bundle.
//
// Shipped to /lib/fps-system.mjs in the initramfs. ac-native's piece
// loader imports from this when a piece exports `system = "fps"`
// (arena.mjs etc.). The bundle contains the subset of the web runtime
// needed: the Camera + Dolly 3D math classes from graph.mjs, and the
// CamDoll input+physics controller from cam-doll.mjs. esbuild
// tree-shakes everything else out of graph.mjs.
//
// Kept deliberately tiny so the bundle size stays small and the
// native/web runtimes use LITERALLY the same source for FPS — any fix
// to cam-doll.mjs or Camera/Dolly applies to both targets at once.

export { Camera, Dolly } from "../../../system/public/aesthetic.computer/lib/graph.mjs";
export { CamDoll } from "../../../system/public/aesthetic.computer/lib/cam-doll.mjs";
