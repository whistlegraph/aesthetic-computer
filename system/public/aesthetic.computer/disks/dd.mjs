// dd, 26.05.18
// @dreamdealer's vibe-coding piece.
//
// Thin wrapper that shares pp.mjs the same way laer-klokken.mjs shares
// chat.mjs: identical sub-gated /api/pp/* bridge, identical logic — only the
// piece title differs. pp.mjs is the shared implementation; this is just a
// branded entry point. Access still requires the user's Auth0 *sub* to be in
// PP_ALLOWED_SUBS on the help bridge (handle is irrelevant).

export { boot, paint, act, sim, leave } from "./pp.mjs";

export function meta() {
  return {
    title: "DD",
    desc: "Vibe-code AC pieces with claude — published under your own handle.",
  };
}
