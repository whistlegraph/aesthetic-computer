// r8Dio, 2025.01.04
// Case-variant alias for r8dio (R8dio.dk radio player)
// Similar to how "ff" aliases to "freaky-flowers"

// 🥾 Boot (Runs once before first paint and sim)
export function boot({ colon, params, alias }) {
  // Pass all parameters to `r8dio`
  alias(`r8dio`, colon, params);
}

export { meta } from "../disks/r8dio.mjs";
