// hha, 23.04.24.15.01
// A shortcut to "happy-hands-assembler"

/* #region 🤝 Read Me 
#endregion */

// 🥾 Boot (Runs once before first paint and sim)
function boot({ alias, colon, params }) {
  alias("wave", colon, params);
}

export { boot };
export { meta } from "../disks/wave.mjs";

// 📚 Library (Useful functions used throughout the piece)
// ...
