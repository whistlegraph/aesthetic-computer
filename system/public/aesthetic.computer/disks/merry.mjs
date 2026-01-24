// merry, 26.01.10
// 🎄 Merry - URL-able merry command router
// URL examples:
//   /merry:0.5-tone:0.5-clock -> merry 0.5-tone 0.5-clock
//   /merry:tone:clock -> merry tone clock (5s default each)
//   Works with KidLisp: /merry:16-$ceo:16-$roz:16-$mtz

export function boot({ colon, params, jump }) {
  // Build the merry command from URL params
  // colon contains everything after the first `:` split by `:`
  // params contains everything after `~` split by `~`
  
  // Combine colon params (piece:duration format) into space-separated args
  const args = [];
  
  if (colon && colon.length > 0) {
    // Each colon segment is a piece with optional duration
    // e.g., /merry:0.5-tone:0.5-clock -> ["0.5-tone", "0.5-clock"]
    args.push(...colon);
  }
  
  if (params && params.length > 0) {
    args.push(...params);
  }
  
  if (args.length === 0) {
    // No pieces specified, jump to prompt
    jump("prompt~merry");
    return;
  }
  
  // Jump to prompt with merry command AND !autorun flag to execute immediately
  jump("prompt~merry " + args.join(" ") + "~!autorun");
}

export const nohud = true;

export function meta() {
  return {
    title: "Merry",
    desc: "🎄 Piece pipeline sequencer",
  };
}
