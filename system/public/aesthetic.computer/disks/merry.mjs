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

  const args = [];
  let fadeOption = "";

  if (colon && colon.length > 0) {
    for (const part of colon) {
      if (/^fade(\.\d+)?$/.test(part)) {
        fadeOption = ":" + part;
      } else {
        args.push(part);
      }
    }
  }

  if (params && params.length > 0) {
    args.push(...params);
  }

  if (args.length === 0) {
    jump("prompt~merry");
    return;
  }

  // Jump to prompt with merry command AND !autorun flag to execute immediately
  jump("prompt~merry" + fadeOption + " " + args.join(" ") + "~!autorun");
}

export const nohud = true;

export function meta() {
  return {
    title: "Merry",
    desc: "🎄 Piece pipeline sequencer",
  };
}
