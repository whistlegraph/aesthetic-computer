// merryo, 26.01.10
// 🎄 Merryo - URL-able looping merry command router
// URL examples:
//   /merryo:0.5-tone:0.5-clock -> merryo 0.5-tone 0.5-clock (loops forever)
//   /merryo:tone:clock -> merryo tone clock (5s default each, loops)

export function boot({ colon, params, jump }) {
  // Build the merryo command from URL params
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
    jump("prompt~merryo");
    return;
  }

  // Jump to prompt with merryo command pre-filled
  jump("prompt~merryo" + fadeOption + " " + args.join(" "));
}

export const nohud = true;

export function meta() {
  return {
    title: "Merryo",
    desc: "🎄 Looping piece pipeline sequencer",
  };
}
