// dm — direct messages, which are mail: `dm @handle` opens your conversation
// with them (compose when there isn't one yet); bare `dm` is the box. A
// letter between two handles is a DM; there is one inbox, not two.
// It jumps rather than aliases, so the address reads `mail~@handle` and mail
// keeps its own hold on Enter and Escape while composing.

// 🥾 Boot
export function boot({ colon = [], params = [], jump }) {
  const options = colon.length ? `:${colon.join(":")}` : "";
  const words = params.length ? `~${params.join("~")}` : "";
  jump(`mail${options}${words}`);
}
