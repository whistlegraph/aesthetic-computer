// help, 26.10.12
// Ask @jeffrey for help in the human chat.

export function boot({ colon, params, alias }) {
  alias("chat", colon, params);
}

export function meta() {
  return { title: "help", desc: "Ask @jeffrey for help." };
}
