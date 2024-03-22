// Wipe, 23.02.14.16.15
// Clears the system painting with a color.

// 🎨 Paint (Executes every display frame)
function paint($) {
  const color = $.params.map((str) => parseInt(str));
  $.wipe(color);
}

export const system = "nopaint";

export { paint };