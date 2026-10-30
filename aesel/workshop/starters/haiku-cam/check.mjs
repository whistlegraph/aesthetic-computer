// A haiku is three lines. This runs after the model answers; anything it
// returns goes back to the model as a problem to fix, once.
export default function check({ text }) {
  const lines = text.split("\n").filter((line) => line.trim());
  return lines.length === 3 ? [] : [`A haiku has 3 lines; this has ${lines.length}.`];
}
