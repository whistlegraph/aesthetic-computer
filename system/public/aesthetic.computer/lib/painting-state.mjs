// Portable editable painting snapshots. Pixel arrays stay binary in the gzip
// payload; this format is separate from the public PNG + step-recording ZIP.
const FORMAT = "aesthetic.computer/painting-state";

function base64(bytes) {
  let binary = "";
  for (let i = 0; i < bytes.length; i += 32768) {
    binary += String.fromCharCode(...bytes.subarray(i, i + 32768));
  }
  return btoa(binary);
}

function bytes(encoded) {
  return Uint8Array.from(atob(encoded), (c) => c.charCodeAt(0));
}

export async function encodePaintingState(piece) {
  const json = JSON.stringify({ format: FORMAT, version: 1, piece }, (_, value) =>
    ArrayBuffer.isView(value) ? { $pixels: base64(new Uint8Array(value.buffer, value.byteOffset, value.byteLength)) } : value);
  const stream = new Blob([json]).stream().pipeThrough(new CompressionStream("gzip"));
  return base64(new Uint8Array(await new Response(stream).arrayBuffer()));
}

export async function decodePaintingState(encoded) {
  const stream = new Blob([bytes(encoded)]).stream().pipeThrough(new DecompressionStream("gzip"));
  const state = JSON.parse(await new Response(stream).text(), (_, value) =>
    value && typeof value.$pixels === "string" ? new Uint8ClampedArray(bytes(value.$pixels)) : value);
  if (state.format !== FORMAT || state.version !== 1 || !state.piece) throw new Error("Unknown painting state");
  return state.piece;
}
