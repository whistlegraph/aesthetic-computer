// 🔢 The four digits both screens show for one pairing code (pairing.mjs):
// the tab that asked draws them beside its QR, the phone that scanned works
// them out from the code it read. They match only for the same code, so a
// person approves the screen in front of them and no other.
export const CODE = /^[A-Za-z0-9_-]{22,64}$/;
export async function pairNumber(code) {
  const h = new Uint8Array(await crypto.subtle.digest('SHA-256', new TextEncoder().encode(`rax-pair:${code}`)));
  return String((((h[0] << 24) | (h[1] << 16) | (h[2] << 8) | h[3]) >>> 0) % 10000).padStart(4, '0');
}
