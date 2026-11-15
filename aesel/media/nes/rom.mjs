import { createHash } from "node:crypto";
// The mappers AC's NES player runs (system/public/nes-player/rom.mjs).
export const NES_TYPE = "application/x-nes-rom";
export const NES_MAPPERS = [0, 1, 2, 3, 4, 5, 7, 9, 11, 34, 38, 66, 71, 79, 94, 118, 119, 140, 180, 240, 241];
const MAX_ROM = 8 * 1024 * 1024;

/** Check a .nes file is a whole cartridge AC's player can boot; returns its header facts. */
export function verifyNES(bytes) {
  if (!Buffer.isBuffer(bytes) || bytes.length < 16 || bytes.length > MAX_ROM) throw new Error("ROM must be an iNES file up to 8 MB.");
  if (bytes.readUInt32BE(0) !== 0x4e45531a) throw new Error('ROM must start with the iNES header: "NES", $1A.');
  const nes2 = (bytes[7] & 12) === 8;
  const size = (low, high, bank) => (high === 15 ? 2 ** (low >> 2) * ((low & 3) * 2 + 1) : ((high << 8) | low) * bank);
  const prg = nes2 ? size(bytes[4], bytes[9] & 15, 16384) : bytes[4] * 16384;
  const chr = nes2 ? size(bytes[5], bytes[9] >> 4, 8192) : bytes[5] * 8192;
  const trainer = bytes[6] & 4 ? 512 : 0;
  if (!prg) throw new Error("Header declares no PRG ROM.");
  const expected = 16 + trainer + prg + chr;
  if (bytes.length !== expected)
    throw new Error(`ROM is ${bytes.length} bytes; its header declares ${expected} (16 header + ${prg} PRG + ${chr} CHR).`);
  let mapper = (bytes[6] >> 4) | (bytes[7] & 240);
  if (nes2) mapper |= (bytes[8] & 15) << 8;
  else if (bytes.subarray(8, 16).some((b) => b !== 0)) mapper &= 15;
  if (bytes[7] & 3) throw new Error("Vs. System and PlayChoice cartridges are not supported.");
  if (!NES_MAPPERS.includes(mapper)) throw new Error(`Mapper ${mapper} is not supported; use one of ${NES_MAPPERS.join(", ")}.`);
  // Every supported mapper boots with the last PRG bank at the top of memory.
  const vectors = 16 + trainer + prg - 6;
  const [nmi, reset, irq] = [0, 2, 4].map((offset) => bytes.readUInt16LE(vectors + offset));
  if (reset < 0x8000) throw new Error(`Reset vector $${reset.toString(16).padStart(4, "0")} is outside the cartridge; put .dw nmi, reset, irq at $FFFA.`);
  return {
    bytes: bytes.length, sha256: createHash("sha256").update(bytes).digest("hex"),
    format: nes2 ? "NES 2.0" : "iNES", mapper, prg, chr, trainer, battery: !!(bytes[6] & 2),
    mirroring: bytes[6] & 8 ? "four-screen" : bytes[6] & 1 ? "vertical" : "horizontal", nmi, reset, irq,
  };
}
