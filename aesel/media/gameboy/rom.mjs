import { createHash } from "node:crypto";
// Cartridge boot header is a format requirement, not a bundled game ROM.
const logo = Buffer.from(
  "ceed6666cc0d000b03730083000c000d0008111f8889000edccc6ee6ddddd999bbbb67636e0eecccdddc999fbbb9333e",
  "hex",
);
export function verifyROM(bytes) {
  if (
    !Buffer.isBuffer(bytes) ||
    bytes.length < 32768 ||
    bytes.length > 8 * 1024 * 1024
  )
    throw new Error("ROM must be 32 KB–8 MB.");
  if (!bytes.subarray(0x104, 0x134).equals(logo))
    throw new Error("Invalid cartridge boot logo.");
  const size = bytes[0x148];
  if (size > 8 || bytes.length !== 32768 * 2 ** size)
    throw new Error("ROM size does not match its cartridge header.");
  let check = 0;
  for (let i = 0x134; i <= 0x14c; i++) check = (check - bytes[i] - 1) & 255;
  if (bytes[0x14d] !== check)
    throw new Error("Invalid cartridge header checksum.");
  let sum = 0;
  for (let i = 0; i < bytes.length; i++)
    if (i !== 0x14e && i !== 0x14f) sum = (sum + bytes[i]) & 65535;
  if (bytes.readUInt16BE(0x14e) !== sum)
    throw new Error("Invalid global ROM checksum.");
  return {
    bytes: bytes.length,
    sha256: createHash("sha256").update(bytes).digest("hex"),
    title: bytes
      .toString("ascii", 0x134, bytes[0x143] & 0x80 ? 0x143 : 0x144)
      .replace(/\0.*$/, ""),
    cartridgeType: bytes[0x147],
    color: !!(bytes[0x143] & 0x80),
    headerChecksum: check,
    globalChecksum: sum,
  };
}
