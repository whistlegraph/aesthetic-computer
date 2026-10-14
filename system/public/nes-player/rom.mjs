// Shared by MIME's upload validator and the browser player; no emulator allocation here.
export const NES_TYPE = 'application/x-nes-rom';
export const MAX_ROM = 8 * 1024 * 1024;
export const NES_TYPES = [NES_TYPE, 'application/x-nes', 'application/x-nintendo-nes-rom'];
export const NES_MAPPERS = [0, 1, 2, 3, 4, 5, 7, 9, 11, 34, 38, 66, 71, 79, 94, 118, 119, 140, 180, 240, 241];
export function isNES(file) {
  return !!file && (NES_TYPES.includes(file.type?.toLowerCase()) || /\.nes$/i.test(file.name || ''));
}
export function inspectROM(input) {
  const bytes = input instanceof Uint8Array ? input : new Uint8Array(input);
  if (bytes.length > MAX_ROM) throw new Error('ROM over 8 MB.');
  if (bytes.length < 16 || bytes[0] !== 0x4e || bytes[1] !== 0x45 || bytes[2] !== 0x53 || bytes[3] !== 0x1a)
    throw new Error('Choose an iNES or NES 2.0 .nes ROM.');
  const nes2 = (bytes[7] & 12) === 8;
  const size = (low, high, bank) => high === 15 ? 2 ** (low >> 2) * ((low & 3) * 2 + 1) : ((high << 8) | low) * bank;
  const prg = nes2 ? size(bytes[4], bytes[9] & 15, 16384) : bytes[4] * 16384;
  const chr = nes2 ? size(bytes[5], bytes[9] >> 4, 8192) : bytes[5] * 8192;
  const trainer = bytes[6] & 4 ? 512 : 0;
  if (!prg || prg + chr + trainer + 16 > MAX_ROM) throw new Error('Invalid NES ROM size.');
  if (bytes.length < 16 + trainer + prg + chr) throw new Error('Incomplete NES ROM.');
  // Match JSNES's handling of old dumps whose padding contains DiskDude text.
  let mapper = (bytes[6] >> 4) | (bytes[7] & 240);
  if (nes2) mapper |= (bytes[8] & 15) << 8;
  else if (bytes.slice(8, 16).some(byte => byte !== 0)) mapper &= 15;
  return { format: nes2 ? 'NES 2.0' : 'iNES', mapper, prg, chr, trainer,
    console: bytes[7] & 3, supported: NES_MAPPERS.includes(mapper) && !(bytes[7] & 3) };
}
