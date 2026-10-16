// One model/identity module graph for both packaged and live native Oskiewar.
import { readFileSync, writeFileSync, mkdirSync, existsSync } from 'node:fs';
import { dirname, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
const root = resolve(dirname(fileURLToPath(import.meta.url)), '../..');
export function nativeSource(source, base = root) {
  const modules = ['oskiewar-fighter.mjs', 'native-account.mjs'].map(name =>
    '(() => {\n' + readFileSync(resolve(base, 'xbox/live', name), 'utf8')
      .replace(/^export (?=(?:const|function) )/gm, '') + '\n})();\n');
  return modules.join('') + source;
}
if (process.argv[1] && resolve(process.argv[1]) === fileURLToPath(import.meta.url)) {
  const target = resolve(process.argv[2] || 'xbox/native-bios/generated/oskiewar.js');
  mkdirSync(dirname(target), { recursive: true });
  // oskiewar is a private product (private-products.json). A public checkout
  // (the AppVeyor compile check) has no game to bundle: it writes an empty
  // file, which the app reads as "no game" and boots its smoke piece. Such a
  // package proves the native code builds; it is not one to install.
  if (!existsSync(resolve(root, 'xbox/live/oskiewar.js'))) {
    writeFileSync(target, '');
    console.warn('oskiewar-native-source: xbox/live/oskiewar.js is not here (private product); packaged an empty game');
    process.exit(0);
  }
  const qr = readFileSync(resolve(root, 'system/public/aesthetic.computer/dep/@akamfoad/qr/qr.mjs'), 'utf8')
    .replace(/\nexport\s*\{[\s\S]*?\};\s*$/, '\n');
  writeFileSync(target, qr + '\n' + nativeSource(readFileSync(resolve(root, 'xbox/live/oskiewar.js'), 'utf8')));
}
