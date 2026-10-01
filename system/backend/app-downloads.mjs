// Fixed release hosts and exact filenames only; never redirect an arbitrary URL.
const releases = [
  ['slab', /^Slab-(\d+(?:\.\d+){1,2})\.dmg$/, 'https://assets.aesthetic.computer/slab/'],
  ['aesel', /^aesel-(\d+(?:\.\d+){1,2})-arm64\.dmg$/, 'https://releases.aesthetic.computer/aesel/mac/'],
  ['aesel', /^aesel-(\d+(?:\.\d+){1,2})-windows-x64-setup\.exe$/, 'https://releases.aesthetic.computer/aesel/windows/'],
];
export function resolveAppDownload(app, file) {
  if (typeof app !== 'string' || typeof file !== 'string') return null;
  for (const [name, pattern, base] of releases) {
    const match = app === name && file.match(pattern);
    if (match) return {version:match[1], location:base+file};
  }
  return null;
}
