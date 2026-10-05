import AdmZip from 'adm-zip';

// Teia's interactive format is an IPFS directory, uploaded through its UI as
// a ZIP with index.html. A plain text/html artifact selects the unknown viewer.
// https://github.com/teia-community/teia-docs/blob/main/teia-docs/docs/howtos/Interactive-OBJKTs.md
export const TEIA_FORMAT = 'application/x-directory';
export const TEIA_PACKAGE_VERSION = 1;
// Our packs need no network. This is a stricter subset of Teia's HTML policy;
// exercise the ZIP in Teia's own preview too, where Teia replaces this policy.
export const PACK_CSP = "default-src 'none'; script-src 'self' 'unsafe-inline' 'unsafe-eval' blob:; style-src 'self' 'unsafe-inline'; img-src 'self' data: blob:; font-src 'self' data:; media-src 'self' data: blob:; connect-src 'self' blob: data:; worker-src 'self' blob:; frame-src 'self'; base-uri 'none'; form-action 'none'";

export function teiaIndexHTML(html) {
  if (!/<head(?:\s[^>]*)?>/i.test(html)) throw new Error('Packed HTML needs a head');
  const clean = html.replace(/<meta\b[^>]*(?:http-equiv=["']Content-Security-Policy["']|property=["'](?:og:image|cover-image)["'])[^>]*>/gi, '');
  return clean.replace(/<head(?:\s[^>]*)?>/i, head => `${head}\n<meta http-equiv="Content-Security-Policy" content="${PACK_CSP}">\n<meta property="og:image" content="cover.gif">`);
}

export function teiaPackage(html, preview) {
  if (!preview.gif?.length || !preview.thumbnail?.length || preview.frames < 2) throw new Error('Animated cover and thumbnail required');
  const files = [
    { name:'index.html', mime:'text/html', content:Buffer.from(teiaIndexHTML(html)) },
    { name:'cover.gif', mime:'image/gif', content:Buffer.from(preview.gif) },
    { name:'thumbnail.png', mime:'image/png', content:Buffer.from(preview.thumbnail) },
  ];
  const zip = new AdmZip();
  for (const file of files) zip.addFile(file.name, file.content);
  return { files, zip:zip.toBuffer() };
}
