const fs = require('node:fs');
const path = require('node:path');
const {execFile} = require('node:child_process');
const {promisify} = require('node:util');
const inspectFile = promisify(execFile);
const dimensionsCache = new Map();
const allowed = new Set(['image/png','audio/wav','application/pdf','text/plain','application/x-gameboy-rom','application/octet-stream']);
async function localPreview(workspace, preview) {
  if (!preview || !allowed.has(preview.mime)) return null;
  const root = fs.realpathSync(path.join(workspace,'.easel-media'));
  const target = fs.realpathSync(preview.path);
  const relative = path.relative(root,target);
  if (!relative || relative.startsWith('..') || path.isAbsolute(relative)) throw new Error('Preview must be inside the media project.');
  const stat = fs.statSync(target);
  if (!stat.isFile() || stat.size > 32*1024*1024) throw new Error('Preview exceeds the 32 MiB display limit.');
  // A PDF page is displayed as US Letter portrait unless platform metadata
  // proves otherwise. This also fixes page geometry off macOS, where `sips`
  // is unavailable and the old generic 3:2 card made papers look landscape.
  let dimensions = preview.mime === 'application/pdf' ? {width:612,height:792} : undefined;
  if (preview.mime === 'application/pdf' && process.platform === 'darwin') {
    const key = JSON.stringify([target,preview.version,stat.size,stat.mtimeMs]);
    if (!dimensionsCache.has(key)) {
      let result = null;
      try {
        const {stdout} = await inspectFile('/usr/bin/sips', ['-g','pixelWidth','-g','pixelHeight',target], {timeout:2000,maxBuffer:8192});
        const width = Number(/pixelWidth:\s*([\d.]+)/.exec(stdout)?.[1]);
        const height = Number(/pixelHeight:\s*([\d.]+)/.exec(stdout)?.[1]);
        if ([width,height].every(n=>Number.isFinite(n)&&n>0&&n<=32768)) result = {width,height};
      } catch {} // Other platforms and unreadable PDF metadata retain whole-page Fit.
      if (dimensionsCache.size >= 32) dimensionsCache.delete(dimensionsCache.keys().next().value);
      dimensionsCache.set(key,result);
    }
    dimensions = dimensionsCache.get(key) || dimensions;
  }
  const bytes = fs.readFileSync(target);
  return {...dimensions,mime:preview.mime,internalPath:target,data:`data:${preview.mime};base64,${bytes.toString('base64')}`,text:preview.mime==='text/plain'?bytes.toString('utf8').slice(0,100000):undefined};
}
module.exports = {localPreview};
