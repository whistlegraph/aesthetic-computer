// Resolve the phone's browser import map in a focused Node test process.
const root = new URL('../../', import.meta.url);
const shims = {'node:events':'events','node:fs':'fs','node:path':'path','node:url':'url','node:crypto':'crypto','node:os':'absent','node:http':'absent','node:child_process':'absent','node:readline':'absent'};
export async function resolve(specifier, context, next) {
  if (context.parentURL?.startsWith(new URL('easel/', root).href)) {
    if (shims[specifier]) return {url: new URL(`easel/phone/shim/${shims[specifier]}.mjs`, root).href, shortCircuit:true};
    if (specifier.startsWith('node:')) throw Error(`Phone import map has no shim for ${specifier}`);
    for (const name of ['revisions','preview-frame','jev-advisor']) if (specifier === `./${name}.mjs`) return {url:new URL(`easel/phone/shim/${name}.mjs`, root).href,shortCircuit:true};
  }
  if (specifier.startsWith('/easel/')) return {url:new URL(specifier.slice(1), root).href,shortCircuit:true};
  return next(specifier, context);
}
