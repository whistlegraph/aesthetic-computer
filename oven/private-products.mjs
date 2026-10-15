import { existsSync, readFileSync } from 'node:fs';
import { join } from 'node:path';
import { pathToFileURL } from 'node:url';

// Every build checkout receives the source-only bundle uploaded with Oven.
// Public checkouts without the inventory predate this migration.
export async function hydratePrivateProducts(root, input = process.env.AC_PRIVATE_PRODUCTS_SOURCE || '/opt/oven/private-products') {
  if (!existsSync(join(root, 'private-products.json'))) return null;
  if (!existsSync(join(input, '.private-products-installed.json')))
    throw new Error('Oven needs a private product source bundle; run oven/deploy.sh from the composed workspace.');
  const { workspace, installBundle } = await import(pathToFileURL(join(root, 'toolchain/private-products.mjs')).href);
  const bundle = JSON.parse(readFileSync(join(input, '.private-products-installed.json')));
  if (bundle.revision === 'local-test') throw new Error('Oven cannot build from a local-test product bundle');
  return installBundle(workspace(root), input);
}
