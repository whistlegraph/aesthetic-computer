import { existsSync } from 'node:fs';

// Public checkouts omit the private products. A present but broken product
// must still fail loudly; only an absent entry point is optional.
export async function optionalProduct(url) {
  return existsSync(url) ? import(url.href) : null;
}
