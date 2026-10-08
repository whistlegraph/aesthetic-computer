// Compatibility for Fastlane versions using Apple's retired Olympus endpoint.
// The public login widget key is not our private App Store Connect API key.
// https://github.com/fastlane/fastlane/pull/30206
import {writeFile, rename, rm} from 'node:fs/promises';
import {randomUUID} from 'node:crypto';

// A bare request: never attach session cookies or follow the signout redirect.
const response = await fetch('https://appstoreconnect.apple.com/logout', {
  method: 'HEAD', redirect: 'manual', signal: AbortSignal.timeout(15000),
});
if (![302, 303].includes(response.status)) throw Error(`Apple login configuration returned HTTP ${response.status}`);
const location = new URL(response.headers.get('location') || '', response.url);
if (location.origin !== 'https://idmsa.apple.com' || location.pathname !== '/appleauth/signout') {
  throw Error('Apple login configuration returned an unexpected redirect');
}
const key = location.searchParams.get('widgetKey');
if (!/^[a-f0-9]{64}$/i.test(key || '')) throw Error('Apple login configuration has no valid widget key');
const cache = '/tmp/spaceship_itc_service_key.txt';
const temporary = `${cache}.${randomUUID()}`;
try {
  await writeFile(temporary, key, {mode: 0o600, flag: 'wx'});
  await rename(temporary, cache);
} finally {
  await rm(temporary, {force: true});
}
console.log('Refreshed Fastlane public login configuration.');
