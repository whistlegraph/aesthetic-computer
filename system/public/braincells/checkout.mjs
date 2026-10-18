const $ = id => document.getElementById(id);
// The aesthetic SPA, the same Auth0 client boot.mjs signs AC in with.
const AUTH0_CLIENT_ID = 'LVdZaMbyXctkGfZDnpzDATB5nR0ZhmMt';
// Mirrors TEZOS_CREDIT_PACKS in system/backend/tezos-credits.mjs. The server
// prices and fixes the pack; these only label the menu before a checkout exists.
const STORE_OFFERS = [{ id:'braincells-600k-v1', credits:600_000, usd:3 }, { id:'braincells-1m-v1', credits:1_000_000, usd:5 }];
const isSecret = value => /^[a-f0-9]{64}$/.test(value || '');
const fragment = location.hash.slice(1);
// Whistlegraph opens a checkout it already made; the page also makes its own.
if (isSecret(fragment)) {
  sessionStorage.setItem('ac-tezos-checkout', fragment);
  sessionStorage.setItem('ac-tezos-from-app', 'true');
  history.replaceState(null, '', location.pathname);
}
let secret = sessionStorage.getItem('ac-tezos-checkout');
if (!isSecret(secret)) secret = null;
const fromApp = sessionStorage.getItem('ac-tezos-from-app') === 'true';
let client, intent = null, offers = [], busy = false, polling = false, usdPerTez = null, door, spa, buyer = null;
const say = text => { $('status').textContent = text; };
const tez = amount => amount.toLocaleString(undefined, { maximumFractionDigits:amount < 10 ? 2 : 1 });
async function api(action, body = {}) {
  const r = await fetch('/api/easel-tezos', { method:'POST', headers:{ 'Content-Type':'application/json', Authorization:`Bearer ${secret}` },
    body:JSON.stringify({ action, ...body }), signal:AbortSignal.timeout(25_000) });
  const result = await r.json();
  if (!r.ok) throw Object.assign(new Error(result.error || 'Checkout unavailable'), { status:r.status });
  return result;
}
function fillPacks(list) {
  offers = list;
  $('pack').replaceChildren(...offers.map(offer => new Option(`$${offer.usd} · ${offer.credits.toLocaleString()} braincells`, offer.id)));
}
function show(next) {
  intent = next;
  if (next.offers) fillPacks(next.offers);
  const savedPack = sessionStorage.getItem(`ac-tezos-pack:${intent.id}`);
  $('pack').value = intent.sender ? intent.pack : offers.some(offer => offer.id === savedPack) ? savedPack : intent.pack;
  $('pack-label').hidden = Boolean(intent.sender) || intent.status !== 'created';
  $('pack').disabled = busy || Boolean(intent.sender);
  showPack();
  $('lede').hidden = true;
  $('signin').hidden = true;
  $('account').textContent = `For @${intent.handle}`;
  $('recipient').hidden = false;
  $('return').hidden = !fromApp;
  $('connect').hidden = ['credited','expired'].includes(intent.status) || Boolean(intent.sender);
  $('check').hidden = !intent.sender || intent.status === 'credited';
  const expired = intent.status === 'expired' || (intent.expiresAt && Date.parse(intent.expiresAt) <= Date.now());
  $('pay').hidden = !intent.sender || intent.status === 'credited' || expired || Boolean(sessionStorage.getItem(`ac-tezos-paying:${intent.id}`));
  $('again').hidden = !(intent.status === 'credited' || expired);
  $('again').textContent = intent.status === 'credited' ? 'Buy more braincells' : 'Start a new checkout';
  if (intent.sender) {
    $('wallet').hidden = false; $('wallet').textContent = intent.sender;
    const amount = (Number(intent.amountMutez) / 1e6).toLocaleString(undefined, { maximumFractionDigits:6 });
    $('price').textContent = `${amount} tez + network fee`;
    $('pay').textContent = `Pay ${amount} tez`;
  }
  if (intent.status === 'credited') { say('Braincells added. They’re ready to use in AC.'); sessionStorage.removeItem(`ac-tezos-op:${intent.id}`); }
  else if (expired) say('Quote expired. Check an existing payment before starting a new checkout.');
}
function showPack() {
  const picked = offers.find(offer => offer.id === $('pack').value);
  const pack = (!intent?.sender && picked) || intent;
  if (!pack) return;
  $('quantity').textContent = `${pack.credits.toLocaleString()} braincells`;
  if (intent?.sender) return;
  $('price').textContent = usdPerTez ? `$${pack.usd}, about ${tez(pack.usd / usdPerTez)} tez, plus the wallet’s network fee.`
    : `$${pack.usd} in tez, plus the wallet’s network fee.`;
}
$('pack').onchange = () => {
  sessionStorage.setItem(intent ? `ac-tezos-pack:${intent.id}` : 'ac-tezos-store-pack', $('pack').value);
  showPack();
};

// ── Signing in to AC ─────────────────────────────────────────────────────────
// A checkout belongs to an AC handle, so the store needs an AC access token to
// open one. Reuse whatever session this browser already has on
// aesthetic.computer; without one the page stays locked.
async function otpDoor() {
  if (!door) door = (await import('/aesthetic.computer/lib/auth0-otp.mjs')).otpSignIn({ clientId:AUTH0_CLIENT_ID });
  return door;
}
function loadScript(src) {
  return new Promise((resolve, reject) => {
    const script = Object.assign(document.createElement('script'), { src, onload:resolve, onerror:reject });
    document.head.append(script);
  });
}
async function spaClient() {
  let cached = false;
  try { cached = Object.keys(localStorage).some(key => key.startsWith('@@auth0spajs@@')); } catch {}
  if (!cached) return null;
  if (!spa) {
    await loadScript('/aesthetic.computer/dep/auth0-spa-js.production.js');
    spa = await window.auth0.createAuth0Client({ domain:'https://hi.aesthetic.computer', clientId:AUTH0_CLIENT_ID,
      cacheLocation:'localstorage', useRefreshTokens:true, authorizationParams:{ redirect_uri:location.origin } });
  }
  return spa;
}
// { token, sub } for the signed-in AC account, or null.
async function signedIn() {
  try {
    const otp = await otpDoor();
    const token = await otp.token();
    if (token) return { token, sub:otp.session().sub };
  } catch {}
  try {
    const auth = await spaClient();
    if (auth) {
      const token = await auth.getTokenSilently();
      const user = await auth.getUser();
      if (token && user?.sub) return { token, sub:user.sub };
    }
  } catch {}
  return null;
}
async function handleFor(sub) {
  try {
    const r = await fetch(`/handle?for=${encodeURIComponent(sub)}`, { cache:'no-store' });
    if (r.ok) return (await r.json()).handle || null;
  } catch {}
  return null;
}
async function showBuyer() {
  buyer = await signedIn();
  const handle = buyer && await handleFor(buyer.sub);
  if (buyer) buyer.handle = handle;
  // Signed out, or no handle yet: locked. Nothing to pick, nothing to read.
  const locked = !handle;
  for (const id of ['lede','pack-label','price','connect']) $(id).hidden = locked;
  $('signin').hidden = !locked;
  $('signin').textContent = buyer ? 'Get a handle' : 'Sign in';
  $('quantity').textContent = 'Braincells';
  $('account').textContent = locked ? '' : `For @${handle.replace(/^@/, '')}`;
  if (!locked) showPack();
}
// ── Wallet and payment ───────────────────────────────────────────────────────
async function useWallet() {
  if (!client) client = new window.beacon.DAppClient({ name:'AC braincells', network:{ type:'mainnet' }, enableMetrics:false,
    appUrl:'https://aesthetic.computer/braincells/', iconUrl:'https://aesthetic.computer/aesthetic.computer/braincell.svg', disableDefaultEvents:false });
  let account = await client.getActiveAccount();
  if (account && account.network?.type !== 'mainnet') { await client.clearActiveAccount(); account = null; }
  if (!account) { await client.requestPermissions({ scopes:['sign','operation_request'] }); account = await client.getActiveAccount(); }
  if (!account || account.network?.type !== 'mainnet') throw new Error('Select a Tezos mainnet wallet.');
  return account;
}
const controls = ['connect','pay','check','pack','again'];
async function action(work) {
  if (busy) return;
  busy = true;
  for (const id of controls) $(id).disabled = true;
  try { await work(); } catch (error) { say(error.message || 'Wallet request interrupted. Check payment before trying again.'); }
  finally { busy = false; for (const id of controls) $(id).disabled = false; $('pack').disabled = Boolean(intent?.sender); }
}
// Opening a checkout reserves nothing and charges nothing; the server makes one
// per purchase so a payment always matches exactly one quote.
async function openCheckout(pack) {
  buyer = buyer?.token ? buyer : await signedIn();
  if (!buyer) { await showBuyer(); throw new Error('Sign in to buy braincells.'); }
  const r = await fetch('/api/easel-tezos', { method:'POST', headers:{ 'Content-Type':'application/json', Authorization:`Bearer ${buyer.token}` },
    body:JSON.stringify({ action:'create' }), signal:AbortSignal.timeout(25_000) });
  const created = await r.json();
  if (!r.ok) throw new Error(created.error || 'Checkout unavailable');
  secret = new URL(created.checkoutURL).hash.slice(1);
  if (!isSecret(secret)) throw new Error('Checkout unavailable');
  sessionStorage.setItem('ac-tezos-checkout', secret);
  sessionStorage.setItem(`ac-tezos-pack:${created.id}`, pack);
  show(await api('status'));
}
$('connect').onclick = () => action(async () => {
  const pack = $('pack').value;
  if (!intent) await openCheckout(pack);
  say('Choose Temple or another Tezos wallet.');
  const account = await useWallet();
  say('Approve the account message in your wallet. This does not move tez.');
  const signed = await client.requestSignPayload({ signingType:'micheline', payload:intent.payload, sourceAddress:account.address });
  show(await api('quote', { pack, address:account.address, publicKey:account.publicKey, signature:signed.signature }));
  say('Review the amount, then pay in your wallet. Quote lasts 15 minutes.');
});
$('pay').onclick = () => action(async () => {
  if (Date.parse(intent.expiresAt) <= Date.now()) { show(intent); return; }
  const account = await useWallet();
  if (account.address !== intent.sender) throw new Error('Select the wallet shown on this checkout.');
  say('Confirm the payment in your wallet.');
  sessionStorage.setItem(`ac-tezos-paying:${intent.id}`, 'true');
  $('pay').hidden = true;
  // The server checks this exact amount and destination independently.
  let response;
  try {
    response = await client.requestOperation({ operationDetails:[{
      kind:'transaction', destination:intent.recipient, amount:intent.amountMutez,
    }] });
  } catch (error) {
    // Only an explicit wallet rejection makes it safe to offer Pay again.
    if (error?.errorType === 'ABORTED_ERROR') { sessionStorage.removeItem(`ac-tezos-paying:${intent.id}`); show(intent); }
    throw error;
  }
  sessionStorage.setItem(`ac-tezos-op:${intent.id}`, response.transactionHash);
  $('pay').hidden = true;
  await checkPayment();
});
async function checkPayment() {
  if (polling || !intent?.sender || intent.status === 'credited') return;
  polling = true;
  try {
    const hash = sessionStorage.getItem(`ac-tezos-op:${intent.id}`);
    show(await api('confirm', hash ? { operationHash:hash } : {}));
    if (intent.status === 'confirming') { $('pay').hidden = true; say('Payment seen. Waiting for three Tezos confirmations…'); }
    else if (intent.status === 'quoted' && !busy) say(sessionStorage.getItem(`ac-tezos-paying:${intent.id}`) ? 'Waiting for your wallet payment. Check again after approving in your wallet.' : 'No confirmed payment yet.');
  } finally { polling = false; }
}
$('check').onclick = () => action(checkPayment);
$('again').onclick = () => action(async () => { forgetCheckout(); say(''); await storefront(); });
document.addEventListener('visibilitychange', () => { if (!document.hidden) checkPayment().catch(error => say(error.message)); });
setInterval(() => { if (!document.hidden && !busy && intent?.sender && intent.status !== 'credited') checkPayment().catch(error => say(error.message)); }, 8000);

function forgetCheckout() {
  secret = null; intent = null;
  sessionStorage.removeItem('ac-tezos-checkout');
  for (const id of ['wallet','pay','check','again','recipient']) $(id).hidden = true;
}
// The store: pick a pack, sign in, then the checkout above takes over.
async function storefront() {
  $('return').hidden = !fromApp;
  fillPacks(STORE_OFFERS);
  const saved = sessionStorage.getItem('ac-tezos-store-pack');
  $('pack').value = STORE_OFFERS.some(offer => offer.id === saved) ? saved : 'braincells-1m-v1';
  $('pack').disabled = false;
  fetch('/api/easel-tezos', { method:'POST', headers:{ 'Content-Type':'application/json' }, body:JSON.stringify({ action:'price' }) })
    .then(r => r.ok ? r.json() : null).then(price => {
      if (Number.isFinite(price?.usdPerTez) && price.usdPerTez > 0) { usdPerTez = price.usdPerTez; if (buyer?.handle) showPack(); }
    }).catch(() => {});
  await showBuyer();
}
try {
  let opened = false;
  if (secret) {
    try { show(await api('status')); opened = true; }
    catch (error) {
      // A stale checkout from an earlier visit falls back to the store; one the
      // app just handed over is reported, since someone is waiting on it.
      if (isSecret(fragment)) throw error;
      forgetCheckout();
    }
  }
  if (opened) { if (intent.sender && intent.status !== 'credited') await checkPayment(); }
  else await storefront();
} catch (error) { $('account').textContent = ''; say(error.message); }
