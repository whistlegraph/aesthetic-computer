const $ = id => document.getElementById(id);
const fragment = location.hash.slice(1);
const secret = /^[a-f0-9]{64}$/.test(fragment) ? fragment : sessionStorage.getItem('ac-tezos-checkout');
if (/^[a-f0-9]{64}$/.test(fragment)) {
  sessionStorage.setItem('ac-tezos-checkout', fragment);
  history.replaceState(null, '', location.pathname);
}
let client, intent, offers = [], busy = false, polling = false;
const say = text => { $('status').textContent = text; };
async function api(action, body = {}) {
  const r = await fetch('/api/easel-tezos', { method:'POST', headers:{ 'Content-Type':'application/json', Authorization:`Bearer ${secret}` },
    body:JSON.stringify({ action, ...body }), signal:AbortSignal.timeout(25_000) });
  const result = await r.json();
  if (!r.ok) throw new Error(result.error || 'Checkout unavailable');
  return result;
}
function show(next) {
  intent = next;
  if (next.offers) {
    offers = next.offers;
    $('pack').replaceChildren(...offers.map(offer => new Option(`$${offer.usd} · ${offer.credits.toLocaleString()} braincells`, offer.id)));
  }
  const savedPack = sessionStorage.getItem(`ac-tezos-pack:${intent.id}`);
  $('pack').value = intent.sender ? intent.pack : offers.some(offer => offer.id === savedPack) ? savedPack : intent.pack;
  $('pack-label').hidden = Boolean(intent.sender) || intent.status !== 'created';
  $('pack').disabled = busy || Boolean(intent.sender);
  showPack();
  $('account').textContent = `For @${intent.handle}`;
  $('recipient').hidden = false;
  $('return').hidden = false;
  $('connect').hidden = ['credited','expired'].includes(intent.status) || Boolean(intent.sender);
  $('check').hidden = !intent.sender || intent.status === 'credited';
  const expired = intent.expiresAt && Date.parse(intent.expiresAt) <= Date.now();
  $('pay').hidden = !intent.sender || intent.status === 'credited' || expired || Boolean(sessionStorage.getItem(`ac-tezos-paying:${intent.id}`));
  if (intent.sender) {
    $('wallet').hidden = false; $('wallet').textContent = intent.sender;
    const tez = (Number(intent.amountMutez) / 1e6).toLocaleString(undefined, { maximumFractionDigits:6 });
    $('price').textContent = `${tez} tez + network fee`;
    $('pay').textContent = `Pay ${tez} tez`;
  }
  if (intent.status === 'credited') { say('Braincells added. They’re ready to use in AC.'); sessionStorage.removeItem(`ac-tezos-op:${intent.id}`); }
  else if (expired || intent.status === 'expired') say('Quote expired. Check an existing payment, or return to Whistlegraph for a new checkout.');
}
function showPack() {
  const pack = !intent.sender && offers.find(offer => offer.id === $('pack').value) || intent;
  $('quantity').textContent = `${pack.credits.toLocaleString()} braincells`;
  if (!intent.sender) $('price').textContent = `$${pack.usd} in tez, plus the wallet’s network fee.`;
}
$('pack').onchange = () => { sessionStorage.setItem(`ac-tezos-pack:${intent.id}`, $('pack').value); showPack(); };
async function useWallet() {
  if (!client) client = new window.beacon.DAppClient({ name:'AC braincells', network:{ type:'mainnet' }, enableMetrics:false,
    appUrl:'https://aesthetic.computer/braincells/', iconUrl:'https://aesthetic.computer/aesthetic.computer/braincell.svg', disableDefaultEvents:false });
  let account = await client.getActiveAccount();
  if (account && account.network?.type !== 'mainnet') { await client.clearActiveAccount(); account = null; }
  if (!account) { await client.requestPermissions({ scopes:['sign','operation_request'] }); account = await client.getActiveAccount(); }
  if (!account || account.network?.type !== 'mainnet') throw new Error('Select a Tezos mainnet wallet.');
  return account;
}
async function action(work) {
  if (busy) return;
  busy = true;
  for (const id of ['connect','pay','check','pack']) $(id).disabled = true;
  try { await work(); } catch (error) { say(error.message || 'Wallet request interrupted. Check payment before trying again.'); }
  finally { busy = false; for (const id of ['connect','pay','check']) $(id).disabled = false; $('pack').disabled = Boolean(intent.sender); }
}
$('connect').onclick = () => action(async () => {
  const pack = $('pack').value;
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
document.addEventListener('visibilitychange', () => { if (!document.hidden) checkPayment().catch(error => say(error.message)); });
setInterval(() => { if (!document.hidden && !busy && intent?.sender && intent.status !== 'credited') checkPayment().catch(error => say(error.message)); }, 8000);
try {
  if (!secret) throw new Error('Open a Tezos checkout from Whistlegraph’s Brain settings.');
  show(await api('status'));
  if (intent.sender && intent.status !== 'credited') await checkPayment();
} catch (error) { $('account').textContent = ''; say(error.message); }
