const $ = id => document.getElementById(id);
const fragment = location.hash.slice(1);
const secret = /^[a-f0-9]{64}$/.test(fragment) ? fragment : sessionStorage.getItem('whistlegraph-mint');
if (/^[a-f0-9]{64}$/.test(fragment)) {
  sessionStorage.setItem('whistlegraph-mint', fragment);
  history.replaceState(null, '', location.pathname);
}
let client, intent, busy = false, polling = false;
const say = text => { $('status').textContent = text; };
async function api(action, body = {}) {
  const response = await fetch('/api/whistlegraph-mint', { method:'POST',
    headers:{ 'Content-Type':'application/json', Authorization:`Bearer ${secret}` },
    body:JSON.stringify({ action, ...body }), signal:AbortSignal.timeout(90_000) });
  const state = await response.json();
  if (!response.ok) throw Error(state.error || 'Could not check this mint');
  return state;
}
function show(state) {
  intent = { ...intent, ...state };
  $('title').textContent = intent.title;
  $('piece').textContent = `${intent.code} · version ${intent.version} · @${intent.handle}`;
  $('art').dataset.aspect = intent.aspect || '2:3';
  $('settings').textContent = `${intent.editions} edition${intent.editions === 1 ? '' : 's'} · ${intent.royalties / 10}% royalties · Tezos mainnet`;
  if (/^ipfs:\/\/Qm[1-9A-HJ-NP-Za-km-z]{44}$/.test(intent.artifactUri || '')) {
    const src = 'https://ipfs.io/ipfs/' + intent.artifactUri.slice(7);
    if ($('art').src !== src) $('art').src = src;
    $('art').hidden = false;
  }
  $('wallet').textContent = intent.sender || '';
  $('connect').hidden = !['packed','binding'].includes(intent.status);
  $('review').hidden = intent.status !== 'ready';
  $('mint').hidden = intent.status !== 'ready';
  $('mint').disabled = busy || !$('checked').checked;
  $('check').hidden = !['requested','confirming'].includes(intent.status);
  if (intent.status === 'packing') say('Preparing the HTML pack…');
  if (intent.status === 'packed') say('Play the packed version, then connect your wallet.');
  if (intent.status === 'binding') say('Reconnect the same wallet to finish preparing metadata.');
  if (intent.status === 'ready') say('Review this version, then approve minting in your wallet.');
  if (intent.status === 'requested') say('A wallet request is pending. After approving or rejecting it, check here before starting another mint.');
  if (intent.status === 'confirming') say('Mint seen. Waiting for three Tezos confirmations…');
  if (intent.status === 'failed') say(intent.error);
  if (intent.status === 'cancelled') say('This preview was discarded. Return to Whistlegraph.');
  if (intent.status === 'minted') {
    say(`Minted OBJKT #${intent.tokenId}.`);
    if (/^\d+$/.test(intent.tokenId)) { $('token').href = `https://teia.art/objkt/${intent.tokenId}`; $('token').hidden = false; }
  }
}
$('checked').onchange = () => { $('mint').disabled = busy || !$('checked').checked; };
async function wallet() {
  if (!client) client = new window.beacon.DAppClient({ name:'Whistlegraph', network:{ type:'mainnet' },
    enableMetrics:false, appUrl:'https://aesthetic.computer/mint/' });
  let account = await client.getActiveAccount();
  if (account && account.network?.type !== 'mainnet') { await client.clearActiveAccount(); account = null; }
  if (!account) { await client.requestPermissions({ scopes:['sign','operation_request'] }); account = await client.getActiveAccount(); }
  if (!account || account.network?.type !== 'mainnet') throw Error('Choose a Tezos mainnet wallet.');
  return account;
}
async function action(work) {
  if (busy) return;
  busy = true;
  for (const id of ['connect','mint','check']) $(id).disabled = true;
  try { await work(); } catch (error) { say(error.message || 'Wallet request interrupted. Check the mint before trying again.'); }
  finally {
    busy = false;
    $('connect').disabled = false; $('check').disabled = false; $('mint').disabled = !$('checked').checked;
  }
}
$('connect').onclick = () => action(async () => {
  const account = await wallet();
  say('Approve the artwork message in your wallet. This does not mint or move tez.');
  const signed = await client.requestSignPayload({ signingType:'micheline', payload:intent.payload, sourceAddress:account.address });
  show(await api('bind', { address:account.address, publicKey:account.publicKey, signature:signed.signature }));
});
$('mint').onclick = () => action(async () => {
  if (!$('checked').checked || intent.status !== 'ready') return;
  const account = await wallet();
  if (account.address !== intent.sender) throw Error('Select the wallet shown on this mint.');
  const begun = await api('begin');
  show(begun);
  // The server fixes the reviewed metadata and all contract arguments. No
  // automatic retry: a suspended iOS callback might already have minted.
  const result = await client.requestOperation({ operationDetails:[begun.operation] });
  sessionStorage.setItem(`whistlegraph-mint-op:${intent.id}`, result.transactionHash);
  show(await api('confirm', { operationHash:result.transactionHash }));
});
async function refresh() {
  if (polling || busy) return;
  polling = true;
  try {
    show(await api('status'));
    if (['requested','confirming'].includes(intent.status)) {
      const operationHash = sessionStorage.getItem(`whistlegraph-mint-op:${intent.id}`);
      show(await api('confirm', operationHash ? { operationHash } : {}));
    }
  } finally { polling = false; }
}
$('check').onclick = () => refresh().catch(error => say(error.message));
document.addEventListener('visibilitychange', () => { if (!document.hidden) refresh().catch(error => say(error.message)); });
setInterval(() => { if (!document.hidden && intent && !['minted','failed'].includes(intent.status)) refresh().catch(error => say(error.message)); }, 6000);
try {
  if (!secret) throw Error('Open Mint on HEN from your piece in Whistlegraph.');
  await refresh();
} catch (error) { say(error.message); }
