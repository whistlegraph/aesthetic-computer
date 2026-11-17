// Cloudflare Workers AI Clairic: typed extraction in one pass, billed on input
// only. Shared by the /api/clairic endpoint, Aesel and local experiments.
// Jev picks among choices; Clairic reads values out of text.
export const CLAIRIC_MODEL = '@cf/cloudflare/clairic';
export const CLAIRIC_TYPES = ['text', 'number', 'date', 'time', 'datetime', 'duration', 'list', 'estimate', 'forecast'];

export async function extractFields({ state = '', fields, images, referenceDate } = {}, {
  accountId = process.env.CLOUDFLARE_ACCOUNT_ID, token = process.env.CLOUDFLARE_AI_TOKEN,
  fetchImpl = globalThis.fetch, signal = AbortSignal.timeout(20000),
} = {}) {
  if (!accountId || !token) throw new Error('Set CLOUDFLARE_ACCOUNT_ID and CLOUDFLARE_AI_TOKEN.');
  const names = Object.keys(fields || {});
  if (!names.length || names.some(name => !CLAIRIC_TYPES.includes(fields[name]?.type)))
    throw new Error('Provide typed fields.');
  if (!state && !images?.length) throw new Error('Provide state text or images.');

  const body = { model: 'clairic', state, fields };
  if (images?.length) body.images = images;
  if (referenceDate) body.reference_date = referenceDate;
  const response = await fetchImpl(
    `https://api.cloudflare.com/client/v4/accounts/${encodeURIComponent(accountId)}/ai/run/${CLAIRIC_MODEL}`, {
      method: 'POST', signal, redirect: 'error',
      headers: { Authorization: `Bearer ${token}`, 'Content-Type': 'application/json' },
      body: JSON.stringify(body),
    });
  if (!response.ok) throw new Error(`Clairic returned HTTP ${response.status}.`);
  const json = await response.json();
  const result = json?.result ?? json; // REST wraps the model output; the binding does not.
  for (const name of names)
    if (!result?.answers || !Object.hasOwn(result.answers, name)) throw new Error(`Missing answer: ${name}`);
  return result;
}
