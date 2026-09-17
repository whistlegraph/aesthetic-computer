// Authenticated transport only. REGARDE owns capability and retention checks.
import { authorize } from '../../backend/authorization.mjs';
import { pseudonym } from './oskiewar-consent.mjs';

const headers = { 'Content-Type': 'application/json', 'Cache-Control': 'no-store' };
const fail = (statusCode, message) => ({ statusCode, headers, body: JSON.stringify({ message }) });

export async function handler(event) {
  if (event.httpMethod !== 'POST') return fail(405, 'POST only.');
  const user = await authorize(event.headers);
  if (!user?.sub) return fail(401, 'Sign in before submitting material.');
  if (typeof event.body !== 'string' || Buffer.byteLength(event.body) > 1500000) return fail(413, 'Upload at most 1 MiB total.');
  let body;
  try { body = JSON.parse(event.body); } catch { return fail(400, 'Unreadable submission.'); }
  if (!body || typeof body.capability !== 'string' || !Array.isArray(body.files)) return fail(400, 'A capability and files are required.');
  const { REGARDE_GATEWAY_URL: gateway, REGARDE_SUBJECT_SALT: salt,
    REGARDE_GATEWAY_TOKEN: token } = process.env;
  if (!gateway || !salt || !token) return fail(503, 'The submission desk is unavailable.');
  try {
    const url = new URL(gateway);
    url.pathname = url.pathname.replace(/\/gateway\/?$/, '/submission');
    if (!url.pathname.endsWith('/submission')) return fail(503, 'The submission desk is unconfigured.');
    const upstream = await fetch(url, {
      method: 'POST', headers: { 'Content-Type': 'application/json', Authorization: `Bearer ${token}` },
      body: JSON.stringify({ capability: body.capability, subject: pseudonym(user.sub, salt),
        files: body.files.map(file => ({ source: file?.source, base64: file?.base64 })) }),
      signal: AbortSignal.timeout(10000),
    });
    const result = await upstream.json();
    if (!upstream.ok) return fail(upstream.status, result.error || 'Submission refused.');
    return { statusCode: 201, headers, body: JSON.stringify({ manifest: result.manifest,
      retention: result.retention, purge_at: result.purge_at }) };
  } catch { return fail(502, 'Submission did not complete. Check your connection before trying again.'); }
}
