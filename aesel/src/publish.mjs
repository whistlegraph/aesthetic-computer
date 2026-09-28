import {retryNetwork, httpError} from "./network.mjs";
import {createHash} from 'node:crypto';
import { PieceRevisions, validatePieceSource } from "./revisions.mjs";
// publish.mjs — put a piece live under the signed-in user's @handle.
//
// This mirrors the web prompt's `publish` command exactly: ask the site for a
// presigned upload grant in the user bucket, PUT the source there, and the
// piece answers at https://aesthetic.computer/@handle/slug. It is not the
// anonymous /api/store-piece short-code path.
import { readFileSync } from "node:fs";
import { basename, extname, resolve } from "node:path";
import { SITE, USER_AGENT } from "./ac-session.mjs";

export const PUBLISHABLE = {
  ".mjs": "application/javascript; charset=utf-8",
  ".lisp": "text/x-lisp; charset=utf-8",
};
export const MAX_SOURCE_LENGTH = 100_000;

export function slugFor(file, explicit = "") {
  const slug = String(explicit || basename(file, extname(file))).trim();
  if (!/^[a-zA-Z0-9_-]+$/.test(slug)) {
    throw new Error(`slug "${slug}" may only use letters, digits, "-" and "_"`);
  }
  return slug;
}

export function looksLikePiece(source, extension) {
  if (extension === ".lisp") return source.trim().length > 0;
  return /export\s+(?:async\s+)?(?:function\s+(?:boot|paint|sim|act|beat|leave|meta)\b|default\b|\{)/.test(source);
}

export function planPublish({ file, slug: explicit = "", handle = "", cwd = process.cwd(), site = SITE }) {
  const path = resolve(cwd, file);
  const extension = extname(path).toLowerCase();
  const mime = PUBLISHABLE[extension];
  if (!mime) throw new Error(`only ${Object.keys(PUBLISHABLE).join(" and ")} pieces can be published`);
  const slug = slugFor(path, explicit);
  const name = `piece-${slug}${extension}`;
  return {
    path,
    extension,
    mime,
    slug,
    name,
    grantUrl: `${site}/presigned-upload-url/${extension.slice(1)}/${name}/user`,
    route: handle ? `${site}/@${handle}/${slug}` : "",
    mediaUrl: handle ? `${site}/media/@${handle}/piece/${slug}${extension}` : "",
  };
}

export async function publishPiece({
  file,
  slug = "",
  session,
  fetch = globalThis.fetch,
  cwd = process.cwd(),
  site = SITE,
  onStep = () => {},
  onRetry = () => {},
  retryOptions = {},
  source: snapshot,
  // Hosts without a desktop revision ledger can explicitly supply null.
  version: snapshotVersion,
}) {
  const handle = session.handle;
  if (!handle) {
    throw new Error(
      session.signedIn
        ? "this account has no @handle yet — claim one at aesthetic.computer/handle"
        : "not signed in — run /login first",
    );
  }
  const plan = planPublish({ file, slug, handle, cwd, site });
  const source = snapshot ?? readFileSync(plan.path, "utf8");
  if (source.length > MAX_SOURCE_LENGTH) {
    throw new Error(`source is ${source.length} characters; the limit is ${MAX_SOURCE_LENGTH}`);
  }
  if (!looksLikePiece(source, plan.extension)) {
    throw new Error("this file does not export a piece (boot, paint, sim, act, or default)");
  }

  await validatePieceSource(source, plan.path);
  const token = await session.token();
  onStep("requesting upload grant");
  const retry = operation => retryNetwork(operation, {...retryOptions, onRetry});
  const grant = await retry(async signal => {
    const presign = await fetch(plan.grantUrl, {
      signal, headers: { Authorization: `Bearer ${token}`, "User-Agent": USER_AGENT, Accept: "application/json" },
    });
    const grant = await presign.json().catch(error => { if (error instanceof SyntaxError) return {}; throw error; });
    if (!presign.ok || !grant.uploadURL) {
      throw httpError(grant.error || grant.message || `upload grant failed (HTTP ${presign.status})`, presign.status);
    }
    return grant;
  });

  onStep("uploading");
  await retry(async signal => {
    const put = await fetch(grant.uploadURL, {
      signal, method: "PUT",
      headers: { "Content-Type": plan.mime, "Content-Disposition": "inline", "x-amz-acl": "public-read" },
      body: source,
    });
    if (!put.ok) throw httpError(`upload failed (HTTP ${put.status})`, put.status);
  });

  onStep("verifying");
  const verified = await retry(async signal => {
    const check = await fetch(plan.mediaUrl, { signal, headers: { "User-Agent": USER_AGENT }, redirect: "follow" });
    if (!check.ok) throw httpError(`verification failed (HTTP ${check.status})`, check.status);
    return (await check.text()).trim() === source.trim();
  });

  let registration=null;
  const savedVersion=snapshotVersion === undefined ? new PieceRevisions(plan.path).list().findLast(v=>v.revision===createHash('sha256').update(source).digest('hex'))?.version : snapshotVersion;
  if(verified&&Number.isInteger(savedVersion)&&savedVersion>=2){
    try{const response=await fetch(`${site}/api/register-piece`,{method:'POST',headers:{Authorization:`Bearer ${token}`,'Content-Type':'application/json','User-Agent':USER_AGENT},body:JSON.stringify({version:savedVersion,slug:plan.slug,ext:plan.extension.slice(1),revision:createHash('sha256').update(source).digest('hex')}),signal:AbortSignal.timeout(10000)});const data=await response.json();registration=response.ok?data:{error:data.error||'Feed registration unavailable'};}
    catch{registration={error:'Feed registration unavailable'};}
  }
  return { ...plan, handle, verified, registration, bytes: Buffer.byteLength(source) };
}
