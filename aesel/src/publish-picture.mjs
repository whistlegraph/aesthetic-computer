// Explicit Picture publishing through AC's existing painting upload/#code stack.
// No provider calls, automatic publication, anonymous fallback, or new tag taxonomy.
import { syncPictureWip, pictureWipRecord, savePictureWipRecord, pictureWipAddress, pictureRecording } from './picture-wip.mjs';
import { readFile, writeFile, mkdir, lstat, rename } from "node:fs/promises";
import { join, resolve, relative } from "node:path";
import { createHash } from "node:crypto";
import { SITE, USER_AGENT } from "./ac-session.mjs";
import { decode } from "../media/picture/png.mjs";
const digest = (b) => createHash("sha256").update(b).digest("hex");
async function safe(root, name) {
  const base = resolve(root),
    file = resolve(base, name);
  if (relative(base, file).startsWith(".."))
    throw new Error("Publication path escapes artifact store.");
  let part = base;
  for (const segment of ["", ...relative(base, file).split("/")]) {
    if (segment) part = join(part, segment);
    try {
      if ((await lstat(part)).isSymbolicLink())
        throw new Error("Publication paths cannot use symlinks.");
    } catch (e) {
      if (e.code !== "ENOENT") throw e;
    }
  }
  return file;
}
async function identity(artifacts) {
  const current = await artifacts.selected();
  if (current?.kind !== "picture")
    throw new Error("Select a Picture before publishing.");
  if (!current.revision.files.includes("composite.png"))
    throw new Error("This Picture has no accepted PNG.");
  const file = await safe(current.root, "composite.png");
  if ((await lstat(file)).size > 32 * 1024 * 1024)
    throw new Error("Painting exceeds 32 MB.");
  const bytes = await readFile(file),
    sha256 = digest(bytes);
  if (sha256 !== current.revision.hashes["composite.png"])
    throw new Error("Accepted Picture hash mismatch.");
  decode(bytes);
  return {
    current,
    bytes,
    sha256,
    receiptName: `publications/${current.id}/v${current.version}.json`,
  };
}
async function receiptRead(artifacts, name) {
  try {
    return JSON.parse(await readFile(await safe(artifacts.root, name), "utf8"));
  } catch (e) {
    if (e.code === "ENOENT") return null;
    throw e;
  }
}
async function receiptWrite(artifacts, name, data) {
  const directory = await safe(
    artifacts.root,
    name.slice(0, name.lastIndexOf("/")),
  );
  await mkdir(directory, { recursive: true, mode: 0o700 });
  const file = await safe(artifacts.root, name),
    temp = await safe(artifacts.root, name + ".pending");
  await writeFile(temp, JSON.stringify(data, null, 2) + "\n", { mode: 0o600 });
  await rename(temp, file);
}
function publication(receipt, site = SITE) {
  if (!/^[A-Za-z0-9]+$/.test(receipt.code || ""))
    throw new Error("Server did not return a valid painting #code.");
  return {
    code: receipt.code,
    tag: `#${receipt.code}`,
    route: `${site}/#${receipt.code}`,
    scanUrl: `${new URL(site).host}/#${receipt.code}`,
    mediaUrl: `${site}/media/paintings/${receipt.code}.png`,
    artifactId: receipt.artifactId,
    version: receipt.version,
    sha256: receipt.sha256,
    handle: receipt.handle,
    verified: receipt.stage === "verified",
    paintingId: receipt.paintingId || null,
  };
}
export async function publishedPicture({ artifacts, handle, site = SITE }) {
  const current = await artifacts.selected();
  if (current?.kind !== "picture") return null;
  const receipt = await receiptRead(
    artifacts,
    `publications/${current.id}/v${current.version}.json`,
  );
  if (
    !receipt ||
    receipt.stage !== "verified" ||
    receipt.handle !== handle ||
    receipt.sha256 !== current.revision.hashes["composite.png"]
  )
    return null;
  return publication(receipt, site);
}
export async function publishPicture({
  artifacts,
  session,
  fetch = globalThis.fetch,
  site = SITE,
  onStep = () => {},
}) {
  if (!session.handle)
    throw new Error(
      session.signedIn
        ? "Claim an @handle before publishing a painting."
        : "Not signed in — run /login first.",
    );
  return artifacts.locked(async () => {
    onStep("saving WIP and steps");
    const wip = await syncPictureWip({artifacts,session,fetch,site});
    const { current, bytes, sha256, receiptName } = await identity(artifacts),
      handle = session.handle;
    let receipt = await receiptRead(artifacts, receiptName);
    if (receipt && (receipt.sha256 !== sha256 || receipt.handle !== handle))
      throw new Error(
        "Publication receipt belongs to another picture or account.",
      );
    const request = (url, options = {}) =>
      fetch(url, { ...options, signal: AbortSignal.timeout(30000) });
    const json = async (response) => response.json().catch(() => ({}));
    const verifyBytes = async (url) => {
      try {
        const r = await request(url, {
          headers: { "User-Agent": USER_AGENT },
          redirect: "follow",
        });
        return r.ok && digest(Buffer.from(await r.arrayBuffer())) === sha256;
      } catch {
        return false;
      }
    };
    const save = async () => receiptWrite(artifacts, receiptName, receipt);
    if (wip.record.status === 'done' && receipt) {
      receipt.code=wip.record.code;receipt.slug=wip.record.slug || receipt.slug;
      if(receipt.stage!=='verified')receipt.stage='registered';
      await save();
    }
    if (wip.record.status === 'done' && !receipt) return { ...pictureWipAddress(wip.record,site), verified:false };
    // User paintings find their recording beside the PNG, using the same name.
    const nameBase = `painting-easel-${current.id}-v${current.version}-${sha256.slice(0, 12)}`;
    if (!receipt) {
      onStep('uploading painting steps');
      const token=await session.token();
      const grantResponse=await request(`${site}/presigned-upload-url/zip/${nameBase}.zip/user`,{headers:{Authorization:`Bearer ${token}`,'User-Agent':USER_AGENT}});
      const grant=await json(grantResponse);
      if(!grantResponse.ok || !grant.uploadURL || new URL(grant.uploadURL).protocol!=='https:')throw new Error('Could not prepare painting steps upload.');
      const uploaded=await request(grant.uploadURL,{method:'PUT',headers:{'Content-Type':'application/zip','x-amz-acl':'public-read'},body:await pictureRecording(wip.piece)});
      if(!uploaded.ok)throw new Error('Could not upload painting steps.');
    }
    if (!receipt) {
      const token = await session.token();
      onStep("requesting painting upload grant");
      const name = `${nameBase}.png`;
      const response = await request(
          `${site}/presigned-upload-url/png/${name}/user`,
          {
            headers: {
              Authorization: `Bearer ${token}`,
              "User-Agent": USER_AGENT,
              Accept: "application/json",
            },
          },
        ),
        grant = await json(response);
      if (
        !response.ok ||
        !grant.uploadURL ||
        typeof grant.slug !== "string" ||
        !grant.slug.endsWith(".png")
      )
        throw new Error(
          grant.error ||
            `Painting upload grant failed (HTTP ${response.status}).`,
        );
      const upload = new URL(grant.uploadURL);
      if (upload.protocol !== "https:")
        throw new Error("Painting upload grant must use HTTPS.");
      receipt = {
        format: 1,
        artifactId: current.id,
        version: current.version,
        sha256,
        handle,
        slug: grant.slug.slice(0, -4),
        uploadedMediaURL: upload.origin + upload.pathname,
        stage: "uploading",
        createdAt: new Date().toISOString(),
      };
      await save();
      onStep("uploading accepted painting");
      const put = await request(upload.href, {
        method: "PUT",
        headers: {
          "Content-Type": "image/png",
          "Content-Disposition": "inline",
          "x-amz-acl": "public-read",
        },
        body: bytes,
      });
      if (!put.ok)
        throw new Error(
          `Painting upload failed (HTTP ${put.status}); /publish checks this receipt before doing anything again.`,
        );
      receipt.stage = "uploaded";
      await save();
    }
    if (receipt.stage === "uploading") {
      onStep("checking previous upload");
      if (!(await verifyBytes(receipt.uploadedMediaURL)))
        throw new Error(
          "Previous upload is not yet verifiable. No duplicate upload was sent; retry /publish later.",
        );
      receipt.stage = "uploaded";
      await save();
    }
    if (!receipt.code) {
      // A tracking response can be lost after the database accepted it. Resolve the
      // saved slug first; never blindly POST a second painting for an unknown result.
      {
        onStep("registering painting #code");
        const token = await session.token();
        receipt.stage = "tracking";
        await save();
        const response = await request(`${site}/api/track-media`, {
            method: "POST",
            headers: {
              Authorization: `Bearer ${token}`,
              "User-Agent": USER_AGENT,
              "Content-Type": "application/json",
            },
            body: JSON.stringify({ slug: receipt.slug, ext: "png", wip:{code:wip.record.code,key:wip.record.key,revision:wip.record.revision} }),
          }),
          tracked = await json(response);
        if (!response.ok || !tracked.code || tracked.slug !== receipt.slug)
          throw new Error(
            tracked.message ||
              `Painting registration failed (HTTP ${response.status}); receipt retained for /publish recovery.`,
          );
        Object.assign(wip.record,{status:"done",slug:tracked.slug});
        await savePictureWipRecord(artifacts,current.id,wip.record);
        receipt.code = tracked.code;
        receipt.paintingId = tracked.paintingId || null;
        receipt.stage = "registered";
        await save();
      }
    }
    const result = publication(receipt, site);
    onStep("verifying public painting");
    const response = await request(
        `${site}/api/painting-code?code=${encodeURIComponent(receipt.code)}`,
      ),
      found = await json(response);
    if (
      !response.ok ||
      found.code !== receipt.code ||
      found.slug !== receipt.slug ||
      found.handle !== handle
    )
      throw new Error(
        "Painting #code is not yet verified for your account; retry /publish to check the saved receipt.",
      );
    if (!(await verifyBytes(result.mediaUrl)))
      throw new Error(
        "Painting was registered but its public PNG is not yet verified; retry /publish to check, without uploading again.",
      );
    receipt.stage = "verified";
    receipt.verifiedAt = new Date().toISOString();
    await save();
    return publication(receipt, site);
  });
}
