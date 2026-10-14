import {
  createNoPaintPiece,
  createNoPaintProposalLayer,
  appendNoPaintLayer,
} from "/aesthetic.computer/lib/nopaint-pieces.mjs";
import { encodePaintingState } from "/aesthetic.computer/lib/painting-state.mjs";
import { read, pngBytes } from "./images.mjs";
const hash = async (bytes) =>
  Array.from(
    new Uint8Array(await crypto.subtle.digest("SHA-256", bytes)),
    (v) => v.toString(16).padStart(2, "0"),
  ).join("");
const secret = () =>
  Array.from(crypto.getRandomValues(new Uint8Array(24)), (v) =>
    v.toString(16).padStart(2, "0"),
  ).join("");
export async function publish(game, onstep = () => {}) {
  if (game.busy || game.publishing) throw Error("Wait for the current move.");
  if (!game.account.value.connected || game.account.value.stale)
    throw Error("Sign in with AC before Done.");
  game.publishing = true;
  game.changed();
  const track = game.track,
    handle = game.account.value.handle.replace(/^@/, "");
  const persist = () => game.persist();
  const request = (path, body) => game.account.request(path, body, 45000);
  try {
    let record = track.publication;
    if (record && record.handle !== handle)
      throw Error("Sign into the AC account that started this Done.");
    if (!record) {
      record = track.publication = {
        id: secret(),
        key: secret(),
        handle,
        pending: true,
        revision: 0,
        stage: "wip",
        createdAt: Date.now(),
      };
      await persist();
    }
    let piece;
    const zip = new globalThis.JSZip(),
      steps = [];
    for (const [index, step] of track.steps.entries()) {
      const c = await read(step.image),
        pixels = c.getContext("2d").getImageData(0, 0, 256, 256).data;
      if (!piece)
        piece = createNoPaintPiece({
          seed: track.id,
          width: 256,
          height: 256,
          pixels,
          role: "nopaint",
        });
      else
        piece = appendNoPaintLayer(
          piece,
          createNoPaintProposalLayer({
            piece,
            proposal: { kind: "nopaint", model: step.model, seed: step.seed },
            pixels,
            pixelMode: "composite",
          }),
          pixels,
        );
      const name = `${step.at} - nopaint~${index}`;
      steps.push({ step: name });
      zip.file(`${name}.png`, await pngBytes(step.image));
      piece.layers.at(-1).timestamp = new Date(step.at).toISOString();
      piece.layers.at(-1).label = `nopaint~${index}`;
    }
    zip.file("painting.json", JSON.stringify(steps));
    const bytes = await pngBytes(game.accepted),
      sha = await hash(bytes),
      name = `painting-nopaint-${track.id}-${sha.slice(0, 12)}`;
    if (record.sha && record.sha !== sha)
      throw Error("The painting changed after Done began.");
    record.sha = sha;
    onstep("Saving painting steps…");
    if (!record.code) {
      Object.assign(
        record,
        await request("/api/painting-wip", {
          action: "create",
          id: record.id,
          key: record.key,
          width: 256,
          height: 256,
          initialLayers: 1,
        }),
      );
      await persist();
    }
    if (record.stage === "wip") {
      Object.assign(
        record,
        await request("/api/painting-wip", {
          action: "save",
          code: record.code,
          key: record.key,
          revision: record.revision,
          state: await encodePaintingState(piece),
        }),
      );
      await persist();
      const zipGrant = await request(
        `/presigned-upload-url/zip/${name}.zip/user`,
      );
      await upload(
        zipGrant.uploadURL,
        await zip.generateAsync({ type: "uint8array", compression: "DEFLATE" }),
        "application/zip",
      );
      record.stage = "steps";
      await persist();
    }
    if (record.stage === "steps") {
      const grant = await request(`/presigned-upload-url/png/${name}.png/user`);
      if (typeof grant.slug !== "string" || !grant.slug.endsWith(".png"))
        throw Error("Invalid painting upload grant.");
      record.slug = grant.slug.slice(0, -4);
      record.uploadURL = grant.uploadURL;
      record.stage = "uploading";
      await persist();
    }
    if (record.stage === "uploading") {
      onstep("Uploading painting…");
      await upload(record.uploadURL, bytes, "image/png");
      record.stage = "uploaded";
      delete record.uploadURL;
      await persist();
    }
    if (record.stage === "uploaded") {
      onstep("Publishing painting…");
      const result = await request("/api/track-media", {
        slug: record.slug,
        ext: "png",
        wip: { code: record.code, key: record.key, revision: record.revision },
      });
      if (result.code !== record.code || result.slug !== record.slug)
        throw Error("Publication could not be verified.");
      record.stage = "registered";
      await persist();
    }
    onstep("Verifying painting…");
    const found = await (
      await fetch(
        `/api/painting-code?code=${encodeURIComponent(record.code)}`,
        { cache: "no-store", signal: AbortSignal.timeout(30000) },
      )
    ).json();
    if (
      found.code !== record.code ||
      found.slug !== record.slug ||
      found.handle !== handle
    )
      throw Error("Painting identity is not yet verified. Retry Done.");
    const response = await fetch(`/media/paintings/${record.code}.png`, {
      cache: "no-store",
      signal: AbortSignal.timeout(30000),
    });
    if (!response.ok || (await hash(await response.arrayBuffer())) !== sha)
      throw Error("The public PNG is not yet verified. Retry Done.");
    record.stage = "verified";
    record.pending = false;
    record.url = `https://aesthetic.computer/#${record.code}`;
    await persist();
    await game.store.setting("published", record.url);
    return record.url;
  } finally {
    game.publishing = false;
    game.changed();
  }
}
async function upload(url, bytes, type) {
  const target = new URL(url);
  if (target.protocol !== "https:" || target.username || target.password)
    throw Error("Invalid upload address.");
  const response = await fetch(target, {
    method: "PUT",
    headers: { "Content-Type": type, "x-amz-acl": "public-read" },
    body: bytes,
    signal: AbortSignal.timeout(45000),
  });
  if (!response.ok)
    throw Error(
      "Painting upload failed. Retry Done to continue the same publication.",
    );
}
