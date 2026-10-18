import {
  NodeOAuthClient, buildAtprotoLoopbackClientMetadata, requestLocalLock,
} from "@atproto/oauth-client-node";
import { memoryStore } from "./link-flow.mjs";

// Deliberately bounded discovery for this localhost experiment. Do not widen
// this list for arbitrary PDSes without DNS/IP and redirect SSRF protection.
export function allowedNetworkURL(input) {
  const url = new URL(input);
  const host = url.hostname;
  return url.protocol === "https:" && !url.username && !url.password &&
    (!url.port || url.port === "443") && (
      ["plc.directory", "bsky.social", "aesthetic.computer", "at.aesthetic.computer"].includes(host) ||
      host.endsWith(".bsky.social") || host.endsWith(".host.bsky.network") ||
      host.endsWith(".at.aesthetic.computer")
    );
}

export async function guardedFetch(input, init) {
  const request = new Request(input, init);
  if (!allowedNetworkURL(request.url)) throw new Error("This prototype supports Bluesky and the AC PDS only.");
  const response = await fetch(request, {
    redirect: "manual",
    signal: AbortSignal.any([request.signal, AbortSignal.timeout(15_000)]),
  });
  if (response.status >= 300 && response.status < 400) {
    await response.body?.cancel();
    throw new Error("Discovery redirects are disabled in this prototype.");
  }
  return response;
}

export function makeOAuth(origin) {
  const stateStore = memoryStore();
  const sessionStore = memoryStore();
  const client = new NodeOAuthClient({
    clientMetadata: buildAtprotoLoopbackClientMetadata({
      redirect_uris: [`${origin}/callback`], scope: "atproto",
    }),
    stateStore,
    sessionStore,
    requestLock: requestLocalLock,
    fetch: guardedFetch,
  });
  return { client, stateStore, sessionStore };
}
