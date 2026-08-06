function requestHeader(context, name) {
  const value = context?.headers?.[name.toLowerCase()];
  return Array.isArray(value) ? value[0] : value || "";
}

export function loopboyWaitIdentity({ context = {}, env = process.env, requestedContact = "" }) {
  const contact = String(
    requestHeader(context, "x-slab-loopboy-contact") || env.SLAB_LOOPBOY_CONTACT || "",
  ).trim().toLowerCase();
  const sessionId = String(
    requestHeader(context, "x-slab-prompt-session-id") || env.SLAB_PROMPT_SESSION_ID || "",
  ).trim();

  if (!contact) {
    throw new Error("prox_loopboy_wait is available only inside a SLAB_LOOPBOY_CONTACT session");
  }
  if (!sessionId) {
    throw new Error("prox_loopboy_wait requires the calling Loopboy session id");
  }

  const requested = String(requestedContact).trim().toLowerCase();
  if (requested && requested !== contact) {
    throw new Error(`this Loopboy is bound to ${contact}, not ${requested}`);
  }

  return { contact, sessionId };
}

export function authorizeLoopboyWait({ context = {}, env = process.env, loops = {}, requestedContact = "" }) {
  const { contact, sessionId } = loopboyWaitIdentity({ context, env, requestedContact });

  const loop = loops[contact];
  if (!loop?.sessionId) {
    throw new Error(`Loopboy ${contact} does not have a bound inbox route`);
  }
  if (String(loop.sessionId) !== sessionId) {
    throw new Error(`this Loopboy session is not the bound ${contact} listener`);
  }

  return { contact, sessionId, loop };
}
