export const OSKIE_RESOURCE = "https://mcp.aesthetic.computer";
export const LEGACY_OSKIE_RESOURCE = "https://aesthetic.computer/mcp/ac";
export const OSKIE_RESOURCES = [OSKIE_RESOURCE, OSKIE_RESOURCE + "/", LEGACY_OSKIE_RESOURCE];
export async function resolveGameAccount(headers, dependencies = {}) {
  const token = (headers?.authorization || "").replace(/^Bearer\s+/i, "");
  let user;
  if (token.startsWith("act_")) {
    const oauth =
      dependencies.oauth || (await (await import("./ac-oauth.mjs")).acOAuth());
    const linked = await oauth.resolve(token);
    if (!linked || !OSKIE_RESOURCES.includes(linked.resource))
      throw Object.assign(Error("Link Aesthetic.Computer again."), {
        status: 401,
      });
    user = { sub: linked.sub, email_verified: true };
  } else {
    const authorize =
      dependencies.authorize || (await import("./authorization.mjs")).authorize;
    user = await authorize(headers || {});
  }
  if (!user?.sub)
    throw Object.assign(Error("Sign in with AC."), { status: 401 });
  if (user.email_verified !== true)
    throw Object.assign(Error("Verify your AC email."), { status: 403 });
  const locked =
    dependencies.locked || (await import("./account-lock.mjs")).accountLocked;
  if (await locked(user.sub))
    throw Object.assign(Error("This account is locked."), { status: 401 });
  const identity =
    dependencies.identity ||
    (await import("./oskiewar-identity.mjs")).fighterIdentity;
  const owner = await identity(user.sub);
  if (!owner?.sub || !owner.handle)
    throw Object.assign(
      Error("Choose your AC handle at aesthetic.computer/handle."),
      { status: 403 },
    );
  return { sub: owner.sub, handle: "@" + owner.handle.replace(/^@/, "") };
}
