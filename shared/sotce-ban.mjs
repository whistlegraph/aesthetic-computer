// Durable Sotce bans. Auth0 blocks new logins; this also rejects existing tokens.
export async function sotceBanned(db, user) {
  const keys = [];
  if (typeof user?.sub === "string" && user.sub) keys.push(`sub:${user.sub}`);
  if (typeof user?.email === "string" && user.email.trim()) {
    keys.push(`email:${user.email.trim().toLowerCase()}`);
  }
  if (!keys.length || !db) return true;
  try {
    return !!(await db.collection("sotce-bans").findOne(
      { _id: { $in: keys } },
      { projection: { _id: 1 }, maxTimeMS: 2000 },
    ));
  } catch {
    // Losing the ban store must not restore access to a banned account.
    return true;
  }
}
