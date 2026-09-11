/**
 * link-email-identity — an emailed code signs you into the account you have.
 *
 * Auth0 post-login Action. Install it in the Login flow of the `aesthetic`
 * tenant; see README.md in this directory.
 *
 * WHY THIS HAS TO EXIST
 *
 * Auth0 treats every connection as a different person. Every Aesthetic
 * Computer account lives on `Username-Password-Authentication` — of the first
 * hundred users in the tenant, ninety-nine are on it and not one has a second
 * identity. The native sign-in oskiewar ships (a code mailed to you, no
 * redirect, no Turnstile) authenticates against the `email` passwordless
 * connection instead, which mints a SEPARATE user with a `email|…` subject.
 *
 * Measured, not assumed: signing in that way as mail@aesthetic.computer
 * produced `email|6aa4572f21241b122b90e4d3`, an Auth0 user one minute old, and
 * `GET /handle?for=<that sub>` answered "No handle(s) found." Shipped without
 * this Action, every returning player would arrive as a stranger and be asked
 * to claim a handle they already own — and could not, because it is taken, by
 * them.
 *
 * WHAT IT DOES
 *
 * When a login arrives on the `email` connection and an existing account has
 * the same verified address, the two are merged — the existing account is the
 * primary, the passwordless identity becomes one of its identities — and the
 * login carries on as the person they actually are.
 *
 * WHY BOTH SIDES MUST BE VERIFIED
 *
 * Linking on an unverified address is an account takeover. The code proves the
 * person at the keyboard controls the mailbox, which settles the passwordless
 * side; the existing account only proves it if Auth0 has already verified it.
 * If it has not, this does nothing at all — better a stranger with no handle,
 * which is recoverable, than a stranger inside somebody else's account, which
 * is not.
 */

const ManagementClient = require("auth0").ManagementClient;

// The connection the emailed codes authenticate against. Anything else — the
// database connection, a social provider — is a login that already knows who
// it is and must be left alone.
const PASSWORDLESS_CONNECTION = "email";

exports.onExecutePostLogin = async (event, api) => {
  if (event.connection?.name !== PASSWORDLESS_CONNECTION) return;
  // The code was accepted, so Auth0 has marked the address verified. Belt and
  // braces: a future connection setting must not quietly turn this into a
  // takeover.
  if (event.user.email_verified !== true || !event.user.email) return;

  // Only an identity created by THIS login. Linking makes the secondary user
  // cease to exist as a subject, and Aesthetic Computer keys by subject —
  // `@handles` is `findOne({_id: sub})`, and it is not alone. A passwordless
  // identity with history behind it therefore cannot be merged here without
  // re-keying that history first: @jeffrey's own `email|…` had thirty-five
  // logins and a handle row of its own when this was written, and linking it
  // would have orphaned both. A brand new identity has nothing to lose, which
  // is the only case this Action is allowed to touch.
  if ((event.stats?.logins_count ?? 0) > 1) return;

  const management = new ManagementClient({
    domain: event.secrets.AUTH0_DOMAIN,
    clientId: event.secrets.AUTH0_M2M_CLIENT_ID,
    clientSecret: event.secrets.AUTH0_M2M_SECRET,
  });

  let candidates;
  try {
    ({ data: candidates } = await management.usersByEmail.getByEmail({
      email: event.user.email,
    }));
  } catch (error) {
    // A lookup that fails must not cost somebody their login. They arrive as
    // the passwordless identity, which is wrong but harmless and recoverable
    // the moment this works again.
    console.log("link-email-identity: lookup failed —", error.message);
    return;
  }

  // The account they already had: same address, verified, and NOT the
  // passwordless identity we just logged in as.
  const primary = (candidates || []).find((user) =>
    user.user_id !== event.user.user_id &&
    user.email_verified === true &&
    (user.identities || []).some((identity) =>
      identity.connection !== PASSWORDLESS_CONNECTION));

  // Nobody to be. This is a genuinely new person, and the passwordless
  // identity they just made is the right one to keep.
  if (!primary) return;

  try {
    await management.users.link({ id: primary.user_id }, {
      provider: event.user.identities[0].provider,
      user_id: event.user.identities[0].user_id,
    });
  } catch (error) {
    console.log("link-email-identity: link failed —", error.message);
    return;
  }

  // The tokens about to be issued are for the identity that started this
  // login, which no longer exists on its own — it is now one of the primary's.
  // Without this the person is merged and still handed a stranger's subject.
  api.authentication.setPrimaryUser(primary.user_id);
};
