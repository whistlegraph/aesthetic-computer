/**
 * ask-handle — a new account picks its @handle on the sign-in page.
 *
 * Auth0 post-login Action for the `aesthetic` tenant; see README.md here.
 * Bind it AFTER link-email-identity, so an emailed-code or social sign-in has
 * already been folded into the account it belongs to.
 *
 * It shows the "Pick your handle" Form when Aesthetic Computer says the
 * account has no handle, and only then. The Form's flow claims the name on
 * AC (POST /handle with the signup secret) and shows AC's own words when the
 * name is taken or won't do, so the rules live in one place: AC.
 *
 * `app_metadata.handle` is a note that the question was answered, so the
 * next login asks nothing. AC's `@handles` stays the truth.
 *
 * NOTHING HERE MAY COST SOMEBODY THEIR LOGIN. If AC can't be reached, or
 * answers oddly, the login carries on without a handle and AC asks the way
 * it always has.
 *
 * Secrets: SIGNUP_SECRET (derived by install.sh), FORM_ID, and optionally
 * ONLY_EMAILS — a regular expression; when set, only matching addresses are
 * asked. It is how the Action is tried on test accounts before everyone.
 */

const AC = "https://aesthetic.computer/handle";

exports.onExecutePostLogin = async (event, api) => {
  try {
    if (event.user.app_metadata?.handle) return;
    const { SIGNUP_SECRET, FORM_ID, ONLY_EMAILS } = event.secrets;
    if (!SIGNUP_SECRET || !FORM_ID) return;
    if (ONLY_EMAILS && !new RegExp(ONLY_EMAILS, "i").test(event.user.email || "")) return;
    // A token exchange or a silent refresh has no page to show a form on.
    if (event.transaction?.protocol && !/^(oidc-basic-profile|oidc-implicit-profile|oidc-hybrid-profile|samlp|wsfed)$/.test(event.transaction.protocol)) return;
    if (event.request?.query?.prompt === "none") return;

    const response = await fetch(AC, {
      method: "POST",
      headers: { "content-type": "application/json", "x-ac-signup": SIGNUP_SECRET },
      body: JSON.stringify({ sub: event.user.user_id, email: event.user.email, email_verified: event.user.email_verified === true }),
      signal: AbortSignal.timeout(5000),
    });
    if (!response.ok) return;
    const known = await response.json();
    if (known.ok === "yes" && known.handle) { api.user.setAppMetadata("handle", known.handle); return; }
    if (known.ok !== "no") return;
    api.prompt.render(FORM_ID);
  } catch (error) {
    console.log("ask-handle: skipped —", error.message);
  }
};

exports.onContinuePostLogin = async (event, api) => {
  try {
    const handle = event.prompt?.fields?.handle;
    if (handle) api.user.setAppMetadata("handle", String(handle).replace(/^@/, ""));
  } catch (error) {
    console.log("ask-handle: continue —", error.message);
  }
};
