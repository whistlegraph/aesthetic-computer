import { Account } from "./account.mjs";
import { signInRequest, sendSignInLink } from "./signin.mjs";
const $ = (id) => document.getElementById(id),
  account = new Account();
const code = new URLSearchParams(location.hash.slice(1)).get("code");
let token,
  busy = false;
function lock(value) {
  busy = value;
  $("connect-account").disabled = $("send-link").disabled = value;
}
async function connect() {
  if (busy) return;
  lock(true);
  try {
    if (!token) {
      const sdk = await account.sdk();
      await sdk.loginWithPopup();
      token = await sdk.getTokenSilently();
    }
    await signInRequest({ action: "claim", code }, { token });
    $("connect-account").hidden = $("link-form").hidden = true;
    $("signin-instructions").hidden = true;
    $("signin-title").textContent = "Signed in";
    $("signin-message").textContent =
      "Return to No Paint on the screen you scanned.";
  } catch (error) {
    token = null;
    $("connect-account").textContent = "Sign in with AC";
    $("signin-message").textContent = error.message;
  } finally {
    lock(false);
  }
}
$("connect-account").onclick = connect;
$("link-form").onsubmit = async (event) => {
  event.preventDefault();
  if (busy) return;
  lock(true);
  try {
    await sendSignInLink(code, $("email").value);
    $("signin-message").textContent =
      "Check your email and tap the sign-in link. No Paint will sign in automatically.";
  } catch (error) {
    $("signin-message").textContent = error.message;
  } finally {
    lock(false);
  }
};
if (!/^[A-Z0-9]{6}$/.test(code || "")) {
  lock(true);
  $("signin-message").textContent =
    "Scan the sign-in QR on No Paint to continue.";
} else {
  // Loading the phone page never claims another screen without a click.
  account
    .token()
    .then(async (value) => {
      if (!value || busy) return;
      token = value;
      const response = await fetch("/api/easel-credits", {
        headers: { Authorization: `Bearer ${token}` },
      });
      if (!response.ok) {
        token = null;
        return;
      }
      const { handle } = await response.json();
      if (handle && !busy)
        $("connect-account").textContent = `Continue as ${handle}`;
    })
    .catch(() => {});
}
