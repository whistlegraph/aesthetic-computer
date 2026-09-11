// account, 26.09.11
// The oskiewar.com sign-in door. Everything about *this* shell's account
// furniture lives here; the Auth0 dance itself is in
// /aesthetic.computer/lib/auth0-otp.mjs, because aesthetic.computer wants the
// same in-page option and the dance is the half that belongs to neither
// surface.
//
// Two doors, and the native one is the front one:
//
//   native — a form in this page. Auth0 mails a six digit code, the code buys
//            tokens, nothing navigates. This is the default because the hosted
//            login page carries Cloudflare Turnstile and that widget loops
//            forever for some people, which is a sign-in that cannot be
//            completed at all.
//   redirect — the old auth0-spa-js `loginWithRedirect`. Kept, because an SSO
//            session on hi.aesthetic.computer is still the only thing that can
//            recognise somebody who signed in on aesthetic.computer, and
//            because if the tenant ever refuses the native path the form says
//            so and offers this instead of dead-ending.
//
// The contract with the game (oskiewar.js) is two globals:
//
//   __oskiewarAccount      — the shell writes, the game reads every frame.
//   __oskiewarAccountDoor  — the game writes "login" / "handle" / "", the shell
//                            reacts. Defined as an accessor so a press opens
//                            the panel on that same frame.
//
// And one the shell publishes for itself, `__oskiewarAccountOpen`: whether a
// panel is actually up. Not the same question as the door — the corner button
// raises the panel without the game asking — and it is what mac-test.html's
// keyboard and gamepad guards read so that typing an address never also throws
// a punch. The game is welcome to read it too.

import { otpSignIn } from "/aesthetic.computer/lib/auth0-otp.mjs";

const DOMAIN = "hi.aesthetic.computer";
const CLIENT = "LVdZaMbyXctkGfZDnpzDATB5nR0ZhmMt";
const AC = "https://aesthetic.computer";

// The handle endpoint reports trouble in a `message`, and the status code is not
// a reliable second opinion — "taken" arrives as a 500. So the message is what
// gets read, and every one of them is shown to the person rather than folded
// into "something went wrong": being told the word filter rejected your handle
// is the difference between trying again and giving up.
const claimTrouble = {
  taken: "someone already has that one",
  invalid: "letters and numbers, with . or _ between them",
  "too long": "sixteen characters at most",
  reserved: "that one is reserved",
  naughty: "the word filter says no to that one",
  same: "that one is already yours",
  unverified: "auth0 has not verified that email address yet",
  unauthorized: "that sign-in expired — sign in again",
  error: "the server choked on it, try again",
};

export default function mountAccount({ sfx = () => {}, probe = true } = {}) {
  const otp = otpSignIn({ domain: DOMAIN, clientId: CLIENT });

  const account = globalThis.__oskiewarAccount;
  const handleLabel = document.querySelector("#account-handle");
  const logout = document.querySelector("#logout");
  const panel = document.querySelector("#account-panel");
  const form = document.querySelector("#account-form");
  const title = document.querySelector("#account-title");
  const label = document.querySelector("#account-label");
  const field = document.querySelector("#account-field");
  const note = document.querySelector("#account-note");
  const go = document.querySelector("#account-go");
  const back = document.querySelector("#account-back");
  const viaWeb = document.querySelector("#account-redirect");

  // Two askers, one panel. The game writes `__oskiewarAccountDoor` on every
  // frame it re-decides — and on the title screen it decides "closed" — so the
  // corner button cannot raise the panel by writing that global: the next frame
  // would slam it shut. The game's request and this shell's own are held apart
  // and the panel shows whichever is asking.
  let door = ""; // What the game asked for.
  let want = ""; // What this shell's own furniture asked for.
  let showing = ""; // What is actually on screen.
  let step = "email"; // "email" → "code" → "handle"
  let address = ""; // Remembered between the two halves of the OTP dance.
  let busy = false;
  // How the live session was established, which decides what logging out means.
  // An OTP session set no cookie anywhere, so forgetting the tokens ends it; an
  // SSO session lives on hi.aesthetic.computer and only /v2/logout can end it.
  let source = "";
  // Whose session this is, in Auth0's spelling. Held because claiming a handle
  // reads the profile back afterwards and the two doors learn the sub in
  // different places.
  let who = "";
  let spa = null; // The auth0-spa-js client, built only if the redirect path is used.

  // ⌨️ The steps, as the field wants to be dressed for each one. The input
  // attributes are the whole reason this panel is DOM and not canvas text: a
  // focused <input> is what summons the on-screen keyboard on a phone and on the
  // Xbox, and `autocomplete` is what makes a mailed code one tap to fill.
  const steps = {
    email: {
      title: "sign in",
      label: "your email",
      note: "we'll mail you a six digit code",
      go: "send the code",
      type: "email", mode: "email", fill: "email", max: 254,
    },
    code: {
      title: "check your mail",
      label: "the code",
      note: "",
      go: "sign in",
      type: "text", mode: "numeric", fill: "one-time-code", max: 6,
    },
    handle: {
      title: "pick a handle",
      label: "your @handle",
      note: "letters and numbers — this is the name you fight under",
      go: "claim it",
      type: "text", mode: "text", fill: "off", max: 16,
    },
  };

  function say(message, trouble = false) {
    note.textContent = message;
    note.classList.toggle("trouble", trouble && Boolean(message));
    // Trouble always means "try that again", and the submit button loses focus
    // while it is disabled — so the field takes it back, with what was rejected
    // selected so the next keystroke replaces it. On a TV that is the difference
    // between typing and hunting for the box with a thumbstick.
    if (trouble) { field.focus(); field.select(); }
  }

  function dress(next, message) {
    step = next;
    const shape = steps[next];
    title.textContent = shape.title;
    label.textContent = shape.label;
    go.textContent = shape.go;
    field.type = shape.type;
    field.inputMode = shape.mode;
    field.autocomplete = shape.fill;
    field.maxLength = shape.max;
    field.value = "";
    say(message ?? shape.note);
    viaWeb.hidden = next !== "email";
    field.focus();
  }

  function working(yes) {
    busy = yes;
    go.disabled = yes;
    field.readOnly = yes;
    panel.classList.toggle("working", yes);
  }

  // The single writer of what is on screen. Everything else moves `door` or
  // `want` and syncs, so the panel and the globals can never disagree.
  //
  // `want` outranks `door` because it is the more informed of the two: the shell
  // knows the flow has reached the handle step, while the game is still asking
  // for the "login" it asked for a second ago and will keep asking until its
  // next frame notices.
  function sync(message) {
    const asked = want || door;
    if (!asked) {
      if (!showing) return;
      showing = "";
      panel.hidden = true;
      globalThis.__oskiewarAccountOpen = false;
      working(false);
      return;
    }
    // Only the first frame of a request dresses the panel. Mid-flow — an email
    // sent, a code being typed — the same standing request must not reset the
    // field back to the email step under the person's hands.
    if (showing === asked) return;
    showing = asked;
    panel.hidden = false;
    // The fact, as opposed to the request: the shell's keyboard and gamepad
    // guards need to know a panel is up even when the game is not the one
    // asking. (mac-test.html reads this every time it samples a pad.)
    globalThis.__oskiewarAccountOpen = true;
    dress(asked === "handle" ? "handle" : "email", message);
  }

  // The game's side of the contract. An accessor rather than a polled value so
  // a press opens the panel on the frame it happened.
  Object.defineProperty(globalThis, "__oskiewarAccountDoor", {
    get: () => door,
    set: (next) => { door = next; sync(); },
    configurable: true,
  });

  // This shell's side: the corner button, and the panel walking itself from the
  // code step to the handle step.
  function ask(next, message) { want = next; sync(message); }
  // Done, either way. Both requests are dropped, the game's included — a shell
  // that only dropped its own would leave the panel standing on a door the game
  // has no way to know was answered.
  function shut() { door = ""; want = ""; sync(); }

  function publish({ handle = "", colors = [], signedIn = account.signedIn } = {}) {
    account.signedIn = signedIn;
    account.handle = handle ? "@" + String(handle).toUpperCase() : "";
    account.colors = colors;
    handleLabel.textContent = handle ? "@" + handle : "";
    handleLabel.hidden = !handle;
    logout.textContent = signedIn ? "log out" : "log in";
  }

  // 🤚 Who this sub is, as the rest of AC already answers it. Two requests
  // rather than one `?colors=true` because these are the exact calls the game's
  // colours have always come from, and a live game is a poor place to change
  // where a fighter's palette comes from.
  async function readProfile(sub) {
    const response = await fetch(`${AC}/handle?for=${encodeURIComponent(sub)}`);
    if (!response.ok) return { handle: "", colors: [] }; // 404 means no handle yet, which is a real state.
    const handle = (await response.json())?.handle;
    if (!handle) return { handle: "", colors: [] };
    let colors = [];
    try {
      const painted = await fetch(
        `${AC}/api/handle-colors?handle=${encodeURIComponent(handle)}`,
        { cache: "no-store" });
      if (painted.ok) {
        const read = (await painted.json())?.colors;
        if (Array.isArray(read)) colors = read;
      }
    } catch {}
    return { handle, colors };
  }

  async function loadAuth0() {
    if (globalThis.auth0) return;
    await new Promise((resolve, reject) => {
      const script = document.createElement("script");
      script.src = "/aesthetic.computer/dep/auth0-spa-js.production.js";
      script.crossOrigin = "anonymous";
      script.onload = resolve;
      script.onerror = () => reject(new Error("auth0 script failed"));
      document.head.appendChild(script);
    });
  }

  async function spaClient() {
    if (spa) return spa;
    await loadAuth0();
    spa = await globalThis.auth0?.createAuth0Client({
      domain: "https://" + DOMAIN,
      clientId: CLIENT,
      cacheLocation: "localstorage",
      useRefreshTokens: true,
      authorizationParams: { redirect_uri: location.origin },
    });
    return spa;
  }

  // The first session check, and the only place the two doors meet. A native
  // session is preferred because it is this page's own and needs no network to
  // establish. Failing that we ask hi.aesthetic.computer silently — a shell on
  // oskiewar.com holds no cookie for aesthetic.computer, and that SSO cookie is
  // the only shared ground between the two domains. A silent check also fails
  // when the origin is missing from Auth0's allowed web origins, which from in
  // here is indistinguishable from being logged out, so a failure shows no
  // handle and still offers the door rather than hiding the way in.
  async function restore() {
    const native = otp.session();
    if (native?.sub && (await otp.token())) {
      source = "otp";
      who = native.sub;
      publish({ ...(await readProfile(who)), signedIn: true });
      account.ready = true;
      return;
    }
    try {
      const client = await spaClient();
      await client?.checkSession();
      const sub = (await client?.getUser())?.sub;
      if (sub) {
        source = "sso";
        who = sub;
        publish({ ...(await readProfile(who)), signedIn: true });
      }
    } catch {}
    account.ready = true;
  }

  // An access token for whichever door is open, because claiming a handle is the
  // one thing this shell asks the AC backend to do on someone's behalf.
  async function bearer() {
    if (source === "otp") return await otp.token();
    try { return await (await spaClient())?.getTokenSilently(); } catch { return null; }
  }

  async function submit() {
    if (busy) return;
    const typed = field.value.trim();
    if (!typed) { field.focus(); return; }
    working(true);
    sfx("block", .85, 0);
    try {
      if (step === "email") {
        address = await otp.sendCode(typed);
        working(false);
        dress("code", `code sent to ${address}`);
        return;
      }

      if (step === "code") {
        const session = await otp.verify(address, typed);
        source = "otp";
        who = session.sub;
        const profile = await readProfile(who);
        publish({ ...profile, signedIn: true });
        working(false);
        // A signed-in person with no handle is halfway through the door, so the
        // panel walks them the rest of the way instead of closing on a state
        // they cannot do anything with.
        if (profile.handle) shut();
        else ask("handle", "you're in — now pick a name");
        return;
      }

      // step === "handle"
      const token = await bearer();
      if (!token) {
        working(false);
        say(claimTrouble.unauthorized, true);
        return;
      }
      const response = await fetch(`${AC}/handle`, {
        method: "POST",
        headers: { "content-type": "application/json",
          authorization: "Bearer " + token },
        body: JSON.stringify({ handle: typed.replace(/^@/, ""),
          tenant: "aesthetic" }),
      });
      const body = await response.json().catch(() => null);
      working(false);
      if (!response.ok) {
        say(claimTrouble[body?.message] || claimTrouble.error, true);
        return;
      }
      // The echo carries the handle but nothing about its colours, and a brand
      // new handle has a palette waiting for it, so the profile is read back.
      const painted = await readProfile(who);
      publish({ handle: painted.handle || body.handle, colors: painted.colors,
        signedIn: true });
      shut();
    } catch (error) {
      working(false);
      // A tenant fault is not this person's problem and no amount of retyping
      // fixes it, so the form says what happened and hands over the redirect.
      say(error.hint || error.message, true);
      if (error.tenant) viaWeb.hidden = false;
    }
  }

  form.addEventListener("submit", (event) => {
    event.preventDefault();
    submit();
  });

  back.addEventListener("click", () => {
    sfx("block", .85, 0);
    // From the code step, back means "wrong address" rather than "never mind".
    if (step === "code") dress("email");
    else shut();
  });

  viaWeb.addEventListener("click", async () => {
    sfx("block", .85, 0);
    try {
      const client = await spaClient();
      await client?.loginWithRedirect({
        authorizationParams: { redirect_uri: location.origin },
      });
    } catch { say("couldn't open the web sign-in page", true); }
  });

  panel.addEventListener("keydown", (event) => {
    if (event.key === "Escape") { event.stopPropagation(); shut(); }
  });

  // The corner button is the same door from the other side.
  logout.addEventListener("pointerenter", () => sfx("hat", .26, 0));
  logout.addEventListener("click", async () => {
    sfx("block", .85, 0);
    if (!account.signedIn) { ask("login"); return; }
    if (source === "otp") {
      otp.forget();
      source = "";
      who = "";
      publish({ signedIn: false });
      return;
    }
    // An SSO session lives in a cookie on hi.aesthetic.computer, so ending it
    // costs a navigation. Return to the host you logged out from, not always the
    // apex — coming back to oskiewar.com from midi.oskiewar.com would silently
    // drop you out of midi mode. Auth0 checks this against its allowed logout
    // URLs, so both hosts have to be listed there.
    const target = new URL(`https://${DOMAIN}/v2/logout`);
    target.searchParams.set("client_id", CLIENT);
    target.searchParams.set("returnTo", location.origin + "/");
    location.assign(target);
  });

  // The poster and the title loop are burned headless from this same shell, and
  // a handle is nobody's business in an Open Graph card — so the harness mounts
  // the door (the game still writes to it) without ever asking who is watching.
  if (probe) restore(); else account.ready = true;

  return { restore };
}
