const $ = id => document.getElementById(id);
const drafts = "whistlegraph-room-draft";
const api = "/api/whistlegraph-chat";
let client, session, requestId, draftText;
let refreshing = false;
let rendered = "";
const clock = new Intl.DateTimeFormat(undefined, { month: "short", day: "numeric", hour: "numeric", minute: "2-digit" });

// Mobile Safari can shrink only the visual viewport when the keyboard opens.
const viewport = window.visualViewport;
if (viewport) {
  const fitRoom = () => {
    if (viewport.scale === 1) document.documentElement.style.setProperty("--room-height", viewport.height + "px");
    else document.documentElement.style.removeProperty("--room-height");
  };
  viewport.addEventListener("resize", fitRoom);
  fitRoom();
}

function saveDraft() {
  const text = $("draft").value;
  if (draftText !== text) requestId = crypto.randomUUID();
  draftText = text;
  try {
    if (text) sessionStorage.setItem(drafts, JSON.stringify({ text, requestId }));
    else sessionStorage.removeItem(drafts);
  } catch {}
}
try {
  const saved = JSON.parse(sessionStorage.getItem(drafts));
  if (saved?.text) {
    $("draft").value = draftText = saved.text;
    requestId = saved.requestId;
  }
} catch {}
$("draft").addEventListener("input", saveDraft);

async function json(url, options) {
  const response = await fetch(url, options);
  const data = await response.json();
  if (!response.ok) {
    const err = new Error(data.message || "Please try again.");
    err.status = response.status;
    throw err;
  }
  return data;
}

async function authorized(url, body) {
  const token = await client.getTokenSilently();
  return json(url, {
    method: body ? "POST" : "GET",
    headers: { Authorization: "Bearer " + token, ...(body && { "Content-Type": "application/json" }) },
    body: body && JSON.stringify(body),
    cache: "no-store",
  });
}

function identity(value) {
  session = value;
  $("identity").textContent = session?.handle ? "Public chat · @" + session.handle : "Public chat · Join free with an AC handle";
  $("logout").hidden = !session;
}

async function readSession() {
  identity(await authorized(api + "?action=session"));
}

async function initAuth() {
  client = new window.auth0.Auth0Client({
    domain: "hi.aesthetic.computer", clientId: "LVdZaMbyXctkGfZDnpzDATB5nR0ZhmMt",
    cacheLocation: "localstorage", useRefreshTokens: true, useRefreshTokensFallback: true,
    authorizationParams: { redirect_uri: location.origin },
  });
  if (await client.isAuthenticated()) {
    $("send").disabled = true;
    try { await readSession(); } catch { identity(null); }
    finally { $("send").disabled = false; }
  }
}

function appendText(node, text) {
  for (const part of text.split(/(https?:\/\/[^\s<>]+)/gi)) {
    if (!/^https?:\/\//i.test(part)) { node.append(part); continue; }
    const a = document.createElement("a");
    a.href = part;
    a.textContent = part;
    a.target = "_blank";
    a.rel = "noopener noreferrer ugc";
    node.append(a);
  }
}

function messageNode(msg) {
  const li = document.createElement("li");
  const author = document.createElement("strong");
  author.textContent = "@" + msg.handle;
  const time = document.createElement("time");
  time.dateTime = msg.when;
  time.textContent = clock.format(new Date(msg.when));
  li.append(author, time);
  if (session?.host) {
    for (const [action, label] of [["remove", "Remove"], ["mute", "Mute 24h"]]) {
      const button = document.createElement("button");
      button.className = "mod";
      button.textContent = label;
      button.onclick = async () => {
        if (!confirm(label + " · @" + msg.handle + "?")) return;
        try {
          await authorized(api + "?action=" + action, { id: msg._id });
          $("notice").textContent = action === "mute" ? "Muted for 24 hours." : "Message removed.";
          await refresh();
        } catch (err) { $("notice").textContent = err.message; }
      };
      li.append(button);
    }
  }
  const content = document.createElement("p");
  content.className = "message";
  appendText(content, msg.text);
  li.append(content);
  return li;
}

async function refresh() {
  if (refreshing) return;
  refreshing = true;
  try {
    const { messages } = await json(api, { cache: "no-cache" });
    const signature = JSON.stringify([!!session?.host, messages]);
    $("empty").hidden = !!messages.length;
    $("empty").textContent = "No messages yet. Say hi.";
    if (signature === rendered) return;
    const timeline = $("timeline");
    const follow = timeline.scrollHeight - timeline.clientHeight - timeline.scrollTop < 48;
    const list = $("messages");
    const nodes = new Map([...list.children].map(li => [li.dataset.key, li]));
    let next = list.firstChild;
    for (const msg of messages) {
      const key = msg._id + (session?.host ? "-host" : "");
      const li = nodes.get(key) || messageNode(msg);
      li.dataset.key = key;
      if (li !== next) list.insertBefore(li, next);
      next = li.nextSibling;
      nodes.delete(key);
    }
    for (const li of nodes.values()) li.remove();
    if (follow) timeline.scrollTop = timeline.scrollHeight;
    rendered = signature;
  } catch {
    $("empty").hidden = false;
    $("empty").textContent = "Reconnecting to the room…";
  } finally { refreshing = false; }
}

function finishLogin() {
  if (!session.handle) {
    $("claim-error").textContent = "";
    $("handle-dialog").showModal();
  } else {
    $("notice").textContent = "Ready to send as @" + session.handle + ".";
    $("draft").focus();
  }
}

$("compose").onsubmit = async event => {
  event.preventDefault();
  if ($("send").disabled || !$("draft").value.trim()) return;
  saveDraft();
  $("send").disabled = true;
  $("draft").readOnly = true;
  $("notice").textContent = "";
  try {
    if (!client) throw new Error("Sign-in is unavailable. Reload to try again.");
    if (!session) {
      await client.loginWithPopup({ authorizationParams: { redirect_uri: location.origin } }, { timeoutInSeconds: 300 });
      await readSession();
      finishLogin();
    } else if (!session.handle) {
      finishLogin();
    } else {
      await authorized(api, { text: draftText.trim(), requestId });
      $("draft").value = "";
      saveDraft();
      $("notice").textContent = "Sent.";
      await refresh();
    }
  } catch (err) {
    if (err.status === 401 || ["login_required", "missing_refresh_token", "invalid_grant"].includes(err.error)) identity(null);
    if (err.status === 403 && err.message.startsWith("Choose an AC handle")) {
      identity({ ...session, handle: null });
      finishLogin();
    } else {
      $("notice").textContent = err.error === "cancelled" ? "Sign-in closed. Your draft is here." : err.message;
    }
  } finally {
    $("send").disabled = false;
    $("draft").readOnly = false;
  }
};
$("draft").onkeydown = e => {
  if (e.key === "Enter" && !e.shiftKey && !e.isComposing) {
    e.preventDefault();
    $("compose").requestSubmit();
  }
};

$("claim").onsubmit = async event => {
  event.preventDefault();
  const button = $("claim").querySelector("[type=submit]");
  if (button.disabled) return;
  button.disabled = true;
  try {
    await authorized("https://aesthetic.computer/api/handle", { handle: $("handle").value.trim().replace(/^@/, ""), createOnly: true });
    await readSession();
    if (!session.handle) throw new Error("Handle not ready. Try again.");
    $("handle-dialog").close();
    finishLogin();
  } catch (err) {
    $("claim-error").textContent = ({ taken: "That handle is taken.", unverified: "Verify your email, then try again.", invalid: "Use letters and numbers, with optional dots or underscores." })[err.message] || err.message;
  } finally { button.disabled = false; }
};
$("cancel").onclick = () => $("handle-dialog").close();
$("logout").onclick = () => client.logout({ logoutParams: { returnTo: location.origin } });

refresh();
setInterval(() => { if (!document.hidden) refresh(); }, 3500);
document.addEventListener("visibilitychange", () => { if (!document.hidden) refresh(); });
initAuth().catch(() => { $("notice").textContent = "Sign-in is unavailable. Reload to try again."; });
