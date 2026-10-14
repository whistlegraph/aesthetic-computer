// Shared MIME interface policy. Runs in the browser and iOS JavaScriptCore.
// Pure data in / data out: adapters own rendering, credentials and side effects.
(() => {
  "use strict";
  const brand = Object.freeze({ name: "MIME", slogan: "every file has a type", title: "MIME — every file has a type" });
  const statuses = ["checking", "signedOut", "needsHandle", "ready", "failed"];
  const labels = {
    "account.login": "Log in", "account.signup": "I'm new", "account.signOut": "Sign out",
    "account.retry": "Try again", "account.handle": "Choose your @handle",
    "editor.open": "Open editor", "saved.open": "Saved", "feed.open": "Back to feed",
  };
  const action = id => ({ id, label: labels[id] });
  function account(input = {}) {
    const handle = typeof input.handle === "string" ? input.handle.replace(/^@/, "") : "";
    let status = statuses.includes(input.status) ? input.status : "signedOut";
    const validHandle = /^[A-Za-z0-9_.-]{1,64}$/.test(handle) ? "@" + handle : null;
    if (status === "ready" && !validHandle) status = "needsHandle";
    return { status, handle: status === "ready" ? validHandle : null,
      notice: typeof input.notice === "string" ? input.notice.slice(0, 300) : "" };
  }
  function normalize(input = {}) {
    return { screen: input.screen === "account" ? "account" : "feed", account: account(input.account),
      capabilities: { saved: input.capabilities?.saved === true } };
  }
  function project(input) {
    const state = normalize(input), { status, handle, notice } = state.account;
    const actions = status === "checking" ? []
      : status === "ready" ? ["editor.open", "account.signOut"]
      : status === "needsHandle" ? ["account.handle", "account.retry", "account.signOut"]
      : status === "failed" ? ["account.retry", "account.login", "account.signup"]
      : ["account.login", "account.signup"];
    return { schemaVersion: 1, brand,
      screen: state.screen,
      logo: { id: "account.open", label: "MIME account" },
      account: { title: handle || brand.name, status, busy: status === "checking",
        notice: notice || (status === "checking" ? "Checking your account…" : ""),
        actions: [...actions, ...(state.capabilities.saved ? ["saved.open"] : []), "feed.open"].map(action) },
      media: { active: state.screen === "feed", viewerHandle: handle },
      creation: { typeAction: "work.create", versionAction: "work.remix" },
    };
  }
  function dispatch(input, event = {}) {
    const state = normalize(input), effects = [];
    if (event.type === "account.changed") state.account = account(event.account);
    else if (event.type === "account.open") state.screen = "account";
    else if (event.type === "feed.open") state.screen = "feed";
    else if (event.type === "editor.open") {
      state.screen = "feed"; effects.push({ type: "openEditor" });
    }
    else if (event.type === "work.create" && typeof event.mimeType === "string" &&
        /^[a-z0-9.+-]+\/[a-z0-9.+-]+$/i.test(event.mimeType) && event.mimeType.length <= 160) {
      state.screen = "feed"; effects.push({ type: "createWork", mimeType: event.mimeType.toLowerCase() });
    } else if (event.type === "work.remix" && typeof event.post === "string" &&
        /^[A-Za-z0-9_~!-]{1,100}$/.test(event.post) && Number.isSafeInteger(event.version) && event.version >= 0) {
      state.screen = "feed"; effects.push({ type: "remixWork", post: event.post, version: event.version });
    }
    else if (project(state).account.actions.some(action => action.id === event.type)) {
      switch (event.type) {
        case "account.login": case "account.signup":
          effects.push({ type: "authenticate", signUp: event.type === "account.signup" }); break;
        case "account.signOut":
          state.account = account(); effects.push({ type: "signOut" }); break;
        case "account.retry":
          state.account = account({ status: "checking" }); effects.push({ type: "refreshAccount" }); break;
        case "account.handle": effects.push({ type: "chooseHandle" }); break;
        case "saved.open": state.screen = "feed"; effects.push({ type: "openSaved" }); break;
      }
    }
    return { state, view: project(state), effects };
  }
  globalThis.MimeInterface = Object.freeze({ schemaVersion: 1, brand, project, dispatch });
})();
