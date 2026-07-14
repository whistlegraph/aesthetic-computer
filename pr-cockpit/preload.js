// Preload bridge — exposes a tiny, explicit API to the renderer.
// contextIsolation is on, so the renderer can only touch what we expose here.

"use strict";

const { contextBridge, ipcRenderer } = require("electron");

contextBridge.exposeInMainWorld("cockpit", {
  // Fetch the PR list. Resolves to { ok, prs?, error?, config }.
  listPRs: () => ipcRenderer.invoke("prs:list"),
  // Open a url in the user's real default browser.
  openExternal: (url) => ipcRenderer.invoke("shell:openExternal", url),
  // Perform a real GitHub review action via the `gh` CLI on this host (approve /
  // request-changes / comment). Goes through gh, not the webviews, so it works
  // even when the tiles aren't signed in. Resolves to { ok, output? } or
  // { ok:false, error }. kind: "approve" | "request-changes" | "comment".
  prAction: ({ kind, number, body }) =>
    ipcRenderer.invoke("pr-action", { kind, number, body }),
  // Import the existing github.com login from Chrome into the shared
  // persist:github session. Resolves to { ok, profile, set, total } or
  // { ok:false, error }. No-op if the main-process glue isn't wired in.
  syncGithub: () => ipcRenderer.invoke("github:syncFromChrome"),
});
