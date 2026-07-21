// tabs — prepare and switch a visible Chrome tab set for Captutor screenplays.
//
// The browser's remote-debugging HTTP endpoint owns tab lifecycle. Keeping this
// separate from the page-attached CDP session matters: activating another tab
// must not detach the Fuser session that drives the take clock and interruption
// guard.

const HOST = process.env.CDP_HOST || "127.0.0.1";
const PORT = process.env.CDP_PORT || "9222";

const endpoint = (path = "") => `http://${HOST}:${PORT}${path}`;

export async function listTabs() {
  const response = await fetch(endpoint("/json/list"));
  if (!response.ok) throw new Error(`could not list Chrome tabs (${response.status})`);
  return (await response.json()).filter((target) => target.type === "page");
}

async function openTab(url) {
  const response = await fetch(endpoint(`/json/new?${encodeURIComponent(url)}`), {
    method: "PUT",
  });
  if (!response.ok) throw new Error(`could not open performance tab (${response.status}): ${url}`);
  return response.json();
}

async function closeTab(id) {
  await fetch(endpoint(`/json/close/${id}`)).catch(() => {});
}

export async function prepareTabs(entries, { keepMatch = "fuser.studio" } = {}) {
  const current = await listTabs();
  const keep = current.find((tab) => (tab.url || "").includes(keepMatch));
  if (!keep) throw new Error(`cannot prepare tabs: no page matching ${JSON.stringify(keepMatch)}`);

  // A performance run should expose exactly the authored set, not yesterday's
  // browsing. Keep one control page alive for Captutor's attached session and
  // close every other page before opening the benchmark tabs.
  await Promise.all(current.filter((tab) => tab.id !== keep.id).map((tab) => closeTab(tab.id)));
  const opened = [];
  for (const entry of entries) {
    opened.push({ ...entry, target: await openTab(entry.url) });
  }
  await activateTab(keepMatch);
  return { control: keep, opened };
}

export async function activateTab(match) {
  const tabs = await listTabs();
  const matches = Array.isArray(match) ? match : [match];
  const target = tabs.find((tab) => matches.some((candidate) =>
    (tab.url || "").includes(candidate) || (tab.title || "").includes(candidate)));
  if (!target) {
    throw new Error(
      `no Chrome tab matching ${JSON.stringify(matches)}; open tabs:\n` +
      tabs.map((tab) => `  ${tab.title || "(untitled)"} — ${tab.url}`).join("\n"),
    );
  }
  const response = await fetch(endpoint(`/json/activate/${target.id}`));
  if (!response.ok) throw new Error(`could not activate tab ${target.id} (${response.status})`);
  return target;
}

export function tabController() {
  return {
    list: listTabs,
    prepare: prepareTabs,
    activate: activateTab,
  };
}
