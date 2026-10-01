// Page keys stay in memory. Only reviewed action names reach account telemetry.
export function sotceResponseAction(method, endpoint, status, result) {
  if (method.toUpperCase() !== "POST" || status !== 200) return null;
  if (endpoint === "/sotce-net/touch-a-page" && result?.touchCreated === true) return "sotce_page_touched";
  if (endpoint === "/sotce-net/ask" && result?.success === true) return "sotce_question_submitted";
  return null;
}

export function startSotceActivity(win = window, doc = document) {
  if (win.acSotceActivity) return win.acSotceActivity;
  let key = null, elapsed = 0, viewed = false, read = false, last = win.performance.now(), wasVisible = false;
  const timer = win.setInterval(() => {
    const now = win.performance.now(), delta = Math.min(1000, Math.max(0, now - last));
    last = now;
    const visible = doc.visibilityState === "visible" && !doc.body.classList.contains("pages-hidden") &&
      !doc.documentElement.classList.contains("editing");
    const page = visible ? win.acSotceVisiblePage?.() : null;
    if (!page) { wasVisible = false; return; }
    if (page !== key) { key = page; elapsed = 0; viewed = false; read = false; wasVisible = false; }
    if (wasVisible) elapsed += delta;
    wasVisible = true;
    if (!viewed && elapsed >= 2000) { viewed = true; win.acAccountActivity?.action("sotce_page_viewed"); }
    if (!read && elapsed >= 30000) { read = true; win.acAccountActivity?.action("sotce_page_visible_30s"); }
  }, 1000);
  const api = {
    response(method, endpoint, status, result) {
      const action = sotceResponseAction(method, endpoint, status, result);
      if (action) win.acAccountActivity?.action(action);
    },
    stop() { win.clearInterval(timer); if (win.acSotceActivity === api) delete win.acSotceActivity; },
  };
  win.acSotceActivity = api;
  return api;
}
