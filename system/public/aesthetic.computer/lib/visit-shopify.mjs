// Use Shopify's existing analytics decision; never change merchant/visitor consent.
export function startShopifyVisits(win = window, doc = document,
  loadTracker = () => import("./visit-tracker.mjs")) {
  win.acVisitTrackingDisabled = true;
  let generation = 0;
  const allowed = () => win.Shopify?.customerPrivacy?.analyticsProcessingAllowed?.() === true;
  async function sync() {
    const current = ++generation;
    if (!allowed()) {
      win.acVisitTrackingDisabled = true;
      win.acVisits?.stop();
      return;
    }
    // Keep auto-initialization disabled until consent is checked after import.
    try {
      const module = await loadTracker();
      if (current !== generation || !allowed()) return;
      win.acVisitTrackingDisabled = false;
      module.startVisitTracker(win, doc);
    } catch { win.acVisitTrackingDisabled = true; }
  }
  doc.addEventListener("visitorConsentCollected", sync);
  function initialize() {
    if (win.Shopify?.customerPrivacy) return sync();
    win.Shopify?.loadFeatures?.([{ name: "consent-tracking-api", version: "0.1" }], error => {
      if (!error) sync();
    });
  }
  initialize();
  if (!win.Shopify) win.addEventListener("load", initialize, { once: true });
}
if (typeof window !== "undefined") startShopifyVisits();
