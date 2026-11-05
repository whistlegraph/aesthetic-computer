// Studio links rendered and activated by the AC piece. Add a reviewed action
// in visit-model.mjs when adding another measured destination.
export const NOTEPAT_PROMOS = Object.freeze([
  { label: "Menu Band - Music in your menu bar >", compact: "Menu Band - menuband.app >", name: "Menu Band >", url: "https://menuband.app/?ac_campaign=notepat-footer", action: "notepat_menuband_clicked" },
  { label: "Take Notepat offline - ACOS >", compact: "Notepat offline - ACOS >", name: "ACOS >", url: "https://aesthetic.computer/os?ac_campaign=notepat-footer", action: "notepat_os_clicked" },
  { label: "Paint a picture - No Paint >", compact: "Paint on nopaint.art >", name: "No Paint >", url: "https://nopaint.art/?ac_campaign=notepat-footer", action: "notepat_nopaint_clicked" },
]);

export function notepatPromoLabel(item, width, measure) {
  return [item.label, item.compact, item.name].find(label => measure(label) <= width) || item.name;
}
