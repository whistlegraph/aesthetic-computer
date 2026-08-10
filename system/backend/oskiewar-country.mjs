// Trusted, deliberately coarse location helpers for Oskiewar.
// Country is the only geographic value retained; IP-derived headers, regions,
// cities, and coordinates never enter replay documents.

// ISO 3166-1 alpha-2 codes. Keeping the allowlist here prevents Cloudflare's
// non-country sentinels (notably XX and T1) from becoming game statistics.
const ISO_COUNTRIES = new Set((
  "AD AE AF AG AI AL AM AO AQ AR AS AT AU AW AX AZ BA BB BD BE BF BG BH BI " +
  "BJ BL BM BN BO BQ BR BS BT BV BW BY BZ CA CC CD CF CG CH CI CK CL CM CN " +
  "CO CR CU CV CW CX CY CZ DE DJ DK DM DO DZ EC EE EG EH ER ES ET FI FJ FK " +
  "FM FO FR GA GB GD GE GF GG GH GI GL GM GN GP GQ GR GS GT GU GW GY HK HM " +
  "HN HR HT HU ID IE IL IM IN IO IQ IR IS IT JE JM JO JP KE KG KH KI KM KN " +
  "KP KR KW KY KZ LA LB LC LI LK LR LS LT LU LV LY MA MC MD ME MF MG MH MK " +
  "ML MM MN MO MP MQ MR MS MT MU MV MW MX MY MZ NA NC NE NF NG NI NL NO NP " +
  "NR NU NZ OM PA PE PF PG PH PK PL PM PN PR PS PT PW PY QA RE RO RS RU RW " +
  "SA SB SC SD SE SG SH SI SJ SK SL SM SN SO SR SS ST SV SX SY SZ TC TD TF " +
  "TG TH TJ TK TL TM TN TO TR TT TV TW TZ UA UG UM US UY UZ VA VC VE VG VI " +
  "VN VU WF WS YE YT ZA ZM ZW"
).split(" "));

export function normalizeCountry(value) {
  if (typeof value !== "string") return null;
  const country = value.trim().toUpperCase();
  return ISO_COUNTRIES.has(country) ? country : null;
}

export function countryFromHeaders(headers = {}) {
  // Lith is behind Cloudflare in production. Netlify's header remains useful
  // for previews. Never accept a country from the JSON body or query string.
  return normalizeCountry(headers["cf-ipcountry"] ??
    headers["CF-IPCountry"] ??
    headers["x-nf-country"] ??
    headers["X-Nf-Country"]);
}

export function isComputerFighter(fighter) {
  return /^(?:DUMMY|BOT|SPIDERDUMMY)$/i.test(String(fighter || ""));
}

export function trustedFighterNations(fighters, country) {
  const trustedCountry = normalizeCountry(country);
  if (!Array.isArray(fighters)) return [];
  return fighters.map((fighter) =>
    trustedCountry && !isComputerFighter(fighter) ? trustedCountry : null);
}
