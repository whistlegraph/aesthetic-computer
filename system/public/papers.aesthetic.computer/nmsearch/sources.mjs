export async function json(url, signal) {
  const response = await fetch(url, { signal });
  if (!response.ok) throw new Error(String(response.status));
  return response.json();
}

function plain(html) {
  const node = document.createElement("textarea");
  node.innerHTML = String(html).replace(/<[^>]+>/g, "");
  return node.value;
}

export const live = {
  async wikipedia(s, q, signal, field) {
    const cats = field.wikipediaCategories.join("|");
    const u = `https://en.wikipedia.org/w/api.php?action=query&list=search&format=json&origin=*&srlimit=8&srsearch=${encodeURIComponent(`${q} incategory:"${cats}"`)}`;
    const b = await json(u, signal);
    return { total: b.query.searchinfo.totalhits, hits: b.query.search.map((x) => ({ title: x.title, sub: plain(x.snippet), url: `https://en.wikipedia.org/?curid=${x.pageid}`, id: `pageid ${x.pageid}` })) };
  },
  async wikidata(s, q, signal, field) {
    // two fast calls: full-text search for candidates, then keep only items whose claims point into the field
    const inField = new Set([...Object.keys(field.genres), ...Object.keys(field.occupations || {})]);
    const su = `https://www.wikidata.org/w/api.php?action=query&list=search&format=json&origin=*&srlimit=50&srsearch=${encodeURIComponent(q)}`;
    const ids = (await json(su, signal)).query.search.map((x) => x.title).filter((t) => /^Q\d+$/.test(t));
    if (!ids.length) return { total: 0, hits: [] };
    const eu = `https://www.wikidata.org/w/api.php?action=wbgetentities&format=json&origin=*&props=claims|labels|descriptions&languages=en&ids=${ids.join("|")}`;
    const ents = (await json(eu, signal)).entities || {};
    const hits = [];
    for (const Q of ids) {
      const e = ents[Q]; if (!e?.claims) continue;
      const points = ["P31", "P106", "P136", "P135", "P101"].flatMap((p) => e.claims[p] || []).some((c) => inField.has(c.mainsnak?.datavalue?.value?.id));
      if (!points) continue;
      hits.push({ title: e.labels?.en?.value || Q, sub: e.descriptions?.en?.value || "", url: `https://www.wikidata.org/wiki/${Q}`, id: Q });
    }
    return { total: hits.length, hits: hits.slice(0, 8) };
  },
  async mediawiki(s, q, signal) {
    const u = `${s.endpoint}?action=query&list=search&format=json&origin=*&srlimit=8&srsearch=${encodeURIComponent(q)}`;
    const b = await json(u, signal);
    return { total: b.query.searchinfo.totalhits, hits: b.query.search.map((x) => ({ title: x.title, sub: plain(x.snippet), url: `${s.home}/${encodeURIComponent(x.title.replace(/ /g, "_"))}`, id: `pageid ${x.pageid}` })) };
  },
  async openalex(s, q, signal) {
    const u = `https://api.openalex.org/works?search=${encodeURIComponent(q)}&filter=primary_location.source.id:${s.sourceIds.join("|")}&per-page=8&select=id,display_name,publication_year,doi,primary_location`;
    const b = await json(u, signal);
    if (b.error) throw new Error(b.message?.slice(0, 60) || b.error);
    return { total: b.meta.count, hits: b.results.map((x) => ({ title: x.display_name, sub: `${x.primary_location?.source?.display_name || ""} · ${x.publication_year || ""}`, url: x.doi || x.id, id: (x.doi || x.id).replace(/^https?:\/\/(doi\.org|openalex\.org)\//, "") })) };
  },
  async zenodo(s, q, signal) {
    const u = `https://zenodo.org/api/communities/${s.community}/records?q=${encodeURIComponent(q)}&size=8`;
    const b = await json(u, signal);
    return { total: b.hits.total?.value ?? b.hits.total, hits: b.hits.hits.map((x) => ({ title: x.metadata.title, sub: `${(x.metadata.creators || []).map((c) => c.name).slice(0, 3).join(", ")} · ${x.metadata.publication_date?.slice(0, 4) || ""}`, url: x.links.self_html || x.doi_url || `https://zenodo.org/records/${x.id}`, id: x.doi || `record ${x.id}` })) };
  },
  async wordpress(s, q, signal) {
    const params = new URLSearchParams({ search: q, per_page: 8, _embed: "self", _fields: "id,title,url,subtype,_embedded,_links" });
    const records = await json(s.endpoint + "?" + params, signal);
    return { hits: records.map((r) => ({
      title: plain(r.title), url: r.url, id: String(r.id),
      text: plain(r._embedded?.self?.[0]?.excerpt?.rendered || "").trim(),
    })) };
  },
  async ia(s, q, signal) {
    const scope = [...s.collections.map((c) => `collection:${c}`), ...(s.subjects || []).map((t) => `subject:"${t}"`)].join(" OR ");
    const u = `https://archive.org/advancedsearch.php?q=${encodeURIComponent(`(${q}) AND (${scope})`)}&fl[]=identifier&fl[]=title&fl[]=creator&fl[]=year&fl[]=mediatype&rows=8&output=json`;
    const b = await json(u, signal);
    return { total: b.response.numFound, hits: b.response.docs.map((x) => ({ title: x.title, sub: `${[].concat(x.creator || []).slice(0, 2).join(", ")} ${x.year || ""} · ${x.mediatype || ""}`, url: `https://archive.org/details/${x.identifier}`, id: x.identifier })) };
  },
};
