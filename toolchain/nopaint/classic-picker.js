// No Paint classic picker, 2026.11.18
// How the next proposal is chosen, as parameters. The 2021 game (Index event
// sheet, "Random") takes its first proposal from the Brushes list; after that
// it takes Brushes two times in three and Filters one time in three, picks
// from the list by weight, and never offers the same filter twice running.
// This is those rules with every number open to tuning
// (toolchain/nopaint/classic-tuning.json), and with room for brushes the 2021
// lists do not have.
//
// It is plain script so the same file runs in the game (appended to the
// runtime by patch-classic.mjs) and in node (classic-odds.mjs).
//
// A tuning is { split: { Brushes: 2, Filters: 1 }, first: "Brushes",
// norepeat: ["Filters"], Brushes: {...}, Filters: {...} }.
// There can be any number of lists; a new kind of brush can have its own.
// An entry is a weight (a number), or { in100: n } to ask for n in 100 of all
// proposals whatever the other weights are. Either kind may carry
// when: "flag" to be offered only while that flag is up (the page raises "ai"
// while an AI turn can be had).
self.nopaintPicker = {
  // Turn a tuning into the chance of each brush among all proposals.
  resolve(tuning, flags = {}) {
    const lists = {};
    // A list with nothing to offer right now (every entry waits on a flag that
    // is down) is left out, and the others share its chance.
    const open = Object.entries(tuning.split).filter(([list]) => Object.values(tuning[list] || {})
      .some((entry) => !(entry.when && !flags[entry.when]) && (entry.in100 > 0 || entry.weight > 0 || entry > 0)));
    const total = open.reduce((sum, [, chance]) => sum + chance, 0);
    for (const [list, chance] of open) {
      const share = chance / total; // of all proposals
      const fixed = {}, asked = {};
      for (const [name, entry] of Object.entries(tuning[list] || {})) {
        const value = typeof entry === "number" ? { weight: entry } : entry;
        if (value.when && !flags[value.when]) continue;
        if (value.in100 !== undefined) asked[name] = value.in100 / 100 / share; // its share of this list
        else if (value.weight > 0) fixed[name] = value.weight;
      }
      // The asked-for shares come off the top; the weights divide what is left.
      const taken = Object.values(asked).reduce((a, b) => a + b, 0);
      if (taken >= 1) throw new Error(`${list}: the in100 entries ask for more than the whole list`);
      const weight = Object.values(fixed).reduce((a, b) => a + b, 0);
      const entries = {};
      for (const [name, value] of Object.entries(fixed)) entries[name] = share * (1 - taken) * value / weight;
      for (const [name, value] of Object.entries(asked)) entries[name] = share * value;
      lists[list] = { share, entries };
    }
    return lists;
  },
  // Every brush with its chance, most likely first.
  table(resolved) {
    return Object.entries(resolved)
      .flatMap(([list, { entries }]) => Object.entries(entries).map(([name, chance]) => ({ name, list, chance })))
      .sort((a, b) => b.chance - a.chance || a.name.localeCompare(b.name));
  },
  // One pick from a list, the way the game asks: the list is already chosen.
  pick(resolved, list, random = Math.random) {
    const entries = Object.entries(resolved[list]?.entries || {});
    let at = random() * entries.reduce((sum, [, chance]) => sum + chance, 0);
    for (const [name, chance] of entries) { at -= chance; if (at < 0) return name; }
    return entries.at(-1)?.[0] ?? null;
  },
  // One whole proposal: a list by the split, then a brush from it.
  propose(resolved, random = Math.random) {
    const lists = Object.entries(resolved);
    let at = random() * lists.reduce((sum, [, list]) => sum + list.share, 0);
    for (const [name, list] of lists) { at -= list.share; if (at < 0) return this.pick(resolved, name, random); }
    return this.pick(resolved, lists.at(-1)[0], random);
  },
  // The next proposal of a run, with the two rules that look back: the first
  // comes from tuning.first, and a list named in tuning.norepeat never gives
  // the same brush twice running. `run` is { count, last }, kept by the caller.
  next(run, tuning, resolved, random = Math.random) {
    const first = !run.count && tuning.first && resolved[tuning.first];
    let name = first ? this.pick(resolved, tuning.first, random) : this.propose(resolved, random);
    const list = (brush) => Object.keys(resolved).find((key) => brush in resolved[key].entries);
    for (let tries = 0; tries < 50 && name === run.last && (tuning.norepeat || []).includes(list(name)) && Object.keys(resolved[list(name)].entries).length > 1; tries++)
      name = this.pick(resolved, list(name), random);
    run.count = (run.count || 0) + 1;
    run.last = name;
    return name;
  },
};
