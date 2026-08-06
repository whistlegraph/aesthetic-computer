import { readFileSync } from "node:fs";
import {
  assertCurationLock,
  curationChanges,
  curationSnapshot,
} from "../toolchain/whistlegraph/curation-lock.mjs";

describe("Whistlegraph curation lock", () => {
  const works = [{ code: "kity", title: "Kitty Head", by: "Jeffrey Alan Scudder", year: 2021, kind: "graph", status: "confirmed" }];
  const posts = [{ id: "1", works: ["kity"] }, { id: "2", works: [] }];

  it("ignores generated metrics but protects work identity and relationships", () => {
    const lock = curationSnapshot(works, posts);
    expect(() => assertCurationLock(lock, [{ ...works[0], views: 999, perf: 3, thumb: "new.jpg" }], posts)).not.toThrow();
    expect(() => assertCurationLock(lock, [{ ...works[0], by: "Whistlegraph" }], posts)).toThrowError(/work changed: kity/);
    expect(() => assertCurationLock(lock, works, [{ id: "1", works: ["lost"] }])).toThrowError(/post relationship changed: 1/);
  });

  it("locks the checked-in generated archive exactly", () => {
    const graphs = JSON.parse(readFileSync(new URL("../system/public/whistlegraph.org/graphs.json", import.meta.url), "utf8"));
    const archive = JSON.parse(readFileSync(new URL("../system/public/whistlegraph.org/posts.json", import.meta.url), "utf8"));
    const lock = JSON.parse(readFileSync(new URL("../toolchain/whistlegraph/downloads/curation-lock.json", import.meta.url), "utf8"));
    expect(curationChanges(lock, curationSnapshot(graphs.works, archive.posts))).toEqual([]);
  });

  it("limits colored markers to featured and exhibited works", () => {
    const graphs = JSON.parse(readFileSync(new URL("../system/public/whistlegraph.org/graphs.json", import.meta.url), "utf8"));
    expect(graphs.works.filter((work) => work.c).map((work) => work.code).sort()).toEqual([
      "grow", "idni", "imab", "l8ly", "long", "lonr", "ppl", "puzz", "sdog", "undr", "w0w", "wiyh",
    ]);
  });

  it("publishes only locally inventoried external media", () => {
    const graphs = JSON.parse(readFileSync(new URL("../system/public/whistlegraph.org/graphs.json", import.meta.url), "utf8"));
    const youtube = JSON.parse(readFileSync(new URL("../toolchain/whistlegraph/downloads/YOUTUBE.json", import.meta.url), "utf8"));
    const inventoried = new Set(youtube.videos.map((video) => video.id));
    const media = Object.fromEntries(["long", "undr"].map((code) => [code, graphs.works.find((work) => work.code === code).versions]));
    expect(media.long.map((item) => item.id)).toEqual(["JgAlnu5L5n0", "PN0H4fWafMk", "RXCMvuERxnE", "xBd23vELJ4o"]);
    expect(media.undr.map((item) => item.id)).toEqual(["XUAaiVDwYCE", "17ftGLwPenA"]);
    expect(Object.values(media).flat().every((item) => item.type === "youtube" && inventoried.has(item.id))).toBeTrue();
  });

  it("keeps the reviewed Kitty-family post distinctions", () => {
    const archive = JSON.parse(readFileSync(new URL("../system/public/whistlegraph.org/posts.json", import.meta.url), "utf8"));
    const byId = Object.fromEntries(archive.posts.map((post) => [post.id, post]));
    expect(byId["6821234484361350405"]).toEqual(jasmine.objectContaining({ kind: "talk", works: [] }));
    expect(byId["6891099074704051462"].works).toEqual(["lost"]);
    expect(byId["7372871528083410222"].works).toEqual(["lkty"]);
    expect(byId["7233527996508884266"].works.sort()).toEqual(["dggy", "kity"]);
  });

  it("renders YouTube media with privacy-enhanced embeds and source links", () => {
    const page = readFileSync(new URL("../system/public/whistlegraph.org/index.html", import.meta.url), "utf8");
    expect(page).toContain("https://www.youtube-nocookie.com/embed/${v.id}");
    expect(page).toContain('${w.film?"Film & features":"Performances"}');
    expect(page).toContain("archive preview above; the complete ~22-minute");
    expect(page).toContain("YouTube ↗");
  });
});
