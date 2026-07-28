import { readFileSync } from "node:fs";

describe("Whistlegraph color curation", () => {
  it("keeps the featured ten colors and defaults the rest to magenta", () => {
    const archive = JSON.parse(
      readFileSync(new URL("../system/public/whistlegraph.org/graphs.json", import.meta.url), "utf8"),
    );
    const featured = [
      ["imab", "#ff9600"],
      ["l8ly", "#7a1fe6"],
      ["grow", "#ff8fbf"],
      ["idni", "#e60e0e"],
      ["ppl", "#be50dc"],
      ["wiyh", "#0000f5"],
      ["lonr", "#ff8282"],
      ["sdog", "#101014"],
      ["w0w", "#ffc800"],
      ["puzz", "#30c8fc"],
    ];

    expect(archive.graphs.slice(0, 10).map(({ code, c }) => [code, c])).toEqual(featured);
    expect(archive.graphs.slice(10).every(({ c }) => c === "#b44887")).toBe(true);
  });

  it("keeps long and undr magenta until Alex assigns their new colors", () => {
    const special = JSON.parse(
      readFileSync(new URL("../toolchain/whistlegraph/downloads/special-works.json", import.meta.url), "utf8"),
    );
    expect(special.long.c).toBe("#b44887");
    expect(special.undr.c).toBe("#b44887");
  });
});
