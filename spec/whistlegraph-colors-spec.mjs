import { readFileSync } from "node:fs";

describe("Whistlegraph color curation", () => {
  it("uses dark mode only for late-night system dark mode unless overridden", () => {
    const page = readFileSync(
      new URL("../system/public/whistlegraph.org/index.html", import.meta.url),
      "utf8",
    );
    expect(page).toContain('const night=hour>=18||hour<6');
    expect(page).toContain('night&&systemDark?"dark":"light"');
    expect(page).toContain('matchMedia("(prefers-color-scheme: dark)")');
    expect(page).toContain('localStorage.setItem(THEME_KEY,button.dataset.themeChoice)');
    expect(page).toContain('data-theme-choice="auto"');
    expect(page).toContain('data-theme-choice="light"');
    expect(page).toContain('data-theme-choice="dark"');
    expect(page).toContain('<meta name="theme-color" content="#fffdf6">');
  });

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
