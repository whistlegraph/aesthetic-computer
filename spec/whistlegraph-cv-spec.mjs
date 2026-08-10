import { readFileSync } from "node:fs";

const page = readFileSync(
  new URL("../system/public/whistlegraph.org/index.html", import.meta.url),
  "utf8",
);

describe("Whistlegraph CV", () => {
  it("places the 2021 CV additions by event type", () => {
    const talks = page.indexOf("Talks &amp; lectures");
    const performances = page.indexOf("Performances &amp; streams");
    const exhibitions = page.indexOf("Exhibitions", performances);
    const programs = page.indexOf("Workshops &amp; public programs");
    const press = page.indexOf("Press &amp; writing");

    expect(page.indexOf("Virtual Creative Industries Discussion: Whistlegraph")).toBeGreaterThan(talks);
    expect(page.indexOf("Virtual Creative Industries Discussion: Whistlegraph")).toBeLessThan(performances);
    expect(page.indexOf("Bananskolen + Whistlegraph")).toBeGreaterThan(performances);
    expect(page.indexOf("Bananskolen + Whistlegraph")).toBeLessThan(exhibitions);
    expect(page.indexOf("Art Beyond Festival")).toBeGreaterThan(programs);
    expect(page.indexOf("Art Beyond Festival")).toBeLessThan(press);
  });

  it("keeps each section newest first", () => {
    const performances = page.slice(
      page.indexOf("Performances &amp; streams"),
      page.indexOf("Exhibitions", page.indexOf("Performances &amp; streams")),
    );
    expect(performances.indexOf("2022")).toBeLessThan(performances.indexOf("2021"));
    expect(performances.indexOf("2021")).toBeLessThan(performances.indexOf("spring 2020"));
  });
});
