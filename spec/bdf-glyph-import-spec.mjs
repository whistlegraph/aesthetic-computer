describe("bdf-glyph lith compatibility", () => {
  it("loads without the retired node-fetch package", async () => {
    const module = await import("../system/netlify/functions/bdf-glyph.js");
    expect(typeof module.handler).toBe("function");
  });
});
