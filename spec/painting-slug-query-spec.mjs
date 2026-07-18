import { userPaintingSlugQuery } from "../system/backend/painting-slug-query.mjs";

describe("user painting slug lookup", () => {
  it("matches exact and qualified storage slugs", () => {
    const query = userPaintingSlugQuery("1782187678513");
    expect(query.$or[0]).toEqual({ slug: "1782187678513" });
    expect(query.$or[1].slug.test("auth0|abc/chat/1782187678513")).toBeTrue();
    expect(query.$or[1].slug.test("auth0|abc/chat/17821876785130")).toBeFalse();
  });
});
