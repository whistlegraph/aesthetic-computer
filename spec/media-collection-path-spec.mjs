import { mediaCollectionPath } from "../system/netlify/functions/media-collection.js";

describe("media collection paths", () => {
  const userId = "auth0|abc";
  it("places bare slugs in their media folder", () => {
    expect(mediaCollectionPath({ userId, mediaType: "painting", slug: "work", extension: "png" }))
      .toBe("painting/work.png");
  });
  it("preserves storage-qualified slugs", () => {
    expect(mediaCollectionPath({ userId, mediaType: "painting", slug: `${userId}/chat/work`, extension: "png" }))
      .toBe(`${userId}/chat/work.png`);
  });
});
