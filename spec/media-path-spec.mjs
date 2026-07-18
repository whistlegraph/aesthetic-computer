import { userMediaTarget } from "../lith/media-path.mjs";

describe("lith user media paths", () => {
  const userId = "auth0|abc";
  it("does not duplicate a storage-qualified painting slug", () => {
    expect(userMediaTarget({ userId, subPath: `${userId}/painting/work.png`, extension: "png" }))
      .toBe("https://user.aesthetic.computer/auth0%7Cabc/painting/work.png");
  });
  it("qualifies a bare painting path once", () => {
    expect(userMediaTarget({ userId, subPath: "painting/work.png", extension: "png" }))
      .toBe("https://user.aesthetic.computer/auth0%7Cabc/painting/work.png");
  });
});
