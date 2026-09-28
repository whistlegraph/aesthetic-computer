import { parseBegin, objectKey, TAKE_FILES } from "../system/netlify/functions/menuband-takes.mjs";

describe("menuband takes", () => {
  const base = {
    takeId: "take-1",
    recordedAt: "2026-09-28T12:00:00Z",
    machine: "neo",
    duration: 42.5,
    bpm: 96,
    files: [{ name: "mix.mp3", bytes: 1000 }, { name: "voice.wav", bytes: 2000 }],
  };

  it("keys objects under the owner's sub", () => {
    expect(objectKey("auth0|abc", "xyz", "mix.mp3")).toBe("auth0|abc/menuband/xyz/mix.mp3");
  });

  it("accepts a well-formed take and types its files", () => {
    const take = parseBegin(base);
    expect(take.takeId).toBe("take-1");
    expect(take.recordedAt.toISOString()).toBe("2026-09-28T12:00:00.000Z");
    expect(take.bpm).toBe(96);
    expect(take.program).toBeNull();
    expect(take.files["voice.wav"]).toEqual({ bytes: 2000, contentType: "audio/wav" });
  });

  it("refuses files outside the take's set", () => {
    expect(() => parseBegin({ ...base, files: [{ name: "../evil.sh", bytes: 1 }] }))
      .toThrowError(/Unsupported file/);
  });

  it("refuses empty or oversized files", () => {
    expect(() => parseBegin({ ...base, files: [{ name: "mix.mp3", bytes: 0 }] })).toThrow();
    expect(() => parseBegin({ ...base, files: [{ name: "mix.mp3", bytes: 200 * 1024 * 1024 }] })).toThrow();
  });

  it("requires a takeId", () => {
    expect(() => parseBegin({ ...base, takeId: "" })).toThrowError(/takeId/);
  });

  it("covers every file Menu Band writes", () => {
    for (const name of ["mix.mp3", "tones.wav", "percussion.wav", "voice.wav", "notes.mid", "mix.json"])
      expect(TAKE_FILES[name]).toBeDefined();
  });
});
