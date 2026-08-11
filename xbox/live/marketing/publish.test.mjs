import assert from "node:assert/strict";
import test from "node:test";

import { audioNameFor, payloadFor } from "./publish.mjs";

const options = {
  igUserId: "123",
  videoUrl: "https://example.test/reel.mp4",
  coverUrl: "https://example.test/cover.jpg",
};

test("a Reel names its original audio after the captured round", () => {
  const payload = payloadFor({
    caption: "fight",
    render: { rounds: [{ round: "vussi868" }] },
  }, options);

  assert.equal(payload.container.body.audio_name, "vussi868");
  assert.equal(audioNameFor({
    render: { rounds: [{ round: "vussi868" }] },
  }), "vussi868");
});

test("an explicit staged audio name wins and missing names are omitted", () => {
  const named = payloadFor({
    caption: "fight",
    audioName: "choggy364",
    render: { rounds: [{ round: "vussi868" }] },
  }, options);
  assert.equal(named.container.body.audio_name, "choggy364");

  const unnamed = payloadFor({ caption: "fight" }, options);
  assert.equal("audio_name" in unnamed.container.body, false);
});
