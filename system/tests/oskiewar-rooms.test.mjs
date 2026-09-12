// Room ledgers and track records: how stored rounds add up to what a room and
// a player have to show for themselves.
//
// Versus rounds began recording on 2026-09-11 (xbox/live/oskiewar-multiplayer.md,
// part five). A round is the honest unit here because every round document has
// a winner; a match is the five that decide one.
import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";
import test from "node:test";
import { canonicalRoomId, foldMatches, standings, roomHistory, trackRecord,
  validateDemo } from "../netlify/functions/oskiewar-replays.mjs";

const source = await readFile(
  new URL("../netlify/functions/oskiewar-replays.mjs", import.meta.url), "utf8");

// One round document, reduced to the fields the ledgers read.
const round = (series, index, winnerSeat, score, {
  fighters = ["@JEFFREY", "@OSKIE"], room = "regga890", at = "2026-09-11T12:00:00Z",
} = {}) => ({
  _id: `ow-${series}${index}`, seriesId: `ow-${series}`, seriesName: series,
  roundIndex: index, roomId: `ow-${room}`, roomName: room, fighters,
  winner: winnerSeat === null ? null : fighters[winnerSeat],
  finalRoundWins: score, recordedAt: at,
});

// A match: five rounds, seat one taking all of them.
const sweep = (series, fighters, room, day) => [0, 1, 2, 3, 4].map((index) =>
  round(series, index, 0, [index + 1, 0],
    { fighters, room, at: `2026-09-${day}T12:0${index}:00Z` }));

test("a room id is the same shape as a match id", () => {
  assert.equal(canonicalRoomId("regga890"), "ow-regga890");
  assert.equal(canonicalRoomId("ow-regga890"), "ow-regga890");
  assert.equal(canonicalRoomId("bafegu-dorimi-kunapo"), "ow-bafegu-dorimi-kunapo");
  assert.equal(canonicalRoomId("nope"), null);
  assert.equal(canonicalRoomId(""), null);
});

test("rounds fold into the match they belong to", () => {
  const matches = foldMatches(sweep("kodde385", ["@JEFFREY", "@OSKIE"], "regga890", 11));
  assert.equal(matches.length, 1);
  const [match] = matches;
  assert.equal(match.rounds, 5);
  assert.deepEqual(match.roundWins, [5, 0]);
  assert.deepEqual(match.score, [5, 0], "the last round carries the final tally");
  assert.equal(match.winner, "@JEFFREY");
  assert.equal(match.complete, true);
  assert.equal(match.roomId, "ow-regga890");
  assert.deepEqual(match.roundIds, [0, 1, 2, 3, 4].map((i) => `ow-kodde385${i}`),
    "and its rounds are listed in the order they were played");
});

test("a match still being played has no winner yet", () => {
  const matches = foldMatches([
    round("vinne123", 0, 0, [1, 0]),
    round("vinne123", 1, 1, [1, 1]),
    round("vinne123", 2, 0, [2, 1]),
  ]);
  assert.equal(matches[0].complete, false);
  assert.equal(matches[0].winner, null);
  assert.deepEqual(matches[0].roundWins, [2, 1]);
});

test("a tie is a round nobody won", () => {
  const matches = foldMatches([round("tello49", 0, null, [0, 0])]);
  assert.equal(matches[0].ties, 1);
  assert.deepEqual(matches[0].roundWins, [0, 0]);
  assert.equal(matches[0].winner, null);
});

test("a room keeps every match played at its address, newest first", () => {
  const rows = [
    ...sweep("kodde385", ["@JEFFREY", "@OSKIE"], "regga890", 9),
    ...sweep("vinne123", ["@FIFI", "@JEFFREY"], "regga890", 11),
  ];
  const history = roomHistory("ow-regga890", rows);
  assert.equal(history.room, "regga890");
  assert.equal(history.rounds, 10);
  assert.equal(history.matches.length, 2);
  assert.equal(history.matches[0].seriesName, "vinne123", "newest first");
  // And what everyone who played there has to show for it. @jeffrey won one
  // match and lost one, so he is level on matches and ahead on rounds.
  const jeffrey = history.players.find((one) => one.handle === "@JEFFREY");
  assert.equal(jeffrey.matchesPlayed, 2);
  assert.equal(jeffrey.matchesWon, 1);
  assert.equal(jeffrey.roundsWon, 5);
  assert.equal(jeffrey.roundsLost, 5);
  assert.deepEqual(jeffrey.opponents.sort(), ["@FIFI", "@OSKIE"]);
});

test("a signed-out fighter's rounds are kept but hold no record", () => {
  // The blank nameplate is a legitimate fighter — its rounds are stored — but
  // it is not a person, and every anonymous player would otherwise share one
  // track record between them.
  const history = roomHistory("ow-regga890",
    sweep("salle108", ["", "@OSKIE"], "regga890", 11));
  assert.equal(history.matches.length, 1, "the match is still in the ledger");
  assert.deepEqual(history.players.map((one) => one.handle), ["@OSKIE"]);
  assert.equal(history.players[0].roundsLost, 5);
  assert.deepEqual(history.players[0].opponents, [],
    "and nobody is recorded as having played a blank");
});

test("a track record follows a handle across rooms", () => {
  const rows = [
    ...sweep("kodde385", ["@JEFFREY", "@OSKIE"], "regga890", 9),
    ...sweep("bivva122", ["@OSKIE", "@JEFFREY"], "sezzi7", 11),
  ];
  const record = trackRecord("@oskie", rows);
  assert.equal(record.handle, "@OSKIE", "asked for in any case");
  assert.equal(record.matchesPlayed, 2);
  assert.equal(record.matchesWon, 1);
  assert.equal(record.roundsWon, 5);
  assert.equal(record.roundsLost, 5);
  assert.deepEqual(record.rooms.sort(), ["ow-regga890", "ow-sezzi7"]);
  assert.equal(record.matches.length, 2);
  assert.equal(record.lastAt, record.matches[0].endedAt);
});

test("a handle with nothing played reads as zero, not as missing", () => {
  const record = trackRecord("@nobody", sweep("kodde385",
    ["@JEFFREY", "@OSKIE"], "regga890", 11));
  assert.equal(record.matchesPlayed, 0);
  assert.equal(record.roundsWon, 0);
  assert.deepEqual(record.matches, []);
});

test("standings rank by matches won, then rounds", () => {
  const ranked = standings(foldMatches([
    ...sweep("kodde385", ["@JEFFREY", "@OSKIE"], "regga890", 9),
    ...sweep("bivva122", ["@JEFFREY", "@FIFI"], "regga890", 10),
    ...sweep("salle108", ["@FIFI", "@SAT"], "regga890", 11),
  ]));
  assert.deepEqual(ranked.map((one) => one.handle),
    ["@JEFFREY", "@FIFI", "@OSKIE", "@SAT"]);
  assert.equal(ranked[0].matchesWon, 2);
});

test("a demo may name the room it was played in, and say it had no clock", () => {
  const demo = {
    format: "ac.oskiedemo", version: 1, game: "oskiewar",
    simulation: "oskiewar-physics-1", tickRate: 60,
    matchId: "ow-kodde385", matchName: "kodde385",
    roundId: "ow-kodde385", roundName: "kodde385",
    seriesId: "ow-vinne123", seriesName: "vinne123",
    roundIds: ["ow-kodde385"], roundIndex: 0, previousRoundId: "",
    roomId: "ow-regga890", roomName: "regga890", timed: false,
    startedAt: 1789163404339, durationTicks: 1800,
    fighters: ["@JEFFREY", "@OSKIE"], winner: "@JEFFREY", finalRoundWins: [1, 0],
    commands: [], events: [], checkpoints: [], rounds: [],
  };
  assert.equal(validateDemo(demo), null);
  assert.equal(validateDemo({ ...demo, roomId: "regga890" }), "Invalid room");
  assert.equal(validateDemo({ ...demo, roomName: "other" }), "Invalid room");
  assert.equal(validateDemo({ ...demo, timed: "no" }), "Invalid timing flag");
  // Both stay optional: every demo recorded before versus rooms began keeping
  // history carries neither, and must still validate.
  const { roomId, roomName, timed, ...older } = demo;
  assert.equal(validateDemo(older), null);
});

test("a ledger reads summaries, not command streams", () => {
  // A room with five hundred rounds in it must not drag five hundred command
  // streams across the wire to answer "who has played here".
  assert.match(source, /const ROUND_SUMMARY = \{[^}]*finalRoundWins: 1/s);
  assert.doesNotMatch(source, /ROUND_SUMMARY = \{[^}]*commands/s);
  assert.match(source, /createIndex\(\{ roomId: 1, recordedAt: -1 \}\)/);
});
