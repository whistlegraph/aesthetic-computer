#!/usr/bin/env node
// oskiewar coach, 26.09.15
// "Add your coach." One file, no dependencies, served from oskiewar.com so any
// player can hand it to their own Claude (or Codex, or anything that speaks
// MCP over stdio) and have it sit in on their matches:
//
//   curl -fsSLO https://oskiewar.com/coach.mjs
//   claude mcp add coach -- node ./coach.mjs
//
// The coach takes the relay's `agent` seat on a match room — the one
// the title screen prints under START — and reads the same frames every phone
// in the grandstand reads. It never presses a button. From those frames it
// keeps a ledger a model can coach from: every hit, who threw it, what you
// were doing when it landed, how often you block, where you stand, how you
// die. `coach_watch` streams the events as they happen; `coach_analyze` folds
// them; `coach_record` pulls your track record from the replay ledger. While
// it is seated the title screen says "coach linked".
//
// Runs on Node 22+. Room tools are available by default; edits are applied by the room host.

import { createInterface } from "node:readline";
import { pathToFileURL } from "node:url";

const RELAY = process.env.OSKIEWAR_RELAY ||
  "wss://session-server.aesthetic.computer/oskiewar-live";
const API = process.env.OSKIEWAR_API ||
  "https://aesthetic.computer/api/oskiewar-replays";
const WORD = "(?:[bdfgklmnprstvz][aeiou]){3}";
const ROOM_NAME = new RegExp(
  `^(?:ow-)?((?:[a-z]{4,7}[0-9]{1,3})|(?:${WORD}-${WORD}-${WORD}))$`);
const HANDLE = /^@?[a-z0-9_-]{1,24}$/i;
const MAX_EVENTS = 4000;
const QUIET_MS = 8000;
const RETRY_MS = 2000;
// A flash on a fighter fades over a few frames; a fresh one steps up by more
// than any fade could, so a rise this big is a new contact, not the old one.
const FLASH_RISE = .1;
// How long a thrown move stays answerable for a landing or a block — spit and
// lobs land well after the arm has come back down.
const ATTACK_MEMORY_MS = 1500;

export function canonicalRoom(value) {
  const match = String(value || "").trim().toLowerCase().match(ROOM_NAME);
  return match ? "ow-" + match[1] : null;
}

export function canonicalHandle(value) {
  const raw = String(value || "").trim();
  return HANDLE.test(raw) ? "@" + raw.replace(/^@/, "").toUpperCase() : null;
}

// ---------------------------------------------------------------------------
// The ledger: frames in, events and folded numbers out. Pure, so it can be
// tested on synthetic frames and reused by anything else reading the relay.

const stateOf = (fighter) => fighter.attack ? "attacking"
  : fighter.blocking ? "blocking"
  : fighter.ducking ? "crouching"
  : fighter.grounded === false ? "airborne" : "standing";

const bump = (table, key, by = 1) => {
  table[key] = (table[key] || 0) + by;
};

const top = (table) => Object.entries(table)
  .sort((a, b) => b[1] - a[1])[0] || null;

const pct = (part, whole) => whole ? Math.round(part / whole * 100) : 0;

function freshFighter(name) {
  return { name, score: 0, roundWins: 0, samples: 0, air: 0, crouch: 0,
    block: 0, distance: 0, attacks: {}, hitsTaken: { total: 0, byAttack: {},
      whileState: {} }, blocks: { total: 0, byAttack: {} }, deaths: 0, kills: 0 };
}

export function createLedger() {
  const fighters = [];
  const previous = [];
  // What each fighter last threw and when, so a delayed landing still has a
  // name — and so one swing is scored once, as landed, blocked or whiffed.
  const swings = [];
  const events = [];
  const rounds = [];
  let phase = "";
  let frames = 0;
  let firstAt = 0;
  let lastAt = 0;
  let cursor = 0;

  function note(event) {
    event.index = ++cursor;
    events.push(event);
    if (events.length > MAX_EVENTS) events.splice(0, events.length - MAX_EVENTS);
    return event;
  }

  function attackName(index, at) {
    const swing = swings[index];
    if (!swing) return "contact";
    if (at - swing.at > ATTACK_MEMORY_MS) return "contact";
    return swing.kind;
  }

  function settle(index, outcome) {
    const swing = swings[index];
    if (!swing || swing.outcome) return;
    swing.outcome = outcome;
    bump(fighters[index].attacks[swing.kind], outcome);
  }

  function observe(frame) {
    if (!frame || !Array.isArray(frame.fighters)) return [];
    const at = Number(frame.at) || lastAt || Date.now();
    if (!firstAt) firstAt = at;
    lastAt = at;
    frames++;
    const fresh = [];
    if (frame.phase && frame.phase !== phase) {
      const from = phase;
      phase = frame.phase;
      if (phase === "fight") rounds.push({ startedAt: at, result: "", cause: "" });
      if ((phase === "round" || phase === "match") && rounds.length) {
        const round = rounds[rounds.length - 1];
        round.result = String(frame.round?.result || "");
        round.cause = String(frame.round?.cause || "");
        round.endedAt = at;
        round.scores = frame.fighters.map((fighter) => fighter.score || 0);
        fresh.push(note({ at, seq: frame.seq, kind: "round", phase,
          result: round.result, cause: round.cause,
          scores: round.scores.slice() }));
      } else {
        fresh.push(note({ at, seq: frame.seq, kind: "phase", from, phase }));
      }
    }
    const prevs = frame.fighters.map((fighter, index) => previous[index] ||
      { hit: 0, blockFlash: 0, attack: "", alive: true, roundWins: 0,
        state: "standing" });
    // Swings first, both seats, so a contact read on the same frame as the
    // rival's first attack frame already knows what hit it.
    frame.fighters.forEach((fighter, index) => {
      const rival = frame.fighters[1 - index];
      const stats = fighters[index] || (fighters[index] =
        freshFighter(fighter.name || ""));
      if (fighter.name && fighter.name !== "NOBODY") stats.name = fighter.name;
      stats.score = fighter.score || 0;
      const prev = prevs[index];
      const distance = rival ? Math.abs((fighter.x || 0) - (rival.x || 0)) : 0;
      // A swing begins the frame the attack kind changes. The old swing is
      // whatever it was by now; if nothing answered it, it missed.
      if (fighter.attack && fighter.attack !== prev.attack) {
        settle(index, "whiffed");
        stats.attacks[fighter.attack] ||= { thrown: 0, landed: 0, blocked: 0,
          whiffed: 0 };
        stats.attacks[fighter.attack].thrown++;
        swings[index] = { kind: fighter.attack, at, outcome: "" };
        fresh.push(note({ at, seq: frame.seq, kind: "attack", who: index,
          name: stats.name, attack: fighter.attack, distance: Math.round(distance) }));
      } else if (!fighter.attack && prev.attack && swings[index] &&
          !swings[index].outcome && at - swings[index].at > ATTACK_MEMORY_MS) {
        settle(index, "whiffed");
      }
    });
    frame.fighters.forEach((fighter, index) => {
      const rival = frame.fighters[1 - index];
      const stats = fighters[index];
      const prev = prevs[index];
      const state = stateOf(fighter);
      const distance = rival ? Math.abs((fighter.x || 0) - (rival.x || 0)) : 0;
      stats.samples++;
      if (fighter.grounded === false) stats.air++;
      if (fighter.ducking) stats.crouch++;
      if (fighter.blocking) stats.block++;
      stats.distance += distance;
      if ((fighter.hit || 0) > (prev.hit || 0) + FLASH_RISE && rival) {
        const by = 1 - index;
        const attack = attackName(by, at);
        // Being hit is read from the frame before: the flinch itself is the
        // state now, and what got you hit is what you were doing coming in.
        const doing = prev.state;
        stats.hitsTaken.total++;
        bump(stats.hitsTaken.byAttack, attack);
        bump(stats.hitsTaken.whileState, doing);
        settle(by, "landed");
        fresh.push(note({ at, seq: frame.seq, kind: "hit", who: index,
          name: stats.name, by, byName: fighters[by]?.name || "", attack,
          doing, distance: Math.round(distance) }));
      }
      if ((fighter.blockFlash || 0) > (prev.blockFlash || 0) + FLASH_RISE && rival) {
        const by = 1 - index;
        const attack = attackName(by, at);
        stats.blocks.total++;
        bump(stats.blocks.byAttack, attack);
        settle(by, "blocked");
        fresh.push(note({ at, seq: frame.seq, kind: "block", who: index,
          name: stats.name, by, byName: fighters[by]?.name || "", attack,
          distance: Math.round(distance) }));
      }
      if (prev.alive !== false && fighter.alive === false) {
        stats.deaths++;
        if (fighters[1 - index]) fighters[1 - index].kills++;
        fresh.push(note({ at, seq: frame.seq, kind: "death", who: index,
          name: stats.name, by: 1 - index, byName: fighters[1 - index]?.name || "",
          attack: attackName(1 - index, at), doing: prev.state }));
      }
      if ((fighter.roundWins || 0) > (prev.roundWins || 0)) {
        stats.roundWins = fighter.roundWins;
        fresh.push(note({ at, seq: frame.seq, kind: "roundWin", who: index,
          name: stats.name, roundWins: fighter.roundWins }));
      }
      previous[index] = { hit: fighter.hit || 0, blockFlash: fighter.blockFlash || 0,
        attack: fighter.attack || "", alive: fighter.alive !== false,
        roundWins: fighter.roundWins || 0, state };
    });
    return fresh;
  }

  function since(index = 0, limit = 200) {
    return events.filter((event) => event.index > index).slice(-limit);
  }

  function summary() {
    const seconds = firstAt && lastAt ? Math.round((lastAt - firstAt) / 1000) : 0;
    const report = fighters.map((stats, index) => {
      const taken = stats.hitsTaken.total;
      const blocked = stats.blocks.total;
      const attacks = Object.entries(stats.attacks).map(([kind, tally]) => ({
        kind, ...tally, accuracy: pct(tally.landed, tally.thrown) }))
        .sort((a, b) => b.thrown - a.thrown);
      const punished = top(stats.hitsTaken.whileState);
      const worstFor = top(stats.hitsTaken.byAttack);
      const reliable = attacks.filter((attack) => attack.thrown >= 3)
        .sort((a, b) => b.accuracy - a.accuracy)[0] || null;
      const wasteful = attacks.filter((attack) => attack.thrown >= 3)
        .sort((a, b) => a.accuracy - b.accuracy)[0] || null;
      const notes = [];
      if (punished && taken >= 3)
        notes.push(`${pct(punished[1], taken)}% of hits taken while ${punished[0]}`);
      if (worstFor && taken >= 3)
        notes.push(`${worstFor[0]} lands on them most (${worstFor[1]} of ${taken})`);
      if (taken + blocked >= 5)
        notes.push(`blocks ${pct(blocked, taken + blocked)}% of contact`);
      if (reliable) notes.push(`${reliable.kind} is the reliable move: ` +
        `${reliable.landed}/${reliable.thrown} landed`);
      if (wasteful && wasteful !== reliable) notes.push(`${wasteful.kind} is ` +
        `thrown ${wasteful.thrown}× and lands ${wasteful.accuracy}%`);
      if (stats.samples >= 40 && pct(stats.air, stats.samples) >= 35)
        notes.push(`airborne ${pct(stats.air, stats.samples)}% of the time`);
      return { seat: index, name: stats.name, score: stats.score,
        roundWins: stats.roundWins, deaths: stats.deaths, kills: stats.kills,
        hitsTaken: taken, hitsTakenBy: stats.hitsTaken.byAttack,
        hitsTakenWhile: stats.hitsTaken.whileState, blocks: blocked,
        blocksBy: stats.blocks.byAttack,
        blockRate: pct(blocked, taken + blocked), attacks,
        airPct: pct(stats.air, stats.samples),
        crouchPct: pct(stats.crouch, stats.samples),
        blockingPct: pct(stats.block, stats.samples),
        averageDistance: stats.samples
          ? Math.round(stats.distance / stats.samples) : 0,
        notes };
    });
    return { frames, seconds, phase, rounds: rounds.length,
      recentRounds: rounds.slice(-10), events: events.length, fighters: report };
  }

  return { observe, since, summary, get cursor() { return cursor; } };
}

// ---------------------------------------------------------------------------
// The seat. One attachment at a time; a round room hands off to the next and
// the session room is the forwarding address when a round goes quiet.

const workshopPending = new Map();
let workshopSequence = 0;
let workshopNextRoom = null;

const state = {
  socket: null, room: null, wantRoom: null, sessionRoom: "", label: "coach",
  generation: 0, quietTimer: null, retryTimer: null, status: null,
  lastFrame: null, ledger: createLedger(), cursor: 0, attachedAt: 0,
  followed: [],
};

function stopTimers() {
  clearTimeout(state.quietTimer);
  clearTimeout(state.retryTimer);
  state.quietTimer = state.retryTimer = null;
}

function armQuiet() {
  clearTimeout(state.quietTimer);
  if (!state.sessionRoom || state.sessionRoom === state.room) return;
  state.quietTimer = setTimeout(() => follow(state.sessionRoom), QUIET_MS);
}

function follow(next) {
  if (!next || next === state.room || !state.wantRoom) return;
  state.followed.push({ from: state.room, to: next, at: Date.now() });
  if (state.followed.length > 20) state.followed.shift();
  open(next).catch(() => {});
}

function open(room) {
  const mine = ++state.generation;
  stopTimers();
  try { state.socket?.close(); } catch { /* replaced */ }
  state.socket = null;
  state.room = room;
  state.wantRoom = state.wantRoom || room;
  return new Promise((resolve, reject) => {
    let settled = false;
    const done = (error) => {
      if (settled) return;
      settled = true;
      clearTimeout(timer);
      error ? reject(error) : resolve();
    };
    const timer = setTimeout(() =>
      done(new Error("The relay did not answer within 10s")), 10000);
    let socket;
    try {
      socket = new WebSocket(`${RELAY}?match=${encodeURIComponent(room)}` +
        `&role=agent&label=${encodeURIComponent(state.label)}`);
    } catch (error) {
      done(error);
      return;
    }
    state.socket = socket;
    socket.addEventListener("message", (event) => {
      if (mine !== state.generation) return;
      let message;
      try { message = JSON.parse(event.data); } catch { return; }
      if (message.type === "oskiewar:workshop-result") {
        const pending = workshopPending.get(message.content?.id);
        if (pending) {
          workshopPending.delete(message.content.id);
          clearTimeout(pending.timer);
          message.content.ok ? pending.resolve(text(message.content.result))
            : pending.reject(new Error(message.content.error || "Workshop command failed"));
        }
        if (workshopNextRoom) {
          const next = workshopNextRoom; workshopNextRoom = null; follow(next);
        }
        return;
      }
      if (message.type === "oskiewar:error") {
        done(new Error(String(message.content?.message || "relay error")));
        return;
      }
      if (message.type === "oskiewar:status") {
        state.status = message.content || null;
        if (!state.attachedAt) state.attachedAt = Date.now();
        done();
        return;
      }
      if (message.type !== "oskiewar:state") return;
      const frame = message.content;
      if (!frame) return;
      state.lastFrame = frame;
      if (frame.sessionId) state.sessionRoom = frame.sessionId;
      state.ledger.observe(frame);
      done();
      armQuiet();
      if (frame.nextRoundId && frame.nextRoundId !== state.room) {
        if (workshopPending.size) workshopNextRoom = frame.nextRoundId;
        else follow(frame.nextRoundId);
      }
    });
    socket.addEventListener("close", () => {
      if (mine !== state.generation) return;
      state.socket = null;
      if (!state.wantRoom) return;
      state.retryTimer = setTimeout(() => {
        if (mine !== state.generation || !state.wantRoom) return;
        if (state.sessionRoom && state.sessionRoom !== state.room)
          follow(state.sessionRoom);
        else open(state.room).catch(() => {});
      }, RETRY_MS);
      done(new Error("The relay closed the socket"));
    });
    socket.addEventListener("error", () => {
      try { socket.close(); } catch { /* already closing */ }
    });
  });
}

function detach() {
  workshopNextRoom = null;
  state.wantRoom = null;
  state.generation++;
  stopTimers();
  try { state.socket?.close(); } catch { /* already gone */ }
  state.socket = null;
  const room = state.room;
  state.room = null;
  state.sessionRoom = "";
  state.status = null;
  state.attachedAt = 0;
  return room;
}

async function fetchJson(url) {
  const response = await fetch(url, { headers: { accept: "application/json" } });
  if (!response.ok) throw new Error(`${url} answered ${response.status}`);
  return response.json();
}

function scoreboard() {
  const frame = state.lastFrame;
  if (!frame) return null;
  return { seq: frame.seq, phase: frame.phase, at: frame.at,
    round: frame.round || null,
    fighters: (frame.fighters || []).map((fighter) => ({ name: fighter.name,
      score: fighter.score, roundWins: fighter.roundWins, alive: fighter.alive,
      attack: fighter.attack || "", x: Math.round(fighter.x || 0),
      y: Math.round(fighter.y || 0) })),
    roundId: frame.roundId || "", sessionId: frame.sessionId || "",
    perf: frame.perf || null };
}

const text = (value) => [{ type: "text",
  text: typeof value === "string" ? value : JSON.stringify(value, null, 2) }];

// ---------------------------------------------------------------------------
// Tools.

async function toolIn(args) {
  const label = String(args.label || "coach").slice(0, 24) || "coach";
  let room = canonicalRoom(args.room || args.match);
  let via = room ? "room" : "";
  if (!room && args.handle) {
    const handle = canonicalHandle(args.handle);
    if (!handle) throw new Error(`"${args.handle}" is not a handle.`);
    const record = await fetchJson(`${API}?fighter=${encodeURIComponent(handle)}`);
    room = record.rooms?.[0] || null;
    via = room ? `latest room of ${handle}` : "";
    if (!room) throw new Error(`${handle} has no recorded versus room yet. ` +
      "Read the room name off the title screen — it is printed under START — " +
      "and pass it as `room`.");
  }
  if (!room) throw new Error("Give a `room` (the name printed under START on " +
    "the oskiewar title screen, e.g. regga890) or a `handle`.");
  detach();
  state.label = label;
  state.wantRoom = room;
  state.ledger = createLedger();
  state.cursor = 0;
  state.lastFrame = null;
  state.followed = [];
  await open(room);
  return text({ linked: true, room, via, label, status: state.status,
    scoreboard: scoreboard(),
    note: state.status?.live
      ? "Seated. The title screen now reads \"coach linked\". Call " +
        "coach_watch to follow the fight and coach_analyze to fold it."
      : "Seated, but nobody is publishing this room right now. Frames " +
        "arrive when the game at that address is open." });
}

function toolStatus() {
  if (!state.wantRoom) return text({ linked: false,
    note: "No seat. Call coach_in with a room name or a handle." });
  return text({ linked: Boolean(state.socket), room: state.room,
    sessionRoom: state.sessionRoom, label: state.label,
    attachedAt: state.attachedAt, status: state.status,
    scoreboard: scoreboard(), events: state.ledger.cursor,
    followed: state.followed });
}

async function toolWatch(args) {
  if (!state.wantRoom) throw new Error("No seat. Call coach_in first.");
  const seconds = Math.min(60, Math.max(0, Number(args.seconds) || 10));
  const limit = Math.min(500, Math.max(1, Number(args.limit) || 120));
  const deadline = Date.now() + seconds * 1000;
  let events = state.ledger.since(state.cursor, limit);
  while (!events.length && Date.now() < deadline) {
    await new Promise((resolve) => setTimeout(resolve, 250));
    events = state.ledger.since(state.cursor, limit);
  }
  if (events.length) state.cursor = events[events.length - 1].index;
  return text({ room: state.room, live: state.status?.live ?? null,
    scoreboard: scoreboard(), events });
}

function toolAnalyze() {
  if (!state.wantRoom) throw new Error("No seat. Call coach_in first.");
  return text({ room: state.room, ...state.ledger.summary() });
}

async function toolRecord(args) {
  const handle = canonicalHandle(args.handle);
  if (!handle) throw new Error("Give a `handle`, e.g. @jeffrey.");
  const record = await fetchJson(`${API}?fighter=${encodeURIComponent(handle)}`);
  const limit = Math.min(100, Math.max(1, Number(args.limit) || 20));
  return text({ ...record, matches: (record.matches || []).slice(0, limit),
    totalMatches: (record.matches || []).length });
}

async function toolReplay(args) {
  let url;
  if (args.id) {
    const id = canonicalRoom(args.id);
    if (!id) throw new Error(`"${args.id}" is not a round id.`);
    url = `${API}?id=${id}`;
  } else if (args.series) {
    const id = canonicalRoom(args.series);
    if (!id) throw new Error(`"${args.series}" is not a series id.`);
    url = `${API}?series=${id}`;
  } else if (args.room) {
    const id = canonicalRoom(args.room);
    if (!id) throw new Error(`"${args.room}" is not a room.`);
    url = `${API}?room=${id}`;
  } else {
    throw new Error("Give an `id` (one round), a `series` (one match) or a " +
      "`room` (every match at an address).");
  }
  const body = await fetchJson(url);
  // Command streams are the bulk of a stored round and a coach reads the
  // ledger, not the joystick. Keep the shape and drop the stream.
  const trim = (replay) => {
    if (!replay || typeof replay !== "object") return replay;
    const { commands, checkpoints, ...rest } = replay;
    return { ...rest, commandCount: Array.isArray(commands) ? commands.length : 0,
      checkpointCount: Array.isArray(checkpoints) ? checkpoints.length : 0,
      events: Array.isArray(rest.events) ? rest.events.slice(0, 200) : rest.events };
  };
  if (body.replay) body.replay = trim(body.replay);
  if (Array.isArray(body.rounds)) body.rounds = body.rounds.map(trim);
  return text(body);
}

function toolOut() {
  if (!state.wantRoom) return text({ linked: false, note: "No seat to leave." });
  const room = detach();
  return text({ linked: false,
    note: `Left ${room}. The title screen's "coach linked" goes dark.` });
}

function toolWorkshop(command) {
  if (!state.socket || state.socket.readyState !== 1)
    throw new Error("Call coach_in and wait for a live connection first");
  if (workshopPending.size) throw new Error("Wait for the previous workshop command");
  const id = String(++workshopSequence);
  const packet = JSON.stringify({ type: "oskiewar:workshop", content: { id, command } });
  if (Buffer.byteLength(packet) > 16384) throw new Error("Workshop command exceeds 16 KiB");
  return new Promise((resolve, reject) => {
    const timer = setTimeout(() => {
      workshopPending.delete(id);
      if (workshopNextRoom) { const next = workshopNextRoom; workshopNextRoom = null; follow(next); }
      reject(new Error("No workshop acknowledgement. Inspect before retrying; the operation may have completed."));
    }, 15000);
    workshopPending.set(id, { resolve, reject, timer });
    try { state.socket.send(packet); } catch (error) {
      clearTimeout(timer); workshopPending.delete(id); reject(error);
    }
  });
}

const TOOLS = [
  { name: "coach_workshop",
    description: "Edit the hosted room while the player plays. Room tools are enabled by default. " +
      "Start with inspect: returns the complete map, revision and player positions. " +
      "apply replaces the map with your edited document; drop adds an item; undo restores the previous map. " +
      "reset-round preserves round wins; restart-level clears them. Both keep the edited map. " +
      "highlight shows spawn markers. save stores a named private draft; publish creates an immutable public version " +
      "using the player's signed-in account. list lists public maps (mine:true lists own drafts and publications); " +
      "load applies a saved map by id. Use the latest revision for every mutation/save/publish/load. " +
      "Map version 1: format ac.oskiewar.map, name, features (ordered contiguous from/to covering 0–40, " +
      "kind flat/bank/transition, lift -450–720, rise 0–720, dir -1/1), decks (col,cols,row), " +
      "two spawns (columns 0–39), pickups (kind,col,amount), skateboard boolean. " +
      "Item kinds: HANDGUN, SPACE LASER, RUBBER SMG, ROCKET LAUNCHER, LIGHT SABER, GRENADE. " +
      "Read the returned map after changes, then watch play and iterate. Network edits switch both players to the host stream. Survival/replay editing is unavailable.",
    inputSchema: { type: "object", additionalProperties: false, required: ["op"], properties: {
      op: { type: "string", enum: ["inspect", "apply", "drop", "undo", "reset-round", "restart-level",
        "highlight", "save", "publish", "load", "list"] },
      revision: { type: "integer", minimum: 0 }, map: { type: "object" },
      item: { type: "object", required: ["kind", "col", "amount"], properties: {
        kind: { type: "string" }, col: { type: "number" }, amount: { type: "integer" } } },
      enabled: { type: "boolean" }, name: { type: "string", maxLength: 60 },
      id: { type: "string" }, mine: { type: "boolean" },
    } } },
  { name: "coach_in",
    description: "Add your coach: take the agent seat on an " +
      "oskiewar match room so this session can watch the fight. Give the " +
      "room name printed under START on the title screen, or a handle to " +
      "sit in their most recent versus room. The title screen shows " +
      "\"coach linked\" while seated. One seat at a time; presence drops " +
      "when this session ends.",
    inputSchema: { type: "object", properties: {
      room: { type: "string", description: "Room name, e.g. regga890 or ow-regga890" },
      handle: { type: "string", description: "A player handle, e.g. @jeffrey — used when no room is given" },
      label: { type: "string", description: "How the seat is labelled on the relay (default coach)" },
    } } },
  { name: "coach_status",
    description: "Where the coach is seated, whether the room is live, the " +
      "current scoreboard and how many events the ledger holds.",
    inputSchema: { type: "object", properties: {} } },
  { name: "coach_watch",
    description: "Wait up to `seconds` for new fight events — hits (with the " +
      "attack that landed and what the victim was doing), blocks, swings, " +
      "deaths, round results — and return them with the live scoreboard. " +
      "Successive calls continue from where the last one stopped.",
    inputSchema: { type: "object", properties: {
      seconds: { type: "number", description: "How long to wait for something to happen (default 10, max 60)" },
      limit: { type: "number", description: "Most events to return (default 120)" },
    } } },
  { name: "coach_analyze",
    description: "Fold everything seen since coach_in into per-fighter " +
      "numbers: hits taken by attack and by what they were doing, block " +
      "rate, each move's thrown/landed/blocked/whiffed, air and crouch " +
      "time, average spacing, deaths, rounds — plus short computed notes " +
      "to coach from.",
    inputSchema: { type: "object", properties: {} } },
  { name: "coach_record",
    description: "A player's track record from the replay ledger: rounds and " +
      "matches won and lost, opponents, the rooms they have played in and " +
      "their recent matches.",
    inputSchema: { type: "object", properties: {
      handle: { type: "string", description: "Player handle, e.g. @jeffrey" },
      limit: { type: "number", description: "Recent matches to include (default 20)" },
    } } },
  { name: "coach_replay",
    description: "Fetch a stored round by id, a match by series id, or every " +
      "match played at a room address, with command streams trimmed to counts.",
    inputSchema: { type: "object", properties: {
      id: { type: "string", description: "Round id, e.g. ow-regga890" },
      series: { type: "string", description: "Series (match) id" },
      room: { type: "string", description: "Room address" },
    } } },
  { name: "coach_out",
    description: "Leave the seat. The title screen's \"coach linked\" goes dark.",
    inputSchema: { type: "object", properties: {} } },
];

async function callTool(name, args = {}) {
  switch (name) {
    case "coach_workshop": return toolWorkshop(args);
    case "coach_in": return toolIn(args);
    case "coach_status": return toolStatus();
    case "coach_watch": return toolWatch(args);
    case "coach_analyze": return toolAnalyze();
    case "coach_record": return toolRecord(args);
    case "coach_replay": return toolReplay(args);
    case "coach_out": return toolOut();
    default: throw new Error(`Unknown tool: ${name}`);
  }
}

export async function handleMessage(message) {
  const { id, method, params } = message;
  try {
    switch (method) {
      case "initialize":
        return { jsonrpc: "2.0", id, result: {
          protocolVersion: params?.protocolVersion || "2024-11-05",
          capabilities: { tools: {} },
          serverInfo: { name: "oskiewar-coach", version: "1.1.0" },
          instructions: "Claude Coach for oskiewar. coach_in seats this " +
            "session as an agent on a match room (the name under " +
            "START on the title screen); coach_watch streams hits, blocks, " +
            "swings and deaths as they happen; coach_analyze folds them into " +
            "numbers to coach from; coach_record and coach_replay read the " +
            "replay ledger. coach_workshop edits hosted room maps and saves or publishes them. Tools are available by default; /workshop opens the room controls. Inspect before editing and use the returned revision." } };
      case "initialized":
      case "notifications/initialized": return null;
      case "ping": return { jsonrpc: "2.0", id, result: {} };
      case "tools/list": return { jsonrpc: "2.0", id, result: { tools: TOOLS } };
      case "tools/call": {
        const content = await callTool(params?.name, params?.arguments || {});
        return { jsonrpc: "2.0", id, result: { content } };
      }
      default: return { jsonrpc: "2.0", id,
        error: { code: -32601, message: `Method not found: ${method}` } };
    }
  } catch (error) {
    if (method === "tools/call") return { jsonrpc: "2.0", id, result: {
      isError: true, content: text(String(error.message || error)) } };
    return { jsonrpc: "2.0", id,
      error: { code: -32000, message: String(error.message || error) } };
  }
}

export function main() {
  const lines = createInterface({ input: process.stdin, terminal: false });
  lines.on("line", async (line) => {
    if (!line.trim()) return;
    let message;
    try { message = JSON.parse(line); } catch (error) {
      console.error(JSON.stringify({ jsonrpc: "2.0", id: null,
        error: { code: -32700, message: `Parse error: ${error.message}` } }));
      return;
    }
    const response = await handleMessage(message);
    if (response) console.log(JSON.stringify(response));
  });
  console.error("🥊 oskiewar coach ready (coach_in, coach_watch, " +
    "coach_analyze, coach_record, coach_replay, coach_status, coach_out)");
}

if (process.argv[1] && import.meta.url === pathToFileURL(process.argv[1]).href)
  main();
