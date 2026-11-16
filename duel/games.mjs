// 🎮 Gametypes: a room whose name a game claims plays by that game's rules
// and wears its interface. Each is one object: `at(room)`, `flat` (bodies
// keep to the z = 0 plane), `ledges` (its fixed boxes), `camera(me, players,
// aspect)`, `legend` ({ keys, pad, touch } rows, as view.mjs's LEGEND) and
// `hud` (false for the parts of dool's own interface it does without:
// reticle, face, aim). Add a game here and the client and view follow.
import { GAME as hedbop } from './bop.mjs';

export const GAMES = [hedbop];
export const gameFor = (room) => GAMES.find((g) => g.at(room)) || null;
