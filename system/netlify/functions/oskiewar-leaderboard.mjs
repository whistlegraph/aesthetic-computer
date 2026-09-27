// Authenticated two-player match standings; independent of unverified replays.
import { authorize } from '../../backend/authorization.mjs';
import { connect } from '../../backend/database.mjs';
import { respond } from '../../backend/http.mjs';
import { createLeaderboardHandler } from '../../backend/oskiewar-leaderboard.mjs';
export const handler = createLeaderboardHandler({ authorize, connect, respond });
