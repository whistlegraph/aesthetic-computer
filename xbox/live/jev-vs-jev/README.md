# Jev vs Jev

`https://oskiewar.com/jev-vs-jev` runs the real Oskiewar simulation with two
independent Jev controllers. Each chooses a direction and action from its own
visible-world snapshot. The built-in bot never supplies input on this route.
Responses hold the chosen buttons for 250 ms (450 ms for a full jump), then release. No response means
neutral input; stale responses and old-round responses are discarded.
Transient provider failures leave a fighter neutral while it requests a fresh
decision; three consecutive failures stop the match.

The page shows separate provider-reported cost, input/output tokens, decision
counts, and browser round-trip latency. Simulation stays at its normal rate
while decisions arrive asynchronously. It is not frame-by-frame inference.
Stop and hidden-tab handling release both pads. A match lasts 60 seconds.
The demo does not publish replays or send match telemetry to PostHog.

`POST /api/oskiewar-jev` holds the OpenRouter key on Lith. A random match ticket
expires after 75 seconds and permits at most 150 calls per seat. MongoDB
atomically reserves 300 calls per match against a global 20,000-call UTC daily
limit. Reservations are not refunded. Starts are limited to one per minute
per source address in the serving process, and that process allows at most
12 concurrent decisions. Database failures refuse paid calls. TTL indexes
remove expired ticket/counter records. The daily call ceiling is a request
limit, not a fixed dollar ceiling; provider pricing can change.

```sh
node --env-file="$HOME/.config/aesthetic-computer/jev.env" xbox/tools/jev-vs-jev-dev.mjs
# http://127.0.0.1:8791/jev-vs-jev
```

The loopback development server uses expiring in-memory match counters.
Production uses persistent Mongo counters. The shared model input normalizer
accepts only bounded game coordinates, booleans and fixed movement-option kinds.
No arbitrary question, prompt, model or provider URL is accepted by the endpoint.
