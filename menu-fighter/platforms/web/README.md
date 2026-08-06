# Web

The web build is the canonical first client: Aesthetic Computer's
`/menu-fighter` piece. Add the browser WebRTC transport here conceptually, but
keep reusable implementation beside the fight libraries rather than creating a
second bundle in this directory.

Shipping gates: gamepad remapping, focus-loss behavior, installable PWA assets,
cross-browser determinism runs, TURN credentials, and immutable match versions.
