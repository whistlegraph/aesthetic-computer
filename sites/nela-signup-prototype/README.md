# NELA signup prototype

A proposed signup flow for [NELA Computer Club](https://nela.computer): identity,
Signal or email verification, community agreement, notification preferences,
optional membership, and review.

Everything is simulated. No account, payment, message, or subscription is created.
Membership amounts are placeholders. Production authentication and membership
integration remain to be built.

Serve `dist/` with a static HTTP server, for example:

```sh
python3 -m http.server 4328 --bind 127.0.0.1 --directory dist
```

Open http://127.0.0.1:4328. Run the flow checks with Node 22 or later:

```sh
node --test tests/flow.test.mjs
```

Part of [Aesthetic Computer's NELA contributions](../../docs/nela-contributions.md).
