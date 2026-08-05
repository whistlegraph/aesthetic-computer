# Release records

`pop/releases/<slug>/release.json` is the public, canonical metadata for a
track. Rights identifiers, legal names, partner IDs, and local master paths can
be supplied by a gitignored `release.private.json` overlay.

`npm run pop:site` publishes a smaller web-safe projection to
`system/public/pop.aesthetic.computer/releases/`. It deliberately omits local
paths, overlay locations, hashes, packet URIs, and source-repository notes.

The first implementation target is DDEX ERN 4.3.2, Release Profiles 2.3.1,
Simple Audio Single. This is an evaluation lane until Aesthetic Computer has a
DDEX implementation licence, sender DPID, and a recipient-specific agreement.
DDEX transport is intentionally out of scope.

```sh
npm run pop:ddex -- check marimbaba
npm run pop:ddex -- ern marimbaba --draft
npm run pop:ddex -- ern marimbaba --private /path/to/release.private.json
```

Draft export creates a `TestMessage` with conspicuous placeholder identifiers.
Live export refuses to run while any required field or delivery asset is
missing. Generated packets go to the gitignored `pop/out/ddex/` directory.

Official references:

- ERN Part 1: https://ern.ddex.net/electronic-release-notification-message-suite-part-1-definitions-of-messages/
- ERN profiles: https://kb.ddex.net/implementing-each-standard/electronic-release-notification-message-suite-(ern)/ern-4-explained/ern-4-profiles/
- ERN structure: https://kb.ddex.net/implementing-each-standard/electronic-release-notification-message-suite-(ern)/ern-4-explained/ern-4-structure/
- DPID registry: https://kb.ddex.net/reference-material/dpid-registry/

DDEX schemas and documentation remain under DDEX's licences and are not
vendored here.
