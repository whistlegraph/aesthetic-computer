# Whistlegraph

The iPhone app for making AC pieces with voice, sound, and typing. Product domain: `whistlegraph.app`. The practice and archive remain at `whistlegraph.org`.

From this directory, run `./run.sh device` to bundle, build, install, and open the app on a paired iPhone. Use `DEVICE=<identifier>` if more than one phone is paired. `./run.sh simulator` builds for a simulator; select one with `SIMULATOR=<identifier>`. XcodeGen generates `Whistlegraph.xcodeproj` from `project.yml`.

Typing uses AC's `compkey` sample and QWERTY pitch mapping. Enter sends the prompt, pasted line breaks become spaces, and the limit is 96 characters. Account settings control key and button sounds together.

## Update compatibility

Whistlegraph updates the existing Walkieware installation. Keep these persisted and deployed contracts until an explicit migration replaces them:

- `computer.aesthetic.walkieware`: iOS bundle identifier and Keychain service. Changing the bundle identifier installs a separate app and loses access to the existing container.
- `walkieware://app`: bundled WebView origin. Its local storage contains pieces, version histories, cloud identities, and recovery state.
- `walkieware-*` preferences, local-storage keys, generated source markers, and `Utterances/` recordings.
- `walkie` script-message handler, `walkieware*` JavaScript bridge, `WALKIE_*` fixture controls, and existing backend routes and schemas. These remain compatible with the deployed service and test tools.

The directory, native Swift types, Xcode targets/schemes, installed app name, permission text, and visible web shell use Whistlegraph. This refactor does not configure DNS or publish a website.

## Checks

From the repository root:

```sh
node apple/whistlegraph/bundle.mjs
node --test apple/whistlegraph/Tests/*.test.mjs
node apple/whistlegraph/Tests/native-bridge.test.cjs --native-shell
```

The browser check uses Puppeteer and Chrome with mock inference; it makes no model requests. Native UI tests live in the `WhistlegraphUITests` scheme.
