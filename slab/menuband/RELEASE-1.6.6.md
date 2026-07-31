# Menu Band 1.6.6 (166)

## Release gate

- [x] Import Neo's current percussion engine and trackpad-surface work.
- [x] Fix menu-bar tap focus surviving mouse-up on Blueberry.
- [x] Add velocity-, drum-, and polyphony-aware melodic sidechain ducking.
- [x] Add a 1.6-second idle hold before the percussion overlay fades.
- [x] Add a recorded program-79 whistle + dense-kit mix test.
- [x] Verify the first take: 8.50 s, -11.8 LUFS-I, -1.0 dBTP.
- [x] Fix the held duck target and simultaneous-event meter labels found by that take.
- [ ] Re-render the mix test; confirm audible recovery between kicks and no limiter plateau.
- [ ] Test at least piano, whistle (program 79), bass, strings, brass, and synth-pad voices.

## Mac App Store blocker

The direct build receives full contact frames through private
`MultitouchSupport`; `MAC_APP_STORE` compiles that path out. Do not describe or
screenshot the percussion trackpad for the store until a public `NSTouch`
multi-contact path drives the same surface in the signed sandbox build.

- [ ] Add multi-contact callbacks to `TouchSensorView` / `LocalKeyCapture`.
- [ ] Drive skin, synth, kit, touch dots, and lift events through that public path.
- [ ] Keep the interaction focused-app-only in the sandbox build.
- [ ] Compile with `-DMAC_APP_STORE` and run the signed archive.
- [ ] Verify no Accessibility prompt and no private-framework linkage.

## Product and editorial

- [ ] Add a concise Percussion Trackpad section to the bundled Help/Tips book.
- [ ] Document focus, touch zones, Tab mode cycling, Shift FX, Escape, and automix.
- [ ] Update App Store release notes only after the sandbox gate passes.
- [ ] Update `STORE-APP-STORE.md` from the live 1.6.5 (165) state.
- [ ] Set `project.yml` and Fastlane defaults to 1.6.6 (166).

## Screenshots

- [ ] Capture the real signed UI; do not use the old mocked HTML as evidence.
- [ ] Add one percussion-pad frame with multiple contacts and readable zones.
- [ ] Add one whistle + dense-kit frame showing simultaneous keys and percussion.
- [ ] Replace the App Store screenshot set exactly once; verify unique checksums.
- [ ] Inspect every 2880x1800 frame full-size and at 10% scale.
- [ ] Confirm every caption names one visible claim and passes Wordcrust/Slidecop.

## Submission

- [ ] Run 62+ Swift tests and the regression suite.
- [ ] Run direct and `MAC_APP_STORE` compile checks.
- [ ] Archive, validate entitlements/resources, and test the exported app.
- [ ] Run `fastlane mac meta`, `shots`, `upload`, then `ship` in that order.
- [ ] Confirm build 166 and screenshot uniqueness through `bin/asc.mjs`.
- [ ] Submit from Poorslice only after all gates above are checked.
