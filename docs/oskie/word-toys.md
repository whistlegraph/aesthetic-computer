# Word games

Published under **@jeffrey** in the [Oskie gallery](https://aesthetic.computer/oskie/?browse=jeffrey).

| Game | Play |
| --- | --- |
| Letter Thief | [Catch missing letters](https://aesthetic.computer/oskie/e56a0e7f1a3e4442e8efa4b962a7b952) |
| Mouth Choir | [Echo a vowel melody](https://aesthetic.computer/oskie/8a2adc934c9112f96883bcd472230683) |
| Word Wobble | [Unscramble words](https://aesthetic.computer/oskie/5c908a5dffda5fb9a07549b20a1e2bef) |
| Alphabet Soup | [Fish for letters](https://aesthetic.computer/oskie/e6ad9403c63090fbe9b7daa425d161dc) |

Each is a standalone `.oskie` using the original `oskiewar-1` runtime. Source
is in `system/public/oskie/games/word-toys/`. The hosted copies live in private
Spaces storage and load through the normal Oskie publication API.

Keyboard and touch both work. Escape pauses; F2 toggles sound. Word Wobble
and Alphabet Soup have a **Settle** control to stop their moving tiles.
Mouth Choir shows every note visually and allows a free replay of the phrase.
Scores last for the current session.

From the repository root:

```sh
node system/tests/oskie/word-toys/build.mjs
node --test system/tests/oskie/word-toys/game.test.mjs
node system/tests/oskie/word-toys/browser-check.mjs
```

The build writes portable bundles into `/tmp/oskie-word-toys`. The browser
check uses installed Chrome and the local original engine, tests desktop keys
and phone-sized pointer controls, and saves screenshots under
`/tmp/oskie-word-toys/qa`. Pass a slug to check one game.
