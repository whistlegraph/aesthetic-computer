# Weekly News From @jeffrey

**status:** draft
**author:** @jeffrey
**for:** news.aesthetic.computer post
**date:** week of march 24-30, 2026
**research:** `node at/news-cli.mjs commits --since "2026-03-24 20:00:00"`

---

another weekly one. this covers the week of march 24-30, 2026.

there were ~188 commits after the little march 23-24 note. the big story is consolidation: the self-hosted monolith is real now, the native os is getting much closer to "one image, many machines", and kidlisp / keeps infrastructure moved off a few brittle dependencies.

## lith is the real server now

the biggest architectural change this week is that aesthetic computer is no longer leaning on netlify as the main production shape. `lith` landed as a self-hosted monolith on its own droplet ([ed1f58a](https://github.com/whistlegraph/aesthetic-computer/commit/ed1f58a17)), then quickly picked up a proper `/media/*` handler ([949dd44](https://github.com/whistlegraph/aesthetic-computer/commit/949dd442e)), response caching, full ssl, and a deploy webhook ([2072099](https://github.com/whistlegraph/aesthetic-computer/commit/20720999b)).

after that, it started getting the things you actually need once a server is no longer hypothetical: caddy access logs and traffic summaries ([7e3385a](https://github.com/whistlegraph/aesthetic-computer/commit/7e3385a7f)), a function stats dashboard in silo ([76959d4](https://github.com/whistlegraph/aesthetic-computer/commit/76959d470)), graceful shutdown so restarts drain in-flight requests instead of clipping them ([0fac581](https://github.com/whistlegraph/aesthetic-computer/commit/0fac58149)), and push-to-main auto-deploy ([22d5987](https://github.com/whistlegraph/aesthetic-computer/commit/22d598764)).

then came the small-but-important routing and production fixes: kidlisp.com got its own dedicated handler instead of falling through the main spa ([ad5adca](https://github.com/whistlegraph/aesthetic-computer/commit/ad5adca5c)), notepat.com routing was corrected ([76680b2](https://github.com/whistlegraph/aesthetic-computer/commit/76680b2bc)), `/api/logo.png` got wired properly ([87a353e](https://github.com/whistlegraph/aesthetic-computer/commit/87a353e05)), version reporting started writing a real `.commit-ref` again ([17aff7b](https://github.com/whistlegraph/aesthetic-computer/commit/17aff7b05), [dadc13d](https://github.com/whistlegraph/aesthetic-computer/commit/dadc13dcd)), and today `news.aesthetic.computer` got its own lith route fix after I noticed a weekly post wasn't resolving ([06a4dba](https://github.com/whistlegraph/aesthetic-computer/commit/06a4dba04)).

it feels less like "a migration" now and more like "this is the server."

## one usb image for macs and thinkpads

on the native side, the project got much closer to a universal boot story. macbook pro 2011 support landed in earnest ([357d0fb](https://github.com/whistlegraph/aesthetic-computer/commit/357d0fb82)), along with hybrid intel mac boot media ([a3b14c8](https://github.com/whistlegraph/aesthetic-computer/commit/a3b14c8dd)), intel mac usb boot discovery fixes ([573b390](https://github.com/whistlegraph/aesthetic-computer/commit/573b390ba)), hfs+ mount handling in the flash helper ([c95f4c4](https://github.com/whistlegraph/aesthetic-computer/commit/c95f4c4a0)), and a chainloader path for old mac efi that can't swallow a huge kernel directly ([cd161aa](https://github.com/whistlegraph/aesthetic-computer/commit/cd161aa98)).

that work turned into a more ambitious result: one usb image that works on both macs and thinkpads ([1a212c3](https://github.com/whistlegraph/aesthetic-computer/commit/1a212c335)). systemd-boot is in the loop for mac efi now ([104bdeb](https://github.com/whistlegraph/aesthetic-computer/commit/104bdeb93)), the mac boot path gets a real splash screen instead of dead-looking blackness ([0d4e160](https://github.com/whistlegraph/aesthetic-computer/commit/0d4e160d6)), and ota uploads now ship a slim kernel + initramfs pair for more universal boot compatibility ([02f5262](https://github.com/whistlegraph/aesthetic-computer/commit/02f526286)).

there was also a long tail of "make the machine feel normal" work: bcm5974 trackpad support for old macbooks ([bdb51a9](https://github.com/whistlegraph/aesthetic-computer/commit/bdb51a97d)), cirrus logic audio support ([b3a16f4](https://github.com/whistlegraph/aesthetic-computer/commit/b3a16f4f9)), more realistic sample-rate negotiation and a safer 48kHz default ([87ca60a](https://github.com/whistlegraph/aesthetic-computer/commit/87ca60ac0), [0e6671f](https://github.com/whistlegraph/aesthetic-computer/commit/0e6671fb6)), cleaner boot output ([c13f47c](https://github.com/whistlegraph/aesthetic-computer/commit/c13f47ce5)), less shutdown text spam ([34dc863](https://github.com/whistlegraph/aesthetic-computer/commit/34dc8639e), [f9fdeda](https://github.com/whistlegraph/aesthetic-computer/commit/f9fdedac6), [ca12d74](https://github.com/whistlegraph/aesthetic-computer/commit/ca12d74c8)), and some practical quality-of-life fixes around trackpad deltas, mic capture, and preset wifi networks ([5249301](https://github.com/whistlegraph/aesthetic-computer/commit/5249301ba), [303b300](https://github.com/whistlegraph/aesthetic-computer/commit/303b3005c)).

the old intel macs stopped feeling like weird exceptions this week. that's a good sign.

## kidlisp and keeps grew some infrastructure

kidlisp is inching further into native execution. there's now a cl-native evaluator ([b0e4c9e](https://github.com/whistlegraph/aesthetic-computer/commit/b0e4c9efe)), a cl piece launcher so typing `notepat.lisp` boots the lisp version directly ([8f694ad](https://github.com/whistlegraph/aesthetic-computer/commit/8f694ad7a)), a config-driven main loop for selecting pieces ([37ffe4b](https://github.com/whistlegraph/aesthetic-computer/commit/37ffe4bc7)), and a unified build that embeds sbcl + swank in the native build path ([98ecb8b](https://github.com/whistlegraph/aesthetic-computer/commit/98ecb8bfa)). the list piece also started showing `.lisp` pieces in their own purple section ([646f760](https://github.com/whistlegraph/aesthetic-computer/commit/646f7600d)).

on the browser side, notebook kidlisp switched from url encoding to `postMessage` for passing programs around ([9ae49f9](https://github.com/whistlegraph/aesthetic-computer/commit/9ae49f95b)). that is one of those changes users may not notice directly, but it takes pressure off a bunch of annoying edge cases.

keeps got a more meaningful backend shift: the mint pipeline now uses a self-hosted ipfs node on lith instead of pinata ([edccefc](https://github.com/whistlegraph/aesthetic-computer/commit/edccefc78)). there are also fixes so the keep pipeline falls back to a thumbnail instead of failing hard ([aa1a800](https://github.com/whistlegraph/aesthetic-computer/commit/aa1a80097)), and keep.kidlisp.com got some auth / asset-path cleanup ([b7a7a02](https://github.com/whistlegraph/aesthetic-computer/commit/b7a7a028e), [9e92179](https://github.com/whistlegraph/aesthetic-computer/commit/9e92179e5), [1ed59f6](https://github.com/whistlegraph/aesthetic-computer/commit/1ed59f68e)).

also: kidlisp pieces now have a farcaster frame endpoint ([8b04520](https://github.com/whistlegraph/aesthetic-computer/commit/8b0452047)). that one feels small, but i like it. a kidlisp piece should be able to leak out into other systems and still feel alive.

## papers, atproto, and the research layer

the writing side kept expanding too. there's a new paper on ucla arts administration, "Two Departments, One Building" ([8116615](https://github.com/whistlegraph/aesthetic-computer/commit/8116615e2)), and then a whole cluster of follow-up work that broadened it, corrected dates, added governance context, expanded references, and shipped translations ([92c9b0c](https://github.com/whistlegraph/aesthetic-computer/commit/92c9b0c72), [ea155db](https://github.com/whistlegraph/aesthetic-computer/commit/ea155dbf8), [b394883](https://github.com/whistlegraph/aesthetic-computer/commit/b39488378), [8493fbd](https://github.com/whistlegraph/aesthetic-computer/commit/8493fbd17), [6b0244c](https://github.com/whistlegraph/aesthetic-computer/commit/6b0244c74)).

the papers site itself got wider and calmer ([31f0090](https://github.com/whistlegraph/aesthetic-computer/commit/31f0090f5)), gained better sorting and live build telemetry ([f36ef49](https://github.com/whistlegraph/aesthetic-computer/commit/f36ef49e8), [bdeaaec](https://github.com/whistlegraph/aesthetic-computer/commit/bdeaaec28)), and tightened up its cards / translation / table behavior in a bunch of places ([d07b9e5](https://github.com/whistlegraph/aesthetic-computer/commit/d07b9e587), [1b84829](https://github.com/whistlegraph/aesthetic-computer/commit/1b8482986), [fb347c1](https://github.com/whistlegraph/aesthetic-computer/commit/fb347c1c8), [67d0072](https://github.com/whistlegraph/aesthetic-computer/commit/67d0072ac)).

the atproto side stayed braided into that work. the "Handle Identity on the AT Protocol" paper picked up a section on running the pds on bare metal and connecting identity back to the os itself ([e5a31ff](https://github.com/whistlegraph/aesthetic-computer/commit/e5a31ff2e)). that connection feels increasingly central to the project: the machine, the account, the paper, and the published artifact are all starting to speak the same language.

---

- @jeffrey

---

## notes to self

- this one is more about consolidation than one flashy feature
- tone should stay plainspoken and concrete
- if i post it, maybe link out to `/os`, `keep.kidlisp.com`, and the papers site more directly next pass
