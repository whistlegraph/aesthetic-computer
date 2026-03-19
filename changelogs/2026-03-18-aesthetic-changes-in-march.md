# Aesthetic Changes in March So Far

**status:** draft
**author:** @jeffrey
**for:** news.aesthetic.computer post
**research:** reports/2026-03-18-changeloggin-4-all.md

---

chat feels different now. if you've been in there recently you probably noticed — scrolling has this bouncy inertia to it. you drag, let go, it coasts and settles. handles have colored shadows. multi-line messages stack properly. it's small stuff but it changes the texture of being in the room together. 18,000+ messages in chat now across 2,800 handles and the space needed to feel better than it did.

the prompt — the first thing you see when you open ac — is more alive. it cycles through three feeds now: chat, clock-chat (laer-klokken timestamps), and content (kidlisp pieces, paintings, tapes). there's a mood of the day from the community. media previews drift with a slow zoom. when your hand stops moving it auto-selects whatever's under it. the prompt used to be a command line. now it's more like a bulletin board that breathes.

profiles show what you're doing right now. your painting count, your pieces, your kidlisp programs, your moods, your chats — all live. if someone visits your profile they can see what piece you're currently in, whether you're online, your last ping. it refreshes every 30 seconds. presence is real on ac now.

notepat keeps growing. the big thing this month: if you have a nuphy air60 he keyboard, every key is pressure-sensitive. hold harder, play louder. the trails on screen get brighter with your finger pressure. there's an echo slider for room reverb. wave type selection (sine, triangle, saw, square, noise). waveform visualization above the pads. mic sampling with pitch-shifted playback. the status bar shows your clock in LA time, wave type, octave, sample rate, volume, battery, wifi. notepat isn't a demo anymore — people play it as an instrument.

kidlisp keeps went live on tezos mainnet. 25 tokens minted so far, 8 owners, floor at 12 XTZ. the mint experience walks you through a timeline: fetch source, initialize, resolve dependencies, thumbnail, bundle, IPFS, metadata, review, sign, done. you watch your code become permanent. buy.kidlisp.com has a neon vegas thing going on — browse available keeps, see what's sold, preview the artifacts fullscreen. keeps.kidlisp.com got a redesign with unkept/kept tabs, a market tab pulling from objkt, and a wallet leaderboard showing keepers vs buyers.

squash is our first real multiplayer game. 2D platformer — you stomp on opponents to score. best of 3 rounds, first to 3 kills. it runs on a dual-channel setup: UDP for your position (fast, might drop a packet, doesn't matter), websocket for game events (joins, scores, round control — stuff that can't be lost). when you land a stomp, both screens burst particles at the same spot. you invite people from chat by typing 'squash. this pattern — UDP + websocket together — is documented now and ready for anyone building networked pieces.

android rendering works properly. if you were on a phone with a mali or adreno GPU and blur/sharpen/contrast looked broken — that's fixed. there's a CPU fallback mode now (mobileSafeMode) and the shaders got rewritten for those chipsets. kidlisp pieces render correctly on mobile.

the ac native os boots on real hardware now with wifi that auto-connects and remembers credentials, a terminal emulator, device login without needing a browser, and over-the-air updates through cloudflare. claude code runs natively on it. the status bar shows time, battery, wifi signal. background tint shifts with time of day. this is still early but it's a real computer you can hold.

we're heading to ars electronica 2026 with a piece called demoplay — an automated conductor that walks through ac's capabilities as a performance. the score has 6 movements: the prompt, the pieces (354 built-in), kidlisp ("a language a child can read"), the social layer, flow state, and a coda.

16,779 kidlisp programs. 4,429 paintings. 265 user-published pieces. 2,812 handles. 20 physical prints mailed. the numbers are small and the community is real.

full technical breakdown lives on the platter at papers.aesthetic.computer/platter.

— @jeffrey

---

## notes to self

- tone: conversational, first person, lowercase. not a press release.
- focus: what people touch and feel, not internal tooling
- the new generative pieces (splat, halley, morpho) are lab experiments — don't announce them as features
- don't list 884 commits, nobody cares about commit counts
- the papers/research stuff is for a different audience — mention ars but don't list paper titles
- keep the keeps section grounded in what the experience is, not contract addresses
- TODO: is the ars electronica framing right? check with actual submission
- TODO: should this link to specific pieces? (aesthetic.computer/squash, etc.)
- TODO: do i want to mention the agent memory system? probably not, it's internal
- TODO: read it out loud — does it sound like me talking?
