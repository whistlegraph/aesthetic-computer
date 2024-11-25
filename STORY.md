### Story

In 2020 during COVID I released the free No Paint software as a website and iOS
app[0].

When No Paint users began to reach out with a desire to submit custom pixel
stamps I would receive their PNGs and add their images to No Paint's Construct 3
[1] project build under mock social @handles to index them as part of the
`stamp` brush.

And when they expressed interest designing and publishing custom brushes, I
provided them with a minimal Construct template project that they would work in
and send back to me for integration.

No Paint had a wide variety of non-technical users who learned a lot about
computing through this process of contributing to and grokking a toy they
enjoyed. But my process for accepting contributions was manual and slow. I had
to add every asset manually to a statically built product which added hours or
days to the iteration time for users who wanted to see their work integrated
into the larger whole.

One of the reasons I was driven to building No Paint in Construct 3 was its
great cross-platform (mobile) browser support and the ability to work on my
project entirely in-browser and then export to native webview based apps or a
static build I could upload to the web server. 

In JavaScript courses and workshops for art and design students I often used a
combination basic HTML and CSS with the Canvas2D API, Processing via the p5.js
web editor[2], and Glitch [3] to achieve a fast iterative loop where student
work was always live so they could test on any device in the classroom by just
visiting a URL on that device.

During COVID I explored outlying technologies and for one assignment used Roblox
which offered beginner friendly 3D development and baked in multiplayer so we
could all experience the work together remotely. Even though Roblox had these
nice platform features, the engine and editor was not elegant like a basic HTML
file or Processing sketch and offered too many distractions to try it again.

Throughout COVID I noticed crossover with the No Paint userbase and my art and
design students. Non-technical No Paint users from around the world were
learning more about dynamic media, entering commands, and contributing to
software they loved, and my students wanted to be able to publish their own
software experiments from class to their families and friends without friction.

I realized that if I built my own system with sane mobile-first defaults; a
"creative coder" compatible JavaScript API I could teach in; networked
multiplayer; and a social handle system for keeping track of and automatically
integrating published media and software from users; then I could squash some of
these errands of my trade in one integrated system. This is why I started
writing Aesthetic Computer. 

---

[0] No Paint iOS Reviews: https://apps.apple.com/us/app/no-paint/id1107427275

[1] Construct 3 Engine: https://construct.net

[2] p5.js Web Editor: https://editor.p5js.org

[3] Glitch editor: https://glitch.com