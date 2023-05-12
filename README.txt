 🧩 AESTHETIC.COMPUTER 🧩

  A *new* computing environment that...

  (Technical Summary)
    - Works in updated web browsers on all major devices, including VR headsets! 
    - Runs "pieces" instead of "apps" or "programs".
    - Pieces are written in a proce55ing-inspired API that's fun to learn and fast
      to code in.
    - Screen defaults to an accessibly low resolution, great for learning
      and counting pixels with, but technically agnostic. (Choose your aesthetic!)
    - Batteries included with system tools and toys that emphasize creative activities
      with media content output.
      - Painting (line, smear, rect, @maya/tuesday, sparkle, [spray, pull])
      - Musicking (metronome, melody, tracker, microphone, bleep)
      - Sculpting (wand)
      - Writing (prompt, play)
      - Shooting (...)
      - Viewing & Listening (wg, ff, bgm)
    - Tools can be creatively strung together to form adaptive sequenced workflows
      for media creation, like Unix.

  (More Tech)
    - Threaded logic, audio, and rendering.
      - Low overhead hypervisor that runs one piece at a time.
        (similar to a disk based operating system)
      - Pieces can transition from one to another (instantly) without refreshing
        the page or losing context. 
      - Pieces can load remotely off any `https://` url so applications can be
        distributed over a network.
      - 2D/3D Software renderer for spatial display of programs as planes.
      - 3D WebGL renderer for VR.

😀 TODO 😆

🐞 Annoying Bugs & Regressions 🪱
  - [] S3 Developer Onboarding Sync Not Working
  - [] Alt / Meta shortcut needs to work for mac to reset pan.
  - [] Live reload: boot will not paint again if paint returns false in a contrived example
  - [] Video download `local` fix.
  - [] Favicon CORS fix (generate the icon / use a data uri)
  - [] Apple Pencil is no longer working with brushes.
  - [] Fix painting rotate / resize cropping issue.
    - [] Check this in Brave browser as well.
  - [] Fix `spray` multiplayer / socket messages not being sent regression.
  - [] Research the use of "requestProvider.js.map" and work it into `index.js`.
  - [] Rename $api.upload to $api.open? or $api.importFile? 
  + In Production
  - [🍽️] Fix Firefox AudioWorklet Initialization Bug `Error: Module resolve hook not set`
  - [] https://gist.github.com/lukaslihotzki/b50ccb61ff3a44b48fc4d5ed7e54303f
  - [] Zooming in on the page a lot will make the margin too large
       and squash the main display.
  - [] The back button does not work in the Instagram in-app browser.
  + Dev Only
  - [] Get ssh working in VS Code on windows via WSL: https://stackoverflow.com/questions/60150466/can-i-ssh-from-wsl-in-visual-studio-code
  - [] `npm run code` does not work offline, due to netlify relying on online conectivity.
  + Done
  - [x] Firefox: Fix blank screen on boot.
  - [x] Painting doesn't fully reset when resizing.
  - [x] Just using `box(x, y w, h, "*center")` does not work.
  - [x] `n` and `p` keyboard shortcut do not ignore history / break navigation
  - [x] Fix Freaky Flowers opensea regression.
  - [x] Chained paint functions do not respect inkrn.
  - [x] Check `wg` and `ff` and other disks including the prompt in the Instagram in-app browser!

🌟 Projects in Progress (PIP) 🌟

*** `bleep` presets ***
  - [] Omnichord-like bleep presets in certain keys.

*** `grocery` ***
  - [] Sampling keyboard with speech to text for grocery lists,
       with a button to cross off items, sendable from user to user.

*** General Painting / Graphics API ***
  - [❤️‍🔥] Examine "paste" not updating automatically in relation to the below.
  - [] Fix 'system.nopaint.bakeOnLeave` `page(screen)` anomaly. (Redo "write")
  - [] Abstract the `rect = null` / `shape = null` / "bake" calls better
      in all brushes.
  - [] change resize to painting
  - [] allow prefix colors for oval, shape, and line
  - [] make `coat` a new command
+ Done
  - [x] Add "paste" command to load a bitmap locally, with the option
        of scaling it down on import by `n` x.

*** Usernames and /p Original Posting ***
  - [] Build a painting listing page!.
  - [-] Limit resolution of paintings?
  + Later
  - [] `/p` painting feed.
  - [] Update / finalize auth0 login screen.
  + Done
  - [x] Get presigned download URLs working.
  - [x] Also rename so that "painting-${timestamp}.png" is the
        name of downloaded paintings remotely (to match local).
  - [x] Live email based file links do not work.
    - https://cloud.digitalocean.com/spaces/user.aesthetic.computer
    - https://aesthetic.computer/media/me@jas.life/painting/2023.5.03.13.38.15.png
    - https://aesthetic.computer/media/@jeffrey/painting/2023.5.03.13.38.15.png
  - [x] Rewrite `media.js` or use the cloudflare worker to
        produce a json list if possible.
  - [x] Rewrite clean URLs to individual media files on DO spaces using
       a Cloudflare worker and a call to a Netlify function.
  - [x] Associate handles with user data somehow...
    - [x] Associate S3 directory with user id and do some handle mapping.
  - [x] Turn the cursor into a spinner when the prompt is locked...
  - [x] Sanitize handle text entry of the input on both client and server.
    - [x] Remove any prepending "@"
    - [x] Only allow a-z0-9, underscores, and periods
    - [x] Set a max length of 16 characters.
  - [x] Greet a user by their handle!
  - [x] Give users the ability to add handles.

*** Bitmark Autonomy App Integration ***
  📇 Autonomy side
    + Now
    - [] Tap logo to open fullscreen webview to: `https://aesthetic.computer?dids=${key}`
      - [] Logo should have a hover state / tap state where the opacity dips.
      - [] The keyboard should float on top of (and NOT resize) the webview when
          opened.
      - [] The background of the webview / border margin around aesthetic.computer should be black.
    - [] Pass DIDs keys to aesthetic.computer on Autonomy "A" logo tap.
    - [] Function to call from JS to "close" the webview.
    - [] If a.c has media / is logged in then show a list in the gallery view. 
        (Make a listing page that pulls json from https://aesthetic.computer/media/@user/painting) and displays the images uploaded.
    - [] Single tap link from the "portfolio" view to fork / edit an already made painting / 
        pop open an aesthetic.computer URL. 
    + Next
    - [] Aggregator page of everyone's published work.
  🖌️️ aesthetic.computer side 
    + Now
    - [] Add custom MOTD greeting for Autonomy users when the open the prompt.
    - [] Authenticate user / create account automagically from DIDs key.
      - [] ❓ How to allow user to associate existing account or tie email address
          to acount?
    - [] Custom webview styling for native embed.
    + Next
    - [] Aggregator API / social features / turn-based painting.

*** Happy Hands (June 1st) 🖐️😃 ***
  😎 Now
    - [-] See `happy-hands-assembler` 
    + Done
    - [x] Put hand-tracking data in core loop?
    - [x] Test `@mediapipe` tracking integration on iOS.
      - [x] Get multiple refreshes working on iOS without losing tracking?
        - (Create new landMarker but dont reload libs?)
      - [x] Get geometry working properly across all systems.
      - [x] Get rotation working on iOS.
    - [x] https://mediapipe-studio.webapps.google.com/demo/hand_landmarker
         Performance comparison ^
      - [x] Also test original demo and hand_landmarker above on Firefox!
    - [x] Profile `@mediapipe` for memory leaks in the worker context.

  ➡️ Next
    - [] Plan next meeting w/ Tina. (6th)
    - [] Explore gesture recognition model: https://mediapipe-studio.webapps.google.com/demo/gesture_recognizer

  📓 Notes & Spec
    (All hands are defined by integer ids 0-n)
    - Creating and Publishing

    (pattern -> body) -> style -> face
      mesh creation

    1. `hhf` aka `happy-hand-factory` with an integer.
          - Tie us to working with a particular hand and jumps to step A by default.
      - Go to another step with: `hhf:body 800` 

    - Viewing and Using
    1. `hh` or `happy-hand` takes you to using a random hand from 0-n.
    2. `hh 800` will view a hand

    + Later
    3. `hh 100 200` will start a 2 person call where a link is given and
        guest 2 is 200. 
    4. `ht` or `hand-time` is just a `hh` alias?
    5. `ht 5` -> a room with 5 random hands?

  ✅ Done
    Thursday
    - [x] Got hand data working!
    - [x] Fix framebuffer initialization error from mediapipe loading delay. (@jeffrey)
    Wednesday
    - [x] Integrate `@mediapipe/hands` into the project. 
    Tuesday
    - [x] Draw and define the 2D mesh thingy inside `pattern`.
    Monday
    - [x] Define command set for pipeline.
    - [x] Create `hhf` piece. (hhf.mjs)
    - [x] Onboard @ttarigh to the repo. 
    - [x] Set on the client a global variable that determines what
          hand we are "on".
    - [x] Limit the count to 1024 or something like that. 
    - [x] Fail if no param.
    - [x] Find good name for this process...
    - [x] Creating `happy-hands-factory` with an alias `hhf`.

*** (w)ordfish ***
+ Later
- [] Create the last 50 ordfish once fees go down.
  - [] Use `paste` to bring in old ones.
- [] Re-automate the process, including upload to spaces... Perhaps
     putting it in the a.c api with some edge functions.
+ Done
- [x] Sign an `ordfish` for Casey.
- [x] Make an `ordfish` slideshow page and route the codes to
     `ordfish~CODE` for url sharing with a button link to ordinals.com.
- [x] Get collection page online.
- [x] Make bw drawings and inscribe them.
- [x] get `word` working again
- [x] make it so exporting of resized paintings works

*** "async message hub" ***
  - [] Pub/sub model.
  - [] Works over a network, across multiple a.c units.
  - [] Can be run locally or remotely.
  - [] The browser is the best runtime now.

*** Colors & Notes & Dictation ***
  - [📘] See teal book.
    Example:
      painting (128)
      black coat
      white oval
      white triangle
      black word fish 🧠 (use any word)
      black line
      download

*** iOS Native App ***
  - [] Figure out the best way to make a wrapper now.
  - [] Get stickers into a keyboard / live sticker creation with the drawing
       tools. (Research in GPT)
  - [] Also get it work on Android too. (Can use Browserstack for APK testing.)
  - [] Add notifications.
  - [] Submit to store with notifications enabled, usernames and posts.

*** ai brush ***
  - [] Write a query and get back brush code in response.
  - [] How can I hook this up easily?
  - [] Test RunwayML: https://app.runwayml.com/video-tools/teams/me841/ai-tools/image-to-image

*** Watchlist ***
  - [] https://www.youtube.com/watch?v=KrPsyr8Ig6M

*** Experiential Art ***
  - [] Answer `experiential-art.txt` questions for Wade / wild.xyz

*** `nopaint` and `nopaintAPI` should be merged ***
  - [] nopaint needs a general code merge / cleanup

*** ⚠️ Color Parser: parseColor ***
  - [-] Add RGB float support, range support for floats, and
       "?" support to ranges!
  + Done
  - [x] Add support for ? ? ? rgb parameters.
  - [x] Add support for both ? and ranges in alpha parameter.

*** CPU Resizable & Navigable 2D Painting ***
    - [🟡] Implement all other brushes:
    - [-] `shape`
    - [] `word`
    - [] `smear`
    - [] `camera`
    - [] `icon`
    - [] `sign`
    - [] `bits`
  - [] Measure performance.
      + Done
      - [x] `line`
      - [x] Add `color words` to `rect`.
      - [x] `oval`
      - [x] Work out a final API / new abstraction that makes sense for every 2D
          brush, taking into account a possible GPU renderer and eliminating
          duplicated logic. 
        - [x] Make `rect` a final API example
      - [x] `rect` and `oval` both can drop a frame...
  + Later
  - [] Refactor translation code to eventually add and store rotation, zoom,
       etc.
    - [] Add software zoom.
    - [] Add software rotation.
  + Done
  - [x] Test two finger drag on mobile.
  - [x] Add a reset pan prompt command or button.
  - [x] Why does holding alt continue the drawing / why is there a frame
        delay after pen?.drawing?   
        - [x] system.nopaint.present return data needs to be decoupled from
            rendering
  - [x] Keep `pan` position on reload?
  - [x] Add gestural `pan` support
    - [x] Hold down `space` key to pan.
    - [x] Add to nopaint act.
    - [x] Two finger touch to pan.
  - [x] Resizing the window should re-present the painting properly.
  - [x] Add support for painting to be a `fixed` resolution.
  - [x] Render a backdrop with 1px border showing the
          edge of the painting.
  - [x] All brush pixels need to be offset by the panned painting buffer
    while rendering.
  - [x] Use `bits` as a case study.
  - [x] Center the rendering of the painting.
  - [x] Add command for setting a constant resolution, which avoids
        any resize hooks on the painting. 
    - [x] Use "crop" brush or `resize` command for this?
  - [x] Make a minimal `bits` brush.

*** Ordinals Collection ***
  + Now
   - 📓 This collection should be fully analog.
        I should keep a list of sats somewhere in a notebook,
        then sign drawings and upload them.
  + Done
   - [x] Inscribe a test ordinal. 

*** Generic Image / Photo Uploader ***
  - Data entry process
   - [] 1. go to ac and type `pic`
   - [] 2. add from camera roll or take photo
   - [] 3. receive code upon upload
   - [] 4. write code on drawing/sticker for drawing
  - Viewing process
    - [] 1. visit ac and type `code` to view the image

*** `evolve` ***
  - [] Research image net APIs. 
    - Transform a painting from one state to another using ML.
      (https://chat.openai.com/chat/1c0e4f20-7cb5-441f-a7e4-7b77fb55135d)

*** Code Lookup / Analog <-> Digital Workflow ***
  - Data entry process
      1. go to ac and type `pic`
      2. add from camera roll or take photo
      3. receive code upon upload
      4. write code on drawing/sticker for drawing
  - Viewing process
      1. visit ac and type `code` to view the image

*** Error Log / `errors` ***
  - [] Log all mistyped commands with a score count and display them in order.
    - [] I could just use redis for this...
      - [] That could actually make it LIVE!

*** `paintings` command should grab a list of all paintings by a user sorted
                in order?
  - [] Deletable paintings.
  - [] Painting feed.

*** Tape & Tapes ***
  `tape`
  - [] Type `tape` and start an audio recording. 
  - [] Recorded segments work with `no` and are built up on the `prompt`.
    - [] `no` removes / cuts a segment of a tape
  - [] Tapes can be `upload`ed and `download`ed just like paintings.
    - [] They can be stored (on server) as audio segments or single recordings
         with timestamp data.
  - [] Tapes can be played back using `play` or `tape:play`.
  - [] `tape` has custom parameters to control effects.
  `tapes`
  - [] View a list of all of a user's tapes / manage tapes.
    - [] Possible to add to the current tape / embedded recorder inside of
         `tapes` ? (Alex's dream workflow...)

*** Album / Kit / Pack / Collection / Grouping / Account ***
  - A meta structure for organizing curated media types like `painting`
    or `tape` or `diary` or `whistlegraph` and playing them back in a
    consumable playlist like format.
  - [] Decide on a name. 
  - [] Implement commands for curating, something akin to TikTok's playlist
       feature where there is a 'current album' and you can add media
       to the current one upon visiting it via `paintings` or `tapes` etc.

*** Painting Viewier ***
  - [] Painting viewer page... what is the command?
  - [] `look` or `examine` or `view`

*** Prompt ***
  - [] Always show whether we are connected to the internet or not.

*** @rcrdlbl ***
  - [💛] Implement auth0 "default" / uncustomizable login styling.
  - [] moving $api calls around in `disk`
  - [] making some brushes
  - [] make `bleep` reset BPM to default on exiting
    - [] @digitpain make BPM settable in the leave function
  + Done
  - [x] IRL meetup
  - [x] auth0 login screen

*** Mood ***
  - [] One mood per day, assigned?
       Notify users via email or a notification somehow?
  - [] Select a mood to change the default color palette.
  - [] Prototype this work in prompt.
  - [] A scheme map:
       - [x] Dark / Light Moods (via prompt)
       - [] Primary or major colors.
  *** Launch ***
      - [] @mollysoda posts an instagram story of a finished camera pic with stuff drawn on 
      - [] there is a call to action where viewers can swipe up
      - [] once they swipe up there shoud be like a "title" screen

*** Sound Effects Support ***
  - [] Load and play a sample from assets when a button is pressed inside a `sample` piece.
    - [] Check the `bgm` implementation in `bios`.

*** GPU Based 2D Painting ***
  - [] Make a brush that only directly writes to the system.painting.
  - [] (Behind a flag.) Render the system.painting as a texture on the GPU, using 3D.js.
  - [] Keep the painting inside an abstraction that can calculate and send its transforms to the GPU (via Form?)
    - [] Basically I just need an orthographic camera with a quad... but I also wanna do the auto-tiling which means
         transforming texture coordinates on a full-screen quad with texture repeat instead of the quad itself.
    - [] And then this whole thing could be rotated in 3D, with bumpmapping, etc / have shaders applied.
    - [] Brushes can choose to opt-in to this 3D display or stick with the less navigable 2D display.

*** Graphics Optimizations & Performance ***
  - [] Fix screen-squash zoom bug.
  - [] Make canvas UI cursor faster.
  - [] Remove smoothing from line / come up with an alternative thickness.

*** Ranges & Color Parameters ***
  - [🟡] Make a helper to parse color parameters.
    - [] Abstract the randomization of ranged integers across
        every brush with integer params.
    - [] How does this work with '?' ?
    - [] Add named colors / support all HTML compatible color names?

*** Fig Brush ***
  - [] Make a pill-like stick figure that can be placed on the surface.

*** New Prompt QOL Updates ***
  - [] Do history stack different. (Smaller text)
  - [] Skip what aliases lead to in the history list. (ff example)
  - [] Should be able to `no` a `no!` in other words `no!` should just
  + Done
  - [x] Don't clear text automatically on sending input. 
       be a wipe!

*** '?' Params Generalization ***
   - [] Colors (see `line`)
   - [] Colon param "?" support.
  + Done
   - [x] Refreshing the page on question mark params should work...

*** Ordinals Runtime ***
  - [] Add pixel buffer support.
  - [] Generate something cute out of only boxes.
    - [] Add a simple bresenham line function w/ lineh from graph.
    - [] Use it to draw text.
  - [] How to write text in the corner... (Embed all the glyph data?)
  - [x] Prototype a dynamic micro runtime of the piece API.

*** What is `fhell` ? ***
  - [] I don't know yet...

*** VS Code Extension ***
  - [] Make VSCode extension / back-seat document API (maya).
    - [🟡] Modify disk load to accept new source code. 
    + Done
    - [x] Wire up extension to a backend route that pubs to a redis channel. 
    - [x] Get icon made for extension (@ingo) 
    - [x] Start work on VSCode extension. 
  - [😀] Passwordless login.
  - [] Home Screen / Better Nav
    - [] Upon leaving the prompt, a tappable ghost of the active
        command (with generated params) appears in the top left corner.
        - [] Tap the word to return to the prompt and leave the command as is.
        - [] Swipe the word in any direction to erase the command
              and return to the prompt. 
  - [] User Accounts
    - [👨‍🦰] How quickly could I tie this to user accounts and make it permanent
            rn? Could I do this all today?
      - [-] Set up passwordless email authorization.
      - [] `name username` will set a user's name to whatever they like if there
          is no existing conflict
        - [] Require confirmation from user via a button press or tap.
  - [] ✏️ Diary
    - [] A simple diary tool / "performative poetry" piece. 
      - [] Write entries which playback in realtime to show the typing, like recei-ving a DM. See:  
    - [] Typing `diary` makes a new entry in your default diary.
    - [] Every entry of every user has a URL and a timestamp.
    - [] Every diary of every user has a URL.
    - [] A user can make multiple labeled diaries via `diary secrets` 
      - [] This will start a new entry in a labeled diary and make that diary
          if it doesn't already exist.
    🔐 Privacy
      - [] Private diaries just show a lock with a shortcut to log the user in.
      - [] Diaries can be marked `private` or public.
      - [] Entries can be moved from one diary to another. (Migrated or made private or public)
      - [] `@user diary` should read the most recent entry by default. 
      - [] Entering an entry into a private diary should be encrypted and 
          unreadable by aesthetic.computer.
          - [] Moving an entry into a private diary will encrypt it.
          - [] aesthetic.computer will not track public diary entries were made
                private, and not cache the moved files, to maintain user privacy.
          - [] A blockchain / wallet might be required for this...
          - [] A user could potentially sell "keys" to their diary, so that
                diaries can be semi-public / exclusive to a set number of
                readers.
  - [] Add ability to make a system-wide recording?
  - [] 🎨 Paintings
    - [] Finish all Brushes todos: https://www.notion.so/aesthetic-computer/aesthetic-computer-hq-97376f353dbd4503970910777d6c7241
      - [] Add DALLE2 / Image2Image API for generating / transforming stuff easily.
    - [] Get smart contract launched. See "Mintable Paintings" below.
  - [] Finish Freaky Flowers client
    - [] Hook up multiplayer presence. 
    - [] Get nice UI

*** lorecore ***
  - [] ...

*** Dev ***
  - [] Switch from prettier to something else...
    - [] Standard...
    - [] ESLint...
    - [] Do research?

*** Sotce ***
  - [] Visit a url. 
  - [] Logging in.
  - [] Type in words and press enter.
  - [] Hit a done button to play back the words.
  - [] Get back resulting URL
  - [] Share that URL / Screen Record / Still Image

*** Ingo ***
  - [] Jeffrey sets up VScode plugin!
  - [] `bst` (Bouncy Square Pattern World)
    - [] Jeffrey will explore: Sketchmachine.net style auto-expanding blingee buffer.
  - [] *** Ingo + Harrison coder cohort micro projects. *** 
    - [] Ingo learns API etc along with Harrison.
  - [] *** Dollmath ***
    - [] Character creator software combined with character operation software.
    - [] From premade assets with a protocol to remote control.
      - [] "Dollmath" refers to a notational system for defining / understanding
          the structure of each character given number of frames per each part.
      - [] Useful for avatar type, useful for fun puppet / finger puppet videos...
          VTuber style calling, etc.
  - [] Re: Avatars: "Dropped" or "Rare" or discoverable parts, similar to early Ultima Online PVP / permadeath.
  - [] *dolls* video recorder - larger project w/ harrison

*** Vector / Raster Foreground / Frame based Instrument ***
  + Now
    - [] RGB Black line tool (with thickness).
    - [] RGBA Fill bucket.
    - [] Frame is transparent.
    - [] Safe zones.
  + Later
    - [] Arrow keys to jump between states. 
    - [] Hold to pause

*** Hole / Fill Tool ***
  - [] Hole / fill tool  
    Goop Bomber
    - Random hole appears on the painting.
    - Little creature comes out with mouth ma ma ma ma.
    - Random bomb appears elsewhere on the canvas.
    - Bomberman style fuse.
    - Bomb makes a splatter.
    - Creature reacts.
    - Creature returns.

*** (Sage Notes) Particle / GOL ***
  - [] Color rule correlation
   - [] Equation in red so when things collide the color gets red.
   - [] Dominant force.
   16 particles
     {x, y, z}
   each particle can emit a sound
   override a particle position

*** "pose" - Hand-Tracked Body Pose Piece ***
  - [] Handtracked WebXR.
  - [] ThreeJS heirarchy of manipulatable.
    - [] https://github.com/Physicslibrary/Threejs-VR-Hand-Input
  - [] Instantiate multiple figures.
  - [] Physics Simulation
    - [] A. Mujoco compiled to WASM.
      - [] Wouldn't have to change much, somebody may have already compiled to WASM.
    - [] B. Or ammo.js which gets us networking. 
      - [] https://rawcdn.githack.com/kripken/ammo.js/99d0ec0b1e26d7ccc13e013caba8e8a5c98d953b/examples/webgl_demo_softbody_rope/index.html
      - [] https://lo-th.github.io/phy/index.html#ragdoll

*** Collaborations / Externally Hosted Pieces ***
  - [] Should subdomains like `m2w2.whistlegraph.com` just rewrite their url by
       default? Or maybe if you jump to the prompt? I guess it kind of
       depends on how localStorage or other origin specific features work...

*** BGM ***
 - [] Make a cool default visualizer for bgm.
  - [] Use phase analysis / both stereo tracks to produce a form.
    (Check existing chatGPT thread to explore traditional methods)
  - [] Make it a 3d representation in the rasterizer with CamDoll controls.
  - [] Implement spatialization of the audio in relation to the position of the
       viewer.
 - [] Global volume / play + pause controls too?
 - [] READ Audio properties (Will do for bgm)
   - [] Amplitude within frequency range function. (Cropped amplitude)
   - [] Time position (formattable as 00:00:00)
   - [x] Global amplitude read out.
 - FILTER Audio properties (Will do later)
   - [] High pass
   - [] Low pass
   - [] Panning
   - [] Speed
   - [] Spatialization
 + Done
  - [x] Add ability to stop bgm from playing.
  - [x] Add vf music to a bucket or a server / make the bgm piece to load a track.
  - [x] This piece should have a common library that any piece can opt into in boot via a music() or track() function. 
  - [x] Play it back via an audio tag in the bios audio-graph.

*** Important 3D Details ***
  - [] Invert Y position in original constructor.
  - [] vec3.dist from glMatrix yields innacurate numbers. Factor it out of the project. 
  - [] The quaternions also don't seem to work?

*** Painting ***
 - [] Add a 'mask' command to crop an existing painting to a certain border or
      aspect ratio?

*** Furthering VR Support ***
  - [] Try to re-enable workers again for VR. (Should basically work great now. 22.11.23.09.20)
  - [] Add grip of controller in addition to hand support.
    - [] Grip: https://threejs.org/docs/#api/en/renderers/webxr/WebXRManager.getControllerGrip
    - [] "hand" event

*** GIF Support ***
  - [] Add keyboard shortcut to download a still frame of whatever you're 
      looking at, (a 100% accurate screenshot in the presented resolution).
      - [] Get working with normal buffer.
      - [] Get working with glaze and mouse.
      - [] Have mouse be optional.
  - [] Add ability to store frames as gif frames and end a recording.
  - [] This can use ffmpeg behind the scenes in (almost) the same way I'm doing now...?
  - [] Just the software renderer for now? Or should I composite?
  - [] https://macr.ae/article/canvas-to-gif

*** Mintable Paintings (Screen Buffers) ***
  - [] 🅰️ Client
    - [] Implement `connect` command in prompt. 
    - [] Add ethereum based client identity authentication.
      - [] Include web3.min as a dependency.
      - [] Then login via... 
          const accounts = await window.ethereum.request({
            method: "eth_requestAccounts",
          });
          const account = accounts[0];
      + Later
      - [] Shop around a bit.
      - [] Examine token gate code on glitch: https://glitch.com/edit/#!/long-whistlegraph?path=public%2Fscript.js%3A14%3A23
    - [] Write more clientside code to trigger the mint. 
    - [] The data needs to be uploaded to the server and a signature
         for minting needs to be signed (payment processor).
    - [] Then a success or failure message needs to be shown to the user via the
         prompt.
    - [] The prompt should have some kind of progress bar...
    - [] Show title and description in the waller signature message so the
         minter can confirm they did not make a mistake!
  - [] 🅱️ Server
    - [] Upload media to S3
      - [] Make a special S3 bucket for mints.
    - [] Upload media to IPFS (Should preview links be scaled up?)
      - [] Pinata: https://docs.pinata.cloud/pinata-api/authentication
      - [] Mix pinata with server upload calls.
      - [] Set up ipfs.aesthetic.computer gateway.
    - [] Create JSON record somewhere... in a noSQL database hosted by Digital Ocean?
      - [] Metadata:
          {
            name: `#${id}To be set by `painting:mint insert-message-here`",
            description: "A default for all paintings?", // Or should I ask the user.
            external_url: `https://aesthetic.computer/paintings/${id}`,
            animation_url: ...,
            image: "ipfs://",
            attributes: [
              {
                "trait_type": "Artist", // or Creator? Research this...
                "value": creator
              },
              {
                "trait_type": "Date" // See also: https://docs.opensea.io/docs/metadata-standards
                "display_type": "date", 
                "value": 1546360800 (Must be a unix timestamp in seconds.)
              }
            ] 
          }
  - [] 🆑 Contract
    - [] Implement contract features: https://docs.openzeppelin.com/contracts/4.x/wizard
    - [] Open contract, mintable by everyone. 
    - [] Unlimited edition.
    - [] Charge a mint fee which goes back to the aesthetic.computer wallet and
         prevents spam and promotes quality.
    - [] Should I freeze the Metadata? https://docs.opensea.io/docs/metadata-standards#freezing-metadata
    - [] Add contract Metadata too: https://docs.opensea.io/docs/contract-level-metadata
    - [] Have a function that allows me to update the mint fee. 
    - [] Make a special wallet for aesthetic.computer.
    - [] Deploy contract!
    - [] Add media and metadata to OpenSea.
  - [] #️⃣ Release!
    - [] Make an actually good drawing.
    - [] What extra features do I need?
      - [] Ability to say NO.
      - [] Lazy line brush using pixel perfect algo for rendering.
      - [] Filled circle and soft circle brush.
      - [] Clear / fill command.
      - [] A palette the first digitpain "styles".
      - [] Add ability to add text to the piece. 
        - `text insert-text-here size`
      - [] Side mission: Add size modifier to dl.
    - [] Mint it.
    - [] Use it as an avatar on @digitpain twitter. 
    - [] Point to @aesthetic_cpu in a tweet to launch the Twitter account.

***No Paint System***
  + Now
  - [] Center painting with the transparent backbuffer if a specific
       size is set (which would turn resize off). (Make crop tool to do this, that can be NOd but automatically runs through.
  - [] The default "mint" command in aesthetic.computer saves
       the system wallpaper. Uniqueness is guaranteed? - https://www.google.com/search?q=sha-512+uniqueness&oq=sha-512+uniqueness&aqs=chrome..69i57j0i13i512j0i390l4.4876j0j4&sourceid=chrome&ie=UTF-8
    - [] Fix weird negative space / non-drawing issues on startup.
  - [] Add "no" command to the prompt.
    - [] Store two history states?
  - [] Enter for paint and ESC for no.
    - [] Prevent "paint" from occuring when pressing Escape.
         (Back button still defaults to paint)
         - [] Esc: Autoflash red on loading prompt or on leave.
         - [] Return: Autoflash green on loading prompt or on leave.
  - [] There seems to be a backbuffer / transparency error
       that's easy to find on mobile iOS but hard to 
       reproduce otherwise.
  - [] Add "sign" command which writes a timestamp with a signature.
       Should "sign" and mint be the same thing?
  - [] Add "mint" command to trigger a wallet signature request
       and produce a work.
       * Should be able to specify a contract here.
  + Later
  - [] How can the "nopaint" system be multiplayer and turn based?
    - [] What if turns could be negotiated?
  - [] Add `nopaint` template for Sage, Casey and Niki.
    - [x] Sage
  + Done
  - [x] Set up a glitch account / editing situation for Sage.
  - [x] Write `rect` tool, which necessitates an extra buffer.
  - [x️] Write boilerplate for painting tool.
  - [x] Make a glaze appear instantly after it loads the first time?
  - [x] The painting resolution should auto-expand, but not contract.
  - [x] Jumping from prompt to a nopaint brush removes the gap.
  - [x] Cache the current painting to indexedDB.
  - [x] Either use the added `umd.js` in dep or what's below:
          See also: https://www.npmjs.com/package/idb#installation
                    https://cdn.jsdelivr.net/npm/idb@7/+esm
  - [x] Add "dl" or "download" command to the prompt.
    - [x] Test this inside of an iOS in-app browser.
    - [x] Shouldn't have to leave the prompt to
         save the image.

*** Piece Caching ***
  - [] Cache already remotely loaded pieces in a given session,
       so they can be reloaded without having to re-download them or show a spinner.
  - [] Disable this in debug / development mode.

*** MUD ***
  - [] Make a multi-user dungeon chat / room system, designed for single session play.
   * welkum 2 mud *
   & there r 32 playrz ONLINE

***Firefox Day***
  - [] The main cursor is not visible...
  + Done
  - [x] 3dline loading
  - [x] Smooth out everything.

*** Developers ***
  - [] Move `maya` from glitch to ssh.
   - [] Document each step so that I can script adding new users in the future.
  - [] Figure out how to best automate the addition of a new developer... 
  - [] Why isn't artur's ssh key working?

*** New Repo / Asset Management ***
  - [x] Hook up whistlegraph assets to assets.aesthetic.computer
       and place behind a dev flag for local dev.
  - [x] Deploy to production in a new Netlify site that copies over all
       the environment flags and doesn't enable Large Media!
  - [x] Any "font" includes should work the same way... this could be tricky,
       also not pressing.
  + Later
  - [] Digitpain assets are also affected, to their references would
       need to be rewritten in the future.

*** i ***
 - [] Make `i` 2D multiplayer with UDP. 

*** Ambient Presence ***
  - [] No matter what disk you're on... queue into the ambient presence of others. You should be able to meet others.

*** Palette ***
  - [] Create a basic palette that enables one finger drawing. 
  - [] Add tools to palette that utilize existing toolset.
  - [] Add save button to palette. (Fix `tracker` buttions?)
  - [] Add "sign" and "mint" to palette as well.

*** p1xelfool additions ***
  - [] Complete TODOS @p1xelgool/blank. 
  - [x] Add ability to directly manipulate the buffer again. (Via `edit`)

*** (`turtle` / `spider`) ***
  - [] Simplify and generalize the Spider out of Wand.
  - [x] Make a spider that can crawl around in 3D and draw.
       (Use a turtle graphics API)

*** Communication ***
  - [] Add text chat.
    - [] Implement for both computer, phone, and headset.
  - [] Add voice chat.
    - [] Implement for both computer, phone, and headset.
    - [] Use https://docs.daily.co for now.

*** Workers ***
 - [] Reconsider where the piece worker boundary is now.
      (It might be just a bad design.)
      (Or it's a great design and just works poorly with the WebXR animation loop.)

*** Physics ***
 - [] Add jumping / movement speed.
      https://sites.google.com/site/zdrytchx/how-to/strafe-jumping-physics-the-real-mathematics

*** Camera Pieces ***
  - [] Pitch shifting / screaming / voice modulation recorder.
  - [] Audio and video recording test.
  - [] Ambient color suport for networked / multi-monitor setups / desktop.
       (Also works with foveated / peripheral rendering)

*** Tongue ***
  - [] Make a tongue tracking game where you catch snowflakes.
  
*** Pieces in Pieces ***
  - [] Bring `bleep` into 3D.
  - [] Make a layout of more than one piece in the same buffer.
  - [] Like bleep and bubble and line... maybe even melody?
  - [] Could melody just be imported as the background audio?
  - [x] Make a contrived wrapper example that imports one piece into another.

*** Slider ***
  - [] How to tie a slider on your phone to a value in a param.
      1. On a device, type `slider 0-255` or just `slider` for the default. 
      2. The slider will give you a special code. 
      3. Use that code while entering parameters like `line CODE 255 255`
          (Pieces do not accept remote controlled parameters fail to load.) 
          - The piece needs to send a request to the server to join
            the slider's room.
      4. The slider can message the piece with a new value. 
      5. The piece can return other data, like color, for the slider.

***Walkie Talkie***
  - [] Maybe recordings / videos can be sent back and forth between two parties?
  - Research: https://capacitorjs.com
  - And/or continue to use my own shim?

*** Help / Learn / Doc ***
  - [] ! What about just adding a "?" at the end of any command?
  - [] Decide on a command to read howto.
  - [] `howto line`
  - [] `doc line`

*** Graphics API ***
  - [] Add better hex support to color via: https://stackoverflow.com/a/53936623/8146077

*** Autopilot / ap ***
  - [] Consider how this could become the next nopaint?
  - [] Take out the test layers?
  - [] Or even how nopaint could be built on top of a remotely controlled ap?

*** 3D Optimization ***
  - [] Use imageBitmap for textures: https://developer.mozilla.org/en-US/docs/Web/API/ImageBitmap

***Cursors***
  - [] View current custom cursor css examples.
  - "Chromium cursor images are restricted to 128x128 pixels by default, but it is
    recommended to limit the cursor image size to 32x32 pixels" - https://developer.mozilla.org/en-US/docs/Web/CSS/cursor
  - [] Sidetrack: Hack in an SVG cursor really quick.
  - [] Replace Canvas 2D cursors with SVGs (or DIVs?).
  - [] Test to see what's the fastest these days? Check for scaling issues on linux (see Figma...)

*** New Compositor ***
  - [] Write a better (faster) compositor that squashes everything?
      Bottom up: 3DPIX, 2DPIX, Glaze, UI (dom), CURSOR (svg / css cursor)

*** WASM ***
  - [] Add wasm infrastructure speed up for blend function.
  - [] WASMify the `blend` function to see if it can be any faster.

*** Storage ***
  - [] Write much shorter, clearer console.logs with `📦 Store (method) message`
  - [] Add "remote:temporary" storage method. (Default for remote)
    - [] Add "remote:token" storage method. (Default for token-gated storage)
  - [x] Add indexedDB (local:db) storage method.

***Device Orientation***
  - [] Add experimental Gyro sustain control to bleep.
    - [] Sounds would need a "continuous" mode or a way to control sustain
         after firing.
  - [] Make a shaker using the average of all that data.
  - [] Make a spatial linear curve to fit the data based on some device orientation.

*** SSH (code.aesthetic.computer) ***
 + Later
   - [] Add ping-ponging back to socket server.
   - [] Only send reload messages to all clients if `export const reload = true`; ?
   - [] Start moving individual glitch accounts over to pieces.aesthetic.computer.
   - [] People should be able to mint / submit their pieces from here.
   - [] Parameterize "npm run reload-piece". 
  + Done
   - [x] Make live reloading better.
        (Catch piece errors... see if errors can be reported?)
   - [x] Deploy piece-server updates more easily.
   - [x] Put piece-server code into this repository.
   - [x] Make sure server can reboot and pm2 is re-enabled.
   - [x] Fix the @digitpain/hello no refresh bug.
   - [x] Enable live reloading from the SSH server somehow...
        (Run a node server from `digitpain` directory that watches
         for file changes and sends a message from that server to the main
         server which will send a socket message to all clients to reload the
         file if `export const reload = true;`
   - [x] Set up a developers / "pieces" SSH server.
   - [x] Prototype live editing with SSH and live share?

***Audio + Video Storage (Microphone)***
  - [] Check to see if ffmpeg encoding can be skipped if the browser doesn't need to do it.
  - [] FFMPEG does not work offline: https://unpkg.com/@ffmpeg/core@0.10.0/dist/ffmpeg-core.js - https://github.com/ffmpegwasm/ffmpeg.wasm#why-it-doesnt-work-in-my-local-environment

 ***Upload Server***
   [] 1. Show the media (video) in a div overlay with a "Retry" or "Done" button.
     A. Retry - Removes the modal div and lets you re-record again.
     B. Done - Allows you to download the file immediately (if possible),
               and uploads it, giving you the code no matter what.
                               (The code page layout could be shared here.)
  
   [] 2. After the file uploads...
     A. You can also have the option to "Post" or "Publish?" which
     will make the work available as part of an ever-growing collection.
       - Idea: Posts are automatically minted to our wallet, but can optionally
               be minted to a poster's wallet if they decide to connect.
     B. Regardless, the file stored at the code will be deleted in 24 hours.
       - [X] Setting expiration policies on DO spaces: https://www.howtogeek.com/devops/how-to-set-an-expiration-policy-on-digitalocean-spaces-buckets
       - [X] Follow along with – https://www.digitalocean.com/community/tutorials/how-to-upload-a-file-to-object-storage-with-node-js
                              - Using multipart-form-data
            Using             - https://www.npmjs.com/package/fastify-multer
            Get code via      - https://glitch.com/edit/#!/nopaint-server?path=server.js
       - Inside of TikTok
         - Tell the user their video is ready and they can visit
           aesthetic.computer/CODE in a browser
           to download their video, which expires after 5 minutes.
  
           aesthetic.computer/microphone -> aesthetic.computer/dl.bn67gff5
           aesthetic.computer/get.dfcx4
  
           After recording in the microphone, you get this...
  
           aesthetic.computer/mic -> aesthetic.codes/bn67gff5
           ... or
           aesthetic.computer/mic -> codes.ac/bn67gff5
             (where codes.ac/bn67gff5 -> aesthetic.computer/codes.bn67gff5)
             (and the resource is stored at -> bin.aesthetic.computer/bn67gff5.ext)
                                               art.aesthetic.computer/...
  
           A pop-up that shows a QR code / a code to enter into aesthetic.codes.
           Upon visiting aesthetic.codes...
       - iOS or Android Mobile Browser or Desktop Browser
         - 1. Show a download button
       - in a native iOS app
         - let the user download the video immediately to their camera roll
       - Where do recordings / files get stored? What is the userflow...
         - Do they get stored at a special code, then users can
           go to the website / open another window and mint from the code
           or download it?
   - [] Look into categorizing uploaded objects with tags...
   https://docs.aws.amazon.com/AmazonS3/latest/userguide/object-tagging.html
   - [] And making use of user defined metadata in s3.
        These would be great for custom file properties likw
        what piece was used to make the art / object and what git commit
        of the project was used or something like that...
      
*** Pieces: Run&Gun ***
 - [] Make a basic character with multi-platform controls.
 - [] Make a ground for them to run on back and forth.
 - [] Make something dangerous that can easily kill them. 
 - [] Add a timer.
 - [] Use screen.save and screen.load functionality for scorekeeping.

*** Cool Debug Feature ***
  - [] Log / visualize all thread communicated messages.
  - [] Add it to the definition of send, on both sides.

*** MIDI / Analyzer Demo ***
  - [] Real-time pitch detection of current microphone.
  - [] Convert to midi notes and send out from the tab.

***Bleep***
  + Now
   - [] Support more wave types in `lib/speaker`.
     - [] Sine
     - [] Triangle
     - [x] Square
  + Later
   - [] Add support for playing samples.
   - [] Add the ability to play sound from other top level functions.
  + Complete
   - [x] Test multi-touch support on a phone.
   - [x] Add multi-touch support through bleep, via `pen`.
     - [x] Add support to `bleep`.
       - ui.mjs:69, bleep.mjs:162
       - Sidenote: https://marketplace.visualstudio.com/items?itemName=alefragnani.Bookmarks
     - [x] Write a `multipen` implementation example with a simple API.
   - [x] Why does the board generate twice on first load.
   - [x] Support roll-over after touch.
   - [x] Hide cursor on finger action, but show it during mouse action.
   - [x] Resize window support.
   - [x] Make a basic bleep box that makes a tone when you tap on it.
   - [x] Automatically generate bleep grids with a command line parameter for WxH.

***Don't Go Upstairs***
  + Plans
    - An interactive story, told in multi-piece parts: https://twitter.com/digitpain/status/1567690125743919104/photo/1
    - This should serve as a test for fast-transitioning / preloading and
      sequencing pieces.
    - Use old Vectrex animation software as inspiration for the editors.
  + Now
    - Make a sub-directory for the pieces, and adjust the prompt so that
      if no file exists in the subdirectory then it checks an `index` file.

 *** Sage's Brush (@sage/hello_line) ***
  + Now
   *** Refactor: Density / Gap / Resize ***
     - [x] Remove the frameCount hack in sage's code.
     - [x] Deprecate both gap and density. Bake gap into resize.

  + Later
   *** API: Glaze ***
     - [] Add ability to update glaze uniforms via
         `glaze.params({uniform1: value, uni2: [0, 1, 2]});`
           function paint() {
             // user draw a dirty box that needs gpu processing as a tile larger than
             // the original pixel drawing
             glaze.params({
               crop: dirtyBox,
               samples: 12 // this dirtyBox will keep being glazed over for 12 frames
               uniform1: 1,
               uniform2: 2,
             });
             return dirtyBox;
           } 
     - [] Add "kiln" or "bake" function to bake in a glaze at the end of a
          nopaint buffer change.
     - [x] Be able to turn glaze on inside of remotely hosted pieces.

  *** Pressure ***
    - [] Get pen and finger working together.
        (When finger is drawing and pen is touched... is it recognized as pen?)
    - [] Pen: Get a good mapping for Apple Pencil / normalize the data. 
    - [] Touch: Two finger pressure. (Second finger regulates it via Y axis.)
    - [] Mouse: Use scroll wheel for delta. 

*** Re-organize Repository *** 
  - [] Make a better entry point for this repository, consider how far the
       monorepo idea should extend and make better room for text files, such as
       splitting up README.txt with PROJECTS.txt, WIP.txt, and GRAVEYARD.txt.

*** Publishing ***
  - [] How to have a default or custom thumbnail... even though things are randomly routed?

*** Refactoring ***
  - [] Clean up the use of "aesthetic.computer/disks/" across the whole project.
  - [] Refactor `wg` to make better use of thumbnails... what should the new structure be?

*** Favicons ***  
  - [] How should I generate favicons for each page or treat favicons in general?
   - Maybe just design a favicon? Should each piece also be able to have an icon?
   - Maybe the system could use these icons if pieces defined them...

***Terminal***
  - [] Make `function terminal()` to render to a text mode layer.

***JtoA***
  - [] Use web speech recognition API to make a program so that Artur and I can 
      communicate more complex thoughts.
      - https://www.google.com/intl/en/chrome/demos/speech.html
  Note: It's only gonna work in Safari on iOS. <https://bugs.webkit.org/show_bug.cgi?id=225298> (And if Dictation is enabled.)

***Spinline***
  - [] Circle algorithm with transparency, has doubly drawn pixels.
  - [] example: ink(255, 100).circle(x, y, 128);
  - [] Circle needs 'pan' support.
  - [] Make the regular circle small, and positioned at the tip of the rotating line.
  - [] No exported boot function does not show the mouse cursor...
  - [] Click and drag selection bug needs to be fixed on iOS. (Chrome + Safari)
  - [] Improve "density" function.

***Developers***
  - [] Add developer console log / catch errors and show them to users
       directly on the screen. Maybe this should be available all the time,
       even in production?
  - [] Catch errors better in the modules, so they can be easily deciphered
       from console messages.
  - [] Get it working on: https://glitch.com/edit/#!/niki-aesthetic-computer
  - [] Make an `edit` or `new` command to open an editor from the main page.

***View-Source***
  - [] Pressing 'Ctrl + `' should open the source code. - Document this in console.log of bios.
  - [] Typing `src` plus the `piece` should jump you to the github source page and these should also be url addressable.
  - [] Each page should print out its own description in the console.
   - [] Then all descriptions of existing pieces can be written.
   - [] Why are the disks located there in the first place?

***Readme***
  - [] Directly load the GitHub readme in a scrollable content window.
  - [] How could this tie into publishing a blog or devlog?

***i***
  - [] See `disks/i.mjs` 

***Shortcuts / open a new URL ***
  - [] Figure out notation: ~user/piece, piece~param1, #notion
    - [] Should this only be for `prompt` or should it also route URLs?
  - [] Make `manual` and `hq` load the notion (separate places) in a new tab.
  - [] Should the manual be a super.so site using one of the chosen typefaces?

***Prompt Start***
  - [] What info goes on the starting prompt page?
       Given that it can change every 60 seconds.
       And anyone who shares the link will see a preview image.
  - [] Short poem? Weekly poem?
  - [] Make the prompt start message easily editable by Niki.
    What would it mean for this message to use the ***Upload Server*** and merit
    a text editor.

***Prompt Updates***
  - [] “search bar” any letter typed shows “ghost” letters of all possible options
  - [] Also make the commands case insensitive!

***Rooms***
  (WIP Notes)
  - Pieces can either be singleplayer or multiplayer, but rooms work everywhere!
  - All piece changes are global. People can only be in one piece at a time together.
  - Start a room with other people and their mouse cursors / fingers appear as pixels.
  - People can join rooms different devices simultaneously.

***Basic Server Work / Rooms / Synchronized Metronome***
  - [] How to get metronomes syncing across a network?
       aesthetic.computer/metronome.180.red
       aesthetic.computer/metronome_300.yellow
       aesthetic.computer/metronome-180.black
       aesthetic.computer/metronome~180.blue

  Option 1: Visit rooms.ac to get a roomID which can be added to a command.
  Option 2: Automatically get a room with every new tab.
            - Join another room using `room roomID`.
            - Entering an activity is part of the room.
            - There should be a basic "shout" program for rooms.

 ***Socket Server*** 
  - [] Fix server.aesthetic.computer... maybe just take it offline for now?

***World***
 - [] Make an interconnected series of pieces that players can walk between, and
      maintain a server connection throughout.
 - [] Players should be able to wave or point their arm.

***Developer / User Login***
- [] Change the contents of user.aesthetic.computer/login.mjs to  ...
   - This would prompt the author to edit a string in login.mjs to match, which
     would grant them a javascript web token?.
   - Look up netlify identity... https://docs.netlify.com/visitor-access/identity

***Thumbnail Longtail***
  - [] Get the external thumbnail server running on a VPS to see how fast it is
       compared to the netlify function.
  - Provision a machine that can actually execute `npx playwright install chrome`.
  - [] Optimize netlify function... switch to jpeg to see if it's any faster? 

***Image Viewer / Media Viewer**
  - [] Write a great image-viewer that implements the `rdp` piece.
  - [] Also implement `tumpin`, `ten-minute-painting`, and `basedballz`.
  - [] Add my small series of Tezos 1of1s as well: `emokidpix` and `1bits`.

***Model Viewer***
 - [] Implement <model-viewer> for my 3d glb files so that when I share
      them to Twitter they can be in 3D, and also so that they can be wrapped
      as aesthetic.computer pieces.
      - https://modelviewer.dev/examples/twitter/generator.html

🪟 Browser Quirks
  - [] Development server only: videos don't load in Firefox (tested on Windows). 2022.05.07.00.01
  - [] Make the cursor faster / match the speed of the native cursor.

***Transcribe the original Proce55ing typeface***

***Multi-player Drawing / Core Drawing***
  - [] Allow users to join / create rooms with codes (QR codes esp.)
  - [] Abstract `line` and `spline` into `nail`. See also: `gesture`.
  - [] Add sound to `nail`.
  - [] Use https://wicg.github.io/video-rvfc/ for video frame updates.

***No Paint***
  - [] Should the old nopaint.art be reachable from within aesthetic.computer
  - [] Should a new nopaint be built from scratch?

***Sidelight***
  - A light you can set the color off.
  - Make it flicker.
  - Have presets.
  - Very useful for making videos with an extra device!

❤️ Side Missions ❤️
  - [] Finish learning about Tezos, LIGO: https://academy.ligolang.org/pascal/chapter-types
  - [] Chalkboard / material simulation.
  - [] Try and get fullscreen on iOS / iPadOS again... maybe sideload the app shim?
  - [] Add `every("1s", () => {})` shortcut to the `sim` api. 
  - [] How to limit the number of Frame's in the start of a disk?
       Perhaps I could have a hidden meta-programming setup line at the top?
  - [] (disk.js:28) Make this a boot choice via the index.html file?
  - [] Fix skippy scale rendering of pixels on non-retina displays.
  - [] Bake `wrap` and `pixel` into the api. 2022.02.01.02.46 (JAS) [via sage.js]
  - [] in `tracker.js`: Recenter boxes on line 174.
  - [] Multiplayer board reset in `spray` and `server`.
  - [] Global recording of user actions (what about application actions) and
       audio+video into one file?
  - [] Prototype the dream I had about a system-wide, radial menu.
  - [] Write a file editor for something crucial in aesthetic.computer, like a
       TODO: Program, or produce media to store with GIT-LFS.
       See also: https://web.dev/file-system-access
       And: https://googlechromelabs.github.io/text-editor
  - [] Make a VSCode extension that opens an official aesthetic.computer pane?

🎃 SETUP 💾

`aesthetic.computer` is virtual computer environment / interface designed for
 creative exploration. development requires `nodejs`, an up-to-date web browser,
 and knowledge of javascript. if you're interested in learning how to contribute 
 visit https://discord.com/invite/aesthetic-computer and i'll help you out - jeffrey 2022.04.24.05.05

 Redis
  0. You may need to install `redis` to get the live reload working properly
     on local.
  1. You can use homebrew for this and a default install should be fine, so 
     long as `redis-cli` works.

Make sure `git` and is installed, (you can do that through `homebrew`) and then get set up for development:
  0. Also install `fnm` the node version manager. (and add environtment variables to your shell: https://github.com/Schniz/fnm#shell-setup)
  0.0a Open new terminal window and cd into aesthetic.computer directory install node version 
  1. Check `ssl-dev/readme.txt` to generate and add local SSL certificates.
  2. `npm install` from the project directory.
  3. `cd` into `session-server` and `npm install` as well.
  4. Get added to the Netlify project as a collaborator.
  5. `cd` into `system` and run `npx netlify login` and then `npx netlify link`
  6. Now from the `aesthetic computer` directory run `npm run code` and in another shell `npm run server:session`.
  7. Visit `https://localhost:8888` to view the running site! 
  8. If you need local copies of the site's rich media `assets` then ask Jeffrey
     and you can slurp them from S3 so you can run the below.

Static Assets
  0. Squadmates can use `npm run assets:sync` after configuring `aws-cli` with
     the aesthetic.computer storage keys provided by Jeffrey.



🧩 Making a new included piece.
- Run `npm run new-piece -- name-of-your-piece` 
- Then open the file in `system/public/aesthetic.computer/disks` and start working!

In production: Add a "#debug" hash to the end of your URL for more verbose output.

If developing: Add a "#nodebug" hash to the end of a URL for less output.

📖 This project originally began as two separate repositories with their own
commit history: `digitpain/system` and `digitpain/disks`.
