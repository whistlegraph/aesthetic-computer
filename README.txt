🐚 Welcome to...

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

🐞 Annoying Bugs & Regressions
  + API
  - [] Fix `spray` multiplayer / socket messages not being sent regression.
  - [] Painting doesn't fully reset when resizing.
  - [] Research the use of "requestProvider.js.map" and work it into `index.js`.
  - [] Rename $api.upload to $api.open? or $api.importFile? 
  + In Production
  - [⏰] Firefox: Fix blank screen on boot.
  - [🍽️] Fix Firefox AudioWorklet Initialization Bug `Error: Module resolve hook not set`
    - [] https://gist.github.com/lukaslihotzki/b50ccb61ff3a44b48fc4d5ed7e54303f
  - [] Zooming in on the page a lot will make the margin too large
       and squash the main display.
  - [] The back button does not work in the Instagram in-app browser.
  + Dev Only
  - [] Get ssh working in VS Code on windows via WSL: https://stackoverflow.com/questions/60150466/can-i-ssh-from-wsl-in-visual-studio-code
  - [] `npm run code` does not work offline, due to netlify relying on online conectivity.
  + Done
  - [x] Chained paint functions do not respect inkrn.
  - [x] Check `wg` and `ff` and other disks including the prompt in the Instagram in-app browser!

🌟 Projects In Progress (PIP) 🌟

*** Freaky Flowers Launch ***
  - OpenSea Test URL: https://testnets.opensea.io/assets/goerli/0x4FEa5E2a8C3bfc742B43b48FC4F2eA9913452B99/0
  - Wild Test URL; https://testing--wildxyz-market-4231.netlify.app/jeffrey-scudder/freaky-flowers/0/

  - Freaky Flowers 
    - [😀] Show capFrom "preview" in demo mode.
    - [] Loading `instant` style should be chunked to avoid crashes
         on lower end hardware and also to enable "..." animation on "Reconstructing...".
    - [] Tap or two finger pinch to zoom and slow rotation.
      - [] Or do some kind of lazy wand follow / rotate camera to wand situation.
    - [] Let go to snap back. 
    - [] [WALK] or [WANDER] button that shows keyboard shortcut overlays /
         touch instructions. (Add chat here?)
    - [] Add background music if it isn't already playing.
    - [] Add arrows to top right to move between slides.
      - [] (Speed up loading)
    - [] Add glaze / lighting / mastering.
   - WebVR Support
     - [] Make wand render again.
     - [] Loading a ff piece prevents drawing and restarts the
          timelapse upon entering VR.
    - Media Export
      - [] Create a rotating GIF / WebP (that even hooks into video) generator.
  - Prompt 
    - [] Do history stack different. (Smaller text)
      - [] Skip what aliases lead to in the history list.
    - [] Add background music to prompt also.
  + Done
    - [x] Wand should be "thicker".
    - [x] Get loading working much nicer both in playback mode and
           in "instant" mode with a visual progress indicator of some kind. 
    - [x] Fetch progress bar.
      - [x] Don't show progress bar if things have already downloaded...
    - [x] Put yellow spinner on special little delay.
    - [x] Make intial boot fuzz a much lower resolution. 
    - [x] What isn't being auto-rotated?
        - [x] Capform
    - [x] Start spinning, at a high rate, (about 30seconds max timelapse) 
    - [x] Add ability to hide / customize hud text.
    - [x] Make sure iframe mockup works for `ff`: https://localhost:8888/sandboxed-iframe-mockup.html
    - All Pieces
      - [x] Every piece by default (unless it opts out) will show its name
          and parameters in the top left corner.
      - [x] Write readouts to separate "secret" buffer (see `HUD` in disk)
    - [x] Add "signature" / title flag feature to `dl` command.
      - `sign`
        - [x] Paste a signature with timestamp in the corner.
              that is NOT opaque and in the bottom left corner.
    - [x] Review spreadsheet metadata: https://docs.google.com/spreadsheets/d/10aeN4WyTQ1UQUT9-IUbKFdoeEfkumqM6_8mvFs5dVq8/edit#gid=1052521247 

*** What is `fhell` ? ***
  - [] I don't know yet...

*** Painting w/ Signature for Yeche Lange***
  - [🚗] Make two or three works for Yeche.
  + Done
  - `line`
    - [x] Add line thickness...
      - [x] Make pppline if thickness is 1
    - [x] Fix color inconsistencies among screen and painting. 
    - [x] Add ? ? ? options with ? expansion?
  - `no`
    - [x] Undo / redo a step.
    - [x] with multistep options

*** Better Logging ***
  - [x] Create a logs.mjs file that can enable / disable certain development log flags when working, such as disabling all "store" logs.

*** Project Order ***
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
    *** Molly Soda Moods ***
    - [] @mollysoda sequence
      - [x] Add support for camera brush.
      - [] Rename sparkle -> icon
      - [] Pull / Move
      - [-] Add glyphs to sparkle brush.
        - [-] Spin up sparkle brush prototype.
      - [] @mollysoda posts an instagram story of a finished camera pic with stuff drawn on 
        "Activity"
        {
        - [] Title Screen
          - [] Loose layout for (Instruction, Icon, Sparkle, Pull, Text, Save)
        - [] Camera 
        - [] Icon (preset palettes with preset graphics, similar to text changing words)
          - [] Rename sparkle to icon.
        + Tonight
        - [] "Sparkle" by Maya
          - [] Jeffrey asks Maya about doing sparkle.
        + Later
        - [] Text 
          - [] Have a bank of changing words
        - [] "Save" / "Download" / "Done"
          - [] Well... where do you put it?
        }
      - [] there is a call to action where viewers can swipe up
      - [] once they swipe up there shoud be like a "title" screen
    - [] Add DALLE2 / Image2Image API for generating / transforming stuff easily.
  - [] Get smart contract launched. See "Mintable Paintings" below.
 - [] Finish Freaky Flowers client
  - [] Hook up multiplayer presence. 
  - [] Get nice UI
 
 + Done
- [x] Prototype screen-based whistlegraph recorder tool.
  - [x] Add it to the API and make a `wgr` disk. 
  - [x] Set up doodads like file hosting.
 - [x] Write micropage for the whistlegraph album. (m2w2)
 - [x] Fix image download function.
 - [x] Get developer live reload working again.
  - [x] Deploy and test everything to see if it works. 
  - [x] Add a post request edge function called "reload". 
 - [x] Wire up plot for Amalia.
 - [x] Queue any sent socket messages to be sent once the socket
      backend connects. 
 - [x] Turn 'sb' -> 'play'... a *tiny* recordable chat program in X turns.
 - [x] Retrofit `prompt` with the new abstraction in `type.mjs`
 - [x] Clear any prompt specifics in bios.
 - [x] Fix major segfault regression.
 - [x] Abstract the text entry logic out of 'prompt'.
 - [x] 🔐 Setup user system! (Ethereum)
  - [x] Add user feedback for web3 connect.
  - [x] Basic web3 integration.
  - [x] Research auth methods.
 - [x] BGM Protoype
 - [x] Spin up session-server in sb.
 - [x] Add a database / hosted DB.
    - [x] Digital Ocean
    - [x] Redis
 - [x] Prototype BGM
 - [x] Spin up session-server in sb.
 - [x] Add a database / hosted DB.
    - [x] Digital Ocean
    - [x] Redis

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

*** Harrison Idea ***
- NO command.
  - no! - painting reset

(Vector / Raster Foreground / Frame based Instrument)
+ Now
  - [] RGB Black line tool (with thickness).
  - [] RGBA Fill bucket.
  - [] Frame is transparent.
  - [] Safe zones.
+ Later
  - [] Arrow keys to jump between states. 
  - [] Hold to pause

- Dolls - (Record Character)
- Character 1 (Left)
  - Blob
- Character 2 (Right)
  - Blob
- Single background for both.
- Third thing (Subject of dialogue).
- Duration that fits the vibe. 
- Vocal Effects?

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

***Live Reloading***
  - [] Fix live reloading for everyone / add some people to dev server.
  - [] Disable nginx cache? https://ubiq.co/tech-blog/disable-nginx-cache

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

*** (Wand) ***
 + Now
  - [] See `wand` todo section for pre-launch Freaky Flower items.
  - [] Add / design / consider better touch controls. [F] [B] [W/L]
  - [] Add circular buffer to wand lines (buffer-geometry) / infinite
      wand with dissolving trail? Spectator mode.
 + Later
  - [] Add audio to demo recording and playback.
  - [] Shrink three.js filesize: https://github.com/mrdoob/three.js/issues/19148
  - [] Fix keyboard hold and drag to symbol in Meta Quest software keyboard.
  - [] Add user connected messages.
  - [] Better mouse and keyboard controls.
  - [] Automatically reload the socket server?
    - [] Try and get web sockets working locally with SSL enabled (in general).
    - [] Try to get websockets server locally connectable in Meta Quest browser.
         https://github.com/nekocode/Websocket-Adb-Forward/blob/master/example-py-client/client.py
  - [] Does each player bring their lines with them?
       (This would make their presence very important.)
  - [] Add a "room" system.
    - [] Just start with one room but add an abstraction layer for it soon.
    - [] Save buffered lines via store["remote"] to the remote room.
         (Delete everything once all clients disconnect for a certain amount
          of time.)
      - [] Perhaps this can just use the system ram, then dump to S3 after
           clients disconnect?
  - [] How to hear someone else's audio stream? https://stackoverflow.com/a/61438244/8146077
  - [] Jiggle every two vertices the same amount so each segment
       drifts as one and maintains its integrity / add jiggle or break-apart
       as a toggle.
  - [] Add ability to delete lines / limit lines.
  - [] 🎖️ Pass the Whistlegraph accessibility/latency threshold.
    (Must work on a wide range of devices with various connections, and
     be fast enough or communicable enough to work.)
    - [] Sockets can always be a fallback.
    - [] Will require UDP over WebRTC.
    - [] Experiment with http/3 transport as well.
  - [] Draw in 2D draw with the mouse and project out the lines.
       - How to switch to an orthographic camera for this?
  - [] Free camera mode / better camera controls on keyboard and mouse.
  - [] Bring `microphone` amplitude into `3dline`. 
       (Draw a linear wave form spatially)
  - [] Bring in font rendering from the plotted system font.
  - [] Touch controls.
  - [] Add other tools other than line.
  - [] Add a 3d prompt so you can change tools / be more active without going 2d.
  - [] Updates on the 3D object pipeline.
    - [] Also be able to delete stuff from the GPU scene.
 + Done
  - [x] Line thickness.
    - [x] https://codepen.io/garciahurtado/pen/WNVjog?editors=0010
    - [x] Work on implementing: https://github.com/mrdoob/three.js/blob/master/examples/webgl_lines_fat.html
    - [x] Tube geometry?
    - [] Billboarded MeshLine?
  - [x] Add CSV unreal exporter that follows this pattern:
      ---,Vertices,Color
      NewRow,"((X=10.000000,Y=20.000000,Z=-30.000000))","((B=0,G=0,R=201,A=0))"
      NewRow_0,"((X=10.000000,Y=20.000000,Z=-30.000000))","((B=0,G=0,R=201,A=0))"
  - [x] Replace Gl.lines with something cooler.
  - [x] Clear console on new disk load. (Only in production.) 
  - [x] Make workers optional / turn them back on. (but not for Meta Quest Browser).
  - [x] Make buffers transferrable.
  - [x] Reduce the latency of the pen information.
  - [x] Retrieve the floor / head height information.
  - [x] Make a "wand" / stick to draw with that comes off the VR controller. 
  - [x] Draw a line in webXR.
  - [x] Better output for Unreal Engine.
  - [x] Android keyboard...
  - [x] Save the buffered lines to indexedDB... and/or to the remote room?
    - [x] Store and reload from indexedDB.
    - [x] Save and load with ram storage.
  - [🌞] Add WebXR session.
    - [x] Use these demos.
      - https://github.com/immersive-web/webxr-samples/blob/main/vr-barebones.html
      - https://github.com/mrdoob/three.js/blob/master/examples/webxr_vr_paint.html
      - https://github.com/mrdoob/three.js/blob/master/examples/js/misc/TubePainter.js
    - [x] Future specs: https://immersive-web.github.io/raw-camera-access
  - [no] Can the keyboard automatically open when returning back to the prompt? 
    - (If it's not the first disk.) (All mobile platforms)
  - [x] Don't kill the keyboard if the disk fails to load. (Meta Quest 2)
  - [x] Get 3dline working on Firefox too.
  - [x] Auto-expand buffer at the start of prompt.
  - [x] Quickly going back and forth between 3dline and itself / prompt
       will cause errors. 
  - [x] Export an array of camera data from user, in addition to,
       where each step / frame is not duplicated and has a time delay.
  - [x] Add websocket support for other players and their lines.
  - [x] Should `Race` lines be more quantized / regulated?
  - [x] Synchronization error on 144hz displays with keep: false.
    (Notes)
    - The sim rate is locked at 120fps so on a 144hz display, we are gonna be
      rendering multiple times per sim which means things like `keep: false`
      will fail on Forms that are defined in sim. 
    - Garbage cannot be collected after each render unless
      the next sim has occurred... or we'll see flickering.
    - And it cannot be collected after each sim, because the next render
      may need it and sometimes sims only run once per frame, always before
      a render.
    - We can know if a render is first or a repeat since each sim.
    - If a render is first, then we can collect any garbage from the last
      frame before rendering.
  - [x] Add final red tail preview to point on `lift`.
  - [x] Add color and transparency to line.
    - [x] Use default params to change the color on load.
  - [x] Clean up smooth sim code.
  - [x] Draw with left mouse button while looking with right. (via `pen`)
  - [x] Density field idea / jiggle lines.
  - [x] Add spline / smooth drawing support: https://threejs.org/docs/#api/en/extras/curves/CatmullRomCurve3
    - [x] Add smooth sim. 
    - [x] Tried and cancelled in favor of doing a lazy nezumi style physical sim in 3D.
        (Better to filter the data on input)
  - [x] Disable ThreeD when leaving a piece that loaded it.
  - [x] Fix the deallocator.
  - [x] Fix "tri's" turn update.
  - [x] Fix "lines" no longer rendering.
  - [x] Break line into separate marks.
  - [x] Draw a line from the 3d cursor in 3d space.
    - [x] Stream numbers to the GPU / don't send copies of vertices every frame?
      - [x] Add identifiers to each Form.
      - [x] Keep track of what identifiers got uploaded to the GPU so far
           after each form call.
      - [x] Only send new identifiers to the GPU.
  - [x] Clean up the whole code and optimize the API for readability.
  - [x] Make a good wasd / mouse fps camera.
    - [x] Refactor existing controls to look nicer.
    - [x] And basic touch-only controls.
    - [x] Also add keyboard-only controls.
  - [x] Add an X on the ground... and maybe a horizon?
  - [x] Add triangle clipping.
  - [x] Add line clipping.
  - [x] Optimize screenMatrix.
  - [x] Basic FPS Camera.
  - [x] Make it all way faster?
  - [x] Draw a bresenham line in 3d.

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


*** Sage Session TODOs ***
 - [] make sure color attributes work for QUADS.
 - [] flip the y on camera position initializer.
 - [] `Form` should scale and then rotate. See `Form.graph`.
 - [] `Form` should have a point / points option.
 - [] `point` primitive that is below forms. 
 - [] Make color vertices work in software3D.  

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

*** Native Apps ***

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

** Mood ***
 - [] Select a mood to change the default color palette.
 - [] Prototype this work in prompt.
 - [] A scheme map:
      - [x] Dark / Light Moods (via prompt)
      - [] Primary or major colors.

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

*** Image Support ***
  *** Save ***
    - [🔶] Make it so that desktop uses the current "save-file" event route,
          but mobile pops open a UI... 
        - Should this UI be shared on all platforms?
        Try web components here: https://developer.mozilla.org/en-US/docs/Web/Web_Components
        - What does it mean to "save" an image?
    - [x] Add API for system-wide saving of webp or png images (through nopaint).
    - [x] Test image saving on different platforms.
    - [x] Prototype image save function.
  *** Load ***
  - [x] From a URL into a buffer.
  - [x] and then be able to paste it.

***Audio + Video Storage (Microphone)***
 + Now
  - [🔶] Make sure that video encoding works on all platforms. 
      - [] Try implementing custom ffmpeg-core files
      : https://github.com/ffmpegwasm/ffmpeg.wasm/issues/299
  - [🌟] Add a "done" or "close" button to the recording UI / share sheet which
         re-activates the UI.
  - [] Add a special UI for uploading the file / receiving a code...
      so it's a choice?
  - [] Make the recording process re-entrant. (Don't reload ffmpeg each time?)  
  - [] Check to see if ffmpeg encoding can be skipped if the browser doesn't need to do it.
  - [] FFMPEG does not work offline: https://unpkg.com/@ffmpeg/core@0.10.0/dist/ffmpeg-core.js - https://github.com/ffmpegwasm/ffmpeg.wasm#why-it-doesnt-work-in-my-local-environment
  + Later
  - [] Add more pointerevents to make a better / final record button: https://developer.mozilla.org/en-US/docs/Web/API/Pointer_events
 + Done
  - [x] Add a progress bar / interstitial that runs while the file is encoding,
      preventing the user from being able to act.
      - [x] Test the interaction.
      - [x] Gray out the screen.
  - [x] Make microphone recording button a bit nicer.
  - [x] Add ffmpeg.wasm
  - [x] Fix microphone record button.
  - [x] Try to just upload the file directly here.
  - [x] Write a netlify serverless function and call it from the microphone
        piece... so the file can be uploaded.
    - [x] Netlify Serverless Hello World
    - https://www.netlify.com/blog/2021/07/29/how-to-process-multipart-form-data-with-a-netlify-function/

***2D Whistlegraph Recorder*** (August 1st - Launch)
 - [] Only record frames that are not part of the interface.
   - [🟢] Make a DOM based button.
     - [] Attach events to it... figure out the smartest way to have it connect to `function act`?
     - [] Should I use a ShadowDOM for this or is it overkill...
          - https://developer.mozilla.org/en-US/docs/Web/Web_Components/Using_shadow_DOM
   OR ...
   - [] Dump frames before painting the interface on top... via a pixel buffer
        being sent over the worker.
        (This might be slow)
        (Could be useful eventually... to selectively record video of certain pixel buffers, or render frame by frame.)
 - [] Record frames at a multiplied resolution over the original canvas.
 - [] Complete ***Upload Server***
 - [] Is there a way for a local install to still use codes? How would this be
      proxied and tested? Maybe with an environment variable and a separate
      upload server that has no expiration?
 - [] Store the video on a camera roll in a mobile browser.
 - [] Explore using the TikTok API Integration : https://developers.tiktok.com/apps
  - This should allow users to save / post? their video directly to TikTok from the web.
 - [] Two finger twist and zoom while recording... use vector data.
 - [X] Make sure that the audio and video are synchronized across devices.
      (Video seems delayed on iOS right now.)
 - [X] Record and playback the user's voice in `microphone`.
 - [X] Record the content of `microphone` as a video.
 - [Commented Out] Add an "audio engine off" system overlay / call to action
      that uses the DOM?
      - [X] It should be removed... each program should handle this on its own...
      - [X] Throw up the html if the audio engine is off and the beat function
            has not been exported.
      - [X] Add all the new Typefaces to CSS. (Berkeley Mono Variable, and Proce55ing)

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

***Routing Longtail***
- [] Do away with hashtag routes entirely.
- [] Refactor ":" parameter encoding (see `wg`) into using classic url parameters.
  - [] Or just use one of these special characters: `. _ ~ -`

      aesthetic.computer/wg.fuzz
      aesthetic.computer/wg_fuzz
      aesthetic.computer/wg-fuzz
      aesthetic.computer/wg~fuzz
      aesthetic.computer/metronome~80~100

      aesthetic.computer/~niki/plot~40~50
      aesthetic.computer/~niki/plot~40~50

      // Maybe the special characters could be default parameters, or represent
      // types of some kind?

      Available domains:
        - prompt.ac
        - wgr.ac (whistlegraph recorder)
        - wgl.ac (whistlegraph live)
        - jas.ac (my new homepage)
        - aesthetic.run
        - aesthetic.codes
        - aesthetic.cx

**Image Viewer / Media Viewer**
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

*** 🎃 DIGITPAIN ***
 - [] Complete DIGITPAIN 0-5.
 - [] Mint the work and have the contract made.
 - [] Make a frame capture tool for generating webp images.
 - [] Add the work to the JSON and release digitpain.com.
 - [] Write a better static exporter script includes only the disks I want, plus resources.
     - [] Automate DIGITPAIN (Teia) exports:
            1. Clone `public` directory.
            2. Remove any `.html` files that aren't `digitpain-ipfs-index.html`.
            3. Process `digitpain-ipfs-index.html`, inserting info for a new mint.
            4. Rename `digitpain-ipfs-index.html` to `index.html`.
            3. Remove all files other than `/digitpain` and `digitpain0.js` from `disks/`.
            4. Zip the contents so there is a zip file with one directory called `public`,
               and rename it to `DIGITPAIN #.zip`.

🌟 Next In Line 🌟
 ***Transcribe the original Proce55ing typeface***

 ***Multi-player Drawing / Core Drawing***
  - [] Allow users to join / create rooms with codes (QR codes esp.)
  - [] Abstract `line` and `spline` into `nail`. See also: `gesture`.
  - [] Add sound to `nail`.
  - [] Use https://wicg.github.io/video-rvfc/ for video frame updates.

 ***No Paint***
  - [] Should the old nopaint.art be reachable from within aesthetic.computer
  - [] Should a new nopaint be built from scratch?

 ❤️Side Missions❤️
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


* Recently ✅ Completed (RC) *

*** Tracker ***
  - [x] Fix buttons.

*** Keyboard Input ***
  - [x] Debug keyboard input on every device again.
    - [x] Android
    - [x] Meta (Seems good.) 22.11.03.23.34
  + Done
  - [x] Fix duplicate "Enter" event bug on desktop.

*** Friendlier Developer Logs ***
  - [x] Optimize the console.log output in production to be
       more terse, and make common sense.

***Android Day***
  - [x] Fix keyboard controls not working (this should also fix Meta Quest 2).
  + Done
  - [x] Provision Android phone.

*** Hex Support ***
  - [x] Add hex support to color inputs.

*** Bonus ***
  - [x] Change `ap` -> `test`. (Rapter)
  - [x] Detect dark mode and modify the prompt accordingly.

*** 3D Renderering ***
  + Done
    - [x] Only load threejs if necessary.
    - [x] Fix thread boundary conditions related to hot reloading.
    - [x] Broken on load.
      - [x] Fix `sim` being busted on quick reload.
    - [x] Fix cursor transparency in 3dline.
         (Make blend function take into account existing alpha.)
    - [x] See if this is faster than the software renderer as-is?
    - [x] Make sure picture ratios match up perfectly.
    - [x] Get lines working.
    - [x] Switch the / convert software rasterizer from left handed to right handed
      rendering.
      What worked on the software renderer side:
       - Flip X and Z position of each vertex before transform.
       - Flip X and Z rotation of each vertex before transform.
       - Flip Y rotation of the camera before transform.
       - Rotation matrix multiply order for camera: YXZ.
       - Rotation matrix multiply order for vertices: XYZ.
      + Future + Eventually I could look into using the exact same matrix data across both?
    - [x] Get textures in there.
    - [x] Enable all objects.
    - [x] Optimize pixel array sending. (Had to make an extra layer)
    - [x] Disable backface culling.
    - [x] Make it so that "form" calls that take an array, render in 3js and return the pixels back.
    - [x] A single "form" call should be able to send over multiple objects.

 *** Resize (gap-density) ***
   - [x] Add "scale" to "paste"
   - [x] Cursor is janky when paint returns -> false.

*** Prompt ***
- [x] Use localStorage for command history.
  - [x] Set up general localStorage system / API.
  - [x] Maybe it could be built off of store?

*** Pieces: Prompt ***
 - [x] No more tildes for user paths...
      Instead, use an '@' for usernames.
      eg. sage/piece
  - [x] Prompt
  - [x] Index
 - [x] Up arrow for previous command. 
 - [x] Update MOTD. 

*** Safari Day ***
- [-] Thin lines appearing from `gap` and no gap in mobile Safari.
- [x] Grey selection bubble appears in mobile Safari.
- [x] Loading spinner not appearing in mobile Safari. 
- [x] Fix iOS Safari page refresh bug. (Test on MacBook Pro)
  (Needed `Vary: *` http header, to prevent Safari's cache from bugging out
  on the SharedArrayBuffer / isolatedOrigin CORS headers.)

***Tracker, Pull***
- [x] Fix interface boxes.
- [x] Make `boxes` test piece.

 ***Meta***
 - [x] Add support for custom / locally generated thumbnail overrides.
   - [x] Test override for some specific route... maybe blank?
 - [x] Serve a 404 page for anything else that breaks.
   - [x] Right now it just crashes here: https://aesthetic.computer/okok
   - [x] Right now it just is black here: https://aesthetic.computer/~niki/whatever
 - [x] Do the TOML routes actually matter? No.
 - [x] Get ~/niki/blank working again.
 - [x] Separate out / parse the text before loading a disk serverside so that
       parameterized disks work again.
 - [x] Make them work on iOS
 - [x] Write a new netlify function that returns the proper html for every page,
       so that prerendering can be turned off (get imessage previews working).
 - [x] Should og:image and twitter:image have file extensions?

***L8 Night***
- [x] Get a debugger working. 

***2D Drawing Primitives for Niki***
 - [x] line from point... at angle with distance
 
***Routing & Twitter Cards***
 - [X] Figure out how to get open-graph info and twitter cards working for all of a.c?
      - [X] How should the og:images be made? Perhaps a special tool is needed or screenshots
           can be taken automatically?
      - [x] Set defaults if the info isn't there...
- [x] Test Netlify pre-rendering to see if it functions.
- [x] Use netlify's pre-rendering and (it's already turned on... and change the og:title)
- [X] Enable Netlify pre-rendering.
       - Validator: https://cards-dev.twitter.com/validator
       - Prototype: https://glitch.com/edit/#!/pepper-efficacious-yellowhorn?path=index.html
 - [X] Implement path based routing for pieces via netlify and on a local dev server.

***User Pieces (niki.aesthetic.computer)***
- [x] History breaks right now across domains, because some parsing goes through the `prompt`.
  - [x] Make sure loading using the `tilde` key works across hosts.
  - [x] Start remote routing from boot instead of only in prompt?
      - [x] Make a `parse.js` library file that can be used in `prompt`, then `boot`.
      - [x] Copy the code from prompt and replicate it in boot, then
      - [x] follow the logic through to check for inconsistencies.
      - [x] Remove `search` from `disk.js` load. (Deprecrate reading queryParams
        at the disk level... they should only be reserved for system settings?)
- [x] Also, the page can't refresh and the path gets rewritten improperly. 
- [x] Get `niki.aesthetic.computer` running.
- [x] Get a prototype of Niki's piece working that she can actually edit as the first user.

***Server Re-organization***
 - [x] Should the uploader be a serverless function? Yes!
      (This would prevent the need for a load balancer if file uploading was a bottleneck.)
      (Would it run locally?)
 - [x] Set netlify environment variables for the digital ocean space.
 - [x] How to run two server processes from one npm command?
 - [x] Should the websocket server run as a separate process from the http api?
 - [x] Should it run on a separate machine as of now?

***Editor***
- [-] Get socket server running under GitHub codespaces as well.
  (wss:// is currently unsupported)
- [x] Get the project working in GitHub codespaces.

🐛 Completed Bugs & Regressions
  - [X] Check to see why custom thumbnails are broken for remote URL disks.
  - [X] `wg` disks no longer work 
  - [X] Kill any microphone (or video) connection after leaving a piece it's connected to.
  - [X] Routing: Check ~tilde~ key - don't push duplicate states in the history.


⏳ Investigate ⏳

* Bugs / Improvements

  Stop sending duplicate event data for "draw" and "touch" in `pen`.
    - e.is("draw") && e.is("touch") should not send duplicate event {x, y} data
    
  Explore: removing: send({ type: "update", content: { didntRender: true, loading } });
  From the bottom of `disk.js`.

  Add blockchain integration for identity: https://docs.moralis.io/introduction/readme
  
  Re-work the depth buffer (in the rasterizer) before making a simple 3D environment?

  Pass 'diskTime' global into the api.

  Add $api.sound.beatCount counter to `beat`.

  Add scriptProcessorNode fallback so audio can run within insecure contexts. (FigJam)

  Stop tracks in all mediaStreams: https://stackoverflow.com/a/12436772
  (Video and Microphone)

  Refactor (screen.width -> screen.w & screen.height -> screen.h) across the board?
    - To be more consistent with `graph.box`.

  Workers can be wrapped in this... https://benohead.com/blog/2017/12/06/cross-domain-cross-browser-web-workers/


* Ideas

  Sidelight
    - A light you can set the color off.
    - Make it flicker.
    - Have presets.
    - Very useful for making videos with an extra device!

  Generative tone matrix.
    - Make microtonal grids of different sizes that can be easily played on
      any device. Use options to specify or control the parameters of the
      instrument.
    - Sliding your finger across notes is different from tapping.
    - It should be easy to record a video and/or audio.

  Rewrite my old Python `Diary` program.
    - Mint diary entries.

  Remake Shrub

  Improve documentation.
    - In console when the whole thing opens.
    - Have a global keyboard shortcut to spit documentation to console... for each
      function!
    - Get jsDocs working: https://jsdoc.app/about-configuring-jsdoc.html

  Make Shrub.

  Development Improvements
    Would the best way to produce a scripting language be by making a fully
    curry-able system or some domain specific languages using `` then
    combining them?
    -- See here for a notation example of inline s-expressions: https://hag.codes

    Could an editing session or new disk be started from the prompt? Maybe it
    could load a GitHub codespace?

    - How to make use of xterm.js? Could it be used for this project or not...

  Implement drawing / gesture based disk launchers from the prompt.

  Take over right click so no context menu shows up.

  Ink types? ... can they be procedural using a buffer?

  What would a build script for releasing a single disk or
  stack of disks look like?

    1. Make a new directory D for the build.
    2. Copy a subset of /disks/public to D/disks
    3. Copy index.html, style.css, boot-rolled.js and /computer to D.
    4. Modify boot-rolled.js to start loading from the first listed disk.
    5. Generate metadata.json for the work.
    6. Zip and upload to fxHash or put whole directory into Pinata.
    7. Test to make sure everything works.
    8. Mint on a contract (or Cancel and remove files from IPFS).

  What would a full system / suite release look like?

    Using the build script with a set of chosen disks where the main
    disk is a shell interface.

    Document the API by generating the commands inside a disk and print
    them to the console using keyboard shortcuts.

    - [] Generate full API docs in the prompt disk.

  Make my first generative disk / piece and mint it.
    - Test it in Chrome / Safari / and Firefox.
    - In larger multi-disk IPFS exports, a new root path must be defined other
      than `prompt`. See disk.js `load` function.

  Make a quick fullscreen button (make a new keyboard input module).

  Fix Firefox: https://bugzilla.mozilla.org/show_bug.cgi?id=1247687

  Add a new deployment scheme that actually allows my websocket server to
  function... this might mean leaving behind Vercel or configuring it
  for different directories in my repository, running separate server code
  for sockets.

  Add syntax notification of some kind for disk files, using a comment at
  the top. This would eventually allow me to completely
  change from JavaScript.

  Add stochastic syntax / DSL for basic drawing?
  Example: wipe.ink.line.line.line.ink("red").box;

  What cryptocurrency / web3.js lib would I hook an editor up to
  for minting, uploading edition data, etc?

  Make a video-recording app that generates 3 letter codes? - to augment
  my hand-written notes and pages.

  Add ability to define the logical / simulation framerate.

  Give myself the ability to automate testing of system using "robot".

  Write a simple note taking program with saving and loading of drawings
  and also recording and playback?


🎃 SETUP 💾

`aesthetic.computer` is virtual computer environment / interface designed for
 creative exploration. development requires `nodejs`, `git-lfs`, an up-to-date web browser,
 and knowledge of javascript. if you're interested in learning how to do any of
 this, visit https://discord.gg/digitpain and i'll help you out - jeffrey 2022.04.24.05.05

Make sure `git` and is installed, (you can do that through `homebrew`) and then get set up for development:
  0. Also install `fnm` the node version manager.
  1. Check `ssl-dev/readme.txt` to generate and add local SSL certificates.
  2. `npm install` from the project directory.
  3. `cd` into `session-server` and `npm install` as well.
  4. Get added to the Netlify project as a collaborator.
  5. `cd` into `system` and run `npx netlify login` and then `npx netlify link`
  6. Now run `npm run code` and in another shell `npm run server:session`.
  7. Visit `https://localhost:8888` to view the running site! 
  8. If you need local copies of the site's rich media `assets` then ask Jeffrey and you can slurp them from S3.

Redis
  0. You may need to install `redis` to get the live reload working properly
     on local.
  1. You can use homebrew for this and a default install should be fine, so 
     long as `redis-cli` works.

🧩 Making a new included piece.
- Run `npm run new-piece -- name-of-your-piece` 
- Then open the file in `system/public/aesthetic.computer/disks` and start working!

In production: Add a "#debug" hash to the end of your URL for more verbose output.

If developing: Add a "#nodebug" hash to the end of a URL for less output.

📖 This project originally began as two separate repositories with their own
commit history: `digitpain/system` and `digitpain/disks`.
