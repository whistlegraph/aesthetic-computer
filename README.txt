A *new* computing environment that...

  (Technical Summary)
    - Works in updated web browsers on all major devices, including VR headsets!
    - Runs "pieces" instead of "apps" or "programs".
    - Pieces are written in a proce55ing-inspired API that's fun to learn and fast
      to code in.
    - Screen defaults to an accessibly low resolution, great for learning
      and counting pixels with,
but technically agnostic. (Choose your aesthetic!)
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
      - 2D/3D Software renderer for spatial display of programs as planes
      and
        ease of accessing pixels while coding a piece and learning graphics.
      - 3D WebGL renderer for VR.

🖥️ Running locally:
  1. Scroll to the bottom of this file and follow the setup instructions.
  2. From the `aesthetic.computer` directory, run `npm run aesthetic`.

😀 TODO 😆

🌟 Projects in Progress (PIP) 🌟

- Jeffrey (Sat)

*** webgpu rasterizer ***
  - [] follow this webgpu rasterizer tutorial: https://github.com/OmarShehata/webgpu-compute-rasterizer/blob/main/how-to-build-a-compute-rasterizer.md

*** etc... ***
  - [] Fix 'download' / dl command.
  - [] Add "..." glyph to font.

*** dev environment ***
  - [❤️‍🔥] Can't kill and re-open platform in the devcontainer...
  - [] Auto enter insert mode when switching tabs in emacs.
    - [] The eat shells seem to stay open?
    - [] Ports are staying open after C-c C-x
    - [] https://elpa.nongnu.org/nongnu-devel/doc/eat.html#Shell-Integration
  + Done
  - [x] VSCode status bar todo goal.

*** speed ***
  - [] Speed up other stuff on the `Performance insights` chart, by eager loading / waiting less.
  - [] Make /prompt load faster than 0.3s in production.

*** keys **
  - [] Make prototype.

*** org-mode learning ***
  - https://zzamboni.org/post/beautifying-org-mode-in-emacs/
  - Add variable pitch fonts to org-mode.

*** aesthetic lisp / s-expression parser and evaluator ***
  - [-] Make a basic graphical editor / viewer.
  + Done
  - [x] Hook a lisp function into the graph / paint api somehow?
  - [x] Write s-expressions directly in the prompt if
        the input begins with a parenthesis / produce
        a dynamic disk / run existing lisp code through
        `load`.
  - [x] Refactor `load` to be more efficient to support this.
  - [x] Make a source .lisp file in `disks`.
  - [x] Make a lisp.mjs file that exports a parser.
  - [x] Read the file in as a piece in load in side `disk.mjs`.

  - [] "piece"
  - [] bracelet
  - [] make a spatial editor / evaluator
  - [] use the `pjs` mock work to support the new syntax / upload type?
  - [] graphical editing with s-expression underneath?
  - [] could it work off an actor model of some kind?
  - [] make a gui editor that stores sequences in s-expressions

*** brushes ***
  - [] Add auto-interpolation to brushes.
  - [] This could be done through...
      `export const smoothing = "none"`
      `export const smoothing = "linear"`
      `export const smoothing = "curve"`

*** chat ***
  - [] Add ability to show pixels in the chat / share current work, with
       the ability to steal pixels as well / remix.
  - [] Notification rollup.
  - [🟠] Get the chat integration in `line` and `rect` and other brushes
       to be an interactive pop-up.
  - [] Add high resolution / PIXIjs renderer to the `chat` piece only.
  ------------------------------------------------------------------------------
  - [] Make `help` channel as the first 2nd instance for chat.
    - [] Users should be able to "swap" channels but just enable it in a separate
        `help` piece similar to how `chat` is implemented.
  - [] Add custom / cool notification icons, and also sounds?
    - [] Templated method from here didn't seem to work: https://firebase.google.com/docs/cloud-messaging/android/send-image
  - [] This could use the redis loop?
  - [] Add radio / background music.
    - [] Or show bgm waveform.
    - [] `chat bgm`
  - [] Use a tinier font for usernames?
  - [] `chat` needs light and dark default
  - [] Natural emojis could potentially be shown
       in a Canvas2D or WebGL overlay.
  - [] So then there should always be a full resolution buffer in the renderer?
  - [] Chat would need to always be everywhere
       even on "low resolution" pieces and the
       UI event loop needs to remain on while pieces are switching out.
  - [] Which means more than one framebuffer will be
       needed - one for "display" and one for "piece".
  - [] Fix email address could not be verified sign up message: https://discord.com/channels/890376329957081138/890376329957081144/1225363479649845352
  - [] Add the ability to show pixels in chat.
    - [] Paintings should be resolution limited
         and re-uploaded / copied to a bucket just
         for that particular chat message.
  - [] Add a 'mask' / 'play' command for dramaturgy.
    - [] How would this enter into the database?
    - [] It should be unhandled but have an "admin" ?
    - [] Just an extra field for what mask was used.
  - [] Make a panel in the logs to watch it stream in.
  - [] Then appear for other users and the current user.
       (Just make the text fully synchronized for now.)
  - [] Make a new "notification type" for chat and add it into an iOS app update.
  + Done
  - [x] Add message sounds.
  - [x] Only render lines of text that appear inside the margin.
  - [x] Keyboard does not reactivate / open after tapping corner word.
  - [x] Show the user's handle in corner if it exists.
  - [x] Move top margin up.
  - [x] Don't render any text / long messages off-screen.
  - [x] Limit chat message length, both on client and on server.
    - [x] Server
    - [x] Client
      - [x] Numeric 0/n counter.
      - [x] Notice
  - [x] Move the paste button up half-way.
  - [x] Integrate chat interface into the `disk.mjs` file,
       using the `chat` piece as a flag for enabling the integration.
  - [x] Enable the chat ui overlay for all pieces in the `nopaint` system.
  - [x] Show last chat message on homepage.
  - [x] Have the chat server running as a separate websocket connection, then that number can be used for total users online.
  - [x] Be able to delete certain recent messages manually in MongoDB.
    - (Can be done through the atlas cloud interface)
  - [x] Add scroll with scroll bar that auto-expands if you go back
        too far.
  - [x] Add line breaks to chat.
  - [x] Add chat to the docs / `list` command and to autocomplete.
  - [x] Fix initial db fetch order.
  - [x] It should persist in the database under a table.
  - [x] These messages should send notifications.
  - [x] Add a `chat` command with a persistent list of messages.
  - [x] Spin up a nanos unikernel test server for the chat.
    - [x] Deploy it to the google cloud.
      - [x] https://console.cloud.google.com/compute/instances?project=aesthetic-computer
    - [x] Add https and ssl and a subdomain.
  - [x] Add the 'chat' tab-bar item / CLI client that can log the chat
        and also send messages?

*** dev / desktop usability ***
  - [-] Add global ctrl+shift+ and ctrl+shift- shortcuts for
       zooming the pixel density of the ac renderer, keeping the crosshair
       size (overall browser zoom) the same.
  - [] The Ctrl+` shortcut should pass through to the VS Code extension
       (by sending the host a message) which would unfocus it and refocus on either the editor or terminal... perhaps whichever had the focus?
  - [] There should also be a shortcut to focus on AC immediately.

** piece authoring helpers ***
  - [] make an `icon` piece where you can `icon prompt` or `icon anything` to
       preview the icon for a piece.
  - [] (do the same for `preview` for preview images)

*** desktop app / electron build ***
  - [] Follow a guide at https://www.electronforge.io.

*** `world` <-> `painting` <-> piece integrations ***
  - [🔴] Joining field and being on the same handle would enter a spectate / take control sort of situation.
  - [] Be able to easily complete a painting and then include it in a piece
       in a single line...
  - [] Add the ability to name a painting so it can more
        easily be referenced, like for example in a `stamp @jeffrey/flower` tool.
        Or especially like old CS 1.5 sprays.
  - [] Should be able to say `done flower` to make the alias.
  - [] Also should be able to remove and change aliases.
  - [] Then a logged in user can `paste flower`.
  - [] There should be a command to set temporary names without persisting
        them to the network? Well... that's just empty `show` or maybe
        there could be a buffer of paintings in progress, similar to how
        text editors have buffers.
  - [] How to handle multiple handled users in the same room.
      (Make primary vs. a ghost type of view-only behavior?)
  - [] How to have two windows or machines up and use a put command while
      modifying pixels. (Always store current painting in the cloud for a registered user.)
  - [] Implement `paste` and send the pixels through the backend.
  + Done
  - [x] User world persistence with floating command notifications.
    - [x] User world persistence.
      - [x] Better visual list of users / skip lines.
      + Done
      - [x] Keep a ghost / location of the user once they leave field or whatever world position.
      - [x] This should require a handle, to reassociate the user?
      - [x] Floating command notifications.

  - [x] Maybe I could overload `paste` to include user strings...
  - [x] Share current painting in `world` using a `show` command.
    - [x] Also add `hide` command.
  - [x] Add connection flash to world.

  *** .pjs compiler ***
    - [🟠] Write the metaprocessor in that slurps the .pjs to OpenAI
        at runtime.
    - [x] Break the below down into steps and get it compiling on each load.
      - [x] Add syntax support to the vscode extension for the .pjs files / metaprocessor.
      // 🔥
      // TODO: Write `prompted` javascript that will compile and run against any
      //       LLM version 24.02.22.19.38
      // Call it `.pjs` ?
      // piece, processing, prompted

      //if (!net.iframe && params[0]^does not start with^) {
      //  return;
      //}

*** fps ***
  - [] Add a draggable window to the vs-code extension, to support dual
       monitor development where the bottom monitor is full-screen.
       (This should enable more immersive development / help with monitoring.)
       - 📆 In a week
       (https://github.com/microsoft/vscode/issues/208790)

- [-] Auth is messed up in the VSCode extension.
  - [] Potentially when switching between Local Development mode and
       production.
  - [-] In general after logging out, one appears to stay logged in?

*** sean meeting w/ ida ***
  - [] system update notification feed
  - [] computer needs to say how it gets better
  - [] moat of learning on the fringes
  - [] ac openings can be a thing / do a feral-file opening

*** swipe to share ***
  - [] dragging the corner label all the way to the right should always reveal a "share" prefix and then letting go will jump to `share x`.

*** vscode extension ***
- [] Fix weird auth things / loss of auth in the vscode extension?

*** aspen demo ***
  - [] Screenshot needs to work / resize if necessary from a webgl / 3d view.

*** note ***
  - [] add note and notes and 'noted' notification

*** yuehao demo ***
  - [] Scream notification after sign up didn't work...

*** Meeting with Petra ***
  - [] Add support for CMYK, with a special prefix?
  + Done
  - [x] Allow hashtag in first color parameter.
  - [x] Downcase hex colors.

*** song demo idea from rip it up party ***
 - [] Add a forward and backward button to song.
 - [] Add different wavetypes to song. (Wavetype per note).
 - [] Add more melodies to song.
 - [] Release a track with song that kind of works like a sampler.
  - [] With a title screen that has [ Play -> `x` | Record -> `tape x` ]

- Miles (Sunday)
  *** `crayon` ***
    - [] Fix the double buffer paint after each gesture.
    - [] Colors drift too much from their original value permanently after
         making a gesture.
    - [] A little splotchy, marks should feel uniform.
    - [] Interpolate gesture using a Catmull-Rom spline or based on speed / physics using a leash.
  *** load performance ***
    - [x] Spend a half day just on getting the site to load faster.

*** making things alive ***
  - [-] Pasting graphics in `field` is important.
  - [] Add `show` and `hide` for temporary show and tell.
  - [] Add `paste` for permanent bitmap pasting, maybe in
       a piece called `graf` that can be linked to from `field`?

*** making the aliveness visible ***
  - [] Build a simple version of `log` now that we have list.

*** casting ***
  - [] Get a nice headset that I can actually record Fedora screencasts with so
       I can start posting simple tutorials from my laptop / possibly edited
       on my phone or even on AC through screen recording?

*** `pal` ***
  - [] Make a palette syntax for sharing a list with tap back functionality.

*** `pic` `pix` ***
  - [] 164 / 128 3x4
  - [] https://prompt.ac/pic~here~is~my~cat

*** API design ***
  - [] Whitelist some of the APIs for the docs and for usage in user pieces.
  - [] Overloaded functions and commands > different function names.
  - [] I guess this means it needs to load the api once whenever a user piece loads?
    - [] Screen user pieces for fetch requests. (Don't allow them.)
    - [] Write a great example / polish the pieces repo.
  - [] Can I programmatically use `api/doc` to whitelist the api
       if it's code running from a user piece and the `run` endpoint?
  + Done
  - [x] Build `docs` pages.
  - [x] Should it be called docs?
  - [x] Remove or deprecate the old `list` html design from `docs`?

*** `code words` ***
  - [] A commission program for people to code ac nouns and verbs in their
       language of choice.

*** prompt regression ***
  - [] `load` should load a photo / be similar to `paste` and work on mobile.

*** media variables ***
  - [] Set "variable" names when using `done`, helpful for show and tell.

*** pray ***
  - [] An altrnative to `scream`.

*** birthday ***
  - [] Add a `birthday` command that taps through the birthday
       and also takes their name and age.

*** aspen suggestions ***
  - [] `tree` command with `tree summer` `tree winter` etc.
  - [] `hurricane` natural disaster sim. (dark)
  - [] downcase / case-match commands in the prompt and world

- [] Zoom in on painting in mobile.

*** `world` ***
  - [] Make shaped worlds using inner poly collision and points for boundaries.
    - [] And then this can be passed instead of width and height?

*** webvm / xterm ***
  - [] Explore using xterm: https://xtermjs.org/
  - [] Explore webvm: https://labs.leaningtech.com/blog/mini-webvm-your-linux-box-from-dockerfile-via-wasm
  - [] Could something like an xterm instance be somehow connected
       to the `chat` feature?
    - [] No because that might prevent different rendering aesthetic, unless
         I have my own rendering layer...
    - [] Which could be text
    - [] Emoji support via https://joypixels.com.

48 Hours:
*** webgl2 renderer ***
  - [] More work on webgl2 renderer.

*** migrate netlify functions ***
  - [] Move ask off of vercel to a netlify 2.0 streaming function.
    - [] https://twitter.com/biilmann/status/1712527635975385165
  - [] https://www.netlify.com/blog/introducing-netlify-functions-2-0/?utm_source=twitter&utm_medium=social&utm_campaign=netlifyfunctions

Full day:
*** `log` ***
  - [-] Print a live global activity log than anyone can watch.
    - [] Choose some good events to broadcast.
  - [] This should eventually be streamable from any piece or visible
       on the prompt.
  - [] Top right tap to enter text, then be able to chat, from any
       piece.
  - [] Add filtering like `log:scream`.

2-3 days:
*** `android app` ***
  - [] Set up / start android app / make sure android studio
       is installed.
  - [] Add support for notifications.

1 day
*** `paintings` ***
  - [] Show a feed of user made paintings in order, only showing one per
       user.

2-3 hrs
*** `painters` ***
  - [] A linked list of users and their paintings by count.

1 day
*** `blog` ***
  - [] Build CRUD pages.
  - [] Maybe use a vscode instance / route it through piece publishing?
  - [] So a piece type can be considered a "post".
  - [] Add the ability to include high resolution text easily.
  - [] Editor
  - [] Viewer

2 days
*** `autocomplete` ***
  - [] `review any text editing bugs in the main prompt`
  - [] Preview commands / show all possible comands like auto-complete.
  - [] How to deal with commands that are textual subsets: `but, butter`.

*** `dync` ***
  - [] Hit a button on an XY axis that maps a percussive sine wave
       to white noise on one axis and pitch on the other.
  - [] Multi touchable.
  - [] Tilt for volume with visual feedback.

*** `chord` ***
  - [] Make a 4-string simulator with chords that can
       also play the words through of a song.

*** `go` ***
  - [] And then `cave:entrance`
  - [] Create a `go` command that always returns you to your last worldly
       location.
  + Done
  - [x] Fix double listing issue (seen via sat)
  - [x] Polish spatial linking.
  - [x] Walk to `horizon`.
    - [x] Make `horizon`.
  - [x] Finish `field`.
  - [x] Show an orange or red status if UDP or sockets get disconnected while
        aesthetic is in use.
  - [x] Both `socket` and `udp` need disconnect messages /
        prevent big fish list?

*** `dia` ***
  - [] Run a written / readable dialogue for two.
  - [] JSON structure with two characters.

*** `ten` ***
  - [] Some kind of Pong clone with 2 player matching and also
       spectators / round-based play?
  - [] Should be visitable from the world.

*** `chanel` ***
  - [] Entering `chanel` will take you to a dress picture.
  - [] Title text:
  - [] Copy text:
  - [] [Dress photos.]
  - [] Button in the corner that says [$256].
  - [] Custom email.

*** new session request ***
  - [] When / if a session server goes offline, try to request a new one.
  - [-] This can be done by running an http get request on the endpoint to
        see if it's still online and also checking against navigator.online

*** `ptt` ***
  - [-] Make `ptt` voice chat prototype.

*** `contain` ***
  - [] Contain the random positions in the drawing API for things like
       `line()` etc.

*** better email verification flow... ***
  - [] Say "email verified" as a notice after email verification is tapped?
  - [] https://chat.openai.com/c/c9898bf6-62f6-4654-a54e-9c8ba47f5681

*** `pond` production ***
  - [] Finish the basics in `go`.
  - [] See `pond.mjs`.

*** automate honest fabric checkout ***
  - [] https://www.honestfabric.com/shop/checkout

*** cab meeting ***
  - [] Set up DMARC for emails.

*** `sno` launch - Dec ?? ***
  - [] See `sno.mjs`.

*** `Paste` for image ***
  - [] Tie the Paste button to image drag and drop.
  - [] Make sure `paste` works on mobile and desktop / could rename it to import?
  - [] Resize dragged in images to fit the display width and height / have a max?

*** Database ***
  - [] Add database setting and clearing on a per-piece basis.
    - [] Need to be able to verify what piece is running in a way that
         is not easily tamperable. (Validate by system or handle based piece / what URL it was loaded from.)
    - [] Need `set` and `clear` and `get` API.

*** readme switch ***
  - [] Split README off from TODO.
  - [] Make it markdown? Or leave it .txt?

*** VSCode Extension Improvements ***
    (See also: https://discord.com/channels/890376329957081138/890376329957081144/1171956446641016932)
  - [] Make sure going to prompt and returning to the piece / entering new
       params will still work and that piece name replaces other pieces.
  - [] Add support for params and piece renaming to the VS code extension.
  - [] Make sure the page can be refreshed and the connection / new code
       still persists.
  - [] Make sure piece-halting errors will still auto-refresh.
  - [] Add / research support for packages.
       (Published groups of `.zip` files based on a directory)

*** wizards ***
  - [] Use the "|" operator to make stepped wizards / looped pieces.

*** user pieces ***
  - [-] Consider the versioning of published pieces /
       how they should be added to the database.
  - [] Jump to any user space with admin:space @handle
       - https://cloud.digitalocean.com/spaces/user.aesthetic.computer?path=auth0%7C650ef5167fd48ae4910e048c%2F&deleteObject=&type=
  - [x] Add user pieces to the database and wire up the
        media collection endpoint.

*** painting forking & remixing ***
  - [] "Stolen from: @user/20323:4";

*** pond awareness ***
  - [] Set ambient pen pointer color to cyan / flicker cyan if the user
       is in the pond.

*** `but` suite ***
  - `helikesmebut`, `helikesmybut`
  - `ilikeubut`, `ilikeurbut`

*** Painting ***
  - [] Finish `brush` template for paint brushes.
  - [*] Add auto-resize back?
  - [] fix `vary`
  - [] Add a `pull` brush.
  - [-] Add Zoom for all brushes. 🔵
    - [🟠] `line`
      (Read implementation in `rect`)
    - [] `oval`
    - [] `word`
  - [🟠] Always have a secondary buffer.
    - [] Add an `info` command to view painting info.
    - [] Add color picker. 🔵
    - [] Also add rotation.
  - [] Entering any valid cssColor, hexColor, or RGB values on
       the prompt should be an alias for `wipe color` and
       `wipe color` needs to be coded properly?
  - [] Double tap Shift key to reset pan. (Remove ALT)
  + Done
  - [x] (Make sure erase works perfectly in rect)
          (Is `pan` much slower now because of the new blend?)
  - [x] `rect erase` no longer works
  - [x] Contextually show whether we are zoomed or not, perhaps with a border?
  - [x] Make sure `nopan` centers a zoomed canvas.
  - [x] Implement for `rect`
  - [x] Add basic rainbow for `rect` and `line`.

*** Painting Listing Pages ***
  - [] Scrape all user media and add records to the database.
    - [] Write a one-time runnable task for this.
  - [] Add a new record whenever something is uploaded.
    - [] See `bios` -> where `presigned-upload-url` succeeds.

*** painting page ***
  - [🧡] Shorten the corner word on painting pages / remove the timestamp
       especially if it will be seen in the corner.
       (Word wrap it)
  - [] Single finger pan.
  - [] Painting pages need a left and right screen tap to advance or retreat.
  - [] Always record a painting by default now?
  - [] Make a nicer loading transition for painting pages that load
       an animation (Always show the last frame for a given duration.)
  - [] Play forward or by a single step.
       (Minting is a form of printing.)
       (See if Zora can accept prefilled form data parameters on their minting page: https://support.zora.co/en/articles/7131468-zora101-how-to-create-a-multi-edition-erc-1155)
       - I want to be able to send them a URL for an image that they can use to prefill
         the form fields.
  + Done
  - [x] Add a transparent image tag overlay for tap to save on every
       painting page.
  - [x] Tap and hold to save on an invisible image tag.
  - [x] Also make a mint command in addition to a print command.
  - [x] Static image on page.
  - [x] Painting pages need a [Mint] button.
  - [x] Move buttons above progress bar (action area)

*** Recordable Paintings ***
  How-to: Enter `painting:start` to start.
          Enter `painting` to view the history so far.
          Enter `painting:done` to download the complete history
          of your painting. Or `painting:done upload` or `u` to upload it,
          along with the current painting.

  - [🟠] Store gesture data in the recording.
  - [] Of each current step...
    - [] See `nopaint.mjs` line 56...
    - [] And the gestures translated to the painting itself needs to be tracked.
    - [] So that live playback can occur if necessary.

  + Done
  - [x] Separate paintings and painting-records in S3, and always
       keep a PNG and a ZIP (if the recording exists) and have the
       timestamps match.
  - [x] Rename "steps.txt" to "steps.json"
    - [x] Store metadata for the media type (in the json filename).
  - [x] Add `timestamp` to the text data format.
  - [x] The `painting` viewer should have the timestamp in the corner.
  - [x] Store recorded painting steps in idb so they persist
        across refreshes.
  - [x] Add recording indicator.
  - [x] `painting insert-code-here` to play back an uploaded recording.
  - [x] Drag in a zip to unpack and play back a recording.
  - [x] Add a special mode for making "recordable paintings".
    - [x] `painting:done` to complete and download a zip of the recording.
      - [x] Upload zips to S3?
    - [x] `painting` to play back the current recording.
    - [x] `painting:record` to start a recording.

** tmi thoughts from amelia on seeing code ***
  - [] "Nobody else wants to do this."
  - [] "There is too much information."
  - [] "Why does it have to look like this?"

*** Research ***
  - [] Piczo

*** Logo ***
  - [] Add ice cream pals to `pals.aesthetic.computer.`
  - [] See if this starts working: https://logo.aesthetic.computer/painting-2023.8.01.14.04.png

*** word -> write ***
  - [] What other commands should be renamed now?

*** mood links ***
  - [] underlined and different colors
  - [] links to pieces and surround with ``.

*** BakTok ***
  - [] Make the audio scrubbable.

*** Prompt / Piece Cache ***
  - [] Fonts sometimes don't load all the way on prompt.
  - [] The piece cache doesn't seem to work / loading times are always
       present on iOS?

*** rainbow bot text ***
  - [] Stop printing rainbow text on the bot replies.

*** `tape` ***
  - [] Test recoding cutoff again / make sure it never happens.
       (Possibly by measuring audio playback?)
  - [] Bring back mouse and touch cursor support, by recording during frame
       capture.
  - [] Work more on recording subtleties.
    - [] Mic playback gain removal.
  - [] Automatically increase playback in `baktok` / mix mic / playback
       special for baktok?
  - [] Implement the TikTok API: https://developers.tiktok.com/doc/login-kit-web
    - [] This should allow users to post draft directly inside of the app?
    - [] https://developers.tiktok.com/app/7280753774627768325
    - [] Make a privacy policy and ToS permalink.
  - [] How can I integrate with whisper to auto-add subtitles?
  - [] Re-enable multi-clip recording.
  - [] Re-enable page refresh support / store previous clips.

*** prompt ***
  - [] The main page prompt should be removed... it kills the feedback loop,
       and instead a help bot should replace it.
  - [] Entering an incorrect command should flash the screen red but
       not clear the text. This also needs to be a negative sound.
  - [] Entering correct things should make a positive sound.

*** @aesthetic.computer ***
  - [] Make the first instagram grid post.
  - [] Have "word" be wrapped and centered based on the painting by default.

- [-] Page sometimes doesn't load on iOS, so what's the hangup?
  - [] Debug the hang to see if it still exists.
- [] Review new Safari release notes: https://webkit.org/blog/14445/webkit-features-in-safari-17-0

*** user generated media ***
  - [] You can do version updates on code but old versions are kept of the same
       name and new versions are auto-incremented but the original name
       always points to the newest version, and there will be a way to get
       a list of versions.
  - [] Paintings cannot be modified but maybe deleted.
  - [] Same goes for all other media.

*** typing `print` ***
  - [] Should be the same as `done` and then typing `print`.
  - [] `user/prints` -> goes to folder and adds a record to the database
                        metadata on how many times this image has been printed

*** picture call ***
  - [] `snaptok` (^-^)
  - [] `meet`, `talk`, `say`
  - [] Implement voice calls with: https://console.agora.io/

*** etude ***
  - [] Play an automated musical piece designed to express the functionality
       of a.c.

*** ✨ Rattle ***
  - [] Make rattle prototype functional.

*** Apple Pencil Painting Support ***
  - [🍏] Hook iPad into a MacBook to fix apple pencil support for brushes.

*** Upgrade some client libs... ***
  - [] Three.JS ?

*** artur painting story ***
  - [] Truncate or fade long prompt corner strings if they go past the screen
       width.

*** faster user media urls ***
  - [x] Why is the handle lookup being so goddamn slow?
    - [x] Migrate data to a mongodb atlas cluster... which should be faster?
      - [-] How fast can it get?
      - [x] Store in redis.
      - [x] Sign up for Cloudflare
        - [] Cache the output of the handle page so it's much faster?
             And uncache it via the API when needed?
        - [] Cache tts replies also?

 - [-] Clean media urls should just go to a painting "viewer" type of page.
 - [] https://docs.netlify.com/edge-functions/limits
 - [] https://answers.netlify.com/t/new-syntax-for-rewrites-in-edge-functions/88257
 - [x] Have local requests use the user bucket directly.

*** Mintable Paintings ***
  - [] Hook it all up to a marketplace instance?
  - [] Everyone should be able to mint?
    - [] Add mint button in playback corner?

*** General Brush Notes ***
  - [] Make sure `left`, `right`, `flip` and `flop` are all added to
       piece history.
    😱 (All commands should be?)

*** handle updates ***
  - [] Handle updates were not working for mitch.

*** Recordable Painting Game / Journal ***
  - [] Game starts by entering `no! 128` or `no! 64 32` etc. with a resolution.
  - [] There is an indicator line or box around the screen the whole game.
  - [] Game ends when typing `yes!`.
    - [] Record the resulting bitmap after each command that modifies it, using
         the `slug` of what was entered as the key.
    - [] Capture the "gestures" for each brush via a global "gesture",
         function of some kind? `system.nopaint.gesture`
    - [] Step through each command, and if a gesture exists, then it can
         be played back. (or skipped)

*** poem ***
  - [] Scaffold the `poem` recorder.
    - [] Poem will implement TextInput, but not the prompt system.

*** Polishing ***
  - [] Optimize the site's initial load...
    - [] Change the starter noise function if the starting
        piece is botce.
    - [] Don't load the auth0 library until after we booted.
  - [] Optimize image loading / database access.
    - [] Measure the latency using a CLI even try a different host?
      (After migrating the database.)
    - [] Try using an edge function or something?

  - [] Painting now needs zoom...
  - [] NaskjdlkjsETWORK FAILURE
  - [👱‍♀️] Add painting include support and images to front-facing bots. (`gargoyle`, `liar` … )
  - [] Add name to top left corner of all bots.
  - [-] Add countdown / progress overlay to no and yes?
  - [ ] Better "yes!" support / cooler download screen.
  - [] Finish all todos in `botce`.
    - [] Remove `botce` from intro text.
  - [🟠] Revisit `worker.js` to fix download link url names.
    - [] Files need to be stored in the database...
      - [] With a flag for completion, ticked after the presigned-upload-url
          completes?
  - [] Spaces on new line breaks no longer work on `prompt` ?
  - [] Add starfield to prompt.
  - [] Tappable floaty words on prompt.
  - [] Gutter max with but no min width.
    - [] Draggable gutter?
  - [] "?" appears after typing a valid word.
    - [] "?" jumps to a `learn piece` page for learning more about a piece.
  - [] `Ctrl+x` fullscreen keyboard shortcut does not work on `prompt`.
  + Done
  - [x] Remove paste button from homepage on first appearance.
  - [x] Dragging a new image into a.c should reset the pan.
  - [x] Eliminate Enter button download for painting and also N and P shortcuts.
  - [x] Better dynamic `preview` / thumbnail images.
  - [x] More forgivable colon spacing.
  - [x] Shouldn't be able to load the prompt from the prompt /
      pressing the [`] shortcut stops the letters from loading / gets weird.
  - [x] Fix Shift+ENTER
  - [x] Publishing should jump you directly to your finished piece url.
  - [x] Typing `profile` or jumping to `profile` should route you to your profile.
  - [c] Typing an email should also take you to the user's profile.
  - [x] Tapping the user profile button should take them to their profile.
  - [x] User profile button should be replaced with their current handle if it
  - [x] Put most recent mood on user's profile.
  - [x] `mood` command / status updates per user
        (visible on their profiles) homepage should show last @jeffrey mood.
  - [x] Typing @user on prompt goes to /@user instead of profile.
  - [x] Profile name at top left is user's username.
  - [x] aesthetic.computer/@user should go to that user's page.
  - [x] Back browser button should bring up mobile keyboard. (This could never happen on Safari, might happen on Android)
  - [x] Fix Safari regression.
  - [x] Design “copy” wrapper text for front-facing bots. @ggacjp.

*** Learn ***
  - [🧡] "?" appears after typing a valid word which jumps to a learn page for learning more about a command.
  - [] Start writing "learn" pages for some existing commands or bots.

*** Characters ***
  - [] Polish bots with background images.
    - [x] boyfriend
    - [x] girlfriend
    - [x] liar
    - [x] angel
    - [x] kid
    - [x] mom, dad, gargoyle, botce
    - [x] Fix `liar` background image sizing code.

  - [x] `angel` bot
    - [x] "Pray to me"

  - [x] `kid` bot
    - [x] "..."

  * One shot bot sharing.
  - [] `liar` and `botce` Screenshot
    - [🧡] A button appears to "download" or "share" which allows user
         to download the image to their camera roll.

         Forgetful: Reply Screen
          ________________________________________
         |
         |    Blah blah.
         |
         |    Ble bleeee bleee blee blee.
         |
         |    - liar
         |
         |    [Copy]
         |    [Back]                 [Share]
         |________________________________________

  * Conversation sharing.
  - [] `boyfriend / girlfriend` conversation share
    - [] Conversation: Reply Screen
          _____________________________________
         |
         |    Blah blah.
         |
         |    Ble bleeee bleee blee blee.
         |
         |    - bf/gf
         |
         |                              [Share]
         |    [Copy]           <--- 4/4 [Reply]
         |_____________________________________
         |                              [Reply]
         |                     <--- 2/2 [Reply]
         |                     <--- 3/3 [Reply]
         |                     <--- 4/4 [Reply]
         |                     <--- 3/4 --->
         |                     <--- 2/4 --->
         |                          1/4 --->
                  (Also include progress bar.)

  - [] `boyfriend` should be able to reply using a "Message read" receipt style
        rendering mode on the reply that gets colored.

  * Multiple characters interacting.
  - [] `relationship` dialogue screen
    - [] Where `boyfriend` and `girlfriend` are in an automated conversation.
      - Screen is separated vertically as two buttons, tapping either button
        sends a response to the other. Multiple sends from one is possible,
        but not while replying.

       | girlfriend |
       --------------
       | boyfriend  |

  + Done
  - [x] User's reply stays on screen while the response is being written.
  - [x] When bot is not forgetful, exiting chat should erase the history
  - [x] Mom and Dad bots should at some point say "ask your mom / dad"
  - [x] Auto-expand the gutter.
  (Requires that gutter is always visible on screen by default.)
  - [x] Make `boyfriend` bot
  - [x] Fix `encode` input / enter regression.
  - [x] Start writing `learn` page content.
    - `rect` (A brush example)
    - `liar` (A bot example)

*** ambient system-wide cursor movement. ***
  - [] Optimize rendering... move scream and label and ambientCursors
       to something that adds to the last stored buffer.
       (Always store a copy of the held buffer for ambientCursor support?)
       (Will this be necessary once I ditch the software renderer?)
  - [] Don't force a repaint.
  + Done
  - [x] Show just a pixel for now.

*** Logo / Load LOGO ***
  - [] Make a "logo" command that sets
       aesthetic.computer's logo,
        - [] Uploads a painting to a special bucket.
        - [] Copies to a cached url for the current one.
       based on whether a user has a permission flag set, (for niki.)
  - [] Make a logo:load option that
       opens the current logo as a painting.

*** CSS Background / Load + Login ***
  - [] Add a `splash` command that
       works similar to logo, for
       a page backdrop that can be swapped out.

*** `code` ***
  - [] https://github.com/yjs/yjs - Integrate CRDT with a provider.
    - [] https://github.com/yjs/y-websocket
  - [] Try to integrate a collaborative editing experience.
  - [] Open monaco in separate window or same window.

*** `prompt` ***
  - [] After returning to a blank prompt / if the answer is empty then
       show the user's text.
  - [] Maintain the cursor's index state within the history stack.
       and reload it as needed for faster edits.

*** `scream` ***
  - [-] How can I get `scream` to notify all devices via my server by using Firebase?
       and how can I associate device IDs with user accounts?
       - [] Probably use MongoDB?
       - [] What about users who have notifications enabled but are not logged in?
  - [] https://mdn.github.io/dom-examples/web-speech-api/speak-easy-synthesis Speak the scream using speech synthesis.
  - [] Make it turn-based.
  - [x] Get JamSocket working again.
    - [c] Implement JamSocket "locks" and deprecate my redis scaling?
  - [] Visualize the socket connection to make everything faster in production.
     - [] Possibly the bottleneck is in connecting to JamSocket?
  - [-] Finish list in `scream`.
  - [] Use NTP for time synchronized delivery of messages.
    - [] Is this good enough for musical timing?
  - [x] Get messaging working.
  + Done
  - [x] Get iOS notifications working in the test app.

*** Song ***
  - [🍊] Finish basic prototype with tone playback, lyric per tone support,
       and loading and saving.

*** Scawy Snake ***
  - [🟠] Finish basic prototype with title / level / game over and leaderboard.

*** WebGL2 / WebGPU Backend ***
  - [] Specify a new GPU backed graphics backend.
  - [] Abstract the graph api into `Y` calls so both compositors can be run
       simultaneously on sample pieces.

*** VSCode Extension Nuances ***
  - [] Refreshing the aesthetic.computer page should load a cached copy of the code. What if `prompt` is being edited?
  - [] Typing the name of the piece from the prompt should go to the cached copy.

--

*** Hands ***
  📔 General
- [] Better user acknowledgement of what instantiates or not related to hands,
     and why.
- [] Fade hand alpha and possibly color based on distance from edge of safe zone.

 🧩 Pieces
   *** `staka` ***
    - [] See `staka`.

  *** `handtime` ***
    - [💚] shared canvas (like digital touch)
    - [] `ht yellow blue` will start a 2 person call where a link is given and
          guest 2 is blue and you are yellow.
    + Done
    - [x] Socket joining errors. @jeffrey
    - [x] Hand-track re-entrance errors. @jeffrey
    - [x] Fix gesture interactions.  (Need a piece that will require it.)
    - [x] Send data across the wire of all connected hands and render them.

  *** `handprint` ***
    - [] See `handprint` for features.
    - [❤️‍🔥] Panning the canvas should also pan the hand properly!

  *** `simon says` ***
    - [] Combos of TIMOP flash hold on the screen until a user matches the combo.
    - [] Then, another combo is shown.

  *** `hand piano` ***
    - [] activate beeps on interactions/ touches

*** bb ***
  - [] Make a game state.
    - [] Title
    - [] Level
    - [] Game Over

*** nopaint ***
  - [] Refactor "test" to nopaint.

*** Profiles and Handles ***
  - [] Fix `handle` setting in all cases / make it work better.
  - [] Make /@handle profile pages actually live.
  - [] Allow people to set handle via profile page by jumping to the
       prompt.
  - [] Show most recent painting on profile page.
  - [] Allow people to set a status.
    - [] Where to store this?
  - [] Is there something in particular that makes the cloudflare worker
      slow, like the handle lookup?
  - [] Profile it...

*plot* (new launch fonts)
  - [-] Make the UI responsive based on grid and scale and window size.
    - [] Print the resolution and the output number in the corner.
  - [] Set up preview / more UI so that new fonts can be produced.
    - [] Gamify it?
  - [] Allow the creation of a custom font using the current glyph set.
  - [] Allow color.
  - [] Design a multi-line prompt / poster design tool.
    - [] Rainbow stripes of different colors delineated horizontally
         kind of like a pastel stenograph pad.
  + Done
  - [x] Fix `resize` call in plot.
  - [x] Is `scale` working?

*** Launch Goodies ***
  - [] Tapping top-left corner label should always highlight the text, and
       prevent other touch events from firing in the piece...
    - [] Doesn't work on `bleep`.
  - [] Add more themes other than the dark boolean?
    - [] You should be able to type colors and have the
         general prompt aesthetic change immediately,
         with a sound that plays per each different color.
    - [] Startup themes can change on a daily basis for visitors.
  - [] Regenerate a background painting on every "no!" /
       starting painting creation.
  - [] Jumping to the prompt after first loading from somewhere else
       is slow. (Always preload prompt locally?)
  - [] Add offline support for the prompt.
    - [] And include an abstraction for caching / loading from
         cached commands.
  - [] Prompts with extra spaces betwene parameters should compress space.
  - [] Upgrade all the packages in `/system` (and manually copy over new dependencies, then test most hot paths for regressions).
  - [] Run a Fast 3G load test to optimize assets and
      make a better startup / loading sequence.
  + Done
  - [x] Auth0 login customization and email notification customization.

*** iOS Native App ***
  - [] Figure out the best way to make a wrapper now.
  - [] Get stickers into a keyboard / live sticker creation with the drawing
       tools. (Research in GPT)
  - [] Also get it work on Android too. (Can use Browserstack for APK testing.)
  - [] Add notifications.
  - [] Submit to store with notifications enabled, usernames and posts.

*** (ida mood setter) / android backspace ***
 - [wip] Allow backspacing on android. (This seems to work now.)
 - [-] Make sure autosuggested text input works on android
       and iOS.
       - [x] Works on iOS.
       - [] Adding space after autocorrected word.
       - [] Not replacing corrections.

*** global screenshot key ***
  - [] There needs to be a system wide method of capturing a screenshot.
    - [] Both on desktop and on mobile.
  - [] These screenshots need to be published automatically upon getting
       a confirmation modal.

*** `prompt` system ***
  + UI Gripes
  - [-] Cannot backspace on Android keyboard.
    - [-] Do a local ngrok phone gap test to fix this.
  - [🌟] Tapping corner word should immediately bring up keyboard.
  + Later
  - [-] How to deal with longer responses?
    - [] Add 'copy" and "cancel" buttons?
  - [] Conversations that have history should be able to be forgotten with a bottom left button.
  - [] `copy` button in the lower left to copy all of the response.
  - [] Keep track of replies somehow?
  - [] Keep character names in the corner?
  - [] Embed two "framed" prompts on a page, or a framed prompt with
       an image below... and with different styles!
  - [] Add a small illustration on bottom right corner.
    - [] Painting @import.
    - [] How would this function? `paste(by("@georgica").painting())`
    - [] Make the illustration:
      - [] Implement a zoomable canvas on `rect`.
        - [] Rename `rect` to `box`.
      - [] Implement a zoomable canvas on `line`.
  - [] Add some sound.
    - [] 1/3 New synth wav types!
    - [] Custom SFX / sampling.
  + Done
  - [x] Text wrap breaks on zoom.
  - [x] Scrub cursor does not respect word-wrapped spacing.
  - [x] Long single word breaks into second line.
  - [x] New words after a line break are not separated alongside the following word.
  - [x] While using word break, words that break at the end of the line cause an extra space at the beginning of the next line.
  - [x] More pixel accurate text scrubbing.
  - [x] Fix halt on 'handle' command.
  - [x] Pressing "done" on iOS keyboard should trigger a handler.
  - [x] Test typing flow and remove code cruft unused handlers and messages.
  - [x] Dragging text should stop keyboard removal on mobile.
  - [x] Remove red square from finished responses.
  - [x] Make `alphapoet` non-conversational
  - [x] The home page button says `start` and you have to press that to start typing. Once you've started the button becomes `go`, and tapping the screen just gets the keyboard up or down
  - [x] Mobile tap to open keyboard should function better...
    - [x] Tap should not delete the character's response to user.
    - [x] Tap on mobile just brings the keyboard up and down.
    - [x] Keyboard disappears after user presses return on their text.
    - [x] Something weird is going on with focus / unfocus... / responses
         become editable somehow.
  - [x] Make sure history works among the different prompts.
  - [x] Line breaks
  - [x] Add some basic conversational support
  - [x] Write new characters / adapt old ones into separate commands.
  - [x] Fix mobile UI.
  - [x] Break prompt programs into multiple commands / characters.
    - [x] Make a new template.
  - [x] Add sound for `Music Box`
  - [x] Decide how to get back to main navigation page *especially on mobile
  - [x] Cancellable responses.
  - [x] Movable cursor support, with arrow keys and touch to move or drag.
    - [x] Draw character once more on top of cursor.
          $.ink(255, 0, 0).draw
    - [x] Reset cursor position on return.
    - [x] Paste needs to work on movable cursor.
  - [x] Auto-wrap the text by word in TextInput objects.
  - [x] Better text typing / replacing experience / autotype clear characters
        and skip intro space.
  - [x] Visual failure messages if offline or given a cancelled request.
  - [x] Progress spinner / prevent interaction.
  - [x] @digitpain: Add conversational support.
  - [x] Tap / Escape to cancel a reply and blank the prompt.
  - [x] @ggcajp: Write a prompt program that suggests commands.
  - [x] Make the main `prompt` part of the prompt system now.

*** Overall Rendering ***
   - [] Fix pixel imperfect rendering / scaling especially noticeable on iOS.

*** General API Design ***
  - [] Add a general "positioning" function to geo that can cover TextButton,
       Button, write, and box. It should already know about the
       current screen size?
    - [] It should support: { center: "xy", top, left, bottom, right, x, y }

*** Painting ***
  - [] Two finger / scroll pan (trackpad).
  - [] Two finger / trackpad zoom.
  - (Use Figma as an example)

*** Viral Checklist ***
  - [] Display current user count on rooms if > 0 users are online.
  - [] Pool `session-server` rooms if necessary.

*** online/offline notification ***
  - [] Show `offline` overlay if the system is not online.
  - [] How can I know this?

*** `evolve` ***
  - [] Rename `vary` to `evolve`.
  - [] Research more image net APIs.
    - Transform a painting from one state to another using ML.
      (https://chat.openai.com/chat/1c0e4f20-7cb5-441f-a7e4-7b77fb55135d)
  - [x] Integrate OpenAI imageVariation.

*** `context-click` ***
  - [] Add a useful right click menu for desktop / mouse users that
       runs across all pieces.

*** `about` ***
  - [] Color code the output / perhaps even show it on the prompt?
+ Done
  - [x] Sketch a `learn` program that writes text and explains how different
        commands work.

*** `bleep` presets ***
  - [] Omnichord-like bleep presets in certain keys.

*** General Painting / Graphics API ***
  - [] Resizing window cuts off painting / interferes with "no" command?
  - [-] Fix 'system.nopaint.bakeOnLeave` `page(screen)` anomaly. (Redo "write")
  - [] Abstract the `rect = null` / `shape = null` / "bake" calls better
      in all brushes.
  - [] change resize to "painting" or resolution.
  - [] allow prefix colors for oval, shape, and line
  - [] make `coat` a new command
+ Done
  - [x] Examine "paste" not updating automatically in relation to the below.
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

*** `grocery` ***
  - [] Sampling keyboard with speech to text for grocery lists,
       with a button to cross off items, sendable from user to user.

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

*** brush ***
  - [] Write a query and get back dynamic brush code in response.

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
    - [] `camera`
    - [] `icon`
    - [] `sign`
  - [] Measure performance.
      + Done
      - [x] `shape`
      - [x] `word`
      - [x] `smear`
      - [x] `bits`
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

*** Generic Image / Photo Uploader ***
  - Data entry process
   - [] 1. go to ac and type `pic`
   - [] 2. add from camera roll or take photo
   - [] 3. receive code upon upload
   - [] 4. write code on drawing/sticker for drawing
  - Viewing process
    - [] 1. visit ac and type `code` to view the image

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

🐞 Annoying Bugs & Regressions 🪱
  - [] Alt / Meta shortcut needs to work for mac to reset pan.
  - [] Live reload: boot will not paint again if paint returns false in a contrived example
  + In Production
  - [] https://gist.github.com/lukaslihotzki/b50ccb61ff3a44b48fc4d5ed7e54303f
  - [] Zooming in on the page a lot will make the margin too large
       and squash the main display.
  + Dev Only
  - [] `sfx` do not decode in Safari when using the development servers.
       (Probably due to a local IP address or a CORS issue?)
  + Done
  - [x] Fix `spray` multiplayer / socket messages not being sent regression.
  - [x] Fix painting device rotate / resize cropping issue.
  - [x️] Fix Firefox AudioWorklet Initialization Bug `Error: Module resolve hook not set`
  - [x] Video download `local` fix.
  - [x] Favicon CORS fix (generate the icon / use a data uri)
    - [x] Get SharedArrayBuffer Working again.
  - [x] `video` In-progress transcoding does not cancel when leaving a piece.
  - [x] S3 Developer Onboarding Sync Not Working
  - [x] Rename $api.upload to $api.open? or $api.importFile?
  - [x] Research the use of "requestProvider.js.map" and work it into `index.js`.
  - [x] The back button does not work in the Instagram in-app browser.
  - [x] Jumping back to the prompt from `word` no longer stamps the word.
  - [x] Fix tap focus / unfocus regression in `type`.

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

macOS from scratch
  1. Get 'homebrew' (https://brew.sh) and install 'fnm' (nodejs)

 Redis
  0. You may need to install `redis` to get the live reload working properly
     on local.
  1. You can use homebrew for this and a default install should be fine, so
     long as `redis-cli` works.

Make sure `git` is installed, (you can do that through `homebrew`) and then get set up for development:
  0. Also install `fnm` the node version manager. (and add environtment variables to your shell: https://github.com/Schniz/fnm#shell-setup)
  0.0a Open new terminal window and cd into aesthetic.computer directory install node version
  1. Check `ssl-dev/readme.txt` to generate and add local SSL certificates.
  2. `npm install` from the project directory.
  3. `cd` into `session-server` and ask Jeffrey for Firebase json file
  4. Get added to the Netlify project as a collaborator.
  5. `cd` into `system` and run `npx netlify login`
  5a. run `npx netlify link`
  6. Now from the `aesthetic computer` directory run `npm run code` and in another shell `npm run server:session`.
  7. Visit `https://localhost:8888` to view the running site!
  8. If you need local copies of the site's rich media `assets` then ask Jeffrey
     and you can slurp them from S3 so you can run the below.

Static Assets
  0. Squadmates can use `npm run assets:sync:down` after configuring `aws-cli` with
     the aesthetic.computer storage keys provided by Jeffrey.

🧩 Making a new included piece.
- Run `npm run new name-of-your-piece Description of your piece.`
- Then open the file in `system/public/aesthetic.computer/disks` and start working!

In production: Add a "#debug" hash to the end of your URL for more verbose output.

If developing: Add a "#nodebug" hash to the end of a URL for less output.

📖 This project originally began as two separate repositories with their own
commit history: `digitpain/system` and `digitpain/disks`.
