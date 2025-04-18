### Write a Piece

Each "piece" on AC is coordinated by a single `.mjs` (JavaScript module) or
`.lisp` (a custom dialect[0]) file.

This follows Processing's "sketchbook"[1] based development model of one source
file per explorable idea, and the Unix philosophy's pattern of offering
user-level composability across small single-purpose programs.

  - Drag & Drop

    1. Enter `source` to download a bare bones `blank.mjs` piece template.  
       (Or prefix any existing piece with `source` to fork, like `source notepat`.)
    2. Open the piece template in a text editor and change `wipe("gray")` -> `wipe("blue")`.
    3. Save the file and drag it into the AC window to run it and preview the
       change.
    4. Press the piece name in the top left corner to return to `prompt`.
    5. Enter `publish`[2] to put the code online.

  - VS Code

    1. Enter `edit` to install the AC VS Code extension.
    2. Press the rectangular AC prompt icon in the VS Code sidebar to open the
       AC panel.
    3. Enter `source` to open a blank JavaScript piece template in the VS Code
       editor.
    4. Save the template to run it in the AC panel.
    5. Press the command name in the top left corner to return to `prompt`.
    6. Enter `publish`[2] to put the code online.

  - Testing

    For instant multi-device testing in studios and classrooms, AC uses a simple
    `channel` feature to broadcast incremental changes during piece development.

    1. Enter `channel custom-name` replacing `custom-name` with something unique
       on the primary development device.  
    2. Enter the same command `channel custom-name` on any additional test device.
    3. Drag & Drop piece code to AC or save the file in VS Code to update all
       devices.

---

[0] "Kid Lisp" source & samples:
     https://github.com/whistlegraph/aesthetic-computer/blob/main/system/public/aesthetic.computer/lib/kidlisp.mjs
     
[1] Processing Foundation homepage: https://processing.org

[2] Published pieces by anonymous users are temporarily uploaded to a global
    namespace using a short random identifier. Registered users publish under
    their `@handle` where the filename is used for the piece name, or can be
    overridden with `publish piece-name-here`. Find published pieces by other
    handles by entering `list @handle`.