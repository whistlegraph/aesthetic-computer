ste: true

# Write a Piece

A piece is one source file. The file is JavaScript (`.mjs`) or KidLisp
(`.lisp`). One file holds one idea. This is the same model as the sketchbook
of Processing.

You can write a piece in a text editor or in VS Code. Use one of the two
procedures that follow.

## Write a piece in a text editor

1. Enter `source` at the prompt.
2. AC downloads the file `blank.mjs` to your device. This file is a template.
3. Open `blank.mjs` in a text editor.
4. Change `wipe("gray")` to `wipe("blue")`.
5. Save the file.
6. Drag the file into the AC window.
7. AC runs the piece. The screen is blue.
8. Go back to the prompt.
9. Enter `publish`. AC puts the code online.

To make a copy of a different piece, enter `source` and the name of the
piece. For example, enter `source notepat`.

## Write a piece in VS Code

1. Enter `edit` at the prompt.
2. AC installs the AC extension for VS Code.
3. In the sidebar of VS Code, select the AC icon. The AC panel opens.
4. Enter `source` in the AC panel. VS Code opens a template.
5. Save the template. The piece runs in the AC panel.
6. Go back to the prompt.
7. Enter `publish`. AC puts the code online.

## Test a piece on more than one device

The `channel` command sends each change of the code to all devices on the
same channel. Use it to test a piece on phones and laptops at the same time.

1. On your main device, enter `channel` and a name. For example, enter
   `channel blue-room`.
2. On each test device, enter the same command.
3. Drag the code into AC, or save the file in VS Code.
4. AC updates the piece on all devices.

## Where AC keeps a published piece

The name of a published piece depends on your account:

- Without an account, AC gives the piece a short random name. AC keeps the
  piece for a limited time.
- With a `@handle`, AC uses the file name as the name of the piece. To use a
  different name, enter `publish` and the name.

To see the pieces of a user, enter `list` and the `@handle`.

## More information

- KidLisp: `system/public/aesthetic.computer/lib/kidlisp.mjs`
- Processing: https://processing.org
