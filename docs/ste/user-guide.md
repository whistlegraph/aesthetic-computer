ste: true

# Aesthetic Computer User Guide

Aesthetic Computer (AC) is a runtime and a social network for creative
programs. AC works best on a phone. You use AC like a musical instrument.
You learn paths through commands and pieces, and you remember them.

A piece is a small program on AC. Each piece has a name. Some pieces are
part of AC. Other pieces come from users with a `@handle`.

## Start AC

1. Go to https://aesthetic.computer.
2. Press the top-left corner of the screen. Or, push a key on the keyboard.
3. The prompt opens.

## Go back to the prompt

Do one of these actions:

- Press the name of the piece in the top-left corner.
- Press the back button of the browser.
- Push `Esc`, `` ` `` or `Backspace` on the keyboard.

## Find pieces

1. Enter `list` at the prompt.
2. A list of pieces and commands opens. Scroll the list.

You can use most of AC without an account. Some pieces, for example `laklok`
and `moods`, need a `@handle` before you can post.

Each piece has a URL. For example, the URL of `notepat` is
https://aesthetic.computer/notepat. Save this URL as a bookmark to open the
piece directly.

## Share a piece

1. Enter `share` and the name of the piece. For example, enter
   `share notepat`.
2. AC shows a QR code.
3. Scan the QR code with a different device to open the same piece.

## Make a painting

> **CAUTION:** Save your painting before you enter `new`. The `new` command
> starts a new painting and removes the current one.

1. Enter `new 128`.
2. AC makes a new painting. The painting is 128 by 128 pixels.
3. Enter `rect red`.
4. Drag on the screen. AC paints red rectangles. You can use the name of any
   CSS color.
5. Go back to the prompt.
6. Enter `smear`.
7. Drag on the screen. The brush moves the pixels in random directions.
8. Go back to the prompt.
9. Do one of these steps:
   - To save a PNG file on your device, enter `dl`.
   - To put the painting on the AC servers, enter `done`.
   - To get a sticker of the painting by mail, enter `print`.

Other brushes are `line`, `shape` and `fill`.

## Play a melody

1. Enter `+`. A second prompt window opens.
2. In the second window, enter `metronome 120`. The metronome plays 120 beats
   each minute.
3. In the first window, enter `notepat`.
4. Play notes in time with the metronome.

For a grid of random tones, enter `bleep:sine 4x4`.

### Record a video

1. Enter `tape notepat`.
2. Play `notepat`. AC records seven seconds of video.
3. AC shows the video as a loop. You can download the video.

## Join laklok

`laklok` is the chat room of AC. You must have a `@handle` to post in
`laklok`.

1. Enter `signup` at the prompt. You can also enter `imnew`.
2. Type the `@handle` that you want.
3. Type your email address.
4. Open the email from AC.
5. Type the six-digit code from the email.
6. AC opens `laklok`. If you started from a piece, AC opens that piece.

You can read `laklok` without an account at https://laklok.com.

## Get help

1. Enter `help` at the prompt.
2. Type your question.
3. The question goes to `@jeffrey` in a public chat.

## More information

- The history of AC is in `STORY.md`.
- To write a piece, read `docs/ste/write-a-piece.md`.
- KidLisp is a small Lisp language for generative art on AC. The KidLisp
  documents are in `kidlisp/`.
