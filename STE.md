# STE — Simplified Technical English for AC Docs

This file gives the rules for AC documents that follow ASD-STE100. ASD-STE100
is the Simplified Technical English standard of ASD (the Aerospace,
Security and Defence Industries Association of Europe). The standard has two
parts: writing rules and a dictionary of approved words.

This file is an AC summary of the writing rules. It is not a copy of the
standard. The full dictionary is in the ASD specification. To get the
specification, go to https://www.asd-ste100.org.

`papers/VOICE.md` controls the voice of essays and papers. This file controls
procedures, references and help text. Use STE when a reader must do a task
correctly the first time.

## Which documents use STE

These documents use STE:

- `docs/ste/user-guide.md`
- `docs/ste/write-a-piece.md`
- New procedures, help text and reference pages that have a `ste: true` line
  at the top

Essays, papers, `STORY.md` and commit messages do not use STE.

## Check a document

Run this command:

```bash
npm run ste -- docs/ste/user-guide.md
```

The checker counts words, sentences and some forbidden forms. It cannot
check meaning. A clean report does not prove that a document is STE. Read
the document after the checker gives no findings.

Use `--strict` to make the command fail when it finds a problem. Use `--json`
to get the findings as data.

## Words

1. Use only approved words. Use each word only with its approved meaning.
2. Use one word for one thing. If you call it a "piece", do not call it a
   "program" or a "sketch" in the same document.
3. You can use technical names. In AC, the names of pieces and commands are
   technical names: `notepat`, `prompt`, `publish`, `channel`. Put them in
   code format.
4. You can use technical verbs for actions that have no approved verb. Use
   the approved verb when one exists.
5. Do not use contractions. Write "do not", not "don't".
6. Do not use a noun cluster of more than three nouns. Change
   "piece source file download link" to "the link to the source file of the
   piece".
7. Use articles ("the", "a", "an") and the word "that" where English
   grammar uses them. Do not remove them to make a sentence shorter.

The checker knows these unapproved words. Use the approved word on the right.

| Do not use | Use |
| --- | --- |
| may, might, could | can (ability), must (obligation) |
| should, shall | must, or an instruction |
| utilize, leverage | use |
| via | through, with |
| ensure | make sure |
| allow, enable | let |
| obtain | get |
| additional | more |
| approximately | about |
| prior to | before |
| please, just, simply, easily, very | (remove the word) |
| e.g., i.e., etc. | for example, that is, (the full list) |

## Verbs

1. Use only these tenses: the simple present, the simple past and the simple
   future. Write "the piece starts", not "the piece is starting".
2. Do not use the -ing form of a verb. Write "to record a tape", not
   "recording a tape". You can use an -ing word that is a technical name or
   a noun, for example "painting" or "setting".
3. Use the past participle only as an adjective ("a published piece") or in
   a passive verb in a description.
4. Use the active voice in procedures. In descriptions, use the active voice
   as much as possible.
5. Do not use a modal verb other than "can" and "must".

## Sentences

1. Write one topic in each sentence.
2. A sentence in a procedure has a maximum of 20 words.
3. A sentence in a description has a maximum of 25 words.
4. A paragraph has a maximum of six sentences.
5. Start a description with a sentence that gives its topic.
6. Use a vertical list to show a series of items.
7. Code, URLs and technical names count as one word.

## Procedures

1. Write an instruction in the imperative: "Enter `notepat`."
2. Write one instruction in each sentence. Write two instructions in one
   sentence only when the reader must do the two actions at the same time.
3. Put a condition before the instruction: "If the prompt is not open,
   press the top-left corner."
4. Write the result of an action in a new sentence, after the instruction.
5. Give each step a number.

## Warnings and cautions

1. Use a warning for a risk to a person. Use a caution for a risk to data or
   equipment.
2. Start a warning or a caution with an instruction.
3. Then give the reason in a separate sentence.

> **CAUTION:** Save your painting before you enter `new`. The `new` command
> removes the current painting.

## Punctuation

1. Use a colon only to introduce a list or code.
2. Do not use a semicolon. Write two sentences.
3. Do not use an em dash in an STE document. Write two sentences, or use
   a comma.

## Add a word to AC STE

1. Make sure that the ASD dictionary does not have an approved word for the
   same thing.
2. Add the word to the list in this file.
3. Add the word to `ING_OK`, `TECHNICAL` or `UNAPPROVED` in
   `toolchain/ste/ste-check.mjs`.
