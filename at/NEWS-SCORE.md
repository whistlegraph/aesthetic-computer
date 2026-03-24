# News Score

Guidelines for writing prose commit summaries on [news.aesthetic.computer](https://news.aesthetic.computer).

---

## Title

Always use: **Commits From {date} to {date}**

Use the dates of the oldest and newest commits in the batch.

## Voice

Write like Jeffrey talking to people who follow the project. Tell the story of what you worked on — the arc of a session or a day. Open with a greeting or a note about what the focus was. This is a devlog, not release notes.

**News vs. Change Blog:** News tells *what happened* — the story of the work. Deeper *why* reasoning (architectural decisions, design philosophy, project direction) belongs on [change.pckt.blog](https://change.pckt.blog) via `publish-changelog.mjs`.

## Structure

- **Tell the story.** "Spent the morning getting CL builds to stand on their own..." not "The build pipeline now separates C and CL paths." The commits are evidence — the post is the narrative.
- **Group by session, not by system.** What did you work on first? What did that lead to? What was the last thing you touched?
- **Name the right place.** Changes live in specific sub-sites and tools:
  - [aesthetic.computer](https://aesthetic.computer) — the main platform, pieces, prompt
  - [kidlisp.com](https://kidlisp.com) — KidLisp language site; Shop/Buy tab is here
  - [news.aesthetic.computer](https://news.aesthetic.computer) — news/updates
  - [prompt.ac](https://prompt.ac) — the AC prompt (also `prompt.mjs`)
  - [blank](https://aesthetic.computer/blank) — the AC laptop product
  - VS Code extension (`vscode-extension/`) — OTA status bar, build triggers
  - oven (`oven/`) — the native build server
  - session server (`session-server/`) — multiplayer, chat
  - AT Protocol / PDS (`at/`) — decentralized identity and data
- **Don't flatten everything into "the web side."** If a change is on kidlisp.com, say kidlisp.com. If it's in the VS Code extension, say that.

## Links

- **Every notable change should link to its commit** using the full GitHub URL: `https://github.com/digitpain/aesthetic-computer/commit/{hash}`
- **Link product names to their AC URLs** — `[blank](https://aesthetic.computer/blank)`, `[prompt](https://aesthetic.computer/prompt)`, etc.
- **Link external tools/libraries** when relevant — QuickJS, Claude Code, etc.
- Use markdown: `**bold**` for emphasis, `` `code` `` for technical names, `[text](url)` for links.

## Tone

- Personal, conversational. You're telling people what you've been up to.
- OK to mention what's in progress or what something is a step toward.
- Skip trivial version bumps and typo fixes unless they're part of the story.
- Save the deeper "why" for the [change blog](https://change.pckt.blog).

## Workflow

```bash
# 1. See what happened
ac-news commits --since "1 week ago"

# 2. Read the commits, group them mentally, write the prose

# 3. Dry run
ac-news post "Commits From March 17 to March 24" "Your prose..." --dry-run

# 4. Post
ac-news post "Commits From March 17 to March 24" "Your prose..."
```

Or use `--editor` to write in your editor, or `--file` to post from a markdown file.
