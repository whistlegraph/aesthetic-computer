# nela-outline

An optional NELA Computer Club theme for [Outline](https://github.com/outline/outline):
retro initials avatars, NELA icons and styles, styled sidebar and document titles,
and a Preferences toggle saved in `UiStore`, with tests.

Kept here as patches against upstream so Outline itself stays out of this repo.
Part of [Aesthetic Computer's NELA contributions](../docs/nela-contributions.md),
with source on [Tangled](https://tangled.org/aesthetic.computer/core) and a
contribution repository on the [NELA forge](https://git.nela.computer/jeffrey/nela-contributions).

The v1.9.2 theme was deployed on October 3, 2026. NELA is the default skin;
Outline remains selectable in Preferences. Each account's skin choice is stored
in its browser, separately from Light / Dark / System appearance.

`patches/v1.9.2/0002` adds Open Graph link previews, so wiki links shared in
Signal, iMessage or Slack show a NELA card instead of "Outline" and the stock
blurb. Every page gets `og:` and `twitter:` tags; published shares get a
1200×630 card drawn with the share's title, and everything else gets the site
card. Private document titles are never put on a card. Set `OG_SITE_NAME` and
`OG_DESCRIPTION` in the deployment environment. Unlike the theme, this patch
changes server code.

| Patch | Applies to |
|---|---|
| `patches/v1.9.2/` | the `v1.9.2` release tag |
| `patches/main/` | upstream `main` at `c775aaf43c` |

```sh
git clone https://github.com/outline/outline.git && cd outline
git checkout v1.9.2
git am ../aesthetic-computer/nela-outline/patches/v1.9.2/*.patch
```

The `main` patch goes on with `git checkout c775aaf43c` instead, and may need
rebasing onto newer upstream.

The [deployment recipe](deploy/) preserves the frontend-only image build used
for the v1.9.2 release.
