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
