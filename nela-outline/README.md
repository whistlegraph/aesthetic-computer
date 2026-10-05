# nela-outline

An optional NELA Computer Club theme for [Outline](https://github.com/outline/outline):
retro initials avatars, NELA icons and styles, styled sidebar and document titles,
and a Preferences toggle saved in `UiStore`, with tests.

Kept here as patches against upstream so Outline itself stays out of this repo.
It can move to its own repo later.

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
