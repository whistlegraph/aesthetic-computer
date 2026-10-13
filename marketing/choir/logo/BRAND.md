# choir logo: where it goes

One master image feeds every surface. To change the logo, replace the master
(or point `MASTER` in `brand.py` at a new file), then:

```sh
python3 marketing/choir/logo/brand.py      # regenerates everything below
```

Then ship both halves:

- **Site:** compush (`ac-deploy`, then `ac-deploy --verify`).
- **App:** bump `CURRENT_PROJECT_VERSION` in `apple/choir/project.yml`, then
  build and upload. The steps are in `apple/choir/fastlane/README.md`.

## The master

`gens/beige-linen.png`. This is Fía's hand-drawn flower (her drawing IMG_8396-136,
"A4") rendered by gpt-image-2.5-flare as glossy navy puffy paint on a soft
linen beige. It's a full-bleed 1024 square with the flower large. jeffrey and Fía
picked it on 2026-10-12 after the studies in this folder. The cutout (just the
flower, transparent) is derived from it: navy paint is wherever blue exceeds red,
with the highlights filled back in.

Paper color everywhere: `#E2DBCC` (`Theme.desk` in the app, `--desk` on choir.ac).

## Surfaces `brand.py` writes

| Surface | File | Made from |
|---|---|---|
| iOS app icon | `apple/choir/Resources/Assets.xcassets/AppIcon.appiconset/icon-1024.png` | master |
| iOS launch screen | `LaunchLogo.imageset` (180 pt) + `LaunchBackground.colorset` (paper), wired in `Info.plist` `UILaunchScreen` | cutout |
| In-app logo: room header (`SingingMark`), loading spinner (`FlowerSpinner`), terms sheet | `Logo.imageset` (160 pt) | cutout |
| Small template badges (`Image("Mark")`): the @choir handle, the daily card's back | `Mark.imageset` (120 pt, drawn as a one-color silhouette) | cutout |
| Browser tab icon | `system/public/choir/favicon.png` (64 px), linked from every choir page | cutout |
| Home-screen web icon | `system/public/choir/icon.png` (512 px, `apple-touch-icon`) | master |
| Splash logo | `system/public/choir/logo.png`, breathing like the app | cutout |
| Link preview | `system/public/choir/social.png` (1200×630: flower, "choir", "Comments on culture") | cutout |
| Instagram avatar (@333_choir, posted by Fía) | `~/Desktop/choir-brand/instagram-avatar-1080.png` | master |
| Hand-offs | `~/Desktop/choir-brand/` `icon-1024.png`, `flower-cutout.png` | both |

## Not covered by `brand.py`

- The App Store listing icon comes from the app binary, so it only changes with
  a new build.
- Fía's other hand-drawn assets stay as they are: the send/reply words, the
  asterisks, `FlowerO`/`X`/`Dot`/`Small` and the daisy camera frames. They're
  her UI handwriting, not the logo.
- Instagram: send Fía the avatar. She posts it herself.

## Adding a surface

Add it to `brand.py` (one block, derived from `icon` or `flower`) and to the table
above, so the next change keeps covering it.
