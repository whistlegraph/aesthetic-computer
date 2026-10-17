# NELA login skin

The login skin for [auth.nela.computer](https://auth.nela.computer): green/cyan
type, square controls, the NCC mark, and light/dark palettes following Authentik's
theme selection. Developed for Authentik 2026.8; source recovered from the live
brand on October 16, 2026.

`login.css` is the brand's custom CSS. `branding.json` records the title, logo
pattern, and favicon path. The three SVG files are the current brand assets.

To apply to a compatible Authentik installation:

1. Save the existing brand configuration and assets for rollback.
2. Upload the SVGs to the media file manager's `branding/` directory, preserving
   their filenames. The logo pattern uses `%(theme)s` for `dark` and `light`.
3. Set the brand title, logo, and favicon to the values in `branding.json`, and
   paste `login.css` into the brand's custom CSS field.
4. Check login, recovery, keyboard focus, and mobile layouts in both themes.

These files contain presentation settings only. Keep authentication flows,
providers, credentials, and access policies in the deployment configuration.
Restore the saved brand values and assets to roll back.

[NELA source](https://git.nela.computer/jeffrey/nela-contributions) ·
[Aesthetic Computer](https://aesthetic.computer) ·
[Tangled](https://tangled.org/aesthetic.computer/core)
