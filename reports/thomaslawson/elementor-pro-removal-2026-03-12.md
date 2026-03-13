# Elementor Pro Removal — thomaslawson.com

**Date:** 2026-03-12
**Performed by:** jeffrey + Claude Code

## Summary

Removed Elementor Pro and 4 unused addon plugins from thomaslawson.com.
The site only used Pro for nav-menu widgets in 3 Theme Builder header templates — Astra's built-in header builder handles this natively.

## What Changed

### Plugins Deactivated (11 → 6 active)
- `elementor-pro` — Pro features were only used for nav-menu widget (3 headers) and Theme Builder templates (3 headers + 1 footer)
- `addon-elements-for-elementor-page-builder` — unused
- `advanced-image-hover-effect-for-elementor` — unused
- `image-hover-effects-addon-for-elementor` — unused
- `image-hover-effects-ultimate` — unused

### Remaining Active Plugins
- `elementor` (free) — still powers all 54 page layouts
- `better-search-replace`
- `custom-fonts`
- `custom-typekit-fonts`
- `filebird`
- `limit-login-attempts-reloaded`

### Theme Builder Templates Disabled
- Header #633 (applied to page 10 / Home)
- Header #639 (applied globally, excluding pages 10 & 68)
- Header #644 (applied to page 68 / About)
- Footer #743 (applied globally)

Conditions were cleared (not deleted) — templates still exist in the DB if needed.

### Astra Configuration
- Transparent header disabled (`astra-settings` option + theme_mod)
- Original header layout restored: `html-2` + logo (left), `html-1` divider (center), `menu-1` PRIMARY menu (right-center)
- PRIMARY menu (Home, About, Contact) assigned to Astra's `primary` + `mobile_menu` locations
- Elementor + Astra CSS caches cleared

## Cost Savings

~$59–69/year (Elementor Pro single-site plan). Estimated $250–350 paid over the life of the site.

## Backup

Full pre-migration backup at `gigs/thomaslawson.com/backups/2026-03-12/` (1.7GB):
- MySQL dumps (2 files)
- wp-config.php
- Complete wp-content directory

## Rollback

If needed:
1. Re-upload `elementor-pro` plugin folder from backup via SFTP
2. Restore Theme Builder conditions via PHP probe (values documented above)
3. Re-enable transparent header in `astra-settings` option

## Verification

- Homepage: header + footer rendering correctly
- /in-the-studio/: no overlap, content properly spaced below header
- /about/: navigation working, content loading
- All changes made via SFTP + PHP probes (no wp-admin session needed)
