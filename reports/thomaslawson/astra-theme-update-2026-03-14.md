# Astra Theme Update Assessment — thomaslawson.com

**Date:** 2026-03-14
**Assessed by:** jeffrey + Claude Code

## TL;DR

**No custom CSS was lost**, but **Elementor free was deleted** during plugin cleanup, which broke all 54 page layouts (images rendered full-size, no responsive grid, flat layout). Reinstalled Elementor free v3.35.7 via SFTP and activated it in the database — site is now rendering correctly again.

## What Happened

1. Astra theme updated to 4.12.4 — this is normal, theme files get overwritten but all customizations live in the database
2. Elementor free plugin was **deleted** from `wp-content/plugins/` (not just deactivated) — this removed the layout engine that powers all 54 pages
3. PHP version was upgraded from 5.6.40 to 8.4.18 (likely by GoDaddy)

## Fix Applied (2026-03-14)

- Downloaded Elementor free v3.35.7 (latest stable) from wordpress.org
- Uploaded to `wp-content/plugins/elementor/` via SFTP
- Activated in database by adding `elementor/elementor.php` to `active_plugins` option
- Verified: 3 Elementor JS files loading, 47 Elementor layout elements rendering on homepage

## What Survived (All Customizations)

| Item | Storage | Status |
|------|---------|--------|
| Customizer Additional CSS (`.rule_1`, `.rule_2`, `hr` styles) | Database (`wp_posts`, ID 231) | Intact, rendering on pages |
| Astra header layout (logo + nav) | Database (`theme_mods_astra`) | Intact: `html-2 + logo (left), html-1 (center), menu-1 (right-center)` |
| PRIMARY menu (Home, About, Contact) | Database (`nav_menu_locations`) | Assigned to `primary` + `mobile_menu` (menu ID 3) |
| Custom fonts (Gotham, Gotham Italic, Felix Titling) | Database + uploaded `.ttf` files in `wp-content/uploads/` | Loading correctly |
| Elementor page layouts (54 pages) | Database (post meta) | Elementor free v3.6.7 still active and rendering |
| Background color (`#fff9ef`) | Database (`astra-settings`) | Intact |
| Logo image | Database (`custom_logo`, ID 327) | Rendering |
| Transparent header disabled | Database (`theme_mods_astra`) | Still disabled |

## Observations

### PHP Version Upgrade: 5.6.40 → 8.4.18

This is a **major** change — much bigger than the theme update. PHP 5.6 reached end-of-life in December 2018. The jump to 8.4 is 6 major versions forward. This likely happened as part of a GoDaddy hosting panel update (not directly from the Astra update).

**Risk:** Some older plugins or custom code may not be compatible with PHP 8.4. The site is currently loading without visible errors, but WordPress is reporting a "critical error" when loading `wp-config.php` in certain contexts (the WordPress bootstrap failed when running standalone PHP probes). This suggests there may be deprecation warnings or errors being suppressed in production.

### Elementor: Reinstalled as v3.35.7

Previously v3.6.7 (March 2022). The plugin was fully deleted from disk during cleanup. Reinstalled with latest stable v3.35.7 which is compatible with PHP 8.4.

### Active Plugins (5 after fix)

- `better-search-replace`
- `custom-fonts`
- `custom-typekit-fonts`
- `filebird`
- `elementor` (v3.35.7, reinstalled 2026-03-14)

### `custom_css_post_id` Set to -1

The `theme_mods_astra` option has `custom_css_post_id => -1`, which typically means "no custom CSS." However, WordPress is still serving the custom CSS from post ID 231 — this appears to be a WordPress core behavior where it looks up `custom_css` posts by post_type regardless of the theme_mod value. This is benign but worth noting.

## Recommendations

1. **Verify all pages visually** — Elementor is reinstalled but spot-check key pages for layout correctness
2. **Take a fresh backup** — given the PHP version jump + plugin reinstall, good to have a snapshot of the current working state
4. **Monitor for errors** — check `wp-content/debug.log` (if WP_DEBUG is enabled) or GoDaddy error logs for PHP 8.4 deprecation notices
5. **Fix `custom_css_post_id`** — optionally update `theme_mods_astra` to set `custom_css_post_id => 231` so WordPress properly links the custom CSS to the theme (low priority, currently working)

## How to Verify Visually

Check these pages for correct rendering:
- Homepage: https://www.thomaslawson.com/
- About: https://www.thomaslawson.com/about/
- Contact: https://www.thomaslawson.com/contact/
- In the Studio: https://www.thomaslawson.com/in-the-studio/

Look for:
- Header with logo (left) and navigation (right)
- Correct background color (#fff9ef, warm cream)
- Custom fonts (Gotham/Poppins)
- Horizontal rule styling (dark gray, 1.5px)
- Elementor page layouts loading (galleries, headings, dividers)
