# Stable slideshow arrows

Published October 6, 2026. The homepage artwork frame now depends on the
viewport, not the current image's aspect ratio or caption height. A two-column
caption row keeps the arrows in the same position when captions wrap.
Images remain uncropped; swipe, keyboard and automatic advance are preserved.

Build against a fresh copy of the live `zzz-tl-quality.php`:

```sh
node build.mjs live-backup.php zzz-tl-quality.php
php -l zzz-tl-quality.php
```

The older `work-quality-2026-10-05/` source predates subsequent production
changes. This patch replaces only its slideshow sizing function and appends
the control layout CSS. Do not deploy that older build over production.

Deployment replaces `wp-content/mu-plugins/zzz-tl-quality.php` through a staged
SFTP upload and rename. Verified readback SHA-256:
`7fd69292e967d49b5f35c7924c236621f0fcf0da2983470b07178128a807337b`.
Rollback restores the pre-change copy of that file.

Preview and production browser checks cover all 24 artworks at 320×568,
390×844, 768×1024 and 1440×900, including exact arrow positions, wraparound,
previous/next, keyboard navigation, image loading and horizontal overflow.
Animated transitions and automatic advance retain the same control positions.
Rendered portrait, landscape and panoramic views were inspected.
Backups, screenshots and measurements are in the private client vault at
`site-refresh-2026-09/arrows-2026-10-06/`.
