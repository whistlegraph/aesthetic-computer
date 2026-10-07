# Art School introduction — October 7, 2026

Replaces the generic introduction on `/elementor-1878/` with Tom's paragraph
supplied by Fía on October 7. Document links, images and layout stay intact.

Build against a fresh production backup of `zzzzz-tl-followup.php`:

```sh
node build.mjs /private/path/live-before.php /private/path/followup.php
php -l /private/path/followup.php
node verify.mjs /private/evidence/path --preview
node verify.mjs /private/evidence/path --live
```

The patch changes only the Art School introduction string. Backups, deployment
receipts, correspondence and browser evidence belong in the client vault.
