# Norman Lewis — October 7, 2026

Publishes `/art-in-context-norman-lewis/`, adds the exhibition between REALLIFE
and Pat Douthwaite in Art in a Broader Context, and includes it in site search.
Tom's supplied introduction is unchanged. The page links the 11-page catalogue
and displays the three installation images at no more than their native width.

```sh
node build.mjs /private/norman-lewis-source /private/norman-lewis-build
php -l /private/norman-lewis-build/zzzzzzz-tl-norman-lewis.php
```

Upload the supplied catalogue and installation images plus a rendered catalogue
cover to `wp-content/uploads/tl-refresh/norman-lewis-1976/` before installing
the built module in `wp-content/mu-plugins/`. Verify HTTP file hashes, the page,
index order and site search on desktop and phone. The module follows the site's
existing virtual-page pattern and changes no WordPress records. Removing it
rolls back the page and index/search additions. Source documents, deployment
receipts and browser evidence stay in the private client vault.
