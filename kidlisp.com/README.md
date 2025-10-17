# kidlisp.com

Static landing page for the KidLisp programming language.

## Deployment

This site is deployed via Netlify from the main `aesthetic-computer` repository.
The domain `kidlisp.com` redirects to serve this content from `system/public/kidlisp-com.html`.

## Netlify Configuration

The routing is configured in `system/netlify.toml`:
- `kidlisp.com` domain redirects are handled there
- Static content is served from the system/public directory

## Local Development

To preview locally, you can open `index.html` directly in a browser or run:
```bash
cd kidlisp.com
python3 -m http.server 8000
```

Then visit http://localhost:8000
