# kidlisp.com

Simple landing page for the KidLisp programming language with the tagline "Friendly pattern programming for all".

## Content

The site provides:
- Clear tagline: "Friendly pattern programming for all"
- Brief introduction to KidLisp
- Four simple code examples to try:
  - Basic drawing: `(wipe navy) (ink yellow) (circle 100 100 50)`
  - Repetition: `(wipe black) (ink lime) (repeat 10 i (box (* i 20) (* i 20) 50 50))`
  - Transformations: `(wipe red) (scroll 5 0) (blur 1)`
  - Random animation: `(ink cyan) (circle (wiggle width) (wiggle height) 30)`
- Call-to-action link to aesthetic.computer to try the examples

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
