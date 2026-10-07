# October closeout

Adds the Portrait of New York film, publication metadata on the six News cards,
and a REALLIFE 15 link to its existing issue details and contents.

`node build.mjs` builds `zzz-tl-closeout.php` for
`wp-content/mu-plugins/`. Removing that file rolls back this pass. It changes no
WordPress records and preserves the deployed design-refresh v1.8.0 (this
checkout's older v1.6.1 build must not be deployed over it).

Deployed October 4, 2026. SFTP read-back SHA256:
`f21d209c9422fa2bc82d8e84e9a7c00730b2256b0d511382c0445b191a92825f`.
PHP and JavaScript syntax checks pass. Production checks at 1440px and 390px
verify six News cards with three metadata lines each, the film preview and
play-to-iframe interaction, REALLIFE 15 destinations, and no horizontal overflow.
Rendered desktop News and phone film views were inspected. Vimeo's player
rejects this test connection, so actual playback is not verified; the direct
Vimeo link remains available. About's existing image viewer and Escape were
also checked; this pass does not replace it.

## Sources

- Portrait film: [Thomas Lawson on Vimeo](https://vimeo.com/1216482818),
  supplied by Fía on September 30. Vimeo oEmbed confirms title and author.
- Law & Order: Tom's August 7 studio note, forwarded by Fía September 30
  (October 1 UTC), with the photograph already installed on the site. The date
  labels Tom's note, not the unidentified episode's broadcast.
- Rabkin: [foundation interview](https://rabkinfoundation.substack.com/p/2024-rabkin-prize-winner-thomas-lawson),
  Mary Louise Schumacher, October 23, 2024.
- Sunny and Warm: [gallery exhibition page](https://www.chezmaxdorothea.com/exhibitions/sunny-and-warm).
  Its header and prose disagree on exact days; use January–February 2025.
  Thomas Lawson is the exhibiting artist, not an asserted press-release author.
- Studio Reader: [publisher](https://press.uchicago.edu/ucp/books/book/chicago/S/bo8725125.html),
  verified against the title page of the site's PDF; editors Mary Jane Jacob
  and Michelle Grabner, University of Chicago Press, 2010.
- Attending to the Bats: [Vimeo record](https://vimeo.com/406160470), uploaded
  by Fellowship on April 10, 2020. This credits the uploader and platform; it
  does not assert a filmmaker or filming date.
- Anthology for Unseen: [title page and colophon](https://www.thomaslawson.com/wp-content/uploads/2024/01/Anthology-for-Unseen-title-page-scaled.jpg),
  editors Amanda Bauer and Ruoyi Shi, R+A Editions, copyright 2023–24.
- REALLIFE 15: [Tom's legacy issue page](https://www.thomaslawson.com/REALLIFE_15.html),
  containing the cover credit and contents. A full issue scan remains unavailable.

The private client todo, commercial terms and email handoff remain in the vault
at `gigs/thomaslawson.com/site-refresh-2026-09/TODO.md`.
