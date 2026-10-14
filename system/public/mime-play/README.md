# MIME piece runtime

`/mime-play/?code=wg…` runs a published Whistlegraph piece; `?piece=@handle/name`
runs a registered AC piece. The wrapper warms, pauses and resumes the same runtime.

MIME supplies the viewer’s public `@handle` through `mime:viewer` messages from
its parent frame. The wrapper checks the sender and origin. Native MIME calls
`__mimeSetViewer(handle)` on this same wrapper. Neither path sends an access
token, account ID or email. A missing handle or sign-out becomes `null`.

Pieces read this display identity with the existing `handle()` API. It grants
no account permissions and does not populate `user`, authorization or storage.
Changes request another paint. The example in
`system/tests/fixtures/mime-viewer.mjs` displays the viewer, not the author.

`noauth=true&mime=1` disables AC’s global prompt navigation shortcuts while
leaving key events available to `act()`. The embedding flag survives AC’s URL
rewrites. The feed’s edge controls own navigation.
