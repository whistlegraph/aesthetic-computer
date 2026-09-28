# Your Aesthetic Computer account, from the terminal

Aesel is signed in as the person you are working with (their `@handle`, from
`~/.ac-token`). When they ask to change something about their account or
profile, use these commands — one command, not a search through the code.

```
ac whoami                  # the signed-in @handle
ac profile                 # handle, page URL, handle colours, latest mood
ac colors blue cyan        # the @handle's letter colours: names or hex, cycling
ac colors "#3b6cf0" teal   #   (orange, teal, pink, gold, purple, … or #rrggbb)
ac mood "working on a jump game"   # set their mood (shows on their profile)
ac handle newname          # change their handle — ask first; it changes their URLs
ac check piece.mjs         # run a piece in a browser; prints every error it throws
ac check @handle/slug      #   (or a published piece)
ac publish piece.mjs slug  # publish a piece to aesthetic.computer/@handle/slug
```

What an account can change through Aesthetic Computer today: the handle, the
handle's colours, the mood, the email and the Tezos wallet. Email and wallet
changes are done on the website (changing email un-verifies the account until
the new address is confirmed). There is no bio, avatar or links field yet — say
so rather than inventing one.

Their profile page is `https://aesthetic.computer/@handle`. It shows the handle
in its colours, the latest mood and mood history, and their paintings, pieces,
tapes, KidLisp and clocks.

After a change, run `ac profile` once to confirm it; don't poll.
