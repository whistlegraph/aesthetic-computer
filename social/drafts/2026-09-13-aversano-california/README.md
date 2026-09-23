# Fondazione Aversano — California

Target: https://x.com/justinaversano/status/2095871975876972564  
Deadline: September 23, 2026  
Offer: acquisition from one artist in each U.S. state, up to $1,000

## Work

`$ngb` by Jeffrey Scudder (@whistlegraph), Los Angeles, California.

Live work: https://aesthetic.computer/$ngb

KidLisp source:

```lisp
black
ink (? white black black black blue red)
repeat 100 line
zoom (+ 1.01 amp*2) 0 0
zoom 0.5
contrast 0.6
blur 5
contrast 2
zoom 4
sharpen 5
scroll frame*5 frame*2
```

The work is public but not yet minted. Offer it as a new 1/1 KidLisp Keep on
Tezos if selected. Do not mint before the acquisition terms and recipient
wallet are agreed.

## Reply

> California — Jeffrey Scudder (@whistlegraph), Los Angeles. $ngb is a live KidLisp work made in Aesthetic Computer, available to mint as a 1/1 Tezos Keep: https://aesthetic.computer/$ngb

Media: `ngb.png`

Alt text: A square frame from $ngb: translucent blue, violet, gold, and white
rectangles recursively overlap into a luminous digital grid.

## Publish gate

Run the dry draft first. Publish only after Jeffrey explicitly approves this
exact text and image:

```bash
node toolchain/x/x.mjs --as promptdotac reply \
  https://x.com/justinaversano/status/2095871975876972564 \
  'California — Jeffrey Scudder (@whistlegraph), Los Angeles. $ngb is a live KidLisp work made in Aesthetic Computer, available to mint as a 1/1 Tezos Keep: https://aesthetic.computer/$ngb' \
  --media social/drafts/2026-09-13-aversano-california/ngb.png \
  --alt 'A square frame from $ngb: translucent blue, violet, gold, and white rectangles recursively overlap into a luminous digital grid.' \
  --dry-run
```

California did not appear among the announced acquisitions found on September
13, but the state may be claimed without appearing in search. Recheck the
thread immediately before publishing.

## Attempt

The approved API reply was attempted on September 13. X accepted the media
upload but rejected the reply with HTTP 403: self-serve API accounts may reply
only when the original author has mentioned or quoted the replying account.
No post was created. Make this reply manually in X; do not work around the
restriction with browser automation.

## Manual reply

Jeffrey reported the reply posted manually on September 13. Treat this as
submitted; confirm its public URL before the September 23 deadline and record
any response from the foundation.
