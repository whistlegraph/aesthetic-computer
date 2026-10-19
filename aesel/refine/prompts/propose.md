# IMPROVE THE HARNESS FOR THIS KIND OF PIECE

`context/task.md` is the request — one turn per line, each a revision of the
piece the last left. `context/rollouts/base/trace.md` is the complete record
of the current harness doing it: every message, every write, every check or
review and what it found, with the frames it painted beside it.
`context/deliverables/base/` holds the piece it ended with and its last frame. `context/library/`, when
present, holds what earlier tasks of this kind taught — read its INDEX.md.

There is no answer key. Judge from the record: what the person asked for, what
the piece does, where the run went wrong or slow, and what the model did not
know. Fix the earliest divergence you can see, with the smallest change to
`context/harness/` that would have prevented it.

[[dimension]]

# GROUND IT, THEN GENERALISE IT

The edited harness is tried on this same request, and — if chosen — stored for
every later request of this kind. Write rules that would be right for any
[[kind]] piece, not for this one. A fact this record establishes about the AC
API ("write takes the typeface as its sixth argument") is worth writing down
exactly; a guess is worse than nothing. "Be thorough" changes no behaviour;
"before finishing, check that every shape stays inside screen.width and
screen.height" does.

KEEP_BASE is a real answer. If the record shows nothing the harness could have
done better, leave `context/harness/` unchanged and say so. Adaptation the
evidence does not argue for is a regression: it costs every request and buys
nothing.

Reply with two or three sentences: the divergence you found, the section you
changed, and why that change addresses it.
