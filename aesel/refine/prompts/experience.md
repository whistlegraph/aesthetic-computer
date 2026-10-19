# WHAT THIS ROUND TAUGHT

You are reading the complete record of one improvement round on a
[[kind]] request: the base harness (`context/harness/`), the candidates
proposed against it (`context/pool/<label>/harness/`, with the editors' notes),
the run each produced (`context/rollouts/<label>/trace.md`), and which piece was finally chosen
(`context/chosen.md`). Write two things for a library that later rounds of this
kind read before they make their own edits.

Selection compared pieces with the harnesses hidden, so the choice says which
piece was best — not which edit worked. Treat every candidate as evidence. An
edit only in a rejected candidate may still have changed behaviour for the
better; an edit in the chosen one may have contributed nothing.

## WHAT EACH EDIT DID

For each section any candidate changed, one row: the section, what the edit
changed, the difference it made in the run, and a verdict — `helped`, `hurt`,
`no observed effect` or `confounded`. Prefer `confounded` whenever a candidate
changed several sections and no run isolates one of them, or a single run is
all that stands behind the difference. A confounded row is a useful record; an
invented attribution poisons the library for every later round. Cite the point
in the run where behaviour diverged.

## THE ONE LESSON THAT TRANSFERS

Then at most one lesson, and only if the rows support it. The `cue` is the
shape of situation it applies to, so it can fire on a different request of
this kind. The `lesson` is the operation to perform on the harness, naming the
section. Too generic is useless ("be careful with layout"); too specific never
fires again ("the snake was 12 px"). A lesson resting only on confounded rows
is a guess: narrow it to what the record shows, or decline by writing the
single line NO_ENTRY in place of the <experience> block.

## OUTPUT FORMAT

<edits>
section: <name as it appears under ## in the harness>
candidates: <labels that carried this edit>
change: <what the edit did>
observed: <the difference in the run, and where you saw it>
verdict: <helped|hurt|no observed effect|confounded>
(repeat these five lines for each changed section)
</edits>

<experience>
cue: <the shape of situation this applies to>
lesson: <the operation on the harness, naming the section>
evidence: <the rows this rests on, and the run contrast>
confidence: <high|medium|low, and why>
</experience>
