# hUGEDriver internals

## Code style

- Comments are Lisp-style:
  - Comments on the same line as code use one semicolon.
  - Stand-alone comments within code blocks use two semicolons.
  - Stand-alone comments outside of code blocks use three semicolons.

- Every function should be preceded with a **doc comment**:
  - Doc comments use triple backticks (as per above).
  - Doc comments begin with a possibly multi-line description of the function's behavior.
  - Parameters, if any, follow, with the following format: `;; Param: (condition) WHAT = Descr.`.
    `WHAT` can be a register (`B`), a flag (`ZF`), or a memory address (`[param]`).
    (For the latter, the description should indicate the variable's size.)

    `Descr.` can be made multi-line if properly aligned (with spaces, of course) to the beginning of its first line.
    It must be proper sentences, periods included!
  - Return value(s), if any, follow, with the following format: `;; Return: WHAT = Descr.`.
    Format is essentially identical to parameters.
  - Lastly, if applicable, anything destroyed follows, with the following format: `;; Destroy: WHAT`.
    `WHAT` must be a space-separated list of things like for parameters and return values.

    Anything modified that isn't a `Return:` should be considered as destroyed.

    Specifying `F` as destroyed means that all flags are destroyed; otherwise, any flag not listed is preserved.
    All variables clobbered need not be documented, but use your best judgement.
  - Register and flag names must be uppercase.

- `hl+` and `hl-`, not `hli` and `hld`.

- Instructions using A as implicit left-hand operand (`add`, `adc`, `sub`, `sbc`, `cp`, `and`, `or`, `xor`) must not specify A.
