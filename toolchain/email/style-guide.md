# Email Style Guide

default_signature: @jeffrey
force_lowercase: true
append_signature_if_missing: true

## Defaults

- keep subjects and body copy all lowercase by default
- sign emails as `@jeffrey`
- append signature automatically if missing

## Overrides

- set `preserve_case: true` in `mail_send` when casing must be preserved
- set `signature` in `mail_send` to override the default sign-off for a specific email
