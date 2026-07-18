# status.prompt.ac

A Cloudflare Worker satellite for Aesthetic Computer. It checks the public front
door, API abstraction, a database-backed read, DigitalOcean asset delivery, and
DigitalOcean Spaces upload signing. Per-handle profile/portfolio/object checks
are an opt-in diagnostic rather than part of the public system summary.

```fish
cd status-prompt
npx wrangler dev
npx wrangler deploy
```

After deployment, attach `status.prompt.ac` as the Worker's custom domain. For
30-day snapshots, create a KV namespace named `STATUS_HISTORY`, add its binding
to `wrangler.toml`, and redeploy. The dashboard is HTML; `/api/status` is the
machine-readable surface. `?handle=@name` runs the portfolio checks for another
user without exposing private credentials or performing a write.
