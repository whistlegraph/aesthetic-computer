# Headless Renderer Regression Tests

This directory contains lightweight checks for the offline headless pipeline.

## Running the tests

```fish
cd /workspaces/aesthetic-computer/reference/tools/recording/tests
node headless-first-line-color.test.mjs
```

The test harness renders a couple of tiny frames via `FrameRenderer` and asserts
that a KidLisp program with a leading color atom wipes the buffer exactly once.
Temporary artifacts are written to your system temp directory and removed after
successful completion.
