# Computer-use architecture review

Follow-up: priorities 1–4 are implemented on Blueberry. Native click holds have
also been swept down to zero: all 100 zero-hold trials per app passed, with
19 ms median on AppKit and 42 ms on Chrome. Resident drag dispatch, absolute
path deadlines, and explicit destination dwell are implemented. The selected
32/32 ms movement/dwell profile passed 200 verified drops; shorter candidates
failed. See [measurements and scope](COMPUTER-USE-PERFORMANCE.md).

Priority 4 uses bounded targeted AX reads; AX notification subscriptions and
priority 5's shared active capture remain future work.

September 21, 2026. Read-only web/GitHub research by a delegated research agent,
checked against our local implementation and measurements. No replacement tool
was installed. We found no verified public result establishing a drop-in tool
that delivers native clicks plus result verification below 100 ms.

## Where to remove work

| Priority | Change | Evidence and expected benefit |
| --- | --- | --- |
| 1 | Lightweight native target guard | `frame-mcp.mjs` captures pixels, walks AX, and encodes JPEG before calling `assertFrameTarget`, which compares only capture status, window ID, PID, and geometry. Query those directly, retaining checks for an unlocked/available display and consistent geometry. This removes an entire unnecessary observation. |
| 2 | Resident native input | A passive Blueberry microbenchmark measured 30.26 ms median for JXA launch + CoreGraphics import, and 78.91 ms with the current 40 ms hold delay, without sending input. Post CoreGraphics events in our running Swift helper. Test shorter down/up intervals instead of assuming zero is reliable. |
| 3 | Event-driven IPC | Swift polls `frame.req` every 30 ms; Node polls completion every 10 ms. A dedicated local socket can carry a request ID and response without these periodic waits or JPEG sidecar files. Preserve the shared machine lease and explicit unknown outcomes on interruption. |
| 4 | Verify only what changed | Subscribe before input, then read the expected AX value or browser result. Return a compact receipt and fresh state revision. Fall back to a full Frame when the app does not provide usable notifications or the result is uncertain. A notification alone is not verification. |
| 5 | Reuse capture during active work | Evaluate an active-session ScreenCaptureKit stream with a bounded latest-frame buffer. Share its frames with Captutor when capture targets agree; avoid a second screenshot request per action. Require a complete frame with a timestamp after the action. Stop the stream when idle and measure CPU/memory on Blueberry. |

The table records the original hypotheses; measured implementation results are
linked above. The earlier full native loop measured about 306 ms median with
zero fixed settling.
The [process microbenchmark](mathplay/results/blueberry-native-process-cost-2026-09-21.json)
and [full click measurements](mathplay/results/blueberry-2026-09-21.json) measure
different portions of the path; their medians should not be added or subtracted
as a precise latency prediction.

We already have warm browser/CDP connections, direct local Frame transport,
batched `AXUIElementCopyMultipleAttributeValues`, AX traversal scoped to the
captured window, concurrent pixels/AX work, optional OCR/contours, and exact-buffer
contour caching. Reimplementing these is not a new optimization. Our Swift app
also already contains AX observers, a Unix-socket implementation, and an SCStream
recorder. Reuse those patterns through a separate computer-use endpoint.

## Comparable implementations

| Source | Useful lesson |
| --- | --- |
| [macOS-harness source](https://github.com/browser-use/macos-harness/blob/main/src/macos_harness/macos.py) | Persistent Python/PyObjC and direct CoreGraphics input illustrate avoiding a process per action. Clicks still contain two 30 ms sleeps. The architecture is relevant; adoption would add another runtime without proving a faster verified loop. |
| [Peekaboo ClickService](https://github.com/openclaw/Peekaboo/blob/main/Core/PeekabooAutomationKit/Sources/PeekabooAutomationKit/Services/UI/ClickService.swift) and [SyntheticInputDriver](https://github.com/openclaw/Peekaboo/blob/main/Core/PeekabooAutomationKit/Sources/PeekabooAutomationKit/Services/UI/SyntheticInputDriver.swift) | References for binding input to snapshots/windows/processes and separating dispatch from verification. Preserve user pointer activity and do not replay an indeterminate action. |
| [Hammerspoon AX observers](https://www.hammerspoon.org/docs/hs.axuielement.observer.html) | Watch selected elements in one process rather than repeatedly scanning the whole application. Its [eventtap implementation](https://github.com/Hammerspoon/hammerspoon/blob/master/extensions/eventtap/eventtap.lua) defaults to a 200 ms click delay, so replacing our input tool with its default is not a speed upgrade. |
| [Playwright actionability](https://playwright.dev/docs/actionability) | Stability checks span two animation frames. That is compatible with, but does not prove the cause of, our roughly 33 ms browser timings. Keep visibility, event-reception, enabled-state and stability checks; `force` or synthetic `dispatchEvent` would measure different behavior. |
| [MiniWoB++](https://github.com/Farama-Foundation/miniwob-plusplus) | More than 100 small browser environments offer interaction-fixture ideas. It is in maintenance mode. Borrow a few tasks through Puppet rather than adding a Selenium stack just for timing. |
| [OSWorld-Human](https://github.com/WukLab/osworld-human) | Measures complete agent efficiency, including model calls and extra actions. Tool latency is only one budget; bounded local routines also reduce repeated model turns. Its results are not a benchmark of our machine or tools. |

Apple exposes [AXObserver callbacks](https://developer.apple.com/documentation/applicationservices/1460133-axobservercreate)
and [streamed capture](https://developer.apple.com/videos/play/wwdc2022/10156/).
Stream metadata includes [display time and frame status](https://developer.apple.com/documentation/screencapturekit/scstreamframeinfo),
which can help prevent a cached pre-action image from being mistaken for fresh
evidence. Frame cadence is not a guarantee of application response time.

## Next experiment

The resident `guard → input → expected-result` transaction and full/compact
comparison now exist. Input hold timing has now been tested down to zero. Next, investigate AX
notification support and host scheduling to reduce the remaining tail. Keep semantic browser
operations as the preferred path for known page controls.

Extend Math sprint with moving answers, disabled-then-enabled controls, overlays,
delayed results, and app/window switches. Add a tiny AppKit counter: native input
into Chrome alone does not establish native-widget performance. Record dispatch,
application acknowledgement, verified response, and total task time separately;
include cold starts, p50/p95/p99 with adequate sample counts, failures, retries,
bytes returned, CPU/memory, and window cleanup. Measure actual model/token costs
separately from the deterministic arithmetic solver.

Use the same fixture and result checks before/after each change. Sub-100 ms is a
target for warm, simple verified actions, not a promise for arbitrary application
animations, network operations, or slow accessibility providers.
