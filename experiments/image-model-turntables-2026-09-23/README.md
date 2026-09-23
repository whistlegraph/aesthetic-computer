# Image model latency and bird turntables

Experiment paused at the user's request on September 23, 2026. No production integration or deployment. The local imagegen skill gained explicit provider/model routing; `provider_image.py` preserves its experimental runner here.

Recraft Flash and Klein took about two seconds per saved bird sheet in this small trial. GPT Image 2 produced the strongest sheet, but took 39.5 seconds and its next request timed out. None produced a reliable 360-degree animation.

| Model / provider | Saved image time | USD per completed sheet | Observed quality |
| --- | ---: | ---: | --- |
| Recraft V4.1 Flash / OpenRouter | 2.07 s median, 3 completed | $0.007, API reported | Repeated poses; flat illustration; inconsistent rotation |
| FLUX.2 Klein 4B / fal | 2.10 s median, 3 completed | ~$0.00545, estimated | Appealing clay bird, but nine birds instead of the requested 12-cell grid |
| GPT Image 2 medium / OpenRouter | 39.53 s, 1 completed | $0.05046, API reported | Best character and grid consistency; some repeated or incorrectly ordered angles |

GPT Image 2's second request hit a 180.02-second read timeout; the third was never attempted. Charges for failed requests are unknown. Klein's estimate uses the published $0.005/megapixel rate and 1216×896 output; billing rounding was not verified. Klein reported approximately 0.40 seconds of inference, which excludes the rest of the request and transfer.

All models received the same [prompt](bird-comparison/prompt.txt), asking for a blue clay bird at 12 angles in a 4-column, 3-row sprite sheet. Recraft and Klein returned 1216×896 images; GPT returned 1536×1152. Klein used four steps, inline image delivery, and a fixed seed: its three identical outputs measure timing, not three independent quality samples. These are sequential calls on one local connection, not a controlled provider-wide ranking.

See the [comparison GIF](bird-comparison/comparison.gif), [MP4](bird-comparison/comparison.mp4), and [selected decoded frames](bird-comparison/decoded-comparison.png). Each loop uses the first completed sheet, split into the requested 4×3 grid in reading order, scaled/padded to 320×320, with 160 ms per frame. No pose repair, reordering, or interpolation. Klein's incorrect grid therefore produces visibly chopped frames. The MP4 repeats the loop four times. Source sheets, settings, and per-request measurements are in each model's folder; [comparison.json](bird-comparison/comparison.json) aggregates them.

## Earlier square and bird trials

The exact square prompt was `a square on red`, with 1024×1024 results.

| Recraft Flash route | Saved image time | Observation |
| --- | ---: | --- |
| Initial fal request | 15.01 s | 8.03 s API plus 6.98 s download/save; next call timed out after 183.20 s |
| fal with pooled HTTP/2 | 7.50 s median, 3 completed | API median 6.60 s; separate CDN download remained |
| OpenRouter with pooled HTTP/2 | 1.94 s median, 3 completed | Inline image; fastest saved result 1.53 s; $0.007 each |

The runner changed from fresh urllib requests to a shared HTTPX client with HTTP/2, connection reuse, and phase timing. Provider switching and network conditions also changed; the measured improvement cannot all be attributed to pooling. Subsecond end-to-end delivery was not observed. A separate fal diagnostic measured about 6.53 seconds to API completion and 3.32 seconds to download the image.

`square/` retains the initial failure and both three-call runs. `bird-pilot/` retains the earlier Recraft sheet and loop: 11.12 seconds including 5.87 seconds of connection setup, $0.007, with repeated angles and incorrect back views.

## Reproduction

Run from this directory. This example only prints the request; remove `--dry-run` to make paid requests. Use `OPENROUTER_API_KEY` or `FAL_KEY` from the environment, or supply `--env-file /path/to/credentials.env.gpg`. Never add credentials to this experiment.

```sh
uv run provider_image.py \
  --provider fal --model fal-ai/flux-2/klein/4b \
  --prompt "$(cat bird-comparison/prompt.txt)" \
  --params bird-comparison/klein-params.json \
  --out-dir /tmp/klein-new-run --samples 3 --timeout 180 --dry-run
```

For Recraft use provider `openrouter`, model `recraft/recraft-v4.1-flash`, and `recraft-params.json`. For GPT use provider `openrouter`, model `openai/gpt-image-2`, and `gpt-image-2-params.json`. Both parameter files are in `bird-comparison/`. The earlier fal Recraft endpoint was `recraft/v4.1/flash/text-to-image`.

The runner requires a new output directory, requests one image at a time, and stops at the first error without retrying paid POSTs. Timeout is per network phase and does not cancel server work. API and download/save times are separate; reported total starts immediately before the API call and ends after saving the image, excluding process startup and credential loading. API authorization is supplied only to the provider request, not the image CDN. Archived JSON removes request identifiers and converts local output paths to paths relative to this experiment directory. Original artifacts remain locally under `~/.codex/generated_images/` in directories ending `20260923`.

Before archiving, mocked transport checks covered both response formats, credential isolation, redirect refusal, no retry on failure, client closure, inline fal images, and seed recording. Live runs supplied the measurements above. No additional generation was performed after the stop request.

Provider references: [OpenRouter image API](https://openrouter.ai/docs/guides/overview/multimodal/image-generation), [fal Klein 4B](https://fal.ai/models/fal-ai/flux-2/klein/4b), [Recraft Flash announcement](https://www.recraft.ai/blog/meet-recraft-v4-1-flash), [GPT Image 2](https://developers.openai.com/api/docs/models/gpt-image-2). Prices are observations from this experiment date.
