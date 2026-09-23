#!/usr/bin/env python3
# /// script
# requires-python = ">=3.10"
# dependencies = ["httpx[http2]>=0.28,<0.29"]
# ///
"""Explicit fal/OpenRouter image requests with per-sample latency records."""
import argparse
import base64
import datetime
import json
import mimetypes
import os
from pathlib import Path
import statistics
import subprocess
import time
import urllib.error
import urllib.parse
import urllib.request


def credential(name, env_file):
    if os.environ.get(name):
        return os.environ[name]
    if env_file:
        path = Path(env_file).expanduser()
        if path.suffix == ".gpg":
            result = subprocess.run(["gpg", "--batch", "--quiet", "--decrypt", str(path)],
                                    capture_output=True, text=True, timeout=30)
            if result.returncode:
                raise ValueError("Cannot decrypt credential file; unlock the local GPG agent")
            contents = result.stdout
        else:
            contents = path.read_text()
        for line in contents.splitlines():
            key, sep, value = line.removeprefix("export ").partition("=")
            if sep and key.strip() == name:
                return value.strip().strip("\"'")
    raise ValueError(f"Missing {name}; configure locally, never paste it into chat")


def request(client, url, timeout, body=None, headers=None, metrics=None):
    """Reuse HTTP connections; measure phases without recording request secrets."""
    metrics = metrics if metrics is not None else {}
    started = time.perf_counter()
    phases = {}

    def trace(event, info):
        phase, _, state = event.rpartition(".")
        if phase not in ("connection.connect_tcp", "connection.start_tls"):
            return
        if state == "started":
            phases[phase] = time.perf_counter()
        elif state in ("complete", "failed") and phase in phases:
            metrics[phase.rsplit(".", 1)[1] + "_seconds"] = time.perf_counter() - phases[phase]

    try:
        with client.stream("POST" if body is not None else "GET", url,
                           content=body, headers=headers or {}, timeout=timeout,
                           extensions={"trace": trace}) as response:
            metrics["response_headers_seconds"] = time.perf_counter() - started
            metrics["http_version"] = response.http_version
            metrics["status_code"] = response.status_code
            if response.status_code >= 400:
                raise urllib.error.HTTPError(url, response.status_code, "Provider HTTP error", {}, None)
            if response.is_redirect:
                raise ValueError("Unexpected redirect; verify endpoint before resubmitting")
            data = response.read()
            metrics["body_seconds"] = time.perf_counter() - started - metrics["response_headers_seconds"]
            return data, response.headers
    finally:
        metrics["total_seconds"] = time.perf_counter() - started


def payload_for(args):
    extra = json.loads(Path(args.params).read_text()) if args.params else {}
    if not isinstance(extra, dict):
        raise ValueError("Parameters must be a JSON object")
    if any(k in extra for k in ("prompt", "model", "stream", "n", "num_images")):
        raise ValueError("Pass prompt/model as flags; this runner requests one image per sample")
    if args.provider == "fal":
        return {**extra, "prompt": args.prompt}
    return {**extra, "model": args.model, "prompt": args.prompt, "n": 1}


def run(args):
    if args.samples < 1 or args.timeout <= 0:
        raise ValueError("samples and timeout must be positive")
    if not args.model or any(x in args.model for x in ("..", "?", "#", ":")) or args.model.startswith("/"):
        raise ValueError("Use an exact provider model ID, not a URL")
    payload = payload_for(args)
    endpoint = ("https://fal.run/" + args.model if args.provider == "fal"
                else "https://openrouter.ai/api/v1/images")
    if args.dry_run:
        print(json.dumps({"endpoint": endpoint, "body": payload, "samples": args.samples}, indent=2))
        return
    try:
        import httpx
    except ImportError:
        raise ValueError("Run with uv run provider_image.py to load its HTTP/2 dependencies") from None
    key = credential("FAL_KEY" if args.provider == "fal" else "OPENROUTER_API_KEY", args.env_file)
    headers = {"Content-Type": "application/json", "Authorization":
               ("Key " if args.provider == "fal" else "Bearer ") + key}
    out = Path(args.out_dir).expanduser().resolve()
    out.mkdir(parents=True, exist_ok=False)
    report = {"provider": args.provider, "model": args.model, "request": payload,
              "started_at": datetime.datetime.now(datetime.timezone.utc).isoformat(),
              "transport": "pooled HTTP/2 when supported; no automatic retries; sequential samples",
              "samples": []}
    body = json.dumps(payload).encode()
    # Keep authorization per request so it can never reach the image CDN.
    client = httpx.Client(http2=True, follow_redirects=False,
                          limits=httpx.Limits(keepalive_expiry=60))
    try:
        for i in range(args.samples):
            row = {"sample": i + 1, "api_network": {}}
            start = time.perf_counter()
            report["samples"].append(row)
            try:
                raw, response_headers = request(client, endpoint, args.timeout, body, headers, row["api_network"])
                api_done = time.perf_counter()
                row["api_seconds"] = api_done - start
                print(json.dumps({"sample": i + 1, "event": "api_complete", "api_seconds": row["api_seconds"]}), flush=True)
                result = json.loads(raw)
                if result.get("error"):
                    raise ValueError("Provider returned an error; no retry performed")
                item = (result["images"] if args.provider == "fal" else result["data"])[0]
                mime = item.get("media_type") or item.get("content_type")
                if item.get("b64_json"):
                    image_bytes = base64.b64decode(item["b64_json"], validate=True)
                elif item.get("url", "").startswith("data:image/"):
                    metadata, separator, encoded = item["url"].partition(",")
                    if not separator or not metadata.endswith(";base64"):
                        raise ValueError("Expected a base64 image data URI")
                    mime = metadata[5:].removesuffix(";base64")
                    image_bytes = base64.b64decode(encoded, validate=True)
                else:
                    url = item["url"]
                    if urllib.parse.urlparse(url).scheme != "https":
                        raise ValueError("Image download requires HTTPS")
                    row["download_network"] = {}
                    image_bytes, download_headers = request(client, url, args.timeout, metrics=row["download_network"])
                    mime = mime or download_headers.get("content-type", "").split(";")[0]
                ext = {"image/jpeg": ".jpg", "image/svg+xml": ".svg"}.get(mime)
                ext = ext or mimetypes.guess_extension(mime or "") or ".bin"
                path = out / f"sample-{i + 1:02d}{ext}"
                path.write_bytes(image_bytes)
                end = time.perf_counter()
                row.update({"decode_download_save_seconds": end - api_done,
                            "total_seconds": end - start, "output": str(path),
                            "bytes": len(image_bytes), "media_type": mime,
                            "request_id": result.get("request_id") or response_headers.get("x-fal-request-id") or response_headers.get("x-request-id")})
                if result.get("usage") is not None:
                    row["usage"] = result["usage"]
                if result.get("timings") is not None:
                    row["provider_timings"] = result["timings"]
                if result.get("seed") is not None:
                    row["seed"] = result["seed"]
                print(json.dumps(row), flush=True)
            except Exception as error:
                row["failed_after_seconds"] = time.perf_counter() - start
                row["error"] = f"HTTP {error.code}" if isinstance(error, urllib.error.HTTPError) else type(error).__name__
                raise
            finally:
                (out / "latency.json").write_text(json.dumps(report, indent=2) + "\n")
        report["summary"] = {field: {"median": statistics.median(r[field] for r in report["samples"]),
                                     "min": min(r[field] for r in report["samples"]),
                                     "max": max(r[field] for r in report["samples"])}
                             for field in ("api_seconds", "total_seconds")}
        (out / "latency.json").write_text(json.dumps(report, indent=2) + "\n")
        print(json.dumps(report["summary"], indent=2))
    except Exception:
        print(f"Stopped; partial measurements saved to {out / 'latency.json'}")
        raise
    finally:
        client.close()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--provider", required=True, choices=("fal", "openrouter"))
    parser.add_argument("--model", required=True)
    parser.add_argument("--prompt", required=True)
    parser.add_argument("--params", help="JSON file of documented model-specific parameters")
    parser.add_argument("--env-file", help="Local dotenv or .env.gpg file; encrypted contents stay in memory")
    parser.add_argument("--out-dir", required=True, help="New directory; existing directories are refused")
    parser.add_argument("--samples", type=int, default=1)
    parser.add_argument("--timeout", type=float, default=60, help="Per-phase network timeout in seconds; does not cancel server work")
    parser.add_argument("--dry-run", action="store_true")
    args = parser.parse_args()
    try:
        run(args)
    except urllib.error.HTTPError as error:
        parser.exit(1, f"HTTP {error.code}; request was not retried\n")
    except Exception as error:
        parser.exit(1, f"{type(error).__name__}: {error}\n")


if __name__ == "__main__":
    main()
