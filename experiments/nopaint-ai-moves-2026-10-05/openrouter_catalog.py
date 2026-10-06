"""Public, read-only image model discovery. No key or inference request."""
import json
import re
import threading
import time
from urllib.request import Request, urlopen


def image_models(rows):
    result = []
    for model in rows:
        architecture = model.get("architecture", {})
        parameters = model.get("supported_parameters", {})
        references = parameters.get("input_references", {})
        formats = parameters.get("output_format", {}).get("values", [])
        if ("image" not in architecture.get("input_modalities", [])
                or "image" not in architecture.get("output_modalities", [])
                or references.get("max", 0) < 1 or references.get("min", 0) > 1
                or formats == ["svg"]):
            continue
        result.append({"id": model["id"], "name": model["name"],
                       "description": model.get("description", ""),
                       "resolutions": parameters.get("resolution", {}).get("values", []),
                       "provider_previews": model.get("supports_streaming", False),
                       "url": "https://openrouter.ai/" + model["id"]})
    return result


class ModelBrowser:
    def __init__(self, fetch=None):
        self.fetch = fetch or self.request
        self.cache = {}
        self.lock = threading.Lock()

    @staticmethod
    def request(path):
        request = Request("https://openrouter.ai/api/v1/images/models" + path,
                          headers={"User-Agent": "NoPaint/0.1"})
        with urlopen(request, timeout=10) as response:
            content = response.read(2_000_001)
        if len(content) > 2_000_000:
            raise ValueError("Model catalog is too large")
        return json.loads(content)

    def read(self, model=None):
        if model and not re.fullmatch(r"[a-z0-9][a-z0-9._-]*/[a-z0-9][a-z0-9._:-]*", model):
            raise ValueError("Invalid model")
        key = model or ""
        with self.lock:
            cached = self.cache.get(key)
            if cached and cached[0] > time.monotonic():
                return cached[1]
            if model:
                result = {"id": model, "endpoints": self.fetch("/" + model + "/endpoints").get("endpoints", [])}
            else:
                result = {"models": image_models(self.fetch("").get("data", []))}
            self.cache[key] = (time.monotonic()+300, result)
            return result
