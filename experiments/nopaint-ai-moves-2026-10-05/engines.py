"""The same RGB canvas crosses every engine boundary."""
import os

ENGINES = {
    "classic": {"name": "Classic", "location": "local · CPU",
                "model": "nopaint/classic-v1", "previews": True, "braincells": 0},
    "classic-pixels": {"name": "Pixels", "location": "local · CPU",
                       "model": "nopaint/classic-pixels-v1", "previews": True, "braincells": 0},
    "classic-color": {"name": "Color", "location": "local · CPU",
                      "model": "nopaint/classic-color-v1", "previews": True, "braincells": 0},
    "classic-primitives": {"name": "Primitives", "location": "local · CPU",
                           "model": "nopaint/classic-primitives-v1", "previews": True, "braincells": 0},
    "evolve": {"name": "Stable Diffusion 1.5", "location": "local",
               "model": "stable-diffusion-v1-5/stable-diffusion-v1-5", "previews": True},
    "turbo": {"name": "SD-Turbo", "location": "local",
              "model": "stabilityai/sd-turbo", "previews": False},
    "fleet-evolve": {"name": "SD 1.5", "location": "Poorslice · remote",
                     "model": "stable-diffusion-v1-5/stable-diffusion-v1-5", "previews": True},
    "fal-klein": {"name": "FLUX.2 Klein 4B", "location": "fal",
                  "model": "fal-ai/flux-2/klein/4b/edit", "previews": False},
    "ac-klein": {"name": "FLUX.2 Klein 4B", "location": "AC cloud",
                  "model": "fal-ai/flux-2/klein/4b/edit", "previews": False},
}


def remote_offer(engine, account=None):
    account = account or {}
    return next((m for m in account.get("models", []) if m["id"] == engine), None) or (account.get("remote") if engine == "ac-klein" else None) or {}


def available(engine, account=None):
    if engine == "fleet-evolve":
        from fleet_run import available as fleet_available
        return fleet_available()
    if engine.startswith("ac-"):
        account = account or {}
        remote = remote_offer(engine, account)
        cloud_ready = not engine.startswith("ac-openrouter:") or (account.get("remote_service") or {}).get("available", True)
        return bool(account.get("connected") and not account.get("stale") and cloud_ready and remote.get("available")
                    and account.get("remaining", 0) + account.get("purchased", 0) >= remote.get("braincells", float("inf")))
    return engine in ENGINES and (engine != "fal-klein" or (os.environ.get("NOPAINT_ENABLE_FAL") == "1"
                                     and bool(os.environ.get("FAL_KEY"))))


def register_image_models(models):
    for model in models:
        ENGINES["ac-openrouter:" + model["id"]] = {
            "name": model["name"], "location": "AC cloud · OpenRouter",
            "model": model["id"], "previews": False,
        }


def catalog(account=None):
    for offer in (account or {}).get("models", []):
        if offer.get("id", "").startswith("ac-openrouter:"):
            ENGINES[offer["id"]] = {key:offer[key] for key in ("name", "location", "model", "previews")}
    return [{"id": key, **value, "available": available(key, account),
             **({"braincells": remote_offer(key, account).get("braincells")} if key.startswith("ac-") else {})}
            for key, value in list(ENGINES.items()) if key != "fal-klein"
            and (key != "fleet-evolve" or available(key, account))]
