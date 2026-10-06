// Raster image-editing catalog reviewed 2026-10-06 against OpenRouter /images/models and each /endpoints.
// usd is the fixed AC per-move cost basis, NOT a token-rate estimate or a
// guarantee of upstream cost. One reference, one smallest supported square
// output, low quality when supported. Never auto-add future catalog entries.
export const profiles = {
  "google/gemini-nano-banana-2.1": {
    "name": "Nano Banana 2.1",
    "usd": 0.04,
    "options": {
      "aspect_ratio": "1:1",
      "resolution": "1K",
      "n": 1
    }
  },
  "tencent/hy-image-v3.5-preview": {
    "name": "Hy Image 3.5 Preview",
    "usd": 0.01,
    "options": {
      "aspect_ratio": "1:1",
      "resolution": "1K",
      "n": 1,
      "seed": true
    }
  },
  "bytedance-seed/seedream-5-0-flash": {
    "name": "Seedream 5.0 Flash",
    "usd": 0.02,
    "options": {
      "aspect_ratio": "1:1",
      "resolution": "1K",
      "n": 1,
      "seed": true
    }
  },
  "black-forest-labs/flux-3-image": {
    "name": "FLUX.3 Image",
    "usd": 0.05,
    "options": {
      "aspect_ratio": "1:1",
      "resolution": "768",
      "n": 1
    }
  },
  "inclusionai/ming-image-0.1-design-layer": {
    "name": "Ming Image 0.1 Design Layer",
    "usd": 0,
    "options": {
      "output_format": "png",
      "n": 1
    }
  },
  "openai/gpt-image-2.5-sunburst": {
    "name": "GPT Image 2.5 Sunburst",
    "usd": 0.02,
    "options": {
      "aspect_ratio": "1:1",
      "quality": "low",
      "n": 1
    }
  },
  "openai/gpt-image-2.5-flare": {
    "name": "GPT Image 2.5 Flare",
    "usd": 0.015,
    "options": {
      "aspect_ratio": "1:1",
      "quality": "low",
      "n": 1
    }
  },
  "microsoft/mai-image-2.6": {
    "name": "MAI-Image-2.6",
    "usd": 0.1,
    "options": {
      "aspect_ratio": "1:1",
      "n": 1
    }
  },
  "microsoft/mai-image-2.6-flash": {
    "name": "MAI-Image-2.6 Flash",
    "usd": 0.05,
    "options": {
      "aspect_ratio": "1:1",
      "n": 1
    }
  },
  "recraft/recraft-v4-styles-pro": {
    "name": "Recraft V4 Styles Pro",
    "usd": 0.105,
    "options": {
      "aspect_ratio": "1:1",
      "n": 1
    }
  },
  "recraft/recraft-v4-styles": {
    "name": "Recraft V4 Styles",
    "usd": 0.04,
    "options": {
      "aspect_ratio": "1:1",
      "n": 1
    }
  },
  "bytedance-seed/seedream-5-0-lite": {
    "name": "Seedream 5.0 Lite",
    "usd": 0.04,
    "options": {
      "aspect_ratio": "1:1",
      "resolution": "2K",
      "n": 1,
      "seed": true
    }
  },
  "bytedance-seed/seedream-5-0-pro": {
    "name": "Seedream 5.0 Pro",
    "usd": 0.05,
    "options": {
      "aspect_ratio": "1:1",
      "resolution": "1K",
      "n": 1,
      "seed": true
    }
  },
  "x-ai/grok-imagine-image-2.0": {
    "name": "Grok Imagine Image 2.0",
    "usd": 0.05,
    "options": {
      "aspect_ratio": "1:1",
      "resolution": "1K",
      "quality": "low",
      "n": 1
    }
  },
  "qwen/qwen-image-3-pro": {
    "name": "Qwen Image 3 Pro",
    "usd": 0.05,
    "options": {
      "aspect_ratio": "1:1",
      "resolution": "1K",
      "n": 1,
      "seed": true
    }
  },
  "qwen/qwen-image-3": {
    "name": "Qwen Image 3",
    "usd": 0.04,
    "options": {
      "aspect_ratio": "1:1",
      "resolution": "1K",
      "n": 1,
      "seed": true
    }
  },
  "microsoft/mai-image-2.5-pro": {
    "name": "MAI-Image-2.5 Pro",
    "usd": 0.5,
    "options": {
      "aspect_ratio": "1:1",
      "n": 1
    }
  },
  "krea/krea-2-large": {
    "name": "Krea 2 Large",
    "usd": 0.07,
    "options": {
      "aspect_ratio": "1:1",
      "resolution": "1K",
      "seed": true
    }
  },
  "krea/krea-2-medium": {
    "name": "Krea 2 Medium",
    "usd": 0.04,
    "options": {
      "aspect_ratio": "1:1",
      "resolution": "1K",
      "seed": true
    }
  },
  "krea/krea-2-medium-turbo": {
    "name": "Krea 2 Medium Turbo",
    "usd": 0.02,
    "options": {
      "aspect_ratio": "1:1",
      "resolution": "1K",
      "seed": true
    }
  },
  "google/gemini-3.1-flash-lite-image": {
    "name": "Nano Banana 2 Lite (Gemini 3.1 Flash Lite Image)",
    "usd": 0.04,
    "options": {
      "aspect_ratio": "1:1",
      "resolution": "1K",
      "n": 1
    }
  },
  "openai/gpt-image-2": {
    "name": "GPT Image 2",
    "usd": 0.01,
    "options": {
      "aspect_ratio": "1:1",
      "quality": "low",
      "n": 1
    }
  },
  "openai/gpt-image-1-mini": {
    "name": "GPT Image 1 Mini",
    "usd": 0.01,
    "options": {
      "aspect_ratio": "1:1",
      "quality": "low",
      "n": 1
    }
  },
  "openai/gpt-image-1": {
    "name": "GPT Image 1",
    "usd": 0.02,
    "options": {
      "aspect_ratio": "1:1",
      "quality": "low",
      "n": 1
    }
  },
  "google/gemini-3.1-flash-image": {
    "name": "Nano Banana 2 (Gemini 3.1 Flash Image)",
    "usd": 0.06,
    "options": {
      "aspect_ratio": "1:1",
      "resolution": "512",
      "n": 1
    }
  },
  "google/gemini-3-pro-image": {
    "name": "Nano Banana Pro (Gemini 3 Pro Image)",
    "usd": 0.15,
    "options": {
      "aspect_ratio": "1:1",
      "resolution": "1K",
      "n": 1
    }
  },
  "sourceful/riverflow-v2.5-pro": {
    "name": "Riverflow V2.5 Pro",
    "usd": 0.13,
    "options": {
      "aspect_ratio": "1:1",
      "resolution": "1K",
      "output_format": "png",
      "n": 1
    }
  },
  "sourceful/riverflow-v2.5-fast": {
    "name": "Riverflow V2.5 Fast",
    "usd": 0.02,
    "options": {
      "aspect_ratio": "1:1",
      "resolution": "1K",
      "output_format": "jpeg",
      "n": 1
    }
  },
  "microsoft/mai-image-2.5": {
    "name": "MAI-Image-2.5",
    "usd": 0.15,
    "options": {
      "aspect_ratio": "1:1",
      "n": 1
    }
  },
  "x-ai/grok-imagine-image-quality": {
    "name": "Grok Imagine Image Quality",
    "usd": 0.06,
    "options": {
      "aspect_ratio": "1:1",
      "resolution": "1K",
      "n": 1
    }
  },
  "recraft/recraft-v4.1-utility-pro": {
    "name": "Recraft V4.1 Utility Pro",
    "usd": 0.21,
    "options": {
      "aspect_ratio": "1:1",
      "n": 1
    }
  },
  "recraft/recraft-v4.1-utility": {
    "name": "Recraft V4.1 Utility",
    "usd": 0.035,
    "options": {
      "aspect_ratio": "1:1",
      "n": 1
    }
  },
  "recraft/recraft-v4.1-pro": {
    "name": "Recraft V4.1 Pro",
    "usd": 0.21,
    "options": {
      "aspect_ratio": "1:1",
      "n": 1
    }
  },
  "recraft/recraft-v4.1": {
    "name": "Recraft V4.1",
    "usd": 0.035,
    "options": {
      "aspect_ratio": "1:1",
      "n": 1
    }
  },
  "recraft/recraft-v4-pro": {
    "name": "Recraft V4 Pro",
    "usd": 0.25,
    "options": {
      "aspect_ratio": "1:1",
      "n": 1
    }
  },
  "recraft/recraft-v4": {
    "name": "Recraft V4",
    "usd": 0.04,
    "options": {
      "aspect_ratio": "1:1",
      "n": 1
    }
  },
  "recraft/recraft-v3": {
    "name": "Recraft V3",
    "usd": 0.04,
    "options": {
      "aspect_ratio": "1:1",
      "n": 1
    }
  },
  "openai/gpt-5.4-image-2": {
    "name": "GPT-5.4 Image 2",
    "usd": 0.05,
    "options": {
      "aspect_ratio": "1:1",
      "quality": "low",
      "n": 1
    }
  },
  "google/gemini-3.1-flash-image-preview": {
    "name": "Nano Banana 2 (Gemini 3.1 Flash Image Preview)",
    "usd": 0.06,
    "options": {
      "aspect_ratio": "1:1",
      "resolution": "512",
      "n": 1
    }
  },
  "sourceful/riverflow-v2-pro": {
    "name": "Riverflow V2 Pro",
    "usd": 0.35,
    "options": {
      "aspect_ratio": "1:1",
      "resolution": "1K",
      "n": 1
    }
  },
  "sourceful/riverflow-v2-fast": {
    "name": "Riverflow V2 Fast",
    "usd": 0.22,
    "options": {
      "aspect_ratio": "1:1",
      "resolution": "1K",
      "n": 1
    }
  },
  "black-forest-labs/flux.2-klein-4b": {
    "name": "FLUX.2 Klein 4B",
    "usd": 0.02,
    "options": {
      "aspect_ratio": "1:1",
      "output_format": "png",
      "n": 1,
      "seed": true
    }
  },
  "bytedance-seed/seedream-4.5": {
    "name": "Seedream 4.5",
    "usd": 0.04,
    "options": {
      "aspect_ratio": "1:1",
      "resolution": "1K",
      "n": 1,
      "seed": true
    }
  },
  "black-forest-labs/flux.2-max": {
    "name": "FLUX.2 Max",
    "usd": 0.08,
    "options": {
      "aspect_ratio": "1:1",
      "output_format": "png",
      "n": 1,
      "seed": true
    }
  },
  "black-forest-labs/flux.2-flex": {
    "name": "FLUX.2 Flex",
    "usd": 0.14,
    "options": {
      "aspect_ratio": "1:1",
      "output_format": "png",
      "n": 1,
      "seed": true
    }
  },
  "black-forest-labs/flux.2-pro": {
    "name": "FLUX.2 Pro",
    "usd": 0.04,
    "options": {
      "aspect_ratio": "1:1",
      "output_format": "png",
      "n": 1,
      "seed": true
    }
  },
  "google/gemini-3-pro-image-preview": {
    "name": "Nano Banana Pro (Gemini 3 Pro Image Preview)",
    "usd": 0.15,
    "options": {
      "aspect_ratio": "1:1",
      "resolution": "1K",
      "n": 1
    }
  },
  "openai/gpt-5-image-mini": {
    "name": "GPT-5 Image Mini",
    "usd": 0.015,
    "options": {
      "aspect_ratio": "1:1",
      "quality": "low",
      "n": 1
    }
  },
  "openai/gpt-5-image": {
    "name": "GPT-5 Image",
    "usd": 0.02,
    "options": {
      "aspect_ratio": "1:1",
      "quality": "low",
      "n": 1
    }
  },
  "google/gemini-2.5-flash-image": {
    "name": "Nano Banana (Gemini 2.5 Flash Image)",
    "usd": 0.05,
    "options": {
      "aspect_ratio": "1:1",
      "n": 1
    }
  }
};
