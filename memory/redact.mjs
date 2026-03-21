// memory/redact.mjs
// Best-effort secret redaction for agent memory metadata and summaries.

const SECRET_PATTERNS = [
  {
    type: "password_assignment",
    regex: /\b(password|passwd|pwd)\s*[:=]\s*([^\s,;]+)/gi,
    replace: (_match, key) => `${key}=[REDACTED]`,
  },
  {
    type: "password_phrase",
    regex: /\b(my\s+password\s+is|password\s+is|passcode\s+is|pin\s+is)\s+([^\s,;]+)/gi,
    replace: (match, phrase) => `${phrase} [REDACTED]`,
  },
  {
    type: "openai_key",
    regex: /\bsk-[A-Za-z0-9]{20,}\b/g,
    replace: "[REDACTED_OPENAI_KEY]",
  },
  {
    type: "anthropic_key",
    regex: /\bsk-ant-[A-Za-z0-9\-_]{20,}\b/g,
    replace: "[REDACTED_ANTHROPIC_KEY]",
  },
  {
    type: "github_token",
    regex: /\bgh[pousr]_[A-Za-z0-9]{20,}\b/g,
    replace: "[REDACTED_GITHUB_TOKEN]",
  },
  {
    type: "aws_access_key",
    regex: /\bAKIA[0-9A-Z]{16}\b/g,
    replace: "[REDACTED_AWS_ACCESS_KEY]",
  },
  {
    type: "jwt",
    regex: /\beyJ[A-Za-z0-9_-]{10,}\.[A-Za-z0-9_-]{10,}\.[A-Za-z0-9_-]{10,}\b/g,
    replace: "[REDACTED_JWT]",
  },
  {
    type: "url_with_credentials",
    regex: /\b([a-z]+:\/\/[^:\s\/]+):([^@\s]+)@/gi,
    replace: (_match, userPart) => `${userPart}:[REDACTED]@`,
  },
];

function normalizeInput(value) {
  if (value == null) return "";
  if (typeof value === "string") return value;
  try {
    return JSON.stringify(value);
  } catch {
    return String(value);
  }
}

function truncatePreview(text, maxLength = 220) {
  if (!text) return "";
  const clean = text.replace(/\s+/g, " ").trim();
  if (clean.length <= maxLength) return clean;
  return `${clean.slice(0, maxLength - 1)}…`;
}

export function redactText(input) {
  const original = normalizeInput(input);
  let redacted = original;
  const findings = [];

  for (const pattern of SECRET_PATTERNS) {
    const matches = redacted.match(pattern.regex);
    if (matches?.length) {
      findings.push({
        type: pattern.type,
        count: matches.length,
      });
      redacted = redacted.replace(pattern.regex, pattern.replace);
    }
  }

  return {
    original,
    redacted,
    findings,
    preview: truncatePreview(redacted),
  };
}

export function isLikelySensitive(input) {
  const { findings } = redactText(input);
  return findings.length > 0;
}
