# Local contract

Aesthetic Code requires no Aesthetic Code server.

- No runtime account, telemetry, analytics, cloud sync, or hosted control plane.
- Configuration, session records, memory, and credentials remain on machines
  controlled by the user.
- Peer discovery and control use an explicitly configured LAN or private
  tailnet. They never create a public listener.
- Licenses are signed files verified offline. The application does not phone
  home to remain usable.
- Updates are user-initiated. An update check may be enabled separately without
  transmitting workspace or session data.

## Inference modes

`local` routes inference to an Ollama server on `127.0.0.1`, rejects known
Ollama cloud model names, and disables the agent loop's telemetry, feedback,
browser integration, WebFetch, and WebSearch surfaces.

`claude` and `codex` are provider modes. The interface must label them as
provider-backed before a prompt is sent. Provider terms govern that traffic.

The local mode does not yet impose an operating-system network sandbox on shell
commands run by the agent. A command the user approves can still access the
network. Strict offline enforcement is required before the product may claim
that arbitrary agent tools are network-isolated.
