# Whistlegraph presents: *Special Sign* & *MacNeoPolitan* — technical rider

Two works, one evening, nine laptops. They share the room and the Kalio system
but they are **different machines running different software**, and the patch
changes between them. Please read both system sections.

---

## Work one — *Special Sign*, live spatial version

### System

- Six artist-supplied salvaged laptops booted from six **AC Native** USB drives
- Independent sound bodies distributed among the six laptops
- Artist control surface for live rotation, trajectory, speed, and voice routing
- Local wired or isolated Wi-Fi network for clock, transport, and spatial control
- Six discrete audio feeds routed to CultureHub LA's 5.1 Kalio system
- House projection showing the live spatial score

### Signal plan

| Source | House destination |
| --- | --- |
| Laptop 1 | Discrete house input 1 |
| Laptop 2 | Discrete house input 2 |
| Laptop 3 | Discrete house input 3 |
| Laptop 4 | Discrete house input 4 |
| Laptop 5 | Discrete house input 5 |
| Laptop 6 | Discrete house input 6, pending Kalio specification |

The laptop-to-speaker relationship changes during the performance; it should not
be hard-coded as six stationary voices. The system performs spatial rotation by
moving sound bodies across the available full-range outputs. The final renderer
depends on whether CultureHub's sixth channel is full-range. If it is LFE-only,
the spatial field will use the five full-range channels and send only derived
low-frequency material to the LFE, or use an additional full-range speaker if
available.

---

## Work two — *MacNeoPolitan*

### System

- **Three MacBook Neos** — indigo, citrus, blush — running **Menu Band** on macOS
- Three discrete audio feeds, one per machine, placed to form a spatial triangle
  around the audience
- The piece runs from a `.mbscore`: a JSON score declaring `machines: 3` and one
  voice per machine. All three fire at a shared `startEpoch` and lock to the same
  downbeat.

### What is different from work one, and why it matters

**These machines do not boot AC Native and must not be asked to.** AC Native is
x86_64 UEFI only; the MacBook Neo is Apple silicon. *MacNeoPolitan* runs Menu
Band, a normal signed macOS app, on the machines' own installed systems. Nothing
is booted from USB for this work.

**Sync is by clock, not by cable.** The three machines lock to one downbeat
through NTP-synced wall clocks — typically tens of milliseconds across the
fleet. There is no audio-clock link and no timecode between them. This means:

- They need **working network time**. If the house network blocks NTP, or the
  machines have been offline long enough to drift, the downbeat will smear.
  Please confirm outbound NTP is reachable, or allow a few minutes on a network
  that permits it before the performance.
- Conducting the three from one host uses ssh across the local network. If the
  house network isolates clients from each other, the fallback is to arm each
  machine locally — workable, but it needs a decision before the technical
  rehearsal, not during it.

### Signal plan

| Source | House destination |
| --- | --- |
| MacBook Neo — indigo | Discrete house input 1 |
| MacBook Neo — citrus | Discrete house input 2 |
| MacBook Neo — blush | Discrete house input 3 |

Three full-range positions, spread as widely as the room allows. Menu Band's
audio comes out of each machine's own output; the interface question is simply
how CultureHub prefers to take three line-level feeds.

---

## CultureHub request

### Audio and network

- Kalio 5.1 system with six independently addressable inputs
- Audio interface or interfaces providing **six** line-level outputs for
  *Special Sign* and **three** for *MacNeoPolitan* — six total is sufficient if
  the patch can be changed between works
- DI boxes, cabling, and adapters appropriate to the confirmed interfaces
- Dedicated router or switch isolated from public traffic
- **Outbound NTP reachable**, and confirmation of whether local clients can
  reach each other over ssh on the same subnet
- **Guest Wi-Fi with internet access in the workshop space** — Menu Band Jam
  participants install the app on their own machines on the day

### Power, room, projection

- **Nine** laptop power positions across the two works, plus conditioned power
  for audio and network gear
- Six low tables or plinths for the salvaged laptops, arranged around the
  audience field, plus **three stable surfaces** for the MacBook Neos
- Projector, screen or projection wall, and an HDMI input near the performance
  position
- Central seating for approximately 20–40 people, subject to room capacity
- Stereo livestream and documentation feed derived separately from the spatial
  mix
- One technical rehearsal with CultureHub's audio and streaming staff, long
  enough to change the patch between the two works and run both

---

## Artist brings

- Six tested salvaged laptops and two spares
- Six AC Native USB drives and two spares
- Three MacBook Neos, each with Menu Band installed and the score pre-loaded
- Laptop power supplies for all nine machines
- Primary control laptop
- USB, HDMI, and network adapters already owned by the artist
- Software images and offline recovery media

---

## Known risks the artist is carrying

**`neo` — the citrus machine — is the working development Mac and has 8 GB of
RAM.** It is documented as exhausting memory and hanging under load. A hang
during *MacNeoPolitan* stops one of three voices with no graceful recovery.
Mitigation, to be settled before load-out, not on site:

- perform from a clean boot with nothing else running, no editors, no agents,
  no background sync; or
- swap in a fourth machine for the performance and keep `neo` as the spare.

**No spare third Neo yet.** The blush machine is unpurchased at the time of
writing. Until it exists there is no redundancy for *MacNeoPolitan* — losing any
one of the three loses a voice.

**Clock drift is the silent failure.** Unlike a dropped cable, a smeared
downbeat still plays and just sounds wrong. It should be verified at the
technical rehearsal with all three machines on the room's actual network.

---

## Confirm with CultureHub

- Exact Kalio speaker layout and sixth-channel bandwidth
- Whether the Kalio system accepts six discrete inputs or requires a house
  spatialization/rendering interface
- Available audio interfaces, channel count, connectors, and sample rate
- Wired network availability and house network policy
- **Whether outbound NTP is permitted and whether local clients can ssh to each
  other on the house network**
- **Whether guest Wi-Fi in the workshop space allows app downloads**
- Projector resolution, throw, connectors, and screen location
- Room dimensions, capacity, furniture, and accessible seating route
- Livestream platform, capture format, archive ownership, and file delivery
- Load-in, soundcheck, public-program, and strike hours
- **How much changeover time is available between the two works**
- Whether the workshop may boot participant laptops from USB
