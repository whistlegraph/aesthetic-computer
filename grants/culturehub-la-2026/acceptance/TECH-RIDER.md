# AC Presents 2 New Pieces — technical rider

Two public events in the CultureHub LA residency, September 16–25, 2026:

| Date | Event | Shape |
| --- | --- | --- |
| Sat Sep 19, 2:00–3:30 p.m. | **Grokaesthetic Workshop** | Participatory, bring your own computer |
| Thu Sep 24, 7:00 p.m. | **Note(s)pat(ial) Native** & **The MacNeoPolitan Trio** | Performance, conversation, free play |

Dates proposed, pending confirmation. The performance is two works plus a guest
set to be announced. They share the room and the Kalio system but are
**different machines running different software**, and the patch changes
between them — please read both system sections.

---

## Event one — Grokaesthetic Workshop (Saturday)

Ninety minutes, hands-on, free and open to the public. Participants arrive with
their own laptops and phones, get Aesthetic.Computer running on them, and the
room finishes by playing together as one ensemble in the surround system.

### Three ways in, all of which need something from the house

1. **Browser** — anyone plays notepat at aesthetic.computer on the device they
   walked in with. Needs guest Wi-Fi that reaches the open internet.
2. **Menu Band on a Mac** — a free, signed macOS app, roughly a minute to
   install. Needs guest Wi-Fi that permits **app downloads**, not a portal that
   allows browsing only.
3. **AC OS from a USB stick** — boots a laptop into the operating system
   without touching its disk. Needs **permission to boot participant machines
   from USB in the space**; nothing is installed or altered.

### What the workshop needs

- **Guest Wi-Fi sized for roughly 25–35 simultaneous devices**, with app
  downloads permitted. This is the single highest-risk item: a captive portal
  or a download block turns the middle path off at the door.
- **Power for participants** — power strips reaching the seating, enough for
  ~20 laptops. Assume nobody arrives charged.
- **Tables or floor seating** for participants plus their machines, arranged so
  people can see each other rather than a stage.
- **Loaner fleet positions** — the artist brings spare machines for anyone
  without a computer; they need power and a place to sit.
- **Stereo playback at minimum**, ideally the surround system, so the room can
  hear the group ensemble. Full six-channel routing is welcome but not required
  for this event.
- **A snack table** with a nearby bin. This is stated as a real requirement:
  the workshop is part hangout by design.
- **A microphone or a room quiet enough to teach in** — the artist is talking
  the whole time.

### Access

No programming or musical experience is assumed. Loaner machines mean no one
is turned away for lack of hardware. Please advise on accessible seating and
whether the space suits participants who cannot sit on the floor.

---

## Event two, work one — *Note(s)pat(ial) Native*

### System

- Six artist-supplied surplus grade-school laptops booted from six **AC OS**
  USB drives
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

## Event two, work two — *The MacNeoPolitan Trio*

### System

- **Three MacBook Neos** — indigo, citrus, blush — running **Menu Band** on macOS
- Three discrete audio feeds, one per machine, placed to form a spatial triangle
  around the audience
- The piece runs from a `.mbscore`: a JSON score declaring `machines: 3` and one
  voice per machine. All three fire at a shared `startEpoch` and lock to the same
  downbeat.

### What is different from work one, and why it matters

**These machines do not boot AC OS and must not be asked to.** AC OS is
x86_64 UEFI only; the MacBook Neo is Apple silicon. *The MacNeoPolitan Trio*
runs Menu Band, a normal signed macOS app, on the machines' own installed
systems. Nothing is booted from USB for this work.

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

## Event two — changeover, guest set, and the coda

The move from work one to work two is a **hardware swap, not a patch change** —
six machines out, three in. The guest set sits between them and covers it;
please budget its length once the guest is confirmed. After the music there is
a conversation, and then the instruments stay on for the audience to play, so
the rig should not be struck until the room clears.

---

## CultureHub request

### Audio and network

- Kalio 5.1 system with six independently addressable inputs
- Audio interface or interfaces providing **six** line-level outputs for
  *Note(s)pat(ial) Native* and **three** for *The MacNeoPolitan Trio* — six
  total is sufficient if the patch can be changed between works
- DI boxes, cabling, and adapters appropriate to the confirmed interfaces
- Dedicated router or switch isolated from public traffic
- **Outbound NTP reachable**, and confirmation of whether local clients can
  reach each other over ssh on the same subnet
- **Guest Wi-Fi with internet access and app downloads permitted**, sized for
  25–35 devices on the Saturday

### Power, room, projection

- **Nine** laptop power positions for the performance, plus conditioned power
  for audio and network gear, plus **participant power strips for ~20 machines**
  on the Saturday
- Six low tables or plinths for the surplus laptops, arranged around the
  audience field, plus **three stable surfaces** for the MacBook Neos
- Projector, screen or projection wall, and an HDMI input near the performance
  position
- Central seating for approximately 20–40 people, subject to room capacity
- Snack table and bin for the workshop
- Stereo livestream and documentation feed derived separately from the spatial
  mix
- One technical rehearsal with CultureHub's audio and streaming staff, long
  enough to change the patch between the two works and run both

---

## Artist brings

- Six tested surplus laptops and two spares
- Six AC OS USB drives and two spares, plus **workshop loaner sticks**
- Three MacBook Neos, each with Menu Band installed and the score pre-loaded
- **Loaner laptops for workshop participants without a machine**
- Laptop power supplies for all nine performance machines
- Primary control laptop
- USB, HDMI, and network adapters already owned by the artist
- Software images and offline recovery media

---

## Known risks the artist is carrying

**`neo` — the citrus machine — is the working development Mac and has 8 GB of
RAM.** It is documented as exhausting memory and hanging under load. A hang
during *The MacNeoPolitan Trio* stops one of three voices with no graceful
recovery. Mitigation, to be settled before load-out, not on site:

- perform from a clean boot with nothing else running, no editors, no agents,
  no background sync; or
- swap in a fourth machine for the performance and keep `neo` as the spare.

**No spare third Neo yet.** The blush machine is unpurchased at the time of
writing. Until it exists there is no redundancy for *The MacNeoPolitan Trio* —
losing any one of the three loses a voice.

**Clock drift is the silent failure.** Unlike a dropped cable, a smeared
downbeat still plays and just sounds wrong. It should be verified at the
technical rehearsal with all three machines on the room's actual network.

**The workshop depends on someone else's network.** Two of the three ways in
need working guest Wi-Fi, and the third needs USB-boot permission. If all three
are blocked the session degrades to the artist's loaner fleet only. Confirm
early; this is not fixable on the day.

---

## Confirm with CultureHub

- Exact Kalio speaker layout and sixth-channel bandwidth
- Whether the Kalio system accepts six discrete inputs or requires a house
  spatialization/rendering interface
- Available audio interfaces, channel count, connectors, and sample rate
- Wired network availability and house network policy
- **Whether outbound NTP is permitted and whether local clients can ssh to each
  other on the house network**
- **Whether guest Wi-Fi permits app downloads, and how many devices it holds**
- **Whether participant laptops may be booted from USB in the space**
- Projector resolution, throw, connectors, and screen location
- Room dimensions, capacity, furniture, and accessible seating route
- Livestream platform, capture format, archive ownership, and file delivery
- Load-in, soundcheck, public-program, and strike hours
- **How much changeover time is available between the two works**
- Guest performer's own technical needs, once confirmed
