# Lenovo ThinkPad Yoga 11e Gen 6

Reference device for AC Blank / AC Native OS.

## Physical Dimensions

| Measurement | Metric | Imperial |
|---|---|---|
| Width | 293 mm | 11.54" |
| Depth | 207 mm | 8.15" |
| Height (closed) | 19.9 mm | 0.78" |
| Weight | 1.41 kg | 3.1 lbs |

### Ratios (for 3D modeling)

- Width:Depth = 1.415:1
- Width:Height = 14.72:1
- Each half-slab thickness ~ 9.95 mm (closed height / 2)

## Display

- **Size:** 11.6" diagonal IPS
- **Resolution:** 1366 x 768 (HD)
- **Touch:** 10-point capacitive multi-touch
- **Glass:** Corning Gorilla Glass, glossy
- **Brightness:** 250 cd/m²
- **Viewing angle:** 170°

## 360-Degree Hinge Mechanism

The Yoga 11e uses a **watchband-style barrel hinge** — two stainless steel barrel
assemblies located at the left and right rear corners of the base. Each barrel consists
of interconnected stainless steel bands that distribute torque evenly.

### Hinge Geometry

The hinge is NOT a simple pivot point. It works as a **chain linkage** where:

1. **Base attachment:** Two hinge barrels are mounted flush with the back edge of the
   base, sitting within the base slab height. They are wider than they are tall
   (roughly 30mm wide x 10mm tall each).

2. **Arc motion:** As the lid opens, the barrel mechanism causes the lid's hinge edge
   to **translate along a circular arc** around the base's rear edge. This gives the
   lid clearance to pass around the base thickness without collision.

3. **Lid attachment:** The lid connects to the outer part of each barrel. When closed,
   the lid sits flush on top of the base. As it opens past ~180°, the chain mechanism
   guides the lid around and behind the base.

4. **Four modes:**
   - **Laptop mode:** 0° to ~135° (traditional use)
   - **Flat/Stand mode:** ~180° (display and base co-planar)
   - **Tent mode:** ~270° (inverted V, propped up)
   - **Tablet mode:** 360° (lid folded flat behind base, screen facing out)

### Key Modeling Insight

The hinge effectively creates a **rolling contact** — the lid's pivot point is not
fixed in space. As the angle increases past 180°, the lid slides backward and
underneath the base. This can be approximated in a 3D model as:

- A fixed pivot that is offset from the base by the barrel radius (~10mm)
- OR a two-segment chain: base → barrel link → lid, where the barrel link rotates
  with the lid but its center stays at the base back edge

The barrel radius determines clearance: it must be >= half the combined thickness
of base + lid for the lid to fold 360° without collision.

## Chassis

- **Material:** Polycarbonate ABS (reinforced)
- **Color:** Black
- **MIL-STD-810G tested** (drop, vibration, humidity, temperature)
- **Hinge durability:** 50,000+ open/close cycles

## Processor Options

- Intel Celeron 4205U (1.8 GHz, 2 cores)
- Intel Core m3-8100Y (1.1 / 3.4 GHz, 2 cores)
- Intel Core i5-8200Y (1.3 / 3.9 GHz, 2 cores)

## Memory & Storage

- **RAM:** 4GB or 8GB DDR4 (soldered)
- **Storage:** 128GB or 256GB M.2 PCIe SSD

## Ports

- 2x USB 3.1 Gen 1 (Type-A)
- 1x USB-C 3.1 Gen 1 (DisplayPort Alt Mode + Power Delivery)
- 1x HDMI
- 1x 3.5mm headphone/mic combo
- 1x microSD card reader

## Battery

- 42Wh Li-ion
- Up to 10.8 hours (MobileMark 2014)
- 45W USB-C charger

## Keyboard & Input

- Spill-resistant keyboard (up to 100ml)
- Full-size keys with 1.8mm travel
- Precision touchpad
- Optional: ThinkPad Pen Pro (Wacom AES)

## Sources

- [PSREF Spec Sheet (PDF)](https://psref.lenovo.com/syspool/Sys/PDF/ThinkPad/ThinkPad_11e_Yoga_Gen_6/ThinkPad_11e_Yoga_Gen_6_Spec.pdf)
- [Product Detail](https://www.productindetail.com/pn/lenovo-thinkpad-11e-yoga-gen-6-20se)
- [iFixit Hinge Replacement Guide](https://www.ifixit.com/Guide/Lenovo+ThinkPad+Yoga+11e+Gen+6+Hinges+Replacement/179161)
- [Lenovo Product Page](https://www.lenovo.com/us/en/p/laptops/thinkpad/thinkpad11e/11e-yoga-g6/22ed11e11n6)
