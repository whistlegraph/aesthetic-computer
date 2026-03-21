// Freaky Flowers, 22.11.23.14.01
// This piece is a router that loads a specific Freaky Flower token in `wand` by
// sending it a sequence starting with the current piece.

/* #region 🏁 todo
#endregion */

// #region 🧮 data
// 0-128
const tokens = [
  "2022.11.22.13.12.16",
  "2022.11.20.11.42.50",
  "2022.11.20.11.45.22",
  "2022.11.20.11.52.27",
  "2022.11.20.12.02.09",
  "2022.11.20.12.09.14",
  "2022.11.20.12.33.20",
  "2022.11.20.12.43.56",
  "2022.11.20.12.50.55",
  "2022.11.20.12.59.23",
  "2022.11.20.13.04.28",
  "2022.11.20.13.21.21",
  "2022.11.20.13.41.27",
  "2022.11.20.13.49.40",
  "2022.11.20.14.16.40",
  "2022.11.20.14.27.09",
  "2022.11.20.14.53.20",
  "2022.11.20.15.17.42",
  "2022.11.20.15.32.51",
  "2022.11.20.15.42.14",
  "2022.11.20.15.47.59",
  "2022.11.20.16.34.34",
  "2022.11.20.16.42.02",
  "2022.11.20.16.50.58",
  "2022.11.20.17.09.10",
  "2022.11.20.17.16.14",
  "2022.11.21.01.29.33",
  "2022.11.21.01.39.04",
  "2022.11.21.01.52.50",
  "2022.11.21.02.06.24",
  "2022.11.21.02.15.40",
  "2022.11.21.02.26.35",
  "2022.11.21.09.25.44",
  "2022.11.21.02.49.15",
  "2022.11.21.02.59.15",
  "2022.11.21.03.10.36",
  "2022.11.21.03.32.46",
  "2022.11.21.03.39.51",
  "2022.11.21.03.51.34",
  "2022.11.21.04.01.36",
  "2022.11.21.04.10.08",
  "2022.11.21.04.55.19",
  "2022.11.21.05.10.15",
  "2022.11.21.05.19.00",
  "2022.11.21.05.34.14",
  "2022.11.21.05.40.56",
  "2022.11.21.05.48.45",
  "2022.11.21.05.54.00",
  "2022.11.21.06.00.23",
  "2022.11.21.06.08.31",
  "2022.11.21.06.14.44",
  "2022.11.21.06.48.51",
  "2022.11.21.07.04.19",
  "2022.11.21.07.10.17",
  "2022.11.21.07.16.21",
  "2022.11.21.07.24.09",
  "2022.11.21.07.40.05",
  "2022.11.21.07.48.06",
  "2022.11.21.07.53.56",
  "2022.11.21.08.00.30",
  "2022.11.21.08.05.45",
  "2022.11.21.09.37.38",
  "2022.11.21.09.48.14",
  "2022.11.21.10.04.06",
  "2022.11.21.10.05.39",
  "2022.11.21.10.14.05",
  "2022.11.21.10.42.13",
  "2022.11.21.10.49.52",
  "2022.11.21.10.58.25",
  "2022.11.21.11.09.41",
  "2022.11.21.11.16.36",
  "2022.11.21.11.35.40",
  "2022.11.21.11.44.11",
  "2022.11.21.12.16.51",
  "2022.11.21.12.22.16",
  "2022.11.21.12.27.03",
  "2022.11.21.12.37.54",
  "2022.11.21.12.44.38",
  "2022.11.21.12.50.22",
  "2022.11.21.12.59.38",
  "2022.11.21.13.11.20",
  "2022.11.21.16.20.47",
  "2022.11.21.16.25.24",
  "2022.11.21.16.32.25",
  "2022.11.21.16.41.08",
  "2022.11.21.16.52.06",
  "2022.11.21.17.02.01",
  "2022.11.21.17.09.15",
  "2022.11.21.17.17.40",
  "2022.11.21.17.42.51",
  "2022.11.21.17.50.54",
  "2022.11.21.18.10.15",
  "2022.11.22.11.21.20",
  "2022.11.21.18.21.23",
  "2022.11.22.03.11.31",
  "2022.11.22.03.21.43",
  "2022.11.23.04.51.09",
  "2022.11.22.03.31.20",
  "2022.11.22.03.42.00",
  "2022.11.22.03.51.47",
  "2022.11.22.04.14.05",
  "2022.11.22.04.39.52",
  "2022.11.22.04.47.34",
  "2022.11.22.04.55.40",
  "2022.11.22.05.06.56",
  "2022.11.22.05.22.39",
  "2022.11.22.05.30.31",
  "2022.11.22.05.37.07",
  "2022.11.22.05.42.35",
  "2022.11.22.05.54.23",
  "2022.11.22.06.01.44",
  "2022.11.22.06.23.02",
  "2022.11.22.06.47.21",
  "2022.11.22.06.58.34",
  "2022.11.22.07.07.23",
  "2022.11.22.07.35.54",
  "2022.11.22.07.18.05",
  "2022.11.22.07.39.12",
  "2022.11.22.07.52.45",
  "2022.11.22.08.03.50",
  "2022.11.22.08.10.58",
  "2022.11.22.08.25.12",
  "2022.11.22.08.31.13",
  "2022.11.22.08.42.29",
  "2022.11.22.08.51.38",
  "2022.11.22.09.06.47",
  "2022.11.22.09.16.45",
  "2022.11.22.09.24.32",
  "2022.11.22.09.33.26",
];

const tokenTitlesAndDescriptions = [
  [
    "Title Sign",
    "Welcome to Freaky Flowers! This piece is locked away in a wallet. The seed phrase of which is hidden among the other 128 flowers that follow this one.",
  ],
  [
    "First Freak",
    'This is the first flower I drew once I got my software working and decided on a basic score. I signed the corner "ff" with a double-entendre pointing to the two repeated consonants in my first name (Jeffrey) and the series title.',
  ],
  [
    "Simple Screamer",
    "The primary gesture (stem) of my previous drawing is repeated here to produce another flower, illustrating the score's repeatability and establishing a motif.",
  ],
  [
    "Tongue Warrior",
    "A small green stick figure stands with gun and sword on the tongue of this large clownflower. Three large emoticons grace the backside.",
  ],
  [
    "Dribbles",
    "A drooling, dribbling yellow flower with a three sided stem, boxed in by a colorful ribbon of its own tears.",
  ],
  [
    "Splayed Face",
    "Cornflower blue petals surround this tiny-faced flower. One petal drifts off to describe a frowning figure in a field.",
  ],
  [
    "Teal Star",
    "A frenetically rendered teal flower with RGB petals lives in a bed of white roses, one of which happily cries blood.",
  ],
  [
    "Connected Tongues",
    "Two dis-integrated flowers are bridged by a common stem and common tongue. A scrawled Butterfly Cosplayer whistlegraph stands on the floor.",
  ],
  [
    "Fake Tree",
    "The facade of this piece presents as an old, bonsai-like tree, hiding its stochastically colored stem. This is the first Rainbow Petal appearance.",
  ],
  [
    "Blown",
    "This scared, zebra stemmed blue flower anchors its long tongue on the ground whilst facing a windy challenge.",
  ],
  [
    "Corner Life",
    "Shooting straight from a predictable backend corner, this basic flower plays it safe and narrow up until the end.",
  ],
  [
    "Rainbow Tipped Tongue",
    "Giant eyebrows, scared stiff, and just trying to be heard intellectually.",
  ],
  [
    "Devout Sun Worshipper",
    "This red flower with wireframe nose lives obediently under a purple cross. A Scared of Stairs whistlegraph graces the floor here.",
  ],
  [
    "Growing Pains",
    "Too long, too coily, too thin... and totally strung out, manifesting two faces. It's hard to be 13!",
  ],
  [
    "Plane Pals",
    "Two flowers wrap around one another, one of which pukes rainbows into the metadata and looks towards the first Red Message in the series. The other is completely petal-less.",
  ],
  [
    "Nose Growth",
    "The nose of the teal flower on the left mutated out into a new species generation, killing its host in the process. First Double Head of the series.",
  ],
  [
    "Snack Back",
    "An extremely excited, gap-toothed flower has turned its back on the audience to enjoy a tasty snack.",
  ],
  [
    "Rainbow Emptiness",
    "The Rainbow Petal here is not healthy for 17. It's too heavy an appendage, sucking in all surrounding interest. This flower is growing from an asteroid.",
  ],
  [
    "Squashed & Destroyed",
    'This facade bears all, but behind is a different spatial metaphor: one of unrequited flower love. First "Flat" flower of the series.',
  ],
  [
    "FREAK",
    "Five faceless buds spell out FREAK. On the right, it appears a flower is peeing. Closer inspection reveals it's just a leaky rainbow petal.",
  ],
  [
    "Eye Junk",
    "A basic, zebra mouthed flower with no frills, other than some polychrome eye junk. (On the 2 as well!)",
  ],
  [
    "Worried Sick",
    'A girthy flower looks out into a space of anxiety. It\'s mouth makes up the label "Fatty", and there is a Red Message on the back.',
  ],
  [
    "Scrappy & Awkward",
    "Can barely keep it together. A beautiful colored bumper is stitched into the face of this flower to pad things out.",
  ],
  [
    "Spiral Displayer",
    "Not shy, showing full spiral stem in the top left quadrant. Trying to show off. All green petals.",
  ],
  [
    "Right Turner",
    'Look on the right to see the expression of this flower drooling over a volumetric patch of grass. First "Turner" of the series.',
  ],
  [
    "Rainbow Gothic",
    "With a figurative facade representative of a Tim Burton character, this flower is just trying to trick you into thinking it's something else, in party mode!",
  ],
  [
    "Vicious Scaler",
    "The stem thickness has incredible range here on a flower that looks as mean as it does ambitious. Boxed in horizontally by its own ribbon petal.",
  ],
  [
    "Flat-stemmed & Happy",
    "This flower has weaving planar stem that holds space for a colorful piece of mesh junk, in addition to a shadow-self backface.",
  ],
  [
    "Purple & Daze",
    "Two stem-sharing friends face outwards in happiness and excitement. The daisy is drooling and there is a drawing on the rainbow wall in the back.",
  ],
  [
    "Broken Waypoint",
    'A sign that reads "Freaky Flower" under a large purple monstrosity. The back of the stem has a sad face though 😢.',
  ],
  [
    "Muzzled Lion",
    "A cage from the ceiling has trapped this yellow sunflower with its tongue sticking out.",
  ],
  [
    "Outburst",
    "A one-toothed complainer tells all with a Red Message in the back.",
  ],
  [
    "Nonagonner",
    "A blue faced nonagon with deteriorating pupils, rainbow mouth and pink gift-bow-like petals.",
  ],
  [
    "Basically Bare",
    "A drop dead simple bald flower stares at a colorful mesh that left a trail of metadata.",
  ],
  [
    "River of Tongue-dement",
    "A crash and burn rainbow-petal flower salivates a river for itself. Contains a figurative backside drawing.",
  ],
  [
    "Quintet",
    "Four solidly colored channels escape the primary mouth of a full blossom, with a chain of depressive subordinates behind.",
  ],
  ["Full Zebra", "Panicking with low chroma stripes... to the highest degree."],
  [
    "Satisfied",
    "Teal and red markers draw out the axes for this happy and healthy freak with thick purple eyebrows.",
  ],
  [
    "Dashed Utterer",
    "A sky blue octagonally faced flower shares from their soapbox: a colorful array of planes!",
  ],
  [
    "Happy Projection",
    "Using all of its imagination a pathetically small bud draws a bit of what happiness might look like.",
  ],
  [
    "Pasty Crisis",
    "Scared and almost fully blocked by its own petals which furl inside themselves.",
  ],
  [
    "CMYK Broken",
    'Cross-eyed and crazed with a Red Message behind that reads: "PRO" as in gamer...',
  ],
  [
    "Square Squirm",
    'Square-stemmed white clowny flower with yellow and black petals. Answer to everything? No. Just, "Basic!"',
  ],
  [
    "Pink Coil",
    "Cute pink coiled flower looks totally normal with a portrait of a crying girl on the back.",
  ],
  [
    "Basic Rainbow Petal",
    "Unable to face the audience, this flower has little self-esteem because despite having a Rainbow Petal its others are all empty.",
  ],
  [
    "Melter",
    'A brightly colored yellow flower on a red backdrop becomes gooey. Its stem is "painted" with various planes with a pink flower melting off the back.',
  ],
  [
    "Shacked Shrieker",
    "Peeking out from a house of some kind this green flower is welcoming you to read the Red Message on the back!",
  ],
  [
    "Hangers",
    "Caught in a bind by this purple predator with CMYK petals are two excited snacks.",
  ],
  [
    "Backplane Poker",
    "An elegently hued impressionist flower juts out of a totally normal, not special, teal back plane.",
  ],
  [
    "Sad & Planar",
    "A planar stem with a black shadow drawing on the back emphasises the heavy emotions of this light olive green blossom.",
  ],
  [
    "Dead Projection",
    'This violently dismembered, deconstructed flower projects an image of a dead face for the audience. The first "Dead" of the series.',
  ],
  [
    "Chased",
    "This flower is scurrying away from the backplane drawing of a two dimensional dual wielder.",
  ],
  [
    "First Base",
    "Pretty much black and red, always knowing what's cool. Has a great eye and solid taste!",
  ],
  [
    "Borderline Pathetic",
    'This is an ultra basic yellow flower whose face is wounded on the left side. The first "Ultra" of the series.',
  ],
  [
    "Crushed",
    "Sometimes you cry so hard that the tears are the colors of your insides and you can't tell the difference anymore. That's where this flower is at rn! Also, a rainbow colored ribbon passes through Sonic-style rings.",
  ],
  [
    "Chunky Nosebleeder",
    "Constructed with large chunks, a natural at commanding the space, and a little unhealthy.",
  ],
  [
    "Nervously Flat",
    "Behind this inconfident flower is someone signalling support and showing a sign of love.",
  ],
  [
    "Gaping Quintet",
    "Five flowers left agape and in awe in a diagonal that reveals a Red Message. The second flower from the top has a Rainbow Petal!",
  ],
  [
    "Rocky No Face",
    "Closed off to the world and growing in the dark (off a rock) with glowing red petals.",
  ],
  [
    "Dismantled Outliner",
    "A broken up pastel stem and discombobulated face tries to keep it together with a black outline.",
  ],
  [
    "Base Flower Eater",
    "This dank snacker just finished consuming a smaller flower and is sucking up the stem like spaghetti.",
  ],
  [
    "Knotted",
    "A super long stem compacted and bunched up in knots is responsible for the issues this flower exhibits.",
  ],
  [
    "Capital Freak",
    "A large, lime-green capital letter F outlines this happily lilac-faced flower.",
  ],
  [
    "Senseless",
    "A fairly simple freak with magenta and mauve petals, losing all senses.",
  ],
  [
    "Halftime",
    "This Nintendo friendly minimal outline of a flower with a thick polychrome petal outline represents the mid-point of the series.",
  ],
  [
    "Tubular CMYK",
    "A very abstract flower, using known palettes. The stem is planar with a thin volumetric core. A Red Message is on the left!",
  ],
  [
    "Monoprinted",
    "The sharp Gestalt on this flower shows how applicable it is towards being flattened, impressed, and ultimately conserved through various media. Please be gentle. First Mono of the series.",
  ],
  [
    "Parasitic Choker",
    "This large green flower is wearing a smaller monochromatic one as a scarf which circumnavigates its stem.",
  ],
  [
    "Left Turner",
    "A tiny welcome wagon under an arch is inviting you to look left and check out the monstrosity it lives under the presence of.",
  ],
  [
    "Focused Drooler",
    "With intense conviction, this Basic flower dribbles left-over mesh that leads to a Red Message.",
  ],
  [
    "Ultra Basic UWU",
    'Small and, exquisitely adorable. The underside of this piece reads: "so basic i wud kms". First UWU of the series.',
  ],
  [
    "Kooning",
    "This spiraling flower has incredible gestural energy, a Zebra stem section and a happy figure with stochastic shirt on the back.",
  ],
  [
    "Just Woke",
    "This flatty just woke up! Behind it's pulled up stem is a new Red Message about Apple Computer.",
  ],
  [
    "Being There",
    "This outlined flower is modest, concerned, and staying present! First Wireframe of the series.",
  ],
  [
    "Windmill",
    "A healthy level of whirlwind energy exists in this Wireframe flower. It keeps things transparent.",
  ],
  [
    "Crossed Wireframe",
    "The Wireframe label on the bottom right of this flower was cancelled out... does it really know itself? No.",
  ],
  [
    "Monumental Wireframe",
    "Boldy stating its major use case, this Wireframe flower is sad and demanding. A Red Message is on the right!",
  ],
  [
    "Staying Put",
    "Fully wedded to its home this plump specimen is consistently concerned about its own labels, even though nobody else can see them.",
  ],
  [
    "Clipping",
    'Geometry keeps intersecting itself in the stem of this flower with a flat projection. Only "noclip" in the series.',
  ],
  [
    "Stood Up",
    "This chunky flower grew legs and stands tall, holding up its own weight for once. An RGB stripe on the back yields an array of faces.",
  ],
  [
    "Low Poly",
    "This Low Poly (only of the series) labeled piece ostensibly flaps one petal, according to the motion lines.",
  ],
  [
    "Barrel",
    "Adjacent bars shoot out from each of this flower's octagonal, pink-eyed face. A small figure in the corner raises their hand for attention.",
  ],
  [
    "Weeper",
    "This mint-faced depressive has a beautiful magenta and cornflower blue stem, which grows from a river of its own tears.",
  ],
  [
    "Pink Ricochet",
    "Thin-stemmed and bouncing around inside of its container. Check that Red Message to the left!",
  ],
  [
    "Idiosyncratic Sign Holder",
    "Purple flower holding a frontal Red Message with a beautiful lightly colored mane of petals. It's crying planar rainbows. 😥",
  ],
  [
    "Homemaker",
    "This flower works hard on its home, found from scraps of plane and mesh. Plus it's got an incredibly long Zebra stem.",
  ],
  [
    "Trick No Face",
    "From the front this flower has no face, but the back reveals something different. A tongue sticks out like a shadow, with eyes on each branch.",
  ],
  [
    "Rainbow Lion",
    "Rainbow stemmed in high contrast this yellow this lion of a flower struts forth awkwardly.",
  ],
  [
    "Pastel Baby",
    "This tiny flower with bold petals is just looking for a comfy home to fly its freak flag in. The Red Message on the back refers to outdated software platforms.",
  ],
  [
    "Anthropromouthbleeder",
    "This humanistic flower face dribbles blood and bunches up in the back, where a 2D red flower sits atop a cliff.",
  ],
  [
    "Somberstem",
    "Upset, in a well constrained palette, this flower has a stem that moves in straight segments before changing tone.",
  ],
  [
    "Junky Baby",
    "This pastel colored yellow beast was crudely grown. Tough youth. It offers a small RGB flower as a gift.",
  ],
  [
    "RGB Error",
    "Something went wrong here and this flower grew in-between the spaces that were allocated to it. It won't survive.",
  ],
  [
    "Hopeful Coiler",
    "Around a thin white arch this flower hangs out and looks up to better days. A Red Message on the back sits in the center of a large face.",
  ],
  [
    "UWU BFFs",
    "Double heads make up this red-faced pastel couple who pretty much always get along and don't mind growing up on the same thin plane! A Butterfly Cosplayer whistlegraph appears behind the UWU.",
  ],
  [
    "Chill AF & Based",
    "Literally so cool this face barely expresses interest in the audience. It's busy doing something else, like... focusing on new ways to avoid you.",
  ],
  [
    "Ocular Charmer",
    "Covered in all kinds of circumscribed ornament this peacock of a flower can barely hold its head up to say hello, but it's doing it! On the back is a confused pixelation.",
  ],
  [
    "Heavy Soaker",
    "Seeded from a raincloud this drenched and sullen friend spits out a stream of polychrome planes that underline its own metadata.",
  ],
  [
    "Extroverted Bricolage",
    "A fairly confident but unkept flower with georgeous planar stem is just so happy to be in the spotlight!",
  ],
  [
    "Little Big UWU Ultra",
    "A densely painted surface makes up the face of one of the cutest positions in the series... so cute it's crying basically all the time.",
  ],
  [
    "RGB Buzz",
    "Growing only in the directions of each color channel and harboring a Red Message on the right! This is the first RGB label in the series.",
  ],
  [
    "Responsive & Adaptable",
    "By showing only the elements that really make up the structure of itself this flower is capable of growing on almost any platform!",
  ],
  [
    "Pastel Monet Goddess",
    "Freaky and seductive the elegantly constrained palette of this flower comes together to produce that cushy modernist aesthetic we all know so well at this point.",
  ],
  [
    "Framed Right Turner",
    "Look to the right and notice a giant, shy purple flower hiding behind a sad, monochromatic drawing of someone else.",
  ],
  [
    "Gee's Stem Bend",
    "Inspired by quilting aesthetics, this flat flower has a crafty feedback loop in lieu of a face. Simply put, there is a Red Message in the back!",
  ],
  [
    "Base Mouthbleeder",
    "Bold and daring mouthbleeder, with colorful flourishes jutting out along the stem, one of which goes quite far before dropping off in the back corner.",
  ],
  [
    "Clue Finder",
    "Cylindrical eyes protrude far out of this excited flower who just discovered an important clue.",
  ],
  [
    "Impressively Sad",
    "This loosely painted monochromatic flower epitomizes sadness. Frought with worry and crying a single, multicolored tear.",
  ],
  [
    "Broken Off",
    "In this all white flower, the head has been broken off from the stem. It still hangs out alone in empty space.",
  ],
  [
    "Cut Off",
    "A desaturated blue plane slices through the head of this extremely lowly feeling flower.",
  ],
  [
    "Sinister Shape",
    "A devilishly floppy-petaled form leans in and smirks at the audience. Behind is a Red Message.",
  ],
  [
    "Rainbow Wing",
    "Flower's got a planar Rainbow Petal so big it could be carried away by the wind.",
  ],
  [
    "Y Combinator Thing",
    "A happy flower with electrons shooting from its eyes is enjoying this Red Message that faces the audience directly.",
  ],
  [
    "Ethereal Doodle",
    "A nervous purple headed flower looks at a linear olive-green flower growing from a volumetric patch. A Scared of Stairs whistlegraph appears on the bottom of the right side.",
  ],
  [
    "Trio",
    "Three Freaky Flower stems held up front and back produce separate projections of a trio. In one version they're terrified, the other looks bleak.",
  ],
  [
    "Personal Eye",
    "A small yellow face grows out of the eye of this hurt flower. Its eyes have been taken over by another being so it's struggling to see. A Red Message is present on the lower back.",
  ],
  [
    "Chunky Grasslicker",
    "Mucky, gloopy, and licking grass, this flower rolls eyes at its own label. A lyrically rendered orange rose grows out the back of its head.",
  ],
  [
    "Three Dead Stems",
    'Three Freaky Flower stems jut from the back ceiling... their heads chopped off, forming pools of blood. First "Dead" of the series.',
  ],
  [
    "Dead Stem",
    "A purple headed flower explosively bleeds out on a red carpet. Do flowers even bleed? Yes!",
  ],
  [
    "Basically Dead",
    "Curling around itself one last time, this otherwise pleasant stem spits coagulations neatly beside its metadata.",
  ],
  [
    "Lovely Error",
    "A logo-like brushy orange flower got erroneously disconnected from its stem. Still happy though! A Red Message is on the right. (The second to last!)",
  ],
  [
    "Dead Attacker",
    "Just because a flower is dead doesn't mean it can't attack you with its bloodspray. Be careful dude.",
  ],
  [
    "Basically Lost",
    "Does this flower have a face? We'll never know because it came through one end of it's containment and went out the other, ignoring the audience completely! First \"Lost\" in the series.",
  ],
  [
    "Dead & Lost",
    "Even though it's been flanked by two happy projections, this flower didn't exit quite early enough and died in transit.",
  ],
  [
    "Love Note",
    "A graciously gentle flower envelopes a goodbye Red Message. A dot appears between the 1 and 2 because 1·24 is also my birthday!",
  ],
  [
    "Blue on Blue",
    "Blue petals on a blue backdrop satisfy the eye with a Gestalt-style play on this seriously expressive, pony-faced flower.",
  ],
  [
    "Out of Bounds Error",
    "The mesh went out of bounds here as a scared flower looks on to read the entire system alert as they get walled in and destroyed.",
  ],
  [
    "Base Master UWU",
    "Holding so much space for conflicting labels but still showing up happy, cute, and alive. This is the most succesful flower I've ever seen. 🥲",
  ],
  [
    "Squiggle Destroyer Ultimate",
    "Beware! This flower is a weapon. Guaranteed to totally destroy any Chromie Squiggle at any auction for all eternity.",
  ],
];

const tokenColors = [
  "2C2920",
  "BAF9EE",
  "8C5065",
  "ED85B3",
  "9ABC3A",
  "733EF4",
  "8C4C94",
  "DFD9FF",
  "AEC147",
  "E272A3",
  "B67DC1",
  "D2C591",
  "D60060",
  "EB77FB",
  "F9FB4C",
  "9C8AB9",
  "DCD56A",
  "FCFEFA",
  "1C1C2C",
  "CD96B9",
  "D4C3B1",
  "B09BE0",
  "E3D726",
  "F1B5E0",
  "6347C5",
  "8AD5FE",
  "FDED56",
  "91C100",
  "B243EF",
  "CEAA89",
  "71730A",
  "F9E2DA",
  "C5CCCC",
  "FABFF9",
  "E8C1FD",
  "8452BD",
  "2F0A29",
  "93B686",
  "8CB0DE",
  "7087E4",
  "4F94A1",
  "C569B8",
  "4A60FB",
  "F754BF",
  "A4BDD4",
  "ED5A17",
  "A5A572",
  "6D8DBA",
  "47C4D0",
  "9ED89E",
  "B80000",
  "7D98B3",
  "969AB8",
  "86FDE8",
  "407B77",
  "FEFAFE",
  "7DCE0A",
  "7F6929",
  "4C2626",
  "AEB9EF",
  "EB8031",
  "B1D0D5",
  "ED48CD",
  "FACAD9",
  "FF7271",
  "23231C",
  "171720",
  "F0CD40",
  "FEC492",
  "D0CAA8",
  "6F3683",
  "EDF6D9",
  "7D4731",
  "2C1C1C",
  "292617",
  "23261C",
  "290A17",
  "A68A8E",
  "922053",
  "8896A9",
  "E7E5A4",
  "2CAA2F",
  "5212C9",
  "BEDD00",
  "916AF6",
  "16001C",
  "A54C5A",
  "23E500",
  "577085",
  "FFBA9F",
  "EFD9CE",
  "D30000",
  "202C2C",
  "D4AE60",
  "F4B3AB",
  "5AE0F0",
  "D4ABF9",
  "84ABB0",
  "B5AAAC",
  "F9A5F3",
  "122917",
  "2F170A",
  "B6DCEB",
  "FAC53C",
  "96ADD2",
  "2C81F9",
  "693185",
  "85405C",
  "26171C",
  "0A2320",
  "83BDBC",
  "906BC8",
  "669583",
  "60432F",
  "E8E5F2",
  "F3CAF2",
  "EADCCB",
  "2C1C2F",
  "6384FB",
  "478E50",
  "17D312",
  "C848A0",
  "C2CBF4",
  "1C2C2C",
  "FFC100",
  "6578F9",
  "232626",
  "7DB76B",
  "12122F",
];

// #endregion

// 🥾 Boot (Runs once before first paint and sim)
export function boot({ wipe, params, jump, store, num }) {
  const i = tokenID({ params, num });

  const headers = (id) => {
    console.log(
      `%cFreaky Flowers`,
      `background: rgba(50, 10, 10);
     color: rgb(255, 255, 25);
     font-size: 140%;
     padding: 0 0.25em;
     border-radius: 0.15em;
     border-bottom: 0.75px solid rgb(120, 0, 0);
     border-right: 0.75px solid rgb(120, 0, 0);`
    );

    console.log(
      `%cSculpture No. ${id}/${tokens.length - 1}`,
      `background: rgba(0, 10, 10);
     color: rgb(150, 150, 150);
     font-size: 120%;
     padding: 0 0.25em;
     border-radius: 0.15em;
     border-bottom: 0.75px solid rgb(120, 120, 120);
     border-right: 0.75px solid rgb(120, 120, 120);`
    );
  };

  store["freaky-flowers"] = { tokenID: i, tokens, headers, meta }; // Note: Storage could automatically
  //                                                                        know the disk. 22.11.25.11.14

  if (store["ff"]) store["freaky-flowers"].hook = "ff";
  else store["freaky-flowers"].hook = "freaky-flowers";

  jump(
    `wand~ff${i}-${tokens[i]}` +
      params
        .slice(1)
        .map((p) => `~` + p)
        .join(""),
    true, // ahistorical (skip the web history stack)
    true // alias (don't change the address bar url)
  );
  wipe(); // Note: Could I possibly nab the background color here for loading and
  //       carry it through to wand?
}

const baseURL = "https://wand.aesthetic.computer";
const handle = "digitpain";

// Retrieve or generate a token index, given this piece's parameter list.
export function tokenID($) {
  const canRandomize = $.num ? true : false; // Return -1 if there is no randomization function (on server)
  const randomToken = canRandomize ? $.num.randInt(tokens.length - 1) : -1;
  const param1 = parseInt($.params[0]);
  return param1 >= 0 && param1 < tokens.length ? param1 : randomToken;
}

// Generates metadata fields for this piece.
// (Run by the server.)
export function meta({ params, num }) {
  const i = tokenID({ params, num });
  let out;
  if (i === -1) {
    out = {
      // Note: high res png's are also stored, but webps are for Open Graph. 22.11.28.13.13
      image_url: `${baseURL}/ff${i}-${0}-still-${handle}.webp`,
      title: "Random",
      desc: "Shows a random Freaky Flower!",
      // https://wand.aesthetic.computer/ff1-2022.11.20.11.42.50-still-digitpain.png
    };
  } else {
    out = {
      // Note: high res png's are also stored, but webps are for Open Graph. 22.11.28.13.13
      image_url: `${baseURL}/ff${i}-${tokens[i]}-still-${handle}.webp`,
      title: tokenTitlesAndDescriptions[i][0],
      desc: tokenTitlesAndDescriptions[i][1],
      // https://wand.aesthetic.computer/ff1-2022.11.20.11.42.50-still-digitpain.png
    };
  }
  return out;
}

export const nohud = true;
