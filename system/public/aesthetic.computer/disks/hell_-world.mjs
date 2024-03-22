// hell_ world, 23.10.23.19.37 😈
// This piece is a router that loads a specific `hell_ world` token in `painting` by sending it a sequence starting with the current piece.

/* #region 🏁 todo
  + Done
  - [x] make the process view zoomable / change how tap to download works?
  - [c] show a "drag to inspect" message if the scale is < 1
  - [c] Hide the display when zooming and dragging or after an idle time of
       no interaction.
  - [x] Test on mobile.
  - [x] Test in embedded views:
    - [x] https://testnets.opensea.io/assets/sepolia/0x2703a4C880a486CAb720770e490c68FD60E1Fa23/0 
    - [x] https://wildxyz-git-oct-final-testing-wildxyz.vercel.app/jeffrey-scudder/hell-world/7/?_vercel_share=sEyI6YwmMlk13VZb7neOO1i3XzGrHyvM
  - [x] The arrow keys should also move you through process in the `painting`.
    - [x] And there needs to be arrow buttons here and there.
      - [x] Here
        - [x] Add a loading display...
      - [x] There
  - [x] Live Demo 
    - [x] 61, 107, 187, 189, 194: Holy Bunnies 
    - [x] 117: Cute White Kitty
    - [x] 72: Alien Kid
    - [x] 171: Only Code 
    - [x] 132: Obby
    - [x] 175: Slitscan Spirit
    - [x] 33: Bloody Forest
    - [x] 67: Glitch Quilt 
    - [x] 197, 18, 31, 78, 97: Hell Holes  
    - [x] 188: City Smile (@bash/rainbowcascade)
    - [x] Hell Holes DIY! 
  - [x] Display the set somehow... maybe on a third line? 
  - [x] force top left button go back to the slideshow view
  - [x] don't change the slug when going to the painting page
  - [x] add autoscaling to painting process page
  - [x] also turn off the print module on the painting process page
  - [x] title second line underneath hell_world #
  - [x] Update metadata when arrow keys change a painting.
  - [x] Implement a custom `hell_ world` player / add a flag to `painting`.
  - [x] Get the hud display correctly / nice formatting.
  - [x] Test preview link in iMessage.
  - [x] Add metadata favicon.
  - [x] Send off spreadsheet.
  - [x] Implement a `hw` shortcut?
#endregion */

import * as sfx from "./common/sfx.mjs";

// #region 🧮 data

// 0-199
const tokens = [
  "2023.10.02.18.46.07.749",
  "2023.10.05.15.54.08.467",
  "2023.10.19.16.54.50.582",
  "2023.10.06.21.10.03.586",
  "2023.10.07.16.28.04.940",
  "2023.10.05.16.34.31.026",
  "2023.10.06.19.38.11.125",
  "2023.10.20.14.38.02.572",
  "2023.10.20.16.22.09.175",
  "2023.10.22.19.20.48.131",
  "2023.10.02.17.08.26.956",
  "2023.10.06.20.14.00.454",
  "2023.10.06.20.05.49.914",
  "2023.10.20.15.31.52.509",
  "2023.9.17.03.28.46.828",
  "2023.10.16.17.18.09.972",
  "2023.10.15.13.04.19.171",
  "2023.10.07.18.09.35.673",
  "2023.10.22.11.29.17.481",
  "2023.10.18.12.30.27.509",
  "2023.8.24.14.26.54.544",
  "2023.10.19.20.03.24.734",
  "2023.10.05.16.07.57.467",
  "2023.10.21.16.04.09.633",
  "2023.10.19.17.44.36.818",
  "2023.10.22.16.50.33.464",
  "2023.10.20.14.49.45.847",
  "2023.10.21.15.35.02.259",
  "2023.9.17.03.44.05.087",
  "2023.10.19.15.39.54.039",
  "2023.10.22.21.39.54.685",
  "2023.10.22.11.24.29.401",
  "2023.10.04.16.52.58.677",
  "2023.10.20.23.56.58.762",
  "2023.10.15.21.52.38.239",
  "2023.10.21.15.41.16.973",
  "2023.10.17.20.42.28.832",
  "2023.10.21.16.49.23.956",
  "2023.10.15.14.13.00.531",
  "2023.10.16.15.00.14.583",
  "2023.10.19.15.20.35.276",
  "2023.10.21.15.00.32.381",
  "2023.10.07.16.51.30.009",
  "2023.10.06.17.34.09.471",
  "2023.10.20.15.39.18.567",
  "2023.10.20.14.23.24.486",
  "2023.10.22.13.33.08.561",
  "2023.10.18.17.42.36.917",
  "2023.10.21.14.51.19.815",
  "2023.10.20.16.40.15.203",
  "2023.10.05.17.09.57.978",
  "2023.10.04.16.28.00.656",
  "2023.10.22.19.56.46.238",
  "2023.10.16.16.38.54.200",
  "2023.10.05.20.05.11.008",
  "2023.10.20.22.32.16.251",
  "2023.10.22.13.39.10.099",
  "2023.10.19.13.52.44.935",
  "2023.10.21.16.15.49.118",
  "2023.10.07.22.18.07.719",
  "2023.10.04.20.11.27.128",
  "2023.10.19.12.26.24.221",
  "2023.10.06.18.21.30.183",
  "2023.10.20.17.17.02.865",
  "2023.10.06.22.32.14.859",
  "2023.10.07.19.08.31.877",
  "2023.10.19.21.02.44.359",
  "2023.10.08.11.41.22.918",
  "2023.10.19.20.25.48.219",
  "2023.10.04.15.20.34.429",
  "2023.10.19.13.12.01.059",
  "2023.10.22.10.39.34.006",
  "2023.10.20.22.53.24.951",
  "2023.10.05.20.10.31.101",
  "2023.10.06.18.46.05.504",
  "2023.10.20.16.11.25.753",
  "2023.10.06.20.27.15.324",
  "2023.10.07.19.42.11.922",
  "2023.10.21.21.13.16.940",
  "2023.10.07.18.41.20.056",
  "2023.10.20.22.59.19.465",
  "2023.10.02.18.31.17.757",
  "2023.10.04.21.20.59.277",
  "2023.10.22.17.30.12.449",
  "2023.10.21.21.18.14.462",
  "2023.10.17.00.02.39.054",
  "2023.10.05.16.58.30.429",
  "2023.10.07.17.00.49.972",
  "2023.10.16.14.40.10.375",
  "2023.10.07.22.12.37.041",
  "2023.10.05.20.29.29.117",
  "2023.10.18.13.07.35.688",
  "2023.10.17.20.01.42.743",
  "2023.10.20.18.32.53.786",
  "2023.10.06.19.21.05.323",
  "2023.10.22.10.51.41.102",
  "2023.10.16.16.24.41.009",
  "2023.10.06.19.25.52.741",
  "2023.10.06.17.55.01.998",
  "2023.10.20.15.18.03.539",
  "2023.10.22.20.09.53.095",
  "2023.10.06.21.49.49.589",
  "2023.10.19.12.19.11.587",
  "2023.10.18.12.47.18.798",
  "2023.10.18.12.54.42.707",
  "2023.10.22.11.45.54.796",
  "2023.10.20.15.06.06.696",
  "2023.10.02.17.38.11.494",
  "2023.10.07.22.14.38.879",
  "2023.10.19.17.19.44.933",
  "2023.10.21.21.45.21.325",
  "2023.10.07.17.55.29.581",
  "2023.10.22.13.51.13.461",
  "2023.10.21.15.26.47.505",
  "2023.10.05.14.49.45.150",
  "2023.10.07.22.36.29.479",
  "2023.9.28.23.13.10.109",
  "2023.10.22.13.05.11.188",
  "2023.10.22.11.56.41.736",
  "2023.10.04.14.02.30.065",
  "2023.10.15.15.05.16.267",
  "2023.10.18.15.26.09.691",
  "2023.10.07.21.03.35.875",
  "2023.10.19.21.12.22.548",
  "2023.10.15.15.48.06.248",
  "2023.10.15.21.12.10.864",
  "2023.10.05.20.17.00.954",
  "2023.10.19.15.48.43.875",
  "2023.10.07.19.35.30.962",
  "2023.10.19.14.28.06.216",
  "2023.10.19.13.59.58.698",
  "2023.10.20.18.22.53.453",
  "2023.10.20.22.41.59.613",
  "2023.10.07.17.50.44.382",
  "2023.10.22.12.44.04.231",
  "2023.10.21.00.30.28.124",
  "2023.9.12.03.53.42.970",
  "2023.10.19.21.34.33.142",
  "2023.10.22.19.41.21.245",
  "2023.10.19.12.55.27.997",
  "2023.10.21.15.07.39.589",
  "2023.10.04.14.54.25.170",
  "2023.10.20.14.58.03.342",
  "2023.10.15.13.32.25.090",
  "2023.10.06.22.07.18.342",
  "2023.10.05.17.17.34.704",
  "2023.10.20.19.01.54.048",
  "2023.10.17.20.54.58.881",
  "2023.10.21.21.52.19.797",
  "2023.10.06.18.04.43.874",
  "2023.10.15.11.33.26.734",
  "2023.10.22.21.01.20.710",
  "2023.10.22.21.46.23.680",
  "2023.10.07.18.52.42.917",
  "2023.10.07.16.33.42.005",
  "2023.10.17.23.41.48.134",
  "2023.10.20.22.08.30.735",
  "2023.10.17.20.18.27.734",
  "2023.10.05.16.17.26.922",
  "2023.10.06.22.37.16.129",
  "2023.10.05.20.44.28.830",
  "2023.10.19.16.35.57.780",
  "2023.10.22.20.23.30.574",
  "2023.9.04.23.52.21.817",
  "2023.10.04.16.00.46.059",
  "2023.10.22.19.49.57.781",
  "2023.10.17.20.28.07.003",
  "2023.10.15.21.34.02.227",
  "2023.10.19.15.57.55.006",
  "2023.10.07.22.41.03.733",
  "2023.10.07.22.23.36.818",
  "2023.10.19.15.27.47.789",
  "2023.10.17.19.54.59.863",
  "2023.10.17.15.03.10.050",
  "2023.10.05.21.12.08.833",
  "2023.10.22.20.13.30.207",
  "2023.10.04.14.25.30.670",
  "2023.10.19.17.26.08.333",
  "2023.10.13.23.42.09.344",
  "2023.10.22.11.06.02.141",
  "2023.10.21.11.44.20.060",
  "2023.10.18.13.14.49.560",
  "2023.10.22.20.52.25.874",
  "2023.10.17.15.08.58.569",
  "2023.10.22.20.17.36.936",
  "2023.10.22.19.29.46.489",
  "2023.10.07.19.33.51.907",
  "2023.10.19.17.36.05.171",
  "2023.10.05.15.45.47.184",
  "2023.10.22.20.05.12.265",
  "2023.10.22.17.41.51.311",
  "2023.10.06.21.24.35.412",
  "2023.10.06.19.58.05.866",
  "2023.10.15.21.25.29.387",
  "2023.10.19.19.50.11.024",
  "2023.10.08.14.51.39.443",
  "2023.10.07.16.08.55.051",
  "2023.10.22.11.38.03.276",
  "2023.10.20.17.51.38.224",
  "2023.10.19.13.04.18.088",
];

const tokenTitlesAndDescriptions = [
  [
    "Hell World",
    "Welcome to hell_ world. A series of 200 paintings by Jeffrey Alan Scudder, made in aesthetic.computer.",
  ],
  ["Jeffrey", '"hello my name is jeffrey and i like to paint :("'],
  ["Purple Cave", "Every mountain is purple and every mountain is hollow."],
  ["Abex VII", "It's abstract."],
  [
    "Test Dummy",
    "Physical tests occur often in hell and run best with basic geometric primitives.",
  ],
  ["Abex XIX", "It's abstract."],
  [
    "Hell Platformer",
    "Shy rainbow starting the bloodiest level of a hell-based platformer game.",
  ],
  [
    "Traffic Light",
    "They can never decide whether to keep going together or stop it all.",
  ],
  ["Never Again", "A big message appears in front of the user's viewport."],
  ["Western Digital", '"western digital"'],
  ["Abex X", "It's abstract."],
  [
    "Television Riddle Trap",
    "Indecipherable text is piped in through a red sign on this outdoor monitor.",
  ],
  ["Level Up", "Red labyrinth with black all around."],
  ["Distinguished Pump", "An exquisitely rendered pumpkin."],
  [
    "First No History",
    "A large pixelated armature houses this running figure.",
  ],
  ["Metallic Stripes", "A surface-level topographic texture."],
  ["Crying Baby", "There is no rattle, toy or snack here to soothe them."],
  ["Jam-Scraped Wooly", "Does it want a hug?"],
  [
    "Earthy Half-Shut Hell Hole",
    "If you squint, it looks like an elevator button from Doom or Quake!",
  ],
  ["Bunched Losers", "That feeling when your team loses with best effort."],
  ["Abex XVI", "It's abstract."],
  ["Love Hate", '"i h8(ave) love"'],
  ["Pull String", "A polychrome line pokes out of pink."],
  [
    "Pollination",
    "Colors fly around this painting like pollens in the spring wind.",
  ],
  ["Too Much Fun", "A crude duck dyes the water with blood from its mouth."],
  ["Loopy Alien", "Not only is the alien loopy, it's a public speaker."],
  [
    "Bloody Horizon",
    "A strip of red marks the horizon in an electric blue world.",
  ],
  ["Curly Spurt Stones", '"How can the stones be bleeding, that\'s so weird."'],
  [
    "Fifth No History",
    "Bloody red-faced kiddo chases after another blue kiddo.",
  ],
  ["Hugless Ambient", "`no_arms === no_hugging`."],
  ["Smile Fish", "We like to see fish being happy."],
  [
    "Half-Shut Hell Hole",
    "Neon growth surrounds this small hell hole with scratches on the side.",
  ],
  ["Calm Happy Screen", "A yellow and green screen with a bloody reflection."],
  [
    "Bloody Forest",
    "Broken hearts and rainbow messes litter the forest floor.",
  ],
  [
    "Projected Runner",
    "A blue sketchy guy who's going to run out of frame to his bloody death.",
  ],
  ["Just A Door", "A doorway appears in a surface reflection."],
  [
    "Outline Crier",
    "Maybe being outlined makes his pain able to be understood.",
  ],
  [
    "Eye Drinkers",
    "Pump kids love drinking each other's pulp from their eyes.",
  ],
  ["Hell Stick", "Is he falling into a pit of torment?"],
  [
    "Worried 'Bout Searing Laser",
    "Just trying to be melancholy by the water but there is a laser.",
  ],
  ["Grabby Tree", "Trees in hell can come alive and try to grab you."],
  [
    "Cute Brown Kitty",
    "A brown kitty stares at the viewer with its back leg up.",
  ],
  ["Abex XIV", "It's abstract."],
  ["Shadow Raiser", "The shadow raiser walks between hell and earth."],
  [
    "Cuboid Boy Pump",
    "A pumpkin that was squished into a cube as it grew up in the patch.",
  ],
  [
    "Big Goth Tree",
    "The red part of this rainbow has come alive and it's a tree.",
  ],
  ["Stitched & Gooey Entrance", "Can the gooey entrance hurt me?"],
  ["First No", '"no."'],
  [
    "Cute Electrocuted Blue Kitty",
    "A blue kitty was struck by lightning while hanging out in hell.",
  ],
  [
    "Bird Eclipse",
    "I wonder what kind of bird that is flying in front of the orange sun setting on a nice green field.",
  ],
  ["Happy Boy", "An unhappy boy screams out for someone else."],
  ["No More Play", "A dog plushie examines a disappearing something."],
  ["Glowing Hell Hole", "Is this one sticking out its tongue?"],
  [
    "So Shiny Pipes",
    "Shiny rainbow pipes weave between one another, all headed in the same direction.",
  ],
  [
    "Blonde Sap Hallucination",
    "A curly-haired blonde would like a pedicure from the viewer.",
  ],
  [
    "Scrappy Pump",
    "Someone got creative with this scrappy scrawly pumpkin on a yellow background.",
  ],
  ["Abex IX", "It's abstract."],
  ["Wanted You", '"i wanted u 2 come BEFORE it got this way"'],
  [
    "Space Suit Uh-Oh",
    "He wore his space suit to hell and it didn't agree with the temperatures.",
  ],
  ["No Time", '"she never wants 2 speND TIME WITH meeeeeeeee"'],
  ["Desperate Mumbler", "He's framed and he's desperate."],
  [
    "Burning Sack of Bunny",
    "Sometimes a flame on the ear can mean the end of the world.",
  ],
  ["Shady Smile", "A blue face happy in different shades."],
  ["Pump Kin", "Bleeding pumpkin has a crisis of identity."],
  [
    "Night Visitor",
    "Arm outstretched, he wants to hold hands, and is just as nervous as you.",
  ],
  [
    "Giving Directions Smile",
    "You can't trust directions from a smile like that.",
  ],
  ["Enter Here", "The first page of my diary contains a sad warning."],
  [
    "Glitch Quilt",
    "Horizontal rainbow stripes are fuzzed up to the point of unrecognizability, in gradient.",
  ],
  ["Hurt Back", '"my back hurts baby"'],
  [
    "Inverted Demolition",
    "What if we didn't blow up but the explosion happened all around our bodies?",
  ],
  ["Abex III", "It's abstract."],
  [
    "Cursed Cartoons",
    "Cathode ray particles ooze through this illuminated kid-friendly display.",
  ],
  [
    "Alien Kid",
    "Alien kid waiting for the alien bus on the first day of alien school. Rainbow stockings will make him cool.",
  ],
  ["Simple Like That", '"ok im gonna just be simple like that" "ok?" "ok!"'],
  ["Death Garden", "The bushes are bleeding and our gardener likes it."],
  [
    "Separated Pumps",
    "Commentary on the social alienation of pumpkins who get picked from the patch.",
  ],
  ["Lost", "Red tunnel through the black depths of hell."],
  ["Super Crazy Happy", "Honestly the most happy painting of them all."],
  [
    "Drippy Half-Shut Hell Hole",
    "Deliquescent vibes are best for blue hell holes.",
  ],
  ["Fire Skeptic", "Is this fire even happening?"],
  ["Little Yellow Hut", "Blood paves the walkway to this friendly entrance."],
  [
    "Song of Spook Tree",
    "A spooky dead tree is the focus of this enchanting evening scene.",
  ],
  ["Abex XX", "It's abstract."],
  ["Abex VIII", "It's abstract."],
  [
    "Trippy Half-Shut Hell Hole",
    "This hell hole had too much, or maybe you did?",
  ],
  ["Glow Cubes", "Three red and pink forms encounter a diagonal."],
  [
    "Left Corner Misery",
    "Navy blue face with red eyes and mouth hides in the corner.",
  ],
  ["Red Interruption", "Partial rainbow stripe, with red all bunched up."],
  [
    "Bullet Stitch",
    "Misfire from the bunny hunt made a hole in this rainbow tapestry.",
  ],
  ["Low Rose World", "Like Tetris but blurrier, smells better, and in hell."],
  ["Dead Heavy Smile", "So happy, can't move."],
  ["Evil Preppy Lawn", "Who manicured this lawn?"],
  [
    "Disintegrating Fish",
    "A polychrome fish breaks apart while hunting at the bottom of `pond`.",
  ],
  ["Hell Hole", "A deep black hole that leads to nowhere."],
  [
    "Emo Bangs Fail",
    "An emo kid being himself in a mostly yellow environment.",
  ],
  ["Love Not Users", '"i want ur love not ur users"'],
  [
    "Opalescent Girl Smile",
    "The opalescent girl is protected by a red force field.",
  ],
  ["Stepped Hell Hole", "Staggered, aliased hole with blood spewing out."],
  [
    "Green Transmit",
    "Repeating rainbow field with a gestaltean green escapement.",
  ],
  [
    "Spiral Nose",
    "A rejected spiral-nosed pumpkin on a strip of green ground.",
  ],
  [
    "Spooked Spirit",
    "This gray fuzzy ghost is soft, and steering clear of a sharp pipe.",
  ],
  [
    "Cursed Carnival",
    "Energetic night scene with colorful carnival lighting vibes.",
  ],
  ["Abex V", "It's abstract."],
  ["Tired & Creamy", "Bright backdrop for a long face."],
  ["Night Waterfall", "At night the water gets extra sparkly."],
  [
    "Drippy Half-Shut Hell Hole",
    "Colors from the edges of the picture are sucked inwards by the rectilinear opening.",
  ],
  [
    "Whole Family",
    "Pumpkins bred more pumpkins to fill this pumpkin portrait.",
  ],
  [
    "Well-Hunted Bunny",
    "This bunny is so happy to die and become food for its hunter.",
  ],
  ["Cool World", '"the world is rly cool"'],
  ["Don't Let Me Die", '"dont let me DIEEeeeeee god"'],
  [
    "Twin Monitor Basement Life",
    "We multi-task in hell the same way you do in heaven.",
  ],
  ["Black Rainbow", "Six deteriorating rainbow pipes."],
  [
    "Three Office Trees",
    "Three blossoming trees arranged side by side in a portrait of manicured nature.",
  ],
  ["Painterly Pumps", "A few pumpkins with painterly lighting and brushiness."],
  [
    "Smile Running By Sad",
    "A meh boy and sad girl hang out by an orange ethereal flower.",
  ],
  [
    "Splatter Carpet",
    "Sometimes a splatter in the carpet can be a work of art.",
  ],
  ["Third No History", "Sad video game controller."],
  [
    "Cute White Kitty",
    "This happy kitty faces to the right on a crimson ground.",
  ],
  [
    "Deteriorating Half-Shut Hell Hole",
    "In cathedrals you have glowing stained glass, but in hell_ world you just have fuzzy hell holes like this one.",
  ],
  [
    "Squiggly Ribbons",
    "Regular gashes allow for polychrome pixel transfer, up and down.",
  ],
  ["Abex VI", "It's abstract."],
  [
    "Warbled Sufferer",
    "A tragically whimsical landscape with fireworks and contemplation.",
  ],
  ["Hell Dancing", "What the hell is he so happy about?"],
  [
    "Red Green Flag",
    "A red and green painting with a simple flag in the middle.",
  ],
  [
    "Blood Static",
    "Strong horizontal blood energy rests on stretched blue light energy.",
  ],
  ["Abex XI", "It's abstract."],
  [
    "Eyelid Hanging",
    "Smol conscience executed by hanging, causing the big guy to cry.",
  ],
  ["Computer Chess Bloodbath", "This chessboard is messy from battle."],
  ["Plateaux Head", "A rough black outline figure on a flat background."],
  ["Happy Girl", "\"Yes, I'm pretty sure that's a happy girl.\""],
  ["Ground Up Close", "The basic bits of hell."],
  [
    "Twinning Pumps",
    "Birds of a feather, pumpkins of the vine, bonded for life.",
  ],
  ["Obby", "The best game genre in hell!"],
  ["Calm Place", "Serene blues make up this calm place."],
  ["Bad Vibe Sweet Potatoes", "These sweet potatoes have bad vibes."],
  ["Abex I", "It's abstract."],
  [
    "Fourth No History",
    "A broken 1px rainbow spiral in a dark hole, on top of a smear test.",
  ],
  [
    "The Last Asparagus",
    "Thieves ripped all the asparagus out of this garden except for this droopy one.",
  ],
  ["Molten Door", "You can look toward, but you better walk away."],
  ["Abex XVII", "It's abstract."],
  ["Blood Steps", "Blood trips down the staircase to hell."],
  ["No Sequence", '"no no no no no no no no no no no no no ne..."'],
  ["Rainbow Road Home", "Rainbow pixels pave the walkway to the neighbors."],
  ["Empty Heartface", "Love is all over his face like a fool."],
  ["Frozen Screaming Kitty Corpse", "The soil of hell is petrified kitties."],
  ["Magenta Snotty Grin", "His blue snot is so gross and weird."],
  [
    "Luscious Archway",
    "Golden yellow light and lush greenery come together to make a beautiful place.",
  ],
  [
    "Tie My Heart Up",
    "Heart-shaped balloons float in compartments like storefronts at the mall.",
  ],
  [
    "Cursed Test Pattern",
    "Two worm-like forms battle across a striped test pattern.",
  ],
  [
    "Blood Layer Cake",
    "Soft frosted layers of densely muted pixels make up this mass.",
  ],
  ["Abex XV", "It's abstract."],
  ["Loitering Spirits", '"Come hang out with us. Be with us."'],
  ["Blehh Flower", "This flower is so blehhhh it's having a hard time."],
  [
    "Blood Circuit",
    "Some circuits are made from slime. Hmm, all blood is slime-like!",
  ],
  ["Sunset Killer", "He kills every day at dusk."],
  ["Grumpy Alien", "This alien's frown unfurls into swirling gray clouds."],
  ["Nesting Pumps", "Is the big pumpkin pregnant with the littler pumpkin?"],
  ["Mute", "A hazy and dirty viewport, hard to make out any horizon."],
  [
    "Rainbow Pipe Cleaner",
    "This full-time cleaner uses blood to make sure the rainbow pipes are all sorted.",
  ],
  ["Shock Therapy", "Everyone needs a little shock therapy."],
  ["Targeting Frays", "One hopes to not be targeted by these frays."],
  [
    "Rainbow Pill Waterfall",
    "A floating rainbow pill breaks open, contents pouring out.",
  ],
  ["Rainbow Invader", "An alien with rainbow pants wields two guns."],
  ["Second No History", "A yellow fossil-like face."],
  [
    "Oasis in the Computer",
    "My oasis is in the computer, your oasis doesn't exist, we are not the same.",
  ],
  ["Yawning Hell Hole", "A pareidolic sewer."],
  ["Dream Door", "A striped door emerges from darkness."],
  ["Abex II", "It's abstract."],
  ["Barricade", "This orange barricade on a blue background keeps evil away."],
  ["Flower Shot", "Flowers have blood inside?"],
  ["Abex XIII", "It's abstract."],
  ["Only Code", '"THOUGHT i had u BUT i ONLY have code"'],
  ["Abex IV", "It's abstract."],
  ["Acid Alien", "An alien with trippy priorities in his happy place."],
  ["Abex XII", "It's abstract."],
  [
    "Slitscan Spirit",
    "A ghost is scanned horizontally as it appears to the user.",
  ],
  ["Campfire Horror", "Is he telling the story or is he part of it?"],
  ["Ember Forest", "There's not much left after the rainbow forest fire."],
  ["Rainbow Ghost", "This is the lowest resolution painting of them all."],
  ["Trapped Balloon", "Balloons are supposed to soar high not sink low."],
  ["Poe Pump", "The way Edgar Allan Poe would draw a pumpkin."],
  ["Crying Over Babies", "What happened to the babies?"],
  ["Sad Spirits", "Two lonely ghosts don't mind if you interact."],
  ["Sad Galactic Resident", "Being in space sounds depressing as hell."],
  ["Busted Spirit", "It was only a few pixels anyway."],
  ["Abex XVIII", "It's abstract."],
  ["Little Dance", '"this is the little dance i do here"'],
  ["Revenge Hunter Bunny", "Some bunnies fight back aggressively."],
  [
    "City Smile",
    "A scary smile culled from a user programmed brush and a stolen `pond` screenshot.",
  ],
  [
    "Holy Bunny Puppeteer",
    "Trying to put on a show by juggling bleeding holes.",
  ],
  ["Mouth Hug", "Not jigglypuff or kirby, nothing at all. Perfect to squeeze."],
  [
    "Acid Plaid Skirt",
    "Even the most cliché patterns exist here, on the most cliché forms.",
  ],
  ["Cobweb Hole", "Wow looks like nobody has used this hole in awhile."],
  [
    "Bubble Incubator",
    "Just another piece of infrastructure, necessary for chaos.",
  ],
  [
    "Semi-Computed Bunny",
    "Affected by the result of a rendering error but still mobile.",
  ],
  ["People Are Scary", '"ppl r scawy"'],
  ["Shell-Shocked Kiddo", "There are traumatic memories being formed here."],
  ["Half-Shut Hell Hole Well", "No water in this well, just a hole to hell."],
  ["Boo Ghost", "Is he foaming at the mouth?"],
  ["Stretched Complainer", "Complainers go to hell."],
];

const tokenSets = {
  "diary pages": [1, 9, 21, 57, 59, 66, 68, 73, 95, 108, 171, 186, 195],
  smearscapes: [2, 89, 130, 138, 157, 161, 193],
  abex: [
    3, 5, 10, 20, 42, 56, 70, 82, 83, 102, 120, 125, 135, 139, 150, 167, 170,
    172, 174, 185,
  ],
  "blood tears": [4, 7, 16, 19, 90, 92, 126, 153, 158],
  "interior designs": [
    6, 12, 22, 26, 76, 85, 115, 127, 132, 133, 140, 166, 192,
  ],
  negatives: [8, 47, 109, 141],
  "cursed screens": [11, 71, 110, 124, 148],
  "still life": [
    13, 40, 55, 63, 75, 88, 101, 104, 113, 131, 134, 156, 159, 168, 180,
  ],
  "no history": [14, 28, 116, 136, 163],
  stripe: [15, 53, 67, 87, 98, 111, 119, 149, 191],
  figures: [
    17, 29, 34, 38, 39, 43, 54, 58, 60, 69, 94, 121, 128, 143, 196, 199,
  ],
  "hell holes": [18, 31, 52, 78, 84, 93, 97, 105, 118, 165, 197],
  outside: [23, 27, 45, 49, 74, 81, 91, 112, 123, 137, 146, 152, 164, 169, 177],
  plushies: [24, 51, 64, 190],
  aliens: [25, 72, 155, 162, 173],
  smiles: [30, 32, 62, 65, 77, 96, 114, 122, 145, 188],
  "rainbow quilts": [33, 46, 160],
  houses: [35, 80, 142],
  faces: [36, 79, 86, 103, 154, 176, 178, 181, 183, 198],
  "pump pals": [37, 44, 99, 106],
  kitties: [41, 48, 117, 144],
  "happy couple": [50, 129],
  "holy bunnies": [61, 107, 187, 189, 194],
  spirits: [100, 151, 175, 182, 184],
  balloons: [147, 179],
};

// #endregion

let index,
  painting,
  headers,
  controller = null;
let zoomed = false;
let zoomLevel = 1;
let debug;

let timestampBtn, prevBtn, nextBtn;
let timestampColor = "white";
let timestampColorBlinker;

let ellipsisTicker;
let process;
let jumpingToProcess = false;

// 🥾 Boot
function boot({ wipe, params, num, pieceCount, jump, api, debug: d, gizmo }) {
  index = tokenID({ params, num });

  //if (params[0] === undefined) {
  // net.rewrite(`${piece}~${index}`, true); // Set the URL to the current param.
  //}

  debug = d;

  process = () => {
    jump(
      `painting:show~@jeffrey/${tokens[index]}` +
        params
          .slice(1)
          .map((p) => `~` + p)
          .join(""),
      false,
      true,
    );
  };

  jumpingToProcess = pieceCount === 0 && params[0] !== undefined;
  if (jumpingToProcess) process();

  ellipsisTicker = new gizmo.EllipsisTicker();
  timestampColorBlinker = new gizmo.Hourglass(120, {
    flipped: () => {
      timestampColor = timestampColor === "white" ? [255, 255, 190] : "white";
    },
    autoFlip: true,
  });

  headers = (id) => {
    console.log(
      `%chell_ world`,
      `background: rgba(50, 10, 10);
     color: rgb(255, 255, 25);
     font-size: 140%;
     padding: 0 0.25em;
     border-radius: 0.15em;
     border-bottom: 0.75px solid rgb(120, 0, 0);
     border-right: 0.75px solid rgb(120, 0, 0);`,
    );

    console.log(
      `%cPainting Index: ${id}/${tokens.length - 1}`,
      `background: rgba(0, 10, 10);
     color: rgb(150, 150, 150);
     font-size: 120%;
     padding: 0 0.25em;
     border-radius: 0.15em;
     border-bottom: 0.75px solid rgb(120, 120, 120);
     border-right: 0.75px solid rgb(120, 120, 120);`,
    );
  };

  getPainting(index, api);

  // TODO: Do I need this for `hell_ world`?
  // store["hell_-world"] = { tokenID: i, tokens, headers, meta };

  // if (store["hw"]) store["hell_-world"].hook = "hw";
  // else store["hell_-world"].hook = "hell_-world";
  wipe();
}

// 🎨 Paint
function paint({
  ink,
  text,
  screen,
  paste,
  wipe,
  noise16DIGITPAIN,
  pen,
  geo,
  ui,
  help,
  paintCount,
}) {
  if (painting) {
    const margin = 34;
    const wScale = (screen.width - margin * 2) / painting.width;
    const hScale = (screen.height - margin * 2) / painting.height;
    let scale = Math.min(wScale, hScale, 1);
    if (wScale >= 2 && hScale >= 2) scale = 2;
    let x = screen.width / 2 - (painting.width * scale) / 2;
    let y = screen.height / 2 - (painting.height * scale) / 2;

    if (pen && zoomed && (scale < 1 || scale === 1)) {
      const imgX = (pen.x - x) / scale;
      const imgY = (pen.y - y) / scale;

      // Adjust scale and position for zoom anchored at pen position
      scale = scale >= 1 ? 1 + zoomLevel : zoomLevel;

      x = pen.x - imgX * scale;
      y = pen.y - imgY * scale;
      ink(0, 64).box(0, 0, screen.width, screen.height);
    } else {
      wipe(0);
    }

    paste(painting, x, y, { scale });

    if (controller) {
      ink(0, prevBtn?.down || nextBtn?.down ? 200 : 127).box(
        0,
        0,
        screen.width,
        screen.height,
      );
      ink("pink").write(`Fetching${ellipsisTicker.text(help.repeat)}`, {
        center: "xy",
      });
    }

    if (!jumpingToProcess) {
      const pos = { x: 6, y: screen.height - 15 };
      const box = text.box(tokens[index], pos).box;
      const blockWidth = 6;
      box.width -= blockWidth * 2;

      // Prev & Next Buttons
      const prevNextMarg = 32;
      const prevNextWidth = 32;

      if (!prevBtn) {
        prevBtn = new ui.Button();
        if (index === 0) prevBtn.disabled = true;
      }

      prevBtn.box = new geo.Box(
        0,
        prevNextMarg,
        prevNextWidth,
        screen.height - prevNextMarg * 2,
      );

      if (!prevBtn.disabled) {
        prevBtn.paint((btn) => {
          ink(btn.down ? "orange" : 255).write("<", {
            x: 6,
            y: screen.height / 2 - 4,
          });
        });
        ink(255, 255, 0, 8).box(prevBtn.box);
        // ink(0, 255, 0, 127).line(
        //   0,
        //   screen.height / 2,
        //   screen.width,
        //   screen.height / 2,
        // ); // 📏
      }

      if (!nextBtn) {
        nextBtn = new ui.Button();
        if (index === tokens.length - 1) nextBtn.disabled = true;
      }

      nextBtn.box = new geo.Box(
        screen.width - prevNextWidth,
        prevNextMarg,
        screen.width,
        screen.height - prevNextMarg * 2,
      );

      if (!nextBtn.disabled) {
        nextBtn.paint((btn) => {
          ink(btn.down ? "orange" : 255).write(">", {
            x: screen.width - 10,
            y: screen.height / 2 - 4,
          });
        });
        ink(255, 255, 0, 8).box(nextBtn.box);
      }

      // 📏 Rulers
      // ink(0, 255, 0, 64).line(6, 0, 6, screen.height);
      // ink(0, 255, 0, 64).line(
      //   screen.width - 6,
      //   0,
      //   screen.width - 6,
      //   screen.height,
      // );

      if (!timestampBtn) timestampBtn = new ui.Button(box);
      timestampBtn.box = geo.Box.from(box);
      timestampBtn.paint((btn) => {
        ink(btn.down ? "orange" : timestampColor).write(tokens[index], pos);
      });

      ink("white").write(tokenTitlesAndDescriptions[index][0], { x: 6, y: 18 });

      const tokenSet = findTokenSet(index);
      if (tokenSet) {
        ink(255, 0, 0, 127).write(tokenSet, {
          x: 6,
          y: screen.height - 15 - 14,
        });
      }
    }
  } else {
    noise16DIGITPAIN();
  }
}

// 🎪 Act
function act({ event: e, api, sound, jump, params, download }) {
  timestampBtn?.act(e, () => {
    sfx.push(sound);
    process();
  });

  function next() {
    if (index === tokens.length - 1) return;
    sfx.push(sound);
    index += 1;
    if (index > tokens.length - 1) {
      index = tokens.length - 1;
    } else {
      getPainting(index, api);
    }
    prevBtn.disabled = false;
    nextBtn.disabled = false;
    if (index === tokens.length - 1) nextBtn.disabled = true;
  }

  function prev() {
    if (index === 0) return;
    sfx.push(sound);
    index -= 1;
    if (index < 0) {
      index = 0;
    } else {
      getPainting(index, api);
    }
    prevBtn.disabled = false;
    nextBtn.disabled = false;
    if (index === 0) prevBtn.disabled = true;
  }

  nextBtn?.act(e, next);
  prevBtn?.act(e, prev);

  if (e.is("keyboard:down:arrowright")) next();
  if (e.is("keyboard:down:arrowleft")) prev();

  if (
    e.is("touch:1") &&
    !timestampBtn?.down &&
    !prevBtn?.down &&
    !nextBtn?.down &&
    e.button === 0
  ) {
    zoomed = true;
  }

  if (e.is("lift:1")) zoomed = false;

  if (e.is("keyboard:down:space")) {
    zoomLevel += 1;
    if (zoomLevel > 3) zoomLevel = 1;
  }

  if (e.is("keyboard:down:p")) {
    process();
    sfx.push(sound);
  }

  if (
    painting &&
    (e.is("keyboard:down:d") ||
      (e.is("touch") && e.device === "mouse" && e.button === 2))
  ) {
    // Download a scaled version of the painting...
    download(`hw-${index}-${tokens[index]}.png`, painting, { scale: 6 });
  }
}

function sim() {
  ellipsisTicker?.sim();
  timestampColorBlinker.step();
}

// Generates metadata fields for this piece.
// (Run by the server.)
function meta({ params, num }) {
  const baseURL = "https://aesthetic.computer/api/pixel";
  // https://aesthetic.computer/api/pixel/2048:conform/@jeffrey/painting/2023.10.19.13.04.18.088.png
  const handle = "@jeffrey";

  const i = tokenID({ params, num });
  // console.log("Token ID:", i);
  let out;
  if (i === -1) {
    out = {
      // image_url: `${baseURL}/2048:conform/${handle}/painting/${tokens[i]}.png`,
      // icon_url: `/api/pixel/128:contain/${handle}/painting/${tokens[i]}.png`,
      title: "Random",
      desc: "Shows a random hell_ world painting!",
    };
  } else {
    out = {
      image_url: `${baseURL}/2048:conform/${handle}/painting/${tokens[i]}.png`,
      icon_url: `/api/pixel/128:contain/${handle}/painting/${tokens[i]}.png`,
      title: tokenTitlesAndDescriptions[i][0],
      desc: tokenTitlesAndDescriptions[i][1],
    };
  }
  return out;
}

// Retrieve or generate a token index, given this piece's parameter list.
function tokenID($) {
  const canRandomize = $.num ? true : false; // Return -1 if there is no randomization function (on server)
  const randomToken = canRandomize ? $.num.randInt(tokens.length - 1) : -1;
  const param1 = parseInt($.params[0]);
  return param1 >= 0 && param1 < tokens.length ? param1 : randomToken;
}

export { boot, paint, act, sim, meta, tokenID };

// 📚 Library
async function getPainting(
  i,
  { get, gizmo, hud, num, net, meta: refreshMetadata },
) {
  hud.label(`hell_ world ${index}`, [255, 255, 0, 255]);
  net.rewrite(`hell_-world~${i}`);
  headers(index);
  refreshMetadata(meta({ params: [index], num })); // Update metadata through the system.
  // ellipsisTicker = new gizmo.EllipsisTicker();

  if (controller) controller.abort();
  controller = new AbortController();
  const signal = controller.signal;

  try {
    const got = await get.painting(tokens[i]).by("@jeffrey", { signal });
    painting = got.img;
    controller = null;
  } catch (err) {
    if (err.name === "AbortError") {
      if (debug) console.log("❌ Request was aborted.");
    } else {
      console.error("Painting load failure:", err);
    }
  }
}

function findTokenSet(index) {
  for (let key in tokenSets) {
    if (tokenSets[key].includes(index)) return key;
  }
  return null;
}
