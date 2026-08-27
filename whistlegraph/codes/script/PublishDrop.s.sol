// SPDX-License-Identifier: MIT
pragma solidity ^0.8.24;

import "forge-std/Script.sol";
import "../src/WhistlegraphCodes.sol";

// Publishes the 128-code opening drop in ONE transaction via publishMany.
// Generated from drop/128.json — regenerate, never hand-edit the arrays.
//   CONTRACT=0x... forge script script/PublishDrop.s.sol --rpc-url $RPC --broadcast
contract PublishDrop is Script {
    function run() external {
        WhistlegraphCodes wg = WhistlegraphCodes(vm.envAddress("CONTRACT"));
        string[] memory codes = new string[](128);
        uint256[] memory prices = new uint256[](128);
        codes[0] = "tri"; prices[0] = 100000000000000000; // Triangles
        codes[1] = "imab"; prices[1] = 250000000000000000; // Butterfly Cosplayer
        codes[2] = "nbff"; prices[2] = 100000000000000000; // My Neighbor is My Best Friend
        codes[3] = "fact"; prices[3] = 100000000000000000; // Factory
        codes[4] = "cult"; prices[4] = 100000000000000000; // The Three of Us Are in a Cult
        codes[5] = "tw12"; prices[5] = 100000000000000000; // Baby It's Twelve
        codes[6] = "h0t"; prices[6] = 100000000000000000; // It's Too Hot, No It's Not
        codes[7] = "mush"; prices[7] = 100000000000000000; // Sad Mushroom
        codes[8] = "appl"; prices[8] = 100000000000000000; // Hey There, Apple
        codes[9] = "sos"; prices[9] = 100000000000000000; // Scared of Stairs
        codes[10] = "itme"; prices[10] = 100000000000000000; // It's Me
        codes[11] = "web"; prices[11] = 100000000000000000; // In My Web
        codes[12] = "w0w"; prices[12] = 100000000000000000; // Mommy Wow
        codes[13] = "lonr"; prices[13] = 100000000000000000; // Loner
        codes[14] = "rshu"; prices[14] = 100000000000000000; // Red Shoe Bullet
        codes[15] = "puzz"; prices[15] = 100000000000000000; // Puzzle
        codes[16] = "cxme"; prices[16] = 100000000000000000; // You Can't Catch Me
        codes[17] = "simp"; prices[17] = 100000000000000000; // Do You Know This Show
        codes[18] = "1p2"; prices[18] = 100000000000000000; // One Plus Two Equals Three
        codes[19] = "2la"; prices[19] = 100000000000000000; // Certain Personality
        codes[20] = "circ"; prices[20] = 100000000000000000; // Circles Outside of Circles
        codes[21] = "star"; prices[21] = 100000000000000000; // Kid With Starry Eyes
        codes[22] = "kity"; prices[22] = 100000000000000000; // Kitty Head
        codes[23] = "pics"; prices[23] = 100000000000000000; // Pointing, Painting, Picture Taking
        codes[24] = "wiyh"; prices[24] = 100000000000000000; // What's Inside Your Heart?
        codes[25] = "tie"; prices[25] = 100000000000000000; // It's a Tie
        codes[26] = "trib"; prices[26] = 50000000000000000; // Draw A Triangle Bug
        codes[27] = "srce"; prices[27] = 50000000000000000; // The Source
        codes[28] = "turt"; prices[28] = 50000000000000000; // Turtle One Line
        codes[29] = "cmal"; prices[29] = 50000000000000000; // Camille and Alex
        codes[30] = "boxs"; prices[30] = 50000000000000000; // Making Boxes
        codes[31] = "fire"; prices[31] = 50000000000000000; // Sad Campfire
        codes[32] = "idni"; prices[32] = 50000000000000000; // I Don't Need an iPhone
        codes[33] = "chee"; prices[33] = 50000000000000000; // Cheerleader
        codes[34] = "tipj"; prices[34] = 50000000000000000; // Tip Jean Allo
        codes[35] = "frog"; prices[35] = 50000000000000000; // Frog Tiara
        codes[36] = "grow"; prices[36] = 50000000000000000; // Time To Grow
        codes[37] = "toh"; prices[37] = 50000000000000000; // Touching Our Heads
        codes[38] = "clth"; prices[38] = 50000000000000000; // Circle Line Triangle Hand
        codes[39] = "pant"; prices[39] = 50000000000000000; // Painting Standing Tall
        codes[40] = "ppl"; prices[40] = 50000000000000000; // People Pleaser
        codes[41] = "hill"; prices[41] = 50000000000000000; // Distant Hills
        codes[42] = "pnda"; prices[42] = 50000000000000000; // When I'm Sad I Whip My Pen
        codes[43] = "cfsh"; prices[43] = 50000000000000000; // Little Christian Fish
        codes[44] = "lump"; prices[44] = 50000000000000000; // I Look Like A Lump
        codes[45] = "bang"; prices[45] = 50000000000000000; // Trimmed My Bangs for You
        codes[46] = "xln"; prices[46] = 50000000000000000; // Will You Cross My Line
        codes[47] = "bord"; prices[47] = 50000000000000000; // Nothing But a Board
        codes[48] = "tppl"; prices[48] = 50000000000000000; // World Of The Triangle People
        codes[49] = "symm"; prices[49] = 50000000000000000; // Symmetry Game
        codes[50] = "bndg"; prices[50] = 50000000000000000; // Broken Heart
        codes[51] = "bubb"; prices[51] = 50000000000000000; // Blown Up Blue Balloon
        codes[52] = "l8ly"; prices[52] = 50000000000000000; // Lately When I Fly
        codes[53] = "bpen"; prices[53] = 50000000000000000; // Blow My Pen
        codes[54] = "sotm"; prices[54] = 50000000000000000; // Some of the Time
        codes[55] = "head"; prices[55] = 50000000000000000; // Headache Splitting
        codes[56] = "well"; prices[56] = 50000000000000000; // Well, Well, Well
        codes[57] = "awdd"; prices[57] = 50000000000000000; // All I Wanna Do Is Die
        codes[58] = "lost"; prices[58] = 50000000000000000; // Lost Kitty
        codes[59] = "sqar"; prices[59] = 50000000000000000; // Trapped in a Square
        codes[60] = "smil"; prices[60] = 50000000000000000; // Battle Between Smiley Faces
        codes[61] = "bite"; prices[61] = 50000000000000000; // Dog Bite
        codes[62] = "bech"; prices[62] = 50000000000000000; // Make This A Beach
        codes[63] = "enuf"; prices[63] = 50000000000000000; // More Than Enough
        codes[64] = "curl"; prices[64] = 50000000000000000; // Little Curl In The Way
        codes[65] = "ghst"; prices[65] = 50000000000000000; // I'm a Ghost
        codes[66] = "fhs"; prices[66] = 50000000000000000; // Fragile Heart Strings
        codes[67] = "m123"; prices[67] = 50000000000000000; // 100 Plus 200 Equals 3
        codes[68] = "dodo"; prices[68] = 50000000000000000; // Do Do Do Do
        codes[69] = "tsss"; prices[69] = 50000000000000000; // The Sun Keeps Beaming Down
        codes[70] = "crpy"; prices[70] = 50000000000000000; // Why Do People Think I Look Creepy
        codes[71] = "swim"; prices[71] = 50000000000000000; // Swimming In The Deep
        codes[72] = "helo"; prices[72] = 50000000000000000; // Say Hello
        codes[73] = "mmgf"; prices[73] = 50000000000000000; // I Miss My Girlfriend
        codes[74] = "hair"; prices[74] = 50000000000000000; // Hearts In Your Hair
        codes[75] = "cham"; prices[75] = 50000000000000000; // Charlie D'Amelio Portrait
        codes[76] = "soda"; prices[76] = 30000000000000000; // Empty Soda Cup
        codes[77] = "clf"; prices[77] = 30000000000000000; // Circle Line Feather
        codes[78] = "ioor"; prices[78] = 30000000000000000; // Inside of the Outside Ring
        codes[79] = "bye"; prices[79] = 30000000000000000; // Say Goodbye
        codes[80] = "gwii"; prices[80] = 30000000000000000; // Guess What It Is
        codes[81] = "lock"; prices[81] = 30000000000000000; // Keep My Heart Safe
        codes[82] = "kiss"; prices[82] = 30000000000000000; // Dot Dot Kiss
        codes[83] = "lhow"; prices[83] = 30000000000000000; // Luh Ow No Way
        codes[84] = "trip"; prices[84] = 30000000000000000; // I Broke My Head and Died
        codes[85] = "sdog"; prices[85] = 30000000000000000; // Slinky Dog
        codes[86] = "buny"; prices[86] = 30000000000000000; // Bunny in a Bowl
        codes[87] = "ship"; prices[87] = 30000000000000000; // Creation Ship
        codes[88] = "drmk"; prices[88] = 30000000000000000; // My Marker's All Dried Up
        codes[89] = "gcrs"; prices[89] = 30000000000000000; // Gold Cross for Girls
        codes[90] = "lhts"; prices[90] = 30000000000000000; // Love Is Here to Stay
        codes[91] = "jeff"; prices[91] = 30000000000000000; // Self-Portrait From My Name
        codes[92] = "bnkk"; prices[92] = 30000000000000000; // Bubba Never Kiki
        codes[93] = "sprk"; prices[93] = 30000000000000000; // Sparkle in the Sky
        codes[94] = "2epn"; prices[94] = 30000000000000000; // Two Eyes and a Pink Nose
        codes[95] = "swch"; prices[95] = 30000000000000000; // Switch Switch Switch
        codes[96] = "bugy"; prices[96] = 30000000000000000; // Buggy on The Wall
        codes[97] = "bowm"; prices[97] = 30000000000000000; // Bow Man
        codes[98] = "iwys"; prices[98] = 30000000000000000; // I Watch You Sleep
        codes[99] = "3x2s"; prices[99] = 30000000000000000; // Three Times Two Is Six
        codes[100] = "fitp"; prices[100] = 30000000000000000; // Fingers in the Paint
        codes[101] = "dalb"; prices[101] = 30000000000000000; // Draw a Little Body
        codes[102] = "pigg"; prices[102] = 30000000000000000; // Being Clean Isn't My Thing
        codes[103] = "bdbf"; prices[103] = 30000000000000000; // Beads for My Best Friend
        codes[104] = "beth"; prices[104] = 30000000000000000; // Bethany the Bubble Tracker
        codes[105] = "chbs"; prices[105] = 30000000000000000; // Curly-Haired Boy's Spaghetti
        codes[106] = "wcud"; prices[106] = 30000000000000000; // As the World Comes Undone
        codes[107] = "weeh"; prices[107] = 30000000000000000; // Well Eh Well Eh
        codes[108] = "bath"; prices[108] = 30000000000000000; // Body in the Bathtub
        codes[109] = "dtln"; prices[109] = 30000000000000000; // A Dot and a Line
        codes[110] = "nsqh"; prices[110] = 30000000000000000; // No Sé Qué Hacer
        codes[111] = "chlk"; prices[111] = 30000000000000000; // As Long as the Chalk Draws
        codes[112] = "grav"; prices[112] = 30000000000000000; // Grave of My Friend
        codes[113] = "tnkm"; prices[113] = 30000000000000000; // Do You Ever Think of Me
        codes[114] = "gdsp"; prices[114] = 30000000000000000; // Going Down to South Park
        codes[115] = "eftr"; prices[115] = 30000000000000000; // Eco-Friendly Tree
        codes[116] = "vlcn"; prices[116] = 30000000000000000; // Volcano in Your Mind
        codes[117] = "tify"; prices[117] = 30000000000000000; // Then I Found You
        codes[118] = "sitc"; prices[118] = 30000000000000000; // Seven in the Corner
        codes[119] = "ccdd"; prices[119] = 30000000000000000; // Circle Circle Dot Dot
        codes[120] = "encr"; prices[120] = 30000000000000000; // Encrypt the Message
        codes[121] = "limb"; prices[121] = 30000000000000000; // Lying in My Bed
        codes[122] = "techn"; prices[122] = 30000000000000000; // Techno Sheep
        codes[123] = "ttdw"; prices[123] = 30000000000000000; // That Thing You Draw With
        codes[124] = "asfm"; prices[124] = 30000000000000000; // A Shape for Me
        codes[125] = "asmr"; prices[125] = 30000000000000000; // Aggro Smile Money Raspberry
        codes[126] = "frpd"; prices[126] = 30000000000000000; // Friction Pad Erase
        codes[127] = "strm"; prices[127] = 30000000000000000; // How to Make a String Mouth
        vm.startBroadcast(vm.envUint("PRIVATE_KEY"));
        wg.publishMany(codes, prices);
        vm.stopBroadcast();
    }
}
