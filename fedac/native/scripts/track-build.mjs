#!/usr/bin/env node
// track-build.mjs — Track OTA builds in MongoDB and generate unique build names
// Usage:
//   node track-build.mjs next-name   → get next build name (adjective-animal)
//   node track-build.mjs record      → record a build after upload
//
// Env: MONGODB_CONNECTION_STRING, MONGODB_NAME
// Stdin for "record": JSON { version, sha256, size, git_hash, build_ts, url }

import { MongoClient } from "mongodb";

const ANIMALS = [
  // Mammals (60)
  "otter","fox","wolf","bear","lynx","puma","deer","hare","mink","vole",
  "stoat","badger","ferret","weasel","moose","bison","yak","ibex","oryx","kudu",
  "okapi","tapir","sloth","koala","lemur","loris","gibbon","chimp","bonobo","mandrill",
  "ocelot","serval","caracal","margay","civet","genet","mongoose","meerkat","numbat","quoll",
  "dingo","jackal","coyote","fennec","arctic","pangolin","aardvark","echidna","platypus","wombat",
  "possum","wallaby","quokka","bandicoot","bilby","dugong","manatee","narwhal","beluga","orca",
  // Birds (60)
  "hawk","eagle","falcon","kite","osprey","owl","heron","crane","stork","ibis",
  "egret","rail","coot","loon","grebe","diver","petrel","shear","gannet","booby",
  "tern","skua","gull","puffin","murre","auk","razorbill","fulmar","albatross","condor",
  "vulture","kestrel","merlin","hobby","harrier","buzzard","sparrow","finch","wren","robin",
  "thrush","pipit","wagtail","dipper","warbler","flicker","nuthatch","creeper","swift","martin",
  "swallow","nightjar","kingfisher","roller","bee-eater","hoopoe","hornbill","toucan","parrot","macaw",
  // Reptiles & Amphibians (40)
  "gecko","lizard","skink","iguana","chameleon","monitor","tegu","cobra","mamba","adder",
  "viper","python","anaconda","rattler","copperhead","racer","kingsnake","milksnake","garter","coral",
  "tortoise","turtle","terrapin","croc","gator","caiman","gharial","newt","salamander","axolotl",
  "treefrog","bullfrog","poison-dart","glass-frog","mantella","reed-frog","toad","midwife","spadefoot","hellbender",
  // Fish & Marine (40)
  "trout","salmon","char","pike","perch","bass","walleye","sturgeon","paddlefish","gar",
  "bowfin","shad","herring","anchovy","sardine","mackerel","tuna","marlin","sailfish","swordfish",
  "barracuda","grouper","snapper","wrasse","goby","blenny","sculpin","flounder","sole","halibut",
  "ray","skate","shark","hammerhead","mako","thresher","whale","catfish","carp","loach",
  // Invertebrates (40)
  "crab","lobster","shrimp","prawn","krill","squid","octopus","nautilus","cuttlefish","clam",
  "mussel","oyster","scallop","conch","whelk","slug","snail","leech","worm","centipede",
  "millipede","scorpion","spider","beetle","ant","wasp","hornet","moth","butterfly","dragonfly",
  "damsel","cricket","katydid","mantis","stick-bug","leaf-bug","cicada","firefly","ladybug","weevil",
  // Mythical (30)
  "dragon","phoenix","griffin","sphinx","hydra","chimera","minotaur","centaur","pegasus","unicorn",
  "kraken","leviathan","basilisk","wyvern","roc","thunderbird","yeti","sasquatch","wendigo","golem",
  "djinn","nymph","sprite","pixie","sylph","dryad","selkie","kelpie","banshee","wraith",
  // Dinosaurs (30)
  "raptor","rex","bronto","stego","trike","ankylo","ptero","diplo","allo","spino",
  "carno","bary","dilo","parasaur","iguano","hadro","pachycephalo","theri","oviraptor","archaeo",
  "compy","gallimimus","deinonych","utahraptor","micro","giganoto","acro","sucho","cerato","maia",
  // Space (30)
  "pulsar","quasar","nebula","nova","comet","meteor","asteroid","aurora","corona","flare",
  "vortex","prism","zenith","apex","orbit","lunar","solar","stellar","cosmic","photon",
  "neutron","proton","quantum","flux","ion","plasma","ember","cinder","spark","blaze",
  // Nature (35)
  "river","brook","creek","delta","marsh","fjord","glacier","canyon","mesa","butte",
  "ridge","summit","caldera","geyser","lagoon","atoll","reef","tide","current","drift",
  "breeze","gale","storm","tempest","zephyr","monsoon","cascade","terrace","solstice","equinox",
  "tundra","steppe","prairie","savanna","taiga",
];

const ADJECTIVES = [
  "swift","bold","keen","sharp","bright","vivid","lucid","agile","nimble","deft",
  "brave","calm","clear","crisp","eager","fierce","gentle","grand","hardy","jolly",
  "lively","merry","noble","proud","quick","quiet","rapid","sleek","smooth","snappy",
  "steady","sturdy","subtle","supple","tender","tough","vital","warm","witty","zesty",
  "amber","ashen","azure","blaze","bronze","cedar","coral","crimson","dusk","ember",
  "flint","frost","golden","hazel","indigo","ivory","jade","khaki","lemon","lilac",
  "maple","mossy","navy","opal","pearl","plum","quartz","russet","sage","scarlet",
  "silver","slate","tawny","umber","velvet","walnut","zinc","cobalt","copper","flax",
  "dusty","foggy","hazy","misty","polar","rustic","stormy","sunlit","twilit","wintry",
  "ancient","arcane","astral","cosmic","cryptic","digital","feral","hollow","lunar","mystic",
  "neural","orbital","primal","radiant","sacred","silent","spectral","temporal","verdant","woven",
  "atomic","binary","cubic","delta","fractal","harmonic","infinite","kinetic","lattice","modular",
  "native","oxide","sonic","turbo","ultra","vector","wavelength","xeric","glacial","thermal",
  "ablaze","acoustic","alpine","boreal","carbon","ceramic","crystal","dynamic","elastic","frozen",
  "galactic","hybrid","igneous","magnetic","mineral","organic","prismatic","resonant","seismic","tidal",
  "alloy","basalt","chalk","granite","jasper","marble","obsidian","pumice","sandstone","shale",
  "lustrous","matte","opaque","pearly","polished","rough","satin","silky","textured","uncut",
  "aerial","aquatic","benthic","coastal","estuarine","insular","littoral","pelagic","riparian","sylvan",
  "arid","humid","tropic","volcanic","stellar","rugged","mossy","dusted","gilded","tempered",
  "chromatic","iridescent","phosphor","titanium","tungsten","vanadium","zircon","rhodium","osmium","bismuth",
];

// Third-word pool. Combined with the 200 adj × 365 animal pools this
// gives ~10M combinations before any structural repeat — and we still
// check the DB on every nextName call, rotating the noun until we find
// an unused name. Hex collision suffix retired; uniqueness is now
// guaranteed by the database, not by entropy.
const NOUNS = [
  "dawn","dusk","ember","cinder","spark","flame","glow","flare","ash","mist",
  "fog","frost","rain","dew","hail","haze","veil","cloud","gale","storm",
  "tide","wave","breeze","zephyr","eddy","gust","whirl","peak","ridge","crag",
  "cliff","spine","summit","mesa","butte","glade","grove","vale","heath","fen",
  "marsh","bog","dune","oasis","steppe","shard","prism","crystal","jewel","orb",
  "spiral","coil","blade","arrow","spear","shield","rune","sigil","crown","throne",
  "key","gate","path","bridge","tower","spire","vault","crypt","nest","cairn",
  "echo","hush","chime","hum","tone","song","chord","pulse","beat","shroud",
  "halo","aura","quill","thread","weave","warp","loom","comet","nebula","vortex",
  "core","seed","root","branch","leaf","vine","moss","fern","bloom","petal",
  "wisp","sprig","bough","bract","frond","spore","husk","pith","sap","resin",
  "amber","beacon","fjord","glacier","grotto","cove","atoll","reef","lagoon","quay",
  "dock","helm","keel","mast","sail","prow","wake","tideline","reef","kelp",
];

const COLLECTION = "builds";

async function getDb() {
  const connStr = process.env.MONGODB_CONNECTION_STRING;
  const dbName = process.env.MONGODB_NAME;
  if (!connStr || !dbName) {
    throw new Error("Missing MONGODB_CONNECTION_STRING or MONGODB_NAME");
  }
  const client = new MongoClient(connStr);
  await client.connect();
  return { db: client.db(dbName), client };
}

// Get day-of-year (1-indexed)
function dayOfYear() {
  const now = new Date();
  const start = new Date(now.getFullYear(), 0, 0);
  return Math.floor((now - start) / 86400000);
}

async function nextName() {
  const { db, client } = await getDb();
  try {
    const counters = db.collection("counters");
    const result = await counters.findOneAndUpdate(
      { _id: "build_number" },
      { $inc: { seq: 1 } },
      { upsert: true, returnDocument: "after" }
    );
    const buildNum = result.seq;
    const builds = db.collection(COLLECTION);

    // Three-word name. Adjective and animal both rotate every build so
    // consecutive names look visibly distinct (no "*-otter-*" streaks).
    // Coprime strides (1 vs 7) against the pool sizes keep the orbit
    // long before any (adj, animal) pair repeats.
    const adj = ADJECTIVES[buildNum % ADJECTIVES.length];
    const animal = ANIMALS[(buildNum * 7) % ANIMALS.length];

    for (let attempt = 0; attempt < NOUNS.length; attempt++) {
      const noun = NOUNS[(buildNum + attempt) % NOUNS.length];
      const name = `${adj}-${animal}-${noun}`;
      const existing = await builds.findOne({ name }, { projection: { _id: 1 } });
      if (!existing) return { name, buildNum };
    }

    // Astronomically unlikely (would need every (adj,animal,noun) combo
    // taken). Fall back to base36 timestamp instead of failing the build.
    const stamp = Date.now().toString(36).slice(-5);
    return { name: `${adj}-${animal}-${stamp}`, buildNum };
  } finally {
    await client.close();
  }
}

async function recordBuild() {
  // Read build info from stdin
  let input = "";
  for await (const chunk of process.stdin) input += chunk;
  const info = JSON.parse(input);

  const { db, client } = await getDb();
  try {
    const builds = db.collection(COLLECTION);
    await builds.insertOne({
      name: info.name,
      buildNum: info.buildNum,
      version: info.version,
      sha256: info.sha256,
      size: info.size,
      gitHash: info.git_hash,
      buildTs: info.build_ts,
      url: info.url,
      target: info.target || "unknown",
      user: info.user || null,
      when: new Date(),
    });
    // Also update a "latest" document for quick lookup
    await builds.updateOne(
      { _id: "latest" },
      { $set: {
        name: info.name,
        version: info.version,
        sha256: info.sha256,
        when: new Date(),
      }},
      { upsert: true }
    );
  } finally {
    await client.close();
  }
}

const cmd = process.argv[2];
const isPlain = process.argv.includes("--plain");
try {
  if (cmd === "next-name") {
    const { name, buildNum } = await nextName();
    if (isPlain) console.log(name);
    else console.log(JSON.stringify({ name, buildNum }));
  } else if (cmd === "record") {
    await recordBuild();
    process.stderr.write("Build recorded in MongoDB\n");
  } else {
    console.error("Usage: track-build.mjs {next-name [--plain]|record}");
    process.exit(1);
  }
} catch (e) {
  // Don't block the build pipeline if MongoDB is down. Local fallback
  // mirrors the new three-word format so callers always get the same shape.
  process.stderr.write(`track-build: ${e.message}\n`);
  if (cmd === "next-name") {
    const doy = dayOfYear();
    const fallbackNum = Date.now() % ADJECTIVES.length;
    const adj = ADJECTIVES[fallbackNum];
    const animal = ANIMALS[doy % ANIMALS.length];
    const noun = NOUNS[(Date.now() / 1000) % NOUNS.length | 0];
    const name = `${adj}-${animal}-${noun}`;
    if (isPlain) console.log(name);
    else console.log(JSON.stringify({ name, buildNum: -1 }));
  }
}
