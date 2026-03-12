#!/bin/bash
# build-name.sh — Generate a unique build name: adjective-animal
# Animal changes daily, adjective changes every build. Never repeats.
# Usage: ./build-name.sh          → prints name (e.g., "swift-otter")
#        ./build-name.sh --bump   → increments counter and prints name
set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
COUNTER_FILE="${SCRIPT_DIR}/../.build-counter"

# 365 animals — one per day of year, then wraps to next category
ANIMALS=(
  # Mammals (60)
  otter fox wolf bear lynx puma deer hare mink vole
  stoat badger ferret weasel moose bison yak ibex oryx kudu
  okapi tapir sloth koala lemur loris gibbon chimp bonobo mandrill
  ocelot serval caracal margay civet genet mongoose meerkat numbat quoll
  dingo jackal coyote fennec arctic pangolin aardvark echidna platypus wombat
  possum wallaby quokka bandicoot bilby dugong manatee narwhal beluga orca
  # Birds (60)
  hawk eagle falcon kite osprey owl heron crane stork ibis
  egret rail coot loon grebe diver petrel shear gannet booby
  tern skua gull puffin murre auk razorbill fulmar albatross condor
  vulture kestrel merlin hobby harrier buzzard sparrow finch wren robin
  thrush pipit wagtail dipper warbler flicker nuthatch creeper swift martin
  swallow nightjar kingfisher roller bee-eater hoopoe hornbill toucan parrot macaw
  # Reptiles & Amphibians (40)
  gecko lizard skink iguana chameleon monitor tegu cobra mamba adder
  viper python anaconda rattler copperhead racer kingsnake milksnake garter coral
  tortoise turtle terrapin croc gator caiman gharial newt salamander axolotl
  treefrog bullfrog poison dart glass mantella reed toad midwife spadefoot
  # Fish & Marine (40)
  trout salmon char pike perch bass walleye sturgeon paddlefish gar
  bowfin shad herring anchovy sardine mackerel tuna marlin sailfish swordfish
  barracuda grouper snapper wrasse goby blenny sculpin flounder sole halibut
  ray skate shark hammerhead mako thresher whale catfish carp loach
  # Invertebrates (40)
  crab lobster shrimp prawn krill squid octopus nautilus cuttlefish clam
  mussel oyster scallop conch whelk slug snail leech worm centipede
  millipede scorpion spider beetle ant wasp hornet moth butterfly dragonfly
  damsel cricket katydid mantis stick leaf cicada firefly ladybug weevil
  # Mythical (30)
  dragon phoenix griffin sphinx hydra chimera minotaur centaur pegasus unicorn
  kraken leviathan basilisk wyvern roc thunderbird yeti sasquatch wendigo golem
  djinn nymph sprite pixie sylph dryad selkie kelpie banshee wraith
  # Dinosaurs (30)
  raptor rex bronto stego trike ankylo ptero diplo allo spino
  carno bary dilo parasaur iguano hadro pachycephalo theri oviraptor archaeo
  compy gallimimus deinonych utahraptor micro giganoto acro sucho cerato maia
  # Space (30)
  pulsar quasar nebula nova comet meteor asteroid aurora corona flare
  vortex prism zenith apex orbit lunar solar stellar cosmic photon
  neutron proton quantum flux ion plasma ember cinder spark blaze
  # Nature (35)
  river brook creek delta marsh fjord glacier canyon mesa butte
  ridge summit caldera geyser lagoon atoll reef tide current drift
  breeze gale storm tempest zephyr monsoon aurora borealis solstice equinox
  tundra steppe prairie savanna taiga
)

# 200 adjectives — one per build, then wraps
ADJECTIVES=(
  swift bold keen sharp bright vivid lucid agile nimble deft
  brave calm clear crisp eager fierce gentle grand hardy jolly
  lively merry noble proud quick quiet rapid sleek smooth snappy
  steady sturdy subtle supple tender tough vital warm witty zesty
  amber ashen azure blaze bronze cedar coral crimson dusk ember
  flint frost golden hazel indigo ivory jade khaki lemon lilac
  maple mossy navy opal pearl plum quartz russet sage scarlet
  silver slate tawny umber velvet walnut zinc cobalt copper flax
  dusty foggy hazy misty polar rustic stormy sunlit twilit wintry
  ancient arcane astral cosmic cryptic digital feral hollow lunar mystic
  neural orbital primal radiant sacred silent spectral temporal verdant woven
  atomic binary cubic delta fractal harmonic infinite kinetic lattice modular
  native oxide quantum reflex sonic turbo ultra vector wavelength xeric
  ablaze acoustic alpine boreal carbon ceramic crystal dynamic elastic frozen
  galactic hybrid igneous magnetic mineral organic prismatic resonant seismic thermal
  alloy basalt chalk flint granite jasper marble obsidian pumice quartz
  sandstone shale slate soapstone travertine basaltic chalky flinty granitic jaspery
  lustrous matte opaque pearly polished rough satin silky textured uncut
  aerial aquatic benthic coastal estuarine glacial insular littoral pelagic riparian
  alpine arid boreal humid polar steppe sylvan tropic tundral volcanic
)

NUM_ANIMALS=${#ANIMALS[@]}
NUM_ADJ=${#ADJECTIVES[@]}

# Animal: based on day-of-year (changes daily)
DOY=$(date -u '+%j' | sed 's/^0*//')  # strip leading zeros
ANIMAL_IDX=$(( (DOY - 1) % NUM_ANIMALS ))

# Adjective: based on build counter (changes every build)
if [ -f "${COUNTER_FILE}" ]; then
  BUILD_NUM=$(cat "${COUNTER_FILE}" 2>/dev/null || echo "0")
else
  BUILD_NUM=0
fi

# Bump if requested
if [ "${1:-}" = "--bump" ]; then
  BUILD_NUM=$(( BUILD_NUM + 1 ))
  echo "${BUILD_NUM}" > "${COUNTER_FILE}"
fi

ADJ_IDX=$(( BUILD_NUM % NUM_ADJ ))

echo "${ADJECTIVES[$ADJ_IDX]}-${ANIMALS[$ANIMAL_IDX]}"
