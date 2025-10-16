// Prutti, 2023.11.09.16.36.06.537
// Lessons NOT Learnt by pruttipal aka goodiepal

import { Typeface } from "../lib/type.mjs";

/* #region 📚 README
#endregion */

/* #region 🏁 TODO
  Bugs, 25.07.20.10.16
  - []  The timecode in the player doees not update when the audio is playing.
  - [] The scrubber is weird - please just simplify it / make it work, using 'stample' as a reference interface.
  (Released)
  - [] Better scroll limiting related to the bottom of the screen.
  + Done
  - [x] Add wheel based scrolling to pruttis.
  - [x] Make params for prutti 1, prutti 2, etc.
  - [x] Print out scrolling text for each.
  - [x] Attach images via the asset url: https://assets.aesthetic.computer/pruttipal/lnl/lnl1.jpeg
#endregion */

const { min } = Math;

const lessons = [
  {
    title: `Lesson NOT Learnt 01`,
    subtitle: `The failure of academic identity politics`,
    text: `
    // Right outside the house were I live, > a Celtic churce-settlement has been found only a #few years ago#—§ This changes the whole idea of Faroish identity completly as most modern - inovativ self-brand-building youngsters on the island's now ses a opotunity to make up a new history around themself as: Ancient Celts and by that pushing aside, the Sagas of the Icelanders, there otherwise has been the concept of real Nordic - national - IDEAS OF HOME - now the most clever of us are Celtic and hates on Norway and Black Metal etc. - But this is down-right a fantasy - as most people here is a >>-bio-dna-product-<< of sailors and shipping trafic routes… 'nuff said here.

    Most identity politics is based on getting a place at the power and capitalist-table on the expense of any scientific or historical facts - YES - This is wonderfull and I can only bee-sing it and enjoy as I myself has changed my acent, history, origin etc. — many a time, in order to fit in and make myself intereting and doing so in a way there was playing whit fact the same way as the already mentioned the Saga's. (((Were is The Goodiepal from etc.)))

    ______Just as K-omputer hacking was inteligent and realativly harmless to begin whit,  it is now a days turned into only being about getting money out of old and lesser informed on-liner's - The Academic identity hacking and story building people use today could be said to share the same faith ((—- maybe?? —)) At least almost all Scandinavien academics only research in fields they are payed in Capital to researh and people in the needs of power ar desperate to build new world views and history's to fit into … just look at teh Balkan changes over the last 30 years for another example - and I guess it is more or less the same anywere else… Butthen agian  this is only some Celtic fantasy thought's - they have no relevance now, but give them a few years…

    [the_prut_is_old_01]
    `,
    pictures: ["lnl1"],
    sounds: ["the_prut_is_old_01"],
  },
  {
    title: "Lesson NOT Learnt 02",
    subtitle: "Hang whit the wrong",
    text: `
    Let's go and in ART find the queer·ness of the left-wing >>>> and mash it ..into.. the outsider art of the modern right-wing ___ Here we could include all the social free thinkers dreams like: modular synth's, bright-colours etc. — & —gently blendt it whit things from the right'ers - that could includ re·li·gious heart-to-brush painting technics & nordic sing-a-song writer traditions etc.  — Do not hold back, but please go give it a try - § § § __ The hopefull conclution should be that, the so called right- and left-wing talk is something from the old France Revolution, that is of today, in no way, a good illustrative way to descrip the strugles and political mind-sets the youth is trying to navigate in…

    __ The workers on a Norwigien 'oil-platform' has a much better idea of what they everyday work destory's, then let's say a Ethereum-Space Karen-Dogecoin-investor, happy in the Berlin Cryto-Capital section of the ART-market, if a Rough-Neck on the Rig sings a song, please listen to it before you judge that person - there is always some things to learn,  don't just doubt ur vibe…

    If you are a Hawaii'en native working on: the Pituffik Space Base and is on leave in Thorshavn on the Faroe Islands, your ART— %% even if you call it: “just a hobby” %% might tell a intereting story about: in·dig·e·nous'nes and enviromental destruction — even thow that you are in the uniform…

    in ART so in life - trust your heart - not what your phone tells you... The only exception here is phone-ART...

    https://www.airforce-technology.com/wp-content/uploads/sites/6/2019/06/US-Air-Force-1.jpg

    Cryptic: KovBoyFilm Ref.

    https://www.youtube.com/watch?v=GhIhJ8w4e2U

    [carl_signe_is_prut_02]
    `,
    pictures: ["lnl2-1", "lnl2-2"],
    sounds: ["carl_signe_is_prut_02"],
  },
  {
    title: "Lesson NOT Learnt 03",
    subtitle: "Purano Sei Diner Kothis & The Guild of the Viking Klokkers",
    text: `
    I only like words in songs and not _general sytax—> as it is the destroy'er of EMO ideas. << For: Bananskolen I tryed NOT to be: Karin Hansson & Åse Andersson's: _TED (Beta 0.3)__ but was / is still iinternally in the gruppe sean as that person (& as I become of age, I really DO hate being in focus - - - -belive it or not), - - - - But all this brings something interesting to the table, a lot of current intellectualism and Academic discussion's is actually not about the subject but more about hove the gruppe feels internaly.

    This = the failure of modern debate in general
    - As it is the unspoken-gruppe stucture that makes the individuals spar·kle and not the $ rules $ that govern hove the gruppe is organised; who is allowed to speak etc. & I do really believe that any political idea is just another power system waiting to be put into place. . .

    - As for now 2000 & HVAD - we try: The Guild of the Viking Klokkers

    01 - if you live in København and owens a flat, you are 3 - 4 X milionar!
    02 - if you live in København and owens a house, you are around 12 X milionar!
    03 - if you live in København and owens more then one house, you are economicaly diretly responsible for the >current< WARS in this world —> København'ers - get super super angry when I raise this openion, in everyday talk… — Part of my current life struge is a product of just that —>  So In life, NEVER should one owen more then 1 X house… else you are a Scandi-Tycoon.
    Most Scandi_tycoon's dos de-emphasize in style and expense and does not show of there weath in the form of Watches & Jewelry etc.

    FRONT your wealth - Do not owen a house - if people will NOT house you, when you come to town your so called ART - is simply not good enough…

    We are the Viking Klokkers
    - 3 X Play
    - 6 X Father's

    [bananskolen_was_prut_03]
    `,
    pictures: ["lnl3"],
    sounds: ["bananskolen_was_prut_03"],
  },
  {
    title: "Lesson NOT Learnt 04",
    subtitle: "",
    text: `
    —————— Fact: ——————	———————————
    Rasmus Sørnes just joined: The Guild of the Viking Klokkers
    —————— Dansk ——————	————————
    Tænd for: Teksti-TV 666
    Nej hved du nu hvad Nyhil er lukket, jo da har du / de overhovedet flere bolsjer i posen ???
    DIG, du er en person der går og tumler lidt med det .  .
    husk at ..—> magten er varm og lige voldsomt nok, hvis ikke kan du ringe til:
    Tove Storch (+45) 2992 9022 —— > Arturo Ruiz del Pozo, ramte plet minst en gang, bare så du hved det, hvis det interessere dig
    Batteri-biller er så tunge som dino'er - SLET DIG SELV kvik-sølv's billist -
    —————— Færøsk ——————
    -> give away your watches, (not all of them) as a gesture, as also they represent POWER<-
    —————— Svensk ——————
    om maleriet er død hved jeg ikke?  Men de fleste ting der kan holdes i hånden fasinere mig,
    jeg er åben overfor håndskulpturen
    Johan Jönsson - er varm, —— tampen brander
    Goodiepal
    Norðuri á Heyggi 2A
    FO-176 Velbastaður
    Føroyar
    the faroe islands FO
    ——————————		——————————
    ——————————————————————————
    new goodiepal website
    https://web.archive.org/web/20231010110621/https://goodiepal.dk/#
    ——————————————————————————
    ——————————————————————————
    who arkived it?

    [acid.talk_and_forest.folk_04]
    `,
    pictures: ["lnl4"],
    sounds: ["acid.talk_and_forest.folk_04"],
  },
  {
    title: "Lesson NOT Learnt 05",
    subtitle: "",
    text: `
    Karlheinz - Fucking (Vinyl Rip)
    we painted red paint on politikens boghal————————————————
    most of foods is dropped in the garbage disposal - why do you not dumpster dive
    try only to eat free food

    Mike Burrows, just joined the Guild the Viking Klokkers
    ————————————————

    [fuck_politikens_boghandel_05]
    `,
    pictures: ["lnl5"],
    sounds: ["fuck_politikens_boghandel_05"],
  },
  {
    title: "Lesson NOT Learnt 06",
    subtitle: "Silence is the power of any overclass, anywere",
    text: `
    & Fansy political ideas is all the same - just power structures waiting to be put into place... — well educated Euro's who jumps on a fashionable idea.. and talk to a lower class in order to highen them to another >>mille plateaux<<- But fails to interact whit the class whit's way of life the well educated Euro's certainly can't stand.. Just Like the fishers living in Vorupør and the bikers who en·gaged in a fight of 300 people around 30 years ago in that little town -  and the white institutional overclass only had silence as a comment - since silence is the way that intelectualisem deals whit ideas it dos not like... = silence is wrong & conceptual high modernism was a dead - NO it is / was not!

    [still_to0_old_to_cold_at_the_age_of_silence_06]
    `,
    pictures: ["lnl6"],
    sounds: ["still_to0_old_to_cold_at_the_age_of_silence_06"],
  },
  {
    title: "Lesson NOT Learnt 07",
    subtitle: "",
    text: `
    ———————————————
    let's stretches the concept if life <Power>
    alone = is golden - youth = is wrong - age = is wrong - time = is a joke
    <Ghost>
    call me up as: We are the Viking KLOKKERS—
    <Rainbow>
    ———————————————
    [nothing_to_prove_so_Call_Me_Maybe_07]
    `,
    pictures: ["lnl7"],
    sounds: ["nothing_to_prove_so_Call_Me_Maybe_07"],
  },
  {
    title: "Lesson NOT Learnt 08",
    subtitle: "w/ 02 variation",
    text: `
    —do not jump at comclusions
    love is really all that I have, look hove far it took me, maybe love can take you futher
    remember that we are just the little viking klokkers
    ding / dong - ding aling long

    Lesson NOT Learnt 08 - 02 variation
    —do not jump at comclusions
    love is really all that I have, look hove far it took me, maybe love can take you futher, now that you are gone form me  - Simone Babes Trust - New York made you a woman, but you were on the way to become a a new man . .  me <—<—you —NY>—>me
    Simone - you are greatly missed in my distrutfull life of forever sadness - this body you can have, as the last drops of my life will be dedicated to bring what you are now represented in, that being just light — ohh remember that we are just the little viking klokkers
    ding / dong - ding aling long for you in sound and haanskulptur - I will skulture you, not your body but who you were and is . . Gender is a human joke, you = always the most beautifull of light in gold and silver…
    Manchester & Edgeley made you, I sing your name out over: Velbastað
    we are the Viking Klokkers - we are the Viking Klokkers


    [rip_angel_simon_was_a_man_became_a_woman_became_a_man_08]
    `,
    pictures: ["lnl8"],
    sounds: ["rip_angel_simon_was_a_man_became_a_woman_became_a_man_08"],
  },
  {
    title: "Lesson NOT Learnt 09",
    subtitle: "",
    text: `
    The modern art-critic of today, has huge problems acepting - reversed signals as a comunication form…'''
    $$$ But no Euro-industiral Culture - could ever had existed whitout possing in Euro facist-left owers and in that way critisising the modern, the new & the global world there was to come…

    Dungeon & Draogns cost-play and so called gay culture was pre-industrial framworks, lets line it up:

    Dungeon & Draogns cost-play and (gay) culture > Industrial - revesed signals culture > Cyber Punk > Internet > Tekno  > Global Village Dreams > Social Media & web 2.0  > The curret Politcal Art-scene…

    and that current art-scene would never accept a man·ly Dungeon & Dragons cost-play picture like this:
    [never_call_me_by_the_name_kristian_09]
    `,
    pictures: ["lnl9"],
    sounds: ["never_call-me-by-the-name_kristian_09"],
  },
  {
    title: "Lesson NOT Learnt 10",
    subtitle: "",
    text: `
    When we come to your shores, people start to  see pyramids everywere…
    = The reaosn being that, we bring the paralel universiterium
    and the best place to leave ideas for the time to come is in pryramidiums

    We are the Viking Klokkers have trust a old goodiepal'ium

    [limerick_you_are_pyramide_lady_10]
    `,
    pictures: ["lnl10"],
    sounds: ["limerick_you_are_pyramide_lady_10"],
  },
  {
    title: "Lesson NOT Learnt 11",
    subtitle: "what punk is not about",
    text: `[listen 2 the audio...]`,
    sounds: ["what punk is not about"],
  },
  {
    title: "Lesson NOT Learnt 12",
    subtitle: "",
    text: `
    __Nobody can handle the wistful Goodiepal instrument-collection just like an: Eysturstein'er!__

    I have decided to create a small  SYNTH LIBRARY  on the Faro islands, or rather I have decided to put all, the music instruments, not just SYNTH's - that I own into a closet/ vetrine and make it all for ** FREE USE ** - you can come and take out what ever you like, bring it on the road to perform or home to play/ re·build just as long as you handle the ''TECH'' whit relative respect & care ________> and return it back and place it nedly into the same closet - when your actions are done whit.
    //------//
    You see, we as so called KLANG-artist's collect instruments like DRAGONS nesting on gold, & we never use the shit we collect - but why is it so?? Is it some strange left-over's from old picture's of Prog-Rock Bands, youtube-gear and videos & the insides of old Jean-Michel jarre record covers?

    ### BRING YOU SHIT TO USE ###

    I have always been ALL about FREE music and FREE this and that - and I will keep sharing whatever that I got, so that's why we are doing this - the KEY and coordinates to the collection is located at:

    BAR Sirkus Föroyar
    2 Gríms Kambansgøta
    Tórshavn 100,
    The Faroe Islands

    Yes - just ask for: Sunneva Eysturstein,  She has the key and will handle it over to you...

    $$$$$$ JUST SAY NO $$$$$$

    Normal Synth Library's work whit Donations, and Memberships and so on bla. bla. - But that already marginalize the concept, and the people who use it - lets make it totally free - ( NO ) DONATIONS - ( NO ) FUNDING, if something breaks let's meet once a year and fix it together ...

    now that should be all... //////// -- come join the guild of the viking klokkers... & lsiten to: The Franciscan Hobbies

    [12_dollars_no_thanks]
    `,
    pictures: ["lnl12"],
    sounds: ["12_dollars_no_thanks"],
  },
  {
    title: "Lesson NOT Learnt 13",
    subtitle: "",
    text: `
    .. clocks and music and life - is my speciality ..

    >ON rainy days - ( like most days out here)  - ROTOR's is all that I do, _ I'am definitely one of the most adventure's ROTOR make's around - & I puts great pride into this little craft - I mean I do rep. the guild OF THE VIKING KLOKKER'S after all, right? —- Let's just make thing's there is around us better, that's all that I would like to share whit my daughter and the people around me …

    >>> HAANDSKULPTUR…HAANDSKULPTUR..HAANDSKULPTUR.HAANDSKULPTR >>>

    N.B. now a days I hardly sign any of my creation's — as I do think that my work should do the talk-ing & I have never really been a artist, in the first place -  I'am more like a UPdate'er - I take things and makes them better…

    If you stay around me just for a little while, things in your life improves drastically —YES - it is true, it is a mystical (also to me) gift that  i inherited when i was very small - - — in watche's people always talk about, unique thing's — but you, just as what I create is a PIECE UNIQUE

    0 - ROTOR's —0 ROTOR's - 0 - ROTOR's - - 0 - ROTOR's - 0

    [13_keeping_the_tekno_rock_oath]
    `,
    pictures: ["lnl13-1", "lnl13-2", "lnl13-3"],
    sounds: ["13_keeping_the_tekno_rock_oath"],
  },
  {
    title: "Lesson NOT Learnt 14",
    subtitle: "Distribution",
    text: `
    Do You Want To Know A Sick-Rat << TORA DALSENG is one of the BEST >>
    yes yes - we have done some kissing in the distant past, but that is of the point, as I would KISS the whole world if it ever gave me the change!

    'NOW' better then the 'BEST' is: distribution,  most fine-loveley-fumbling art-works, first come's to life when they are distributed … ### Some individual's produce to much and distribute to little ### - If it was not for other people's story-telling of my craft's and life, my work would only be one and the same as “various crap-forms of old manliness”= not something the world would fined particularly HOT——— SO——— YOU, yes all of YOU distributer's continue to make me shine, by altering my output — Danke Schoen!
    -THE DALSENG-
    NOW back to: ms. DALSENG = When you meet the BEST, you should always UPDATE, whit-out destroy'ing the work, —— & by doing so taking the art and the life of the artist into your heart - - - You know that people actually did this for the first 3 - 4 seconds of social media web—2.0 before the distribution of ideas - was eschewed and replaced w. self-promotion & identity bla. bla. bla.
    I will try to still be all about distro, — like Atlantic- I got them works flyin- '-aesthetic computer'ly-' -cross the Atlantic (Woo!)  & -00- LOOK the DALSENG-work  is though it may seem strange, even better now, but no NOT for me flagging it up, no no = simply for context-flipping it - embrace: Stanley Brouwn

    [14_let_them_files_fly]
    `,
    pictures: ["lnl14-1", "lnl14-2", "lnl14-3"],
    sounds: ["14_let_them_files_fly"],
  },
  {
    title: "Lesson NOT Learnt 15",
    subtitle: "The Drug of WAR destroys EVERYTHING!",
    text: `
    [under the drugs belvie me]

    §§ Ohh Prutti, §§ Ohh Pruttipal - this is the Kitchi-Man >' Vladimir Obradovic ' … The picture is a 1990's sel-fi - from when he was young & full of MTV-music knowledge & dreams… — —- §§ ohh young prutti, ohh young prutti  §§ > the Yugoslav Wars was hard on the Kitchi-Man, he never fully recovered - - the WAR-drug destroy's EVERYTHING, war-info-drugs destroy's all there is good - but I will carry Vladimir, in my heart and will forever sing his unwritten & unfinished songs - no further info is needed there...
    `,
    pictures: ["lnl15"],
    sounds: ["under the drugs belvie me"],
  },
  {
    title: "Lesson NOT Learnt 16",
    subtitle: "a PAST not remembered",
    text: `
    .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..
    o0OoO0o Owls & BLACK fanthom helicopters = as replacement memory, Damascus steel & viking hands - for the viking times, when it is just around viking o’ klok — the guild of the Viking klo0OoO0okkers
    .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..
    [16_damask_dials_and_replacement_memory]
    `,
    pictures: ["lnl16-1", "lnl16-2"],
    sounds: ["16_damask_dials_and_replacement_memory"],
  },
  {
    title: "Lesson NOT Learnt 17",
    subtitle: "Das real IRDIAL's",
    text: `
    Most Euro-crafts is about: avoiding nature - but let me tell you I was painting one of my bicykle's the other day in: Velbastaður - I was using some wild & crazy chemical-paint that I had mixed from leftovers found at an abandoned construction site, and the rain was pouring down & interacting w. the paint - the result was so so so beautiful -

    I enjoy the act of painting bicykle's in the rain - BUT now > back to watch-making ... take a look at the unfinished experiments w. Damascus dial's here // - let nature - o0OoO0o rpl-memory and black phantom helicopters o0OoO0o - take part in your craft-making >--- what comes from the outside is often much better, then what come's from the inside - the outside world is THE real inside 'YES' & this unfinished dial's are the real irdial's - Me·chan·ics and interruption from nature is what's usually missing in the digital art's --- NOW go get a skeletonized pocket watch my friend, - - - they come fancy and they come cheap, BUT get 1 and start to think of civilization vs. nature - until you fall asleep…

    [17_get_out]
    `,
    pictures: ["lnl17-1", "lnl17-2"],
    sounds: ["17_get_out"],
  },
  {
    title: "Lesson NOT Learnt 18",
    subtitle: "Euro-Bell",
    text: `
    ————…………….
    Would you like to hear a a real electro-akustic-contemprary bell struck? - § made by my music-friend: V-Lars - Villads von Possel ?? §
    #WELL# here you go - IN - life & music I hardly, see myself as taking part anymore, I am just the one who is ringing the BELL… YES the same right now I just start and maintain the output of in this case the musical action . .  bla. BLA. bla. BLA. bla.
    ————
    Ohh do you know the reason for this tintinnabulation - ? ? ? - §IT IS because§ —Stanley Brouwn— just joined: the GUILD of the viking KLOKKER's
    ………………§……….§……….§……….§:……..§:………………………………..

    [v-lars]
    `,
    pictures: ["lnl18"],
    sounds: ["v-lars"],
  },
  {
    title: "Lesson NOT Learnt 19",
    subtitle: "50 years - Energy & why Aesthetic.computer?",
    text: `
    [robotic]
    -…————……………..
    ***************************
    *_JUST STOP OIL_*
    ***************************
    You know that I started all that Kommunal Klon Komputer 02 -SHIT- for ONE simple reason:  —: to find: //  aesthetics & beauty in e-energy-saving, travel & the self-generating of Electricity ——— NOW here many years later, I Iive off the Grid in the outskirts of Velbastaður……IS hated on by the Copenhagen'ers….§§§§§§§§ - & the sole reason why I joined the development of: aesthetic.computer, is to keep experimenting with the same things, as years back:  - DATA reduction = more INFO —vs.— LESS info = MORE data…
    ***************************
    *—STILL PRUTTIPAL—*
    ***************************
    All my so called activist-friends just get bigger and bigger flat's and houses - & is more and more on high-energy using social-media, yes & it is so It's ironic, but social media platforms consume a very significant amounts of energy, and this contributes to greenhouse gas emissions which exacerbates the climate change problem & bla. BLA. bla. BLA. but let's not get fanatic here, no no, but let's just try to go: LITTLE DATA ---Many of my younger friends also get's in a mentally exhausted state of so called “cognitive overload” Since using social-media puts you in this state because you are constantly evaluating different types of text-, photo-  & video-posts from so many different people. again let's try to go: LITTLE DATA! - Lil Data - Lil Data lil dat.a

    I will still stand up for ALL of us, just being a part of nature and this little-universe, & still here where I turn 50 I will try stand up for new innovations and real dreams, in the field of an interesting co-existing future: animals, plants, minerals, cosmic-entities, dwarfs, elf's —— you name it ..- That is what I would like to share with my daughter and others that I might be able to inspire, - if just a little bit.
    ####
    - I was: Kommunal Klon Komputer 02 then I was: Kommunal Klon Komputer 04 - now i am just: Pruttie - but at least I am still: Prutti - - - Because even though: My robotic skills have failed, time and again - Baldur & Jesus' blood never failed me YET - Oh yes & that's why I stand w. Aesthetic.Computer as an experiment, - 50 years of age & still just “Pruttipal” - -PRUT -- PRUT…
    `,
    pictures: ["lnl19"],
    sounds: ["robotic"],
  },
  {
    title: `Lesson NOT Learnt 20`,
    subtitle: "",
    text: `
    §————————————————§
    …Some Classic Pruttipal -/- Goodiepal Talk…
    §————————————————§
    000 §§§ — if you do not have a house, then never should you have more things then, what you can store-away unnoticed in a middle-sized Euro-art-gallery -
    Today SO much SPACE is used in handling and storing art - & Since the cost of living is getting higher and higher in most places - t.t.to hide your belongings among museum & gallery-things might not be a bad idea… — §§§ 000

    [20_always_on_the_move]
    `,
    pictures: ["lnl20"],
    sounds: ["20_always_on_the_move"],
  },
  {
    title: `Lesson NOT Learnt 21`,
    subtitle: "",
    text: `
    $$——————————————————$$$$
    Best background music for boring lecture videos
    _______—______ peak mandolin ———§§$$$$
    ingen tid - Ingen saaar
    WARNING - to not listen to this mu-sick, as it is a NO GO!

    [ingen tid - ingen mandolin]
    `,
    pictures: ["lnl21"],
    sounds: ["ingen tid - ingen mandolin"],
  },
  {
    title: `Lesson NOT Learnt 22`,
    subtitle: "",
    text: `
    ——————————————————————————————————
    RAP-music & Roll-play Games
    ——————————————————————————————————
    good song, good for you…
    ——————————————————————————————————
    [ingen tid - ingen saaar]
    `,
    pictures: ["lnl22-1", "lnl22-2", "lnl22-3"],
    sounds: ["ingen tid - ingen saaar"],
  },
  {
    title: `Lesson NOT Learnt 23`,
    subtitle: "",
    text: `
    ——————————————————————————————————
    Mandolin Cafe
    ——————————————————————————————————
    Welcome - please do not listen…

    [life is mandolin]
    `,
    pictures: ["lnl23"],
    sounds: ["life is mandolin"],
  },
  {
    title: `Lesson NOT Learnt 24`,
    subtitle: "",
    text: `
    ———————————————
    Music From: B-tar for: LIV
    ———————————————
    §§ Pruttipal-music-distro-pro
    black-dystopian-swiss-cheese §§Lesson NOT Learnt 24
    ———————————————
    Music From: B-tar for: LIV
    ———————————————
    §§ Pruttipal-music-distro-pro
    black-dystopian-swiss-cheese §§
    __________________________________
    2023- 2024 Pruttipal - When you visited Fleisher/Ollman Gallery in Philadelphia (maybe in/around 2006), you mentioned that computers would "Neeeeever be able to tell" (emphasis yours) what is written in a distorted captcha.

    Despite my tragically terrible memory, I remember much of the event at F/OG quite clearly (and fondly). I believe in expansive thought and experimentation, so I was enthralled. With all the developments in AI over these past few years, do you still believe this to be true? Are you willing to concede that you were incorrect on this point? - I appreciate your time and your ideas. Also, you mentioned "Trust your heart, not what your phone tells you." I have a thought on this: in this world, 'heart' is generally synonymous with 'desire' (I would equate these words to the Sanskrit word 'chitta'). Even now, phones are able to recommend, with relative accuracy, what content a user may be interested in. I suggest that recommendations are nothing more than attempts at fulfilling the 'desires' of the user.__________________________________
    Given the rate of technological change and the current global investment in AI, do you feel that it is possible for a phone to more accurately predict the user's desires to a point where there is very little distinction between the two? Could it get to the point where our phones know us better than ourselves? And would this nourish our own understanding of ourselves, thus potentially bettering society?__________________________________
    Like I said, I'm a curious, thinking person. Sorry for the possibly irrelevant ideas and questions.

    Sanātana Dāsa
    `,
    sounds: ["life is liv"],
  },
  {
    title: `Lesson NOT Learnt 25`,
    subtitle: "",
    text: `
    Narc Beacon - Nag Nag Bacon

    §§§§ Flap Nipper _ Goodiepal
    §§§§ Flap Nipper _ Mainpal Inv.
    §§§§ Plat Nipper _ Pruttipal
    `,
    pictures: ["lnl25"],
    sounds: ["flap-nipper-main"],
  },
  {
    title: `Lesson NOT Learnt 26`,
    subtitle: "Pause",
    text: `
    —————————————
    '//**Clip Talk**//'' 2
    —<HERE you will have no audio - for this 2 wonders of this world>— - The %%world is full of good things… NO music is good - Good music is NO - often I find myself whit so much love inside, that even my space-friends can not hold me back .. & why should They ??? - you are my space friend - i'am your time friend & time never really existed in the first place %%
    `,
    pictures: ["lnl26-1", "lnl26-2"],
  },
  {
    title: `Lesson NOT Learnt 27`,
    subtitle: "",
    text: `
    ———————————————
    ANGEL's mandolin-ium bum BuM
    [angel mandolinur]
    `,
    pictures: ["lnl27"],
    sounds: ["angel mandolinur"],
  },
  {
    title: `Lesson NOT Learnt 28`,
    subtitle: "",
    text: `
    ———————————————-
    ANGEL EYES for angel's in the sky & angel's under the sea. . .
    [angel eyes]
    `,
    pictures: ["lnl28"],
    sounds: ["angel eyes"],
  },
  {
    title: `Lesson NOT Learnt 29`,
    subtitle: "",
    text: `
    ———————————————-
    THE REAL ANGEL EYE's
    - - N.B.THIS IS were the Angel Eye's song really comes to its right- -
    :_*:_*:_*:_*: This version of: Angel Eyes is done by §§ Koolroc'e §§ — She dos also go by the name:  ''''ULRIKKE'''''' & probably a thousand other names - who knows?;:_;_:;
    She is insanely bright and I love hear Energy and spirit -! ^'^'^'^^' The outcome of this lady's produce is manifold and baroque - in the true meaning of the word - ! ^'^'^'^
    She is one of the reasons why I am still alive and here today - - - A Fantasitc young artist - ready-made for the times to come… trust a Pruttipal . . .
    ^'§^'§^'Ohhhh - I am not a big fan of cigarettes - but in this world, - differences is what make life a better place, then the grave. ^§'^§'^^'§^§'^
    *_:__*_:__*_:__*_:__*_:__*_:__*_:__*_:__*_:__*_:__*_:__*_:__*_:
    - ANGEL EYE's - ANGEL EYE's - ANGEL EYE's - ANGEL EYE's -
    ^'?^?^'?^'?^?^'?^'?^?^'?^'?^?^'?^'?^?^'?^'?^?^'?^'?^?^'?^'?^?^'?^'?^?^'?^'?^?^'?^'?^?^'?^'?^?^'?^'?^?^'?^'?^?^'?^'?^?^'?^'?^?^'?^'?^?^'?^?^'?'

    [new recording 16]
    `,
    pictures: ["lnl29"],
    sounds: ["new recording 16"],
  },
  {
    title: `Lesson NOT Learnt 30`,
    subtitle: "",
    text: `
    ———————————————-
    L.O.V.E. RAP-rock : §§§—LOVE-YOU-POP—
    ^'?^?^'?^'?^?^'?^'?^?^'?^'?^?^'?^'?^?'?^'?^?^'?^'?^?^'?^'?^?^'?^'?^?^'?^'?^?^'?^'?^?^'?^'?^?^'?^'?^?^'?^'?^?^'?^'?^?^'?^'?^?^'?^'?^?^'?^?^'?'

    [i love you kat - mandolin]
    `,
    pictures: ["lnl30-1", "lnl30-2"],
    sounds: ["i love you kat - mandolin"],
  },
  {
    title: `Lesson NOT Learnt 31`,
    subtitle: "",
    text: `
    ———————————————-
    LOVE You dad
    Original Version - B-tar = very good…
    !—!—^*^_!0^0_*0—!—^*^!_!0^0_!*0_!
    *_:__*_:__*_:__*_:__*_:__*_:__*_:__*_:__*_:__*_:__*_:__*_:__*_:

    [i love you pop]
    `,
    pictures: ["lnl31-1", "lnl31-2"],
    sounds: ["i love you pop"],
  },
  {
    title: `Lesson NOT Learnt 32`,
    subtitle: "",
    text: `
    Everybody would like to play: POPCORN

    __.*_-:*__:***_—:__'''___''__:_**___**__*__*

    Here first a few seconds (maybe 6?) of digital-silence before I perform: Gershon Kingsley's melody Popcorn - just like that ——But w. extra beat's!
    'WarninG' *YES*YES*YES*YES*YES*YES*- the sounds first start in a few. sec. - Making computer-programs defunct is an §§artform§§ taht only few masters & even fewer than that can a appreciate…
    //* : * : * :'* *-':_-****.:_'_-:-****_-*
    <no no no no picture>
    **** : * : * :'* *-':_-****.:_'_-:-****_-******
    —— Why is there no picture? = the program is defunct - & de-composed. . . Like the old Franck Muller - we are 0oO0O0O= The VikInG Klokkers, so peaceful & we are Master of Complication's
    ———————————————-
    `,
    sounds: ["popcorn"],
  },
  {
    title: `Lesson NOT Learnt 33`,
    subtitle: "Should I make some pornography?",
    text: `
    In cold country's you will typically find coloured poles by the side of the road %%% - * These poles are typically used as markers for: - snow-plow-drivers- * to follow during snowstorms. They help the drivers stay on the road and avoid going too far to the side, where they might hit lets say a ditch or a tree… Additionally, ON-top they can also be used as guides for drivers in low visibility conditions. // //
    —bla. bla. bla. bla. bla. bla. bla.—
    now last-early evening I was going on my bicikel, downtown Torshavn, and there behind a interactive-Bus-info-map, was 3 lusty road poles engaing in xxx-activities, and I was just thinking that at my AGE, there is Not much to be seen, or for some people to much to be seen? ? — But maybe I should make some content, looking at a BEAR-like me, would be for the few, YES only for the people with the most distinct and perverted sexual DREAMS, — AND there should be something for them as well, ROAD POLES and OLD BEAR's - uhhh so perverted you are Pruttipal'ur… - WELL you just hit START in your head and, you will never come back…
    ———————————————

    [33_freaky_ts]
    `,
    pictures: ["lnl33"],
    sounds: ["33_freaky_ts"],
  },
  {
    title: "Lesson NOT Learnt 34",
    subtitle: "Loin-El Glitchie-Banan",
    text: `
    WOW *** Glitch today has started to be  ——enjoyed as some fancy wine uhh uhh - and ALL the NFT losers tell funny refs. to the subject ! — NFT's was a BIG JOKE - do not NFT - BUT but let me share with you the real deal… yOu see ^^Real Glitch* takes the OUTput of malfunctioning technology and crafts it into adventurous new things & bla. bla. bla. bla. or so they say -%%%%% BUT — NO o no NO no -that is not the whole story, you need to live the Glitchie life - §§§§ YES Squelchy & glitshiiii §§§§ - In order to understand it - !!§§§ and some of the best to do so has been my heart-friend's:- Yaloopop, Rosa Menkman & Legacy Russell - there you have words and lives to dive into -WARNING - could be A bumpy ride…but output =  FANTASTIC . . ._:___ Now a days Computer people in the U.S. has a lot to say about acting Glitchie, in gender - and life and YES YES —But —People in: New York S.F. & the tech-valley took years and years to understand us Glitch'ers  ——   :_*:_*:_:*:*: — Well I guess if you are interested in function & composing it takes you years and years to learn to: DE-compose - and DE-function - now Rosa Menkman was very good at that especially in life - she is fantasic you should go and experience hear work . …&”/!&”!/“&! “&/“  All my Beo_Bio_Lystrup Syre-tapes & DAT*s & Mini-disc's was send to mailing lists in Japan -that is hove I got not know wUnders as  Toshiyuki Kobayashi - Timepoco - OHHH those recordings - they were all different renditions of my Glitchy Lystrup set-up . __

    Glitch Feminism §§ init ding §§ Glitch Studies Manifesto - || beyond resolution §§ Ten Minute Painting & maybe just maybe my lanscape paintings in my book: El Camino del Hardcore  rejsen TIL nordens INDRE! §§ ohh and everything  by: Yaloopop §§ — $$ N.B. When the academy's joined the fun was over real Glitchie's should never go for the PHD!

    [nice dvvvvice and glitch]
    `,
    pictures: ["lnl34"],
    sounds: ["nice dvvvvice and glitch"],
  },
  {
    title: `Lesson NOT Learnt 35`,
    subtitle: "",
    text: `
    …. & raD & Mandolin'ur

    V54ttur, Nissar, Dv54rgir, Viking'ur, Picts ——— V54ttur, Nissar, Dv54rgir, Viking'ur, V54ttur, Nissar, Dv54rgir, Viking'ur, Picts

    <<< Pict's …. & raD >>> <<< …. & raD >>> <<< Pict's …. & raD >>> <<< …. & raD >>> <<< …. & raD >>> <<< …. & raD >>> <<< Pict's …. & raD >>>

    Sometimes it's just too hard to work in music-archaeology, YOU  are up at libraries & in the field day and night - YOU find things that is not listed anywhere, & if you are good as the Goodiepal / Pruttipal - you try to bring it back into life, because you know that only true bringing you finds on the road - people will unlock a interest in your most §§§cryptik and obscure artifacts§§§
    - NOW - Listen
    THis is: def - N- RAD - an  VERY VERY early Pictish rap-duo-from the orkney islands - later, much much later one of the members would move to mainland Europe and become SUPER happening in the art's of Wien & München  the name was: MISS le BOMB —

    The Pict's had it all and still has it & this is why you should care and research this fantastic duo…

    BIG BIG big LOVE TO the Orkney Islands for always being a safe-harbor for the pruttipal - …. & raD …. & raD …. & raD …. & raD …. & raD …. & raD …. & raD
    V54ttur, Nissar, Dv54rgir, Viking'ur, Picts ——— V54ttur, Nissar, Dv54rgir, Viking'ur, Picts ——— V54ttur, Nissar, Dv54rgir, Viking'ur, Picts

    <<< …. & raD >>> <<< Pict's …. & raD >>> <<< …. & raD >>> <<< …. & raD >>> <<<  Pict's …. & raD >>> <<< …. & raD >>> <<< Pict's …. & raD >>>

    [pictish_dsc_str]
    `,
    pictures: ["lnl35-1", "lnl35-2", "lnl35-3"],
    sounds: ["pictish_dsc_str"],
  },
  {
    title: `Lesson NOT Learnt 36`,
    subtitle: "",
    text: `
    Mxyzptlk - D0ni or D0ny -

    §§§§ Do you dare to scroll up and down on this entity? §§§§ up ^ & > over —— & bacK <<<<
    V54ttur, Nissar, Dv54rgir, Viking'ur, V54ttur, Nissar, Dv54rgir, Viking'ur,
    Mxyzptlk Madness is D0ni or D0ny - the master of asymmetric dis-fuctional music behaviour '_*:__ & :_;:__; D0ni is something that exists apart from other things, D0ny is having its own independent existence. - Mxyzptlk DOni =  master of the strange and A stranger to MASTER's**… - I do hold this entity close to my heart and I think that it's output is on the level of: Karlheinz - Fucking - RRRecords - RRR-KARL - and Jessica Rylan --- Little Boy Blue and the likes… *YES ;:_;:;_YES:_*:_'
    V54ttur, Nissar, Dv54rgir, Viking'ur, V54ttur, Nissar, Dv54rgir, Viking'ur,
    — This sounds & -> THIS entity should inspire you to go out and say:  I know where I am going and I know that I am talking the wrong way there,
    but I really do not care, because you are somehow not there…
    V54ttur, Nissar, Dv54rgir, Viking'ur, V54ttur, Nissar, Dv54rgir, Viking'ur,

    —JUST LIKe THaT—be kind to the world's- it all come's back around… A Key 2.snd Eurobot distro...

    [snakkende_shadow_d0nni]
    `,
    pictures: ["lnl36-1", "lnl36-2"],
    sounds: ["snakkende_shadow_d0nni"],
  },
  {
    title: "Lesson NOT Learnt 37",
    subtitle: "",
    text: `
    more on distro…

    take notes of almost everything… — but not all…
    Trust: din Nisse - Trust: THE GUILD OF THE VIKIGN klokker's

    [09_distribution-manfler]
    `,
    pictures: ["lnl37"],
    sounds: ["09_distribution-manfler"],
  },
  {
    title: "Lesson NOT Learnt 38",
    subtitle: "",
    text: `
    Hygge (Injected With a Poison)

    V-Lars-V-Lars-V-Lars-V-Lars-V-Lars-V-Lars-V-Lars-V-Lars-V-Lars-V-Lars-V-Lars-V-Lars-V-Lars

    My old old ancient friends of Cholchester, you want that V-lars London feeling - you wonna have that god old V-Lars - TIMMERs<>Drunkard feeling. §§§ Well also if you are living somewhere else you probably still wonna have the same feeling????

    —THE time is here: 'Kick'Back'For'The'HYGGE and make the sounds from the Faro islands bring you here - and back to Colchester.

    >>>20th of January = Save this mystery date<<< 50 years old and stil living off the grid. * No Mortgage, No Utilities * ((no daughter (sadly) ))

    V-Lars-V-Lars-V-Lars-V-Lars-V-Lars-V-Lars-V-Lars-V-Lars-V-Lars-V-Lars-V-Lars-V-Lars-V-Lars

    [dsj]
    `,
    pictures: ["lnl38-1", "lnl38-2"],
    sounds: ["dsj"],
  },
  {
    title: "Lesson NOT Learnt 39",
    subtitle: "Cosmic Entities of Pure Energy",
    text: `
    §_§ MAgisKE nisser hates WAR - The Viking Klokker's hates WAR & is down w. monsters, vættur & d.d.dragons = WAR_is_shit_!

    - It takes 5 UFO's to construct an: ANGEL…

    it takes 9 GOODIEBAGS to form 9 demonbags §_§_
    -come, come & join the guild of the viking klokkers!

    [angelic_birthday]
    `,
    pictures: ["lnl39-1", "lnl39-2", "lnl39-3"],
    sounds: ["angelic_birthday"],
  },
  {
    title: "Lesson NOT Learnt 40",
    subtitle: "",
    text: `
    Bolle Borgen - via. Lasserne > p.r.u.t.t.i

    Fredag's bar'en
    My Name is: Lasser'ne I am a young Scandi^^^^taken true THE PRUTTI's of Aesthetic.COMPUERrrr, Goodiepal loves me - he surely does - Goodiepal is A mirror, - I come whit a warning…——>>>>>>> § Warning, § Warning, $ Warning: people of:  Scandinavia  you  can  easily * lose 12 years of your life to: Bolle Borgen —— S.U. + U.S.A. culture = long talks > no original thoughts & opinions … YOu are nearly reiterateing the -.brain -.work of -.others - far away from you in culture and time…

    you know what they say when it's: the Wrong Time Capsule

    § Bend the machine § See the pendulum swing and go in between §
    § Bend the machine § See the pendulum swing and go in between §
    _When you hunger number one_
    _It's no wonderland of fun_
    _Don't be mean to see “Bolle Borgen”_
    _You get caught in the machine_
    _You're caught in the machine_
    ____________________________________
    _Here's a message they would say_
    _Don't forget me yesterday_
    _'Cause today's no place to stay_
    _Will you spirit me away?_
    _You spirit me away_
    ____________________________________
    _Bend the machine, Bend the machine_
    _See the pendulum swing and go in between_
    _Skip the waves, syncopate_
    _Forwards backwards_

    and so on…. As *Lasser'ne* I think that I would like to join the guild of the Viking KLOKKER's they say that they have a seat waiting for me at there table…

    [rider_G_las_prut_slip]
    `,
    pictures: ["lnl40-1", "lnl40-2"],
    sounds: ["rider_G_las_prut_slip"],
  },
  {
    title: "Lesson NOT Learnt 41",
    subtitle: "Tele",
    text: `
    §§ The best ideas for performing, might be right at your hand(y) - tele-communix has long been the means of ultra-dimentinal > idea > exchange...§§

    [41_telemeterskive_Soap_Bubble_Fever_birthday]
    `,
    pictures: ["lnl41-1", "lnl41-2"],
    sounds: ["41_telemeterskive_Soap_Bubble_Fever_birthday"],
  },
  {
    title: "Lesson NOT Learnt 42",
    subtitle: "disfunkta-video-game's & sick prut's",
    text: `
    Rui Liu made a comput-arixur coded game about the story where the statement “once and once again” comes from. It’s about an old person, a collapsing village, and a sigh of eternal sadness. Everything in this series is an iteration of the sigh.

    What if it is the truth of everything?

    ^Ŵ Once and Once Again - game, Once and Once Again - code ^¤-Z^ Once and Once Again - game, Once and Once Again - code ^§&"^ Once and Once Again - game, Once and Once Again - code ^&#^ Once and Once Again - game, Once and Once Again - code ^ - ^ Once and Once Again - game, Once and Once Again - code *

    [prutti_the_sick]
    `,
    pictures: ["lnl42-1.png", "lnl42-2.png"],
    sounds: ["prutti_the_sick"],
  },
  {
    title: "Lesson NOT Learnt 43",
    subtitle: "use HANDS of dvb TIME",
    text: `
    The current Estate of the Viking Klokkers at Pachinko calculated by: the Hands of dvb TIME

    I love to make things that I collect into sets, I have a huge art collection, but my own passion is music and to make dials, rotors, & other watch parts with my expert-friends.

    ==  set of 2 pieces ==

    01. Teppop = Amandus Rock - just that, no need to add anything, not even a watch

    02. Miki Skak = Omega, early tarmkreft modification Watch

    03. Lady Lucy = BLANCPAIN PLATINUM lever POCKET WATCH w. Heavy Purttipal SPECIAL dial

    04. Símun Poulsen = Piaget w. PRUTTIPAL ultra small ACID dial

    05. Anna Munk = Patek Philippe Golden Ellipse w. PRUTTIPAL extreme ACID dial

    06. Tora Dalseng = JLC triple date - Pruttipal BURN YOUR BANK dial & much more
    ( Tik Tak Ure Ålborg & Steven Stapleton )

    ==  set of 3 pieces (or more) ==

    01.(07) Sufie Elmgreen = Teppop = Open Face Ultra-Thin Platinum //r0ver// Pocket Watch

    02.(08) David Shrigley = Tora Dalseng & Prut = Patrik Sjögren PIECE UNIQUE w. Pruttipal Roter & dial

    03.(09) Jonathan Monk = Daniel Cabrillos Jacobsen = Jaeger-LeCoultre Memovox Alarm Pruttipal Case

    == in the making ( as of right now! ) ==

    01.(10) Henrik Plenge = Piaget Beta 21 w. Special Pruttipal zzzzZUPER engraved dial

    02.(11) Claus Carstensen = Franck Muller w. Special Pruttipal engraved DIAL

    N.B. I Sometimes also make instruments and things, you see creating things w. purpose is important in a purpose-lacking life-environment.

    == instruments ==

    01.(-) Kasper & Peter Schmeichel guitar w. Pruttipal anti-Dane embellishment

    02.(-) The Goodiepal FREE instrument library ( prutti 12 )
    ___________________
    ALL klokk's comes whit 2 years Carl Oppermann & pruttipal service warranty

    -

    f0dselsdags havna-tek by SUPERVISJÓN - f0dselsdags havna-tek by SUPERVISJÓN - f0dselsdags havna-tek by SUPERVISJÓN - f0dselsdags havna-tek by SUPERVISJÓN - f0dselsdags havna-tek by SUPERVISJÓN - f0dselsdags havna-tek by SUPERVISJÓN - f0dselsdags havna-tek by SUPERVISJÓN - f0dselsdags havna-tek by SUPERVISJÓN - f0dselsdags havna-tek by SUPERVISJÓN - f0dselsdags havna-tek by SUPERVISJÓN
    `,
    pictures: ["lnl43-1", "lnl43-2", "lnl43-3", "lnl43-4"],
    sounds: ["tillukku_prut_prut"],
  },
  {
    title: "Lesson NOT Learnt 44",
    subtitle: "",
    text: `
    14_18 // 14_18 // 14_18 //
    the fall of the fountain of youth, is the beginning of Ye_olde —new— YOU - —— Ye_olde new YOU is a numerical  you —&— the NEW music that they in that timeline of Ye_olde —new— is just traditional trad…

    Du er en trett
    trett
    trett phonix…

    Ohh -please enjoy this music from the following list… —You see— The art of listing things that you like of musically interesting material is a slumbering art-form, let@s bring it back..- the dreamer has awakened —

    [fiona_kruger_is_a_grand_dreamer]
   `,
    pictures: ["lnl44-1", "lnl44-2", "lnl44-3"],
    sounds: ["fiona_kruger_is_a_grand_dreamer"],
  },
  {
    title: "Lesson NOT Learnt 45",
    subtitle: "",
    text: `
    Pluto existed before modern humans discovered the p-entity in 1930. Pluto exists after 2006 when Rational Thinking humans reclassified it. Pluto does not care BUT Pluto is not in solar system models because it is not a “planet” by the IAU definition. - bla bla bla.. Pluto is still a planet to Alice Pamuk & so it should be to you. PLUTO is a astronomical influencer - a title only a few can take…
    - Here a is a few personal lessons for me send by Pluto via Alice Pamuk.
    Learn from: Polle

    Learn from: Pippi@s father

    Learn from: FIONA Kruger

    Learn from: Christian Klings

    You see Pluto is right almost all the time…

    In order to communicate whit Pluto or Alice Pamuk you need Platinum, and Pluto is cheap from a modern human perspective but also fancy at the same time, so a perfect watch fit for the meeting would be a: SAWTCH made in platinum, like the one at the picture here…

    [platinum_alice_pluto]
    `,
    pictures: ["lnl45-1", "lnl45-2"],
    sounds: ["platinum_alice_pluto"],
  },
  {
    title: "Lesson NOT Learnt 46",
    subtitle: "",
    text: `
    All stories is made up by events following each other, the more chaotic the events fall out the better the story, go and find signs in repetitions and intervals in events as your life takes place, they all have hidden code to tell you. And that hidden code is, if not decipherable then at least playable just like a board game.

    Real players and real——Chairman Of The Bored——like // Rank Sinatra // plays the game whit a platinum bar as a watch and a diamond on the crown… that is the only way or so I have been told. Ohhh // Rank // just Joined the guild of the viking klokkers. - on the picture you see my ingot (s)

    [alice_platinum_pamuk_eye_low]
    `,
    pictures: ["lnl46-1", "lnl46-2"],
    sounds: ["alice_platinum_pamuk_eye_low"],
  },
  {
    title: "Lesson NOT Learnt 47",
    subtitle: "",
    text: `
    the true meaning of mastery

    To be real ---Saedfuck---m0e---based--- in - the now, you need to be able to take apart an Urban Jürgensen yourself, Carl oppermann and I did just that-YES YES-( I was cheerleading &  Carl did the hard work --no cap--)  - the former owner of this watch was ///Smadremanden///  Urban Jürgensen is known for their mastery of details, a distinction particularly well suited for the realm of pruttipal-horology… bla. bla. bla.  - But the reason that I'm telling you this, is that even not an Urban Jurgensen, that comes from a four-century long tradition of Danish-Swiss watchmaking etc. etc. ----is hardly making anything themself. It´s true... we just reverse engineered one ----

    The movement the (caliber P5) is made by one company, so is the watch chain etc. etc. Urban Jurgensen is simply putting there well designed things together --- That is what forms a modern ## Jurgensen -##  and this is the way you make MASTERY-things... - People there thinks that they should be good a everything ends up being bad at most things...

    Make your-self dependent on the expertise of your friends, I have done so in all aspects of my life, and is continuously doing so -- even though my surroundings is currently punishing me for doing so... oohhhh Smadremanden - JOINED THE GUILD of the Viking klokkers

    Pruttipal we eat all the crumbs - Saedfuck---m0e / Saedfuck---m0e / Saedfuck---m0e / Saedfuck---m0e

    [urban_ostens_ulve_mastery_prut]`,
    pictures: ["lnl47-1", "lnl47-2"],
    sounds: ["urban_ostens_ulve_mastery_prut"],
  },
  {
    title: "Lesson NOT Learnt 48",
    subtitle: "",
    text: `
    the Gunner Waerness aethetic challenge. . .

    just as you thought that the synthesizer was the drippin' New Drip, you wake up and realize, that it is the guitar there is back...

    and if it@s a guitar there is back you need not just any guitar NO NO §§§§ you need a ----red and white--- Danish football guitar from the Martin guitars custom shop, designed by: Akustikken Kasper & Michael Schmeichel -- else it will not take offff... 

    -- And when you have that, you need to lend it out to everyone you know --YES YES-- as everybody else plays music much better then you will ever be able to... JUST listen to this audio, a wonderful mixtura of recording technique, sound manipulation, and performative skills. - pachinko magic - 


    Gunner Wærness should play the Football guitar once at least, that is a aethetic.computer challenge //...No Cap...//

    [star_under_1_elmgreen]
    `,
    pictures: ["lnl48-1", "lnl48-2"],
    sounds: ["star_under_1_elmgreen"],
  },
  {
    title: "Lesson NOT Learnt 49",
    subtitle: "",
    text: `
    never just party like a PRUTTIPAL - GO above the bar

    //-§§ Invitation - YES just come out I am swimming in Prizes and honours §§---%-§-§

    Missed call Pruttipal | 8:11 AM
    Missed call Pruttipal | 8:20 AM

    [Goodie_Invitation]
    `,
    pictures: ["lnl49"],
    sounds: ["Goodie_Invitation"],
  },
  {
    title: "Lesson NOT Learnt 50",
    subtitle: "",
    text: `
    50 Ye Olde Kommunal Klon Komputer 04

    we-e build Ye Olde Kommunal Klon Komputer 04 for other riders as well as us --- it has w. unknown riders over the years been the utmost of crazy places and is now on the way back***---It is a --trickster tool-- / --So it can travel and move everywhere --- incl. The Elizabeth line in a drippy drippy-transportation way, right under London's ''--Sorry mate you can't do that''-- nose. . . drip drip///  We are the Wikingur klokurinár we are radical transporter's of entities... you will __never__ever be able to stop us... I am now 50 and we are still doing this...///BRING IT ON~~~~

    Finlay Burch just joined: The Guild of The Viking Klokker'zzzzz...

    [sweden_&_the_elizabeth_line.m4a]
    `,
    pictures: ["lnl50-1", "lnl50-2"],
    sounds: ["sweden_&_the_elizabeth_line"],
  },
  {
    title: "Lesson Not Learned 51",
    text: `
    In order to keep the flame burning, oh Yes we are a new mail-art Orchester!

    [prut_goodiepal_&_galls.m4a]
    `,
    pictures: ["lnl51"],
    sounds: ["prut_goodiepal_&_galls"],
  },
  {
    title: "Lesson Not Learned 52",
    text: `
    - >> setting dials to measure: > tenpo li wan §§§ what time is it? -klokken 01 ^^ it's cosmic time = The time is cosmic! >>feat. Enla Pene Jakosen <<

    --- there is a r8dio-program and it was out at time signature 01:00 --- so I set the dials on these 2 watches to indicate the length of the program. ---I was thinking of giving you a sample, but then again you can go find it yourself---no need to bother you with the same data twice. ##. $XX BUT a precise watch dial picture - That you need... I measure all kind of, for you mostly useless data, but then again it is Horology... I could do this acid posts for for all the Radio programs that I make, but you get the picture.

    Klokken 01 = jan Enla Pene Jakosen li jan pona sin. mi toki e ijo tenpo epika en mute sona pi ilo tenpo. jan mama Putipa li poka mi.
    jan Putipa il jan lawa pi kalama musi kin en ilo tan ma Tansi kin. jan Putipa li lawa pi kalama pana pona kin.
    
    // // // -- // Pruttipal sometimes talks to conceptual artist Henrik Plenge Jakobsen about mind-bending clockworks, this includes Jakobsens work on the larger-than-life spielwerk/automatron/magnificent cosmic clock at Odeon in Odense, Denmark...  // // // -- // Wili Beans & Sebastian Lawrence Højland reports that all this is Komputer controled as jan Enla Pene Jakosen is not a gear individual... 

    [we_are_all_luther_blissett.m4a]
    `,
    pictures: ["lnl52"],
    sounds: ["we_are_all_luther_blissett"],
  },
  {
    title: "Lesson Not Learned 53",
    text: `
    lesson not learned about: --BURSDAGS--in--general-- PIKBLOD TIL DIG

    Yes - A birthday present from us at: Aesthetic Laer Klokken, YOU
    requested -out-there-music- as gift for the day & hmmmmm then think that
    you should have some: PIKBLOD - the movie star from: INTERNET IS DEATH -
    & --master of the organ-- I know you that you will like that - the
    UTMOST  of fantastic performances in Scandinavia as you also do
    yourself, yes SOME PIKBLOD Rett fra: Prut-Karlen´s gemakker & V/Vm Test
    Nydelig musikk og ting som faaaar deg til aaaa gaaaa: Hmmmm? HAPPY
    ---BIRTH--day---

    -PIKBLOD-//-PIKBLOD-//-PIKBLOD-
     
    [PIKBLOD-when-you-walked-speed-up-slow_version.m4a]
    `,
    pictures: ["lnl53"],
    sounds: ["PIKBLOD-when-you-walked-speed-up_slow-version"],
  },
];

let lesson = 0,
  scroll = 0,
  scrollMax;
const lessonPaintings = [],
  lessonPaintingsCache = new Map(), // Cache for scaled dimensions and draw info
  preRenderedPaintings = new Map(), // Cache for pre-rendered scaled paintings
  picWidth = 320;
let color,
  noiseTint = [255, 0, 0];

// Audio playback state
let playingSfx = null,
  isPlaying = false,
  pausedAt = 0,
  playStartTime = 0,
  actualDuration = null,
  preloadedAudio = null, // Store preloaded audio like wipppps
  progress = 0, // Track playback progress
  isScrubbing = false, // Track if actively scrubbing (pause normal progress updates)
  scrubOccurred = false, // Track if scrubbing occurred during this button interaction
  scrubOffset = 0, // Offset between touch position and current playback position
  waveformData = null, // Store compressed waveform data for progress bar visualization
  startTimeSeconds = null, // Store the requested start time in seconds
  requestedTimeString = null, // Store the original time string for immediate display
  isBuffering = false, // Track if audio is currently loading
  playbackAttempted = false, // Track if user has attempted to play audio
  scrubButton = null; // UI button for the progress bar area

// Parse time string like "1:53" into seconds
function parseTimeString(timeStr) {
  if (!timeStr || typeof timeStr !== "string") {
    return null;
  }

  // Handle formats like "1:53", "0:30", "2:15", etc.
  const timePattern = /^(\d+):(\d+)$/;
  const match = timeStr.match(timePattern);

  if (!match) {
    return null;
  }

  const minutes = parseInt(match[1], 10);
  const seconds = parseInt(match[2], 10);

  // Validate seconds (should be 0-59)
  if (seconds >= 60) {
    return null;
  }

  const totalSeconds = minutes * 60 + seconds;

  return totalSeconds;
}

// 🥾 Boot
 function boot({ api, wipe, params, hud, help, num, sound, net, ui, screen }) {
  wipe(0);
  noiseTint = num.randIntArr(255, 3);
  lesson = params[0] ? parseInt(params[0]) - 1 : num.randInd(lessons);
  if (lesson > lessons.length - 1 || lesson < 0) lesson = 0;

  // Create the scrub button for the progress bar area - only if lesson has sounds
  if (lessons[lesson].sounds?.length > 0) {
    const barHeight = 22;
    
    // Calculate positioning based on timecode layout (matching paint function calculations)
    const glyphWidth = 6; // Each glyph is 6 pixels wide (typeface blockWidth)
    const promptStartX = 6; // Prompt starts at x=6
    const cornerLabelText = "prutti"; // The corner label text
    const cornerLabelWidth = cornerLabelText.length * glyphWidth; // Character width
    const lessonNumberWidth = lesson.toString().length * glyphWidth; // Width of lesson number
    const spaceWidth = glyphWidth; // Space character width
    const timeCodeWidth = 5 * glyphWidth; // "00:00" is 5 characters
    const totalTimeWidth = 7 * glyphWidth; // "/ 00:00" is 7 characters
    
    // Calculate where the timecode starts and ends
    const timeCodeStartX = promptStartX + cornerLabelWidth + spaceWidth + lessonNumberWidth + spaceWidth;
    const timeCodeEndX = timeCodeStartX + timeCodeWidth + totalTimeWidth;
    
    // Start the scrub button from the middle of the timecode area
    const timecodeMiddleX = timeCodeStartX + Math.floor((timeCodeWidth + totalTimeWidth) / 2);
    const buttonStartX = timecodeMiddleX; // Start from middle of timecode
    const buttonWidth = screen.width - buttonStartX; // Extend to right edge of screen
    
    scrubButton = new ui.Button(buttonStartX, 0, buttonWidth, barHeight);
    scrubButton.offScreenScrubbing = true; // Allow scrubbing to continue horizontally off-screen
    console.log("🟣 BUTTON CREATED: Timecode-aware position (" + buttonStartX + ", 0) Size:", buttonWidth, "x", barHeight);
     } else {
       scrubButton = null; // Clear button if no sounds
    console.log("🟣 BUTTON CREATED: No button created - lesson has no sounds");
  }

  // Parse start time from params[1] if provided (e.g., "1:53")
  startTimeSeconds = parseTimeString(params[1]);
  requestedTimeString = params[1]; // Store the original string for immediate display

  if (startTimeSeconds !== null) {
    console.log(
      "🕐 Prutti: Start time requested:",
      params[1],
      "(" + startTimeSeconds + "s)",
    );
    // Set pausedAt to a placeholder - it will be properly set when duration is known
    pausedAt = 0.1; // Non-zero placeholder so we know we have a start time
    // Don't set progress yet - we'll set it when duration is known
  } else {
    pausedAt = 0; // No start time, start from beginning
    progress = 0; // Reset progress for new lesson
  }

  console.log("🪄 Prutti:", lesson + 1);
  hud.label("prutti " + (lesson + 1));

  // Reset audio state
  playingSfx = null;
  isPlaying = false;
  isScrubbing = false; // Reset scrubbing state
  scrubOccurred = false; // Reset scrub tracking for next interaction
  scrubOffset = 0; // Reset scrub offset
  // pausedAt and progress are set above based on whether we have a start time
  actualDuration = null;
  preloadedAudio = null; // Reset preloaded audio for new lesson
  waveformData = null; // Reset waveform data for new lesson
  isBuffering = false; // Reset buffering state
  playbackAttempted = false; // Reset playback attempt state

  loadLesson(api, sound);
  color = help.choose("white", "cyan", "yellow", "orange", undefined);
}

// 🎨 Paint
// 🎨 Paint
function paint({ wipe, ink, screen, help, text: txt, painting, paste }) {
  // Light grey background - web 1.0 NY Times style
  wipe(192, 192, 192); // #C0C0C0
  
  // Get lesson data
  const { title, subtitle, text } = lessons[lesson];
  
  // Align with HUD label - 6px from left, lower down
  const leftMargin = 6;
  const rightMargin = 6;
  const contentWidth = screen.width - leftMargin - rightMargin;
  const titleY = 24; // Lower position to align better with corner label
  
  // Colors
  const titleColor = [240, 240, 240]; // Light light grey for title
  const titleShadow = [128, 128, 128]; // Medium grey shadow
  const subtitleColor = [255, 182, 193]; // Light pink (pinkish)
  const bodyColor = [60, 60, 60]; // Dark grey for body text
  const bodyShadow = [255, 255, 255]; // White shadow for better visibility
  
  // Render title with 1px drop shadow (normal font)
  // Shadow first
  ink(...titleShadow).write(title, { x: leftMargin + 1, y: titleY + 1 }, undefined, contentWidth);
  // Then main text
  ink(...titleColor).write(title, { x: leftMargin, y: titleY }, undefined, contentWidth);
  
  let currentY = titleY + 16; // Start position for subtitle
  
  // If there's a subtitle, render it with unifont in pinkish color with shadow
  if (subtitle && subtitle.length > 0) {
    const subtitleBuffer = painting(contentWidth, 50, (api) => {
      // Shadow first
      api.ink(128, 128, 128).write(subtitle, { x: 1, y: 1 }, undefined, contentWidth, true, "unifont");
      // Then main text
      api.ink(...subtitleColor).write(subtitle, { x: 0, y: 0 }, undefined, contentWidth, true, "unifont");
    });
    
    paste(subtitleBuffer, leftMargin, currentY);
    currentY += 24; // Space after subtitle
  } else {
    currentY += 8; // Less space if no subtitle
  }
  
  // Render body text with default font (to support all characters)
  if (text && text.trim().length > 0) {
    // Shadow first
    ink(...bodyShadow).write(text, { x: leftMargin + 1, y: currentY + 1 }, undefined, contentWidth);
    // Then main text
    ink(...bodyColor).write(text, { x: leftMargin, y: currentY }, undefined, contentWidth);
  }
  
  return true; // Keep repainting
}

/* COMMENTED OUT - Original paint function with rendering issues
function paint({
  wipe,
  ink,
  paste,
  screen,
  text: txt,
  help,
  noiseTinted,
  shape,
  write,
  sound,
  box,
  hud,
  ui,
  api,
  painting,
}) {
  wipe(0); // Clear the canvas each frame to prevent rendering issues
  
  // Define constants used throughout paint function
  const glyphWidth = 6; // Each glyph is 6 pixels wide (typeface blockWidth)
  const promptStartX = 6; // Prompt starts at x=6
  const cornerLabelY = 12; // Corner label is positioned at top: 12px
  
  let { title, text } = lessons[lesson];
  text = text.trim();

  // Adjust margins for scroll bar - left margin accounts for 4px scroll bar + 6px gap
  const leftMargin = 10; // 4px scroll bar + 6px gap
  const rightMargin = 6; // Keep right margin at 6px
  const contentWidth = screen.width - leftMargin - rightMargin;

  const titleBox = txt.box(
    title,
    { x: leftMargin, y: 24 + scroll },
    contentWidth,
    1,
    true,
    "unifont",
  );
  const textBox = txt.box(
    text,
    { x: leftMargin, y: 24 + scroll + 10 + titleBox.box.height },
    contentWidth,
  );

  noiseTinted(noiseTint, 0.4, 0.2)
    .ink(0)
    .box(
      textBox.pos.x,
      textBox.pos.y,
      textBox.box.width - 4,
      textBox.box.height,
    )
    .ink()
    .write(title, titleBox.pos, noiseTint, contentWidth, true, "unifont");
  ink(color).write(text, textBox.pos, 0, contentWidth);

  // Debug: Display image loading status
  const imageStatus = `Images: ${lessonPaintings.length}/${lessons[lesson].pictures?.length || 0} loaded`;
  ink("cyan").write(imageStatus, { x: leftMargin, y: screen.height - 20 });

  let lastHeight = 0;
  
  // Pre-calculate the target width to avoid recalculating it every frame
  const targetWidth = Math.min(picWidth, contentWidth);
  
  lessonPaintings.forEach((painting, index) => {
    if (!painting) {
      console.log("⚠️ PRUTTI: Painting", index + 1, "is", painting, "- skipping");
      return;
    }
    
    // Check for pre-rendered painting first (fastest path)
    const preRenderKey = `${index}-${targetWidth}`;
    const preRendered = preRenderedPaintings.get(preRenderKey);
    
    if (preRendered) {
      // Use pre-rendered painting - fastest possible rendering
      try {
        paste(
          preRendered.painting,
          leftMargin,
          10 + textBox.pos.y + textBox.box.height + lastHeight
        );
        lastHeight += preRendered.height + 10;
        return;
      } catch (error) {
        console.error("❌ PRUTTI: Failed to paste pre-rendered painting", index + 1, ":", error);
        // Fall through to regular rendering
      }
    }
    
    // Fall back to cached scaling info if no pre-rendered version
    const cacheKey = `${index}-${targetWidth}`;
    let cachedInfo = lessonPaintingsCache.get(cacheKey);
    
    if (!cachedInfo) {
      // Handle different image object types - check for img property first
      let imageObj = painting;
      if (painting.img) {
        imageObj = painting.img; // Use the actual image element if available
      }
      
      // Check if painting has valid dimensions - handle different image object types
      const paintingWidth = imageObj.width || imageObj.naturalWidth || imageObj.videoWidth || painting.width;
      const paintingHeight = imageObj.height || imageObj.naturalHeight || imageObj.videoHeight || painting.height;
      
      if (!paintingWidth || !paintingHeight) {
        console.error("❌ PRUTTI: Painting", index + 1, "has invalid dimensions - width:", paintingWidth, "height:", paintingHeight);
        return;
      }
      
      // Calculate scale factor for more efficient scaling
      const scaleX = targetWidth / paintingWidth;
      const scaleFactor = scaleX; // Use uniform scaling
      const scaledHeight = paintingHeight * scaleFactor;
      
      // Cache the calculated info
      cachedInfo = {
        imageObj,
        scaleFactor,
        scaledHeight,
        paintingWidth,
        paintingHeight
      };
      lessonPaintingsCache.set(cacheKey, cachedInfo);
      
      // Trigger pre-rendering for next frame (async)
      setTimeout(() => {
        createPreRenderedPainting(index, targetWidth, cachedInfo, api, painting);
      }, 0);
    }
    
    try {
      // Check if we can use a simple integer scale for performance
      if (cachedInfo.scaleFactor > 0 && cachedInfo.scaleFactor <= 8 && Number.isInteger(cachedInfo.scaleFactor)) {
        // Use integer scale factor for fast path in paste function
        paste(
          cachedInfo.imageObj,
          leftMargin,
          10 + textBox.pos.y + textBox.box.height + lastHeight,
          cachedInfo.scaleFactor
        );
      } else {
        // Use width/height object for complex scaling
        paste(
          cachedInfo.imageObj,
          leftMargin,
          10 + textBox.pos.y + textBox.box.height + lastHeight,
          {
            width: targetWidth,
            height: cachedInfo.scaledHeight,
          }
        );
      }
    } catch (error) {
      console.error("❌ PRUTTI: Failed to paste painting", index + 1, ":", error);
    }
    
    lastHeight += cachedInfo.scaledHeight + 10;
  });

  // Progress bar under the corner label - only show if lesson has sounds

  if (progress !== undefined && lessons[lesson].sounds?.length > 0) {
    const barHeight = 22; // Increased height to balance with text positioning
    const cornerLabelY = 12; // Corner label is positioned at top: 12px
    const barY = 0; // Position progress bar at the very top of the screen with no margin

    // Calculate the start position after the corner label - align with first number in timecode
    const glyphWidth = 6; // Each glyph is 6 pixels wide (typeface blockWidth)
    const promptStartX = 6; // Prompt starts at x=6
    const cornerLabelText = "prutti"; // The corner label text
    const cornerLabelWidth = cornerLabelText.length * glyphWidth; // Character width
    const lessonNumberWidth = lesson.toString().length * glyphWidth; // Width of lesson number
    const spaceWidth = glyphWidth; // Space character width
    const timeFirstDigitX =
      promptStartX +
      cornerLabelWidth +
      spaceWidth +
      lessonNumberWidth +
      spaceWidth; // Position of first digit of time
    const progressBarStartX = 0; // Start progress bar at the left edge of the screen
    const progressBarWidth = screen.width; // Full width of the screen

    // Background bar - use very subtle dark color instead of bright red
    ink(24, 24, 24, 255).box(
      progressBarStartX,
      barY,
      progressBarWidth,
      barHeight,
    );

    // Draw waveform efficiently if available - with static needle approach
    if (waveformData && waveformData.length > 0) {
      // Calculate the position for the white needle - between time code and play/pause button
      const glyphWidth = 6; // Each glyph is 6 pixels wide
      const promptStartX = 6; // Prompt starts at x=6
      const cornerLabelText = "prutti"; // The corner label text
      const cornerLabelWidth = cornerLabelText.length * glyphWidth;
      const lessonNumberWidth = lesson.toString().length * glyphWidth;
      const spaceWidth = glyphWidth;
      const timeCodeWidth = 5 * glyphWidth; // "00:00" is 5 characters
      const totalTimeWidth = 7 * glyphWidth; // "/ 00:00" is 7 characters
      const playButtonX = screen.width - 14 - 4; // Play button is 14px + 4px margin from right
      
      // Position needle between time code and play button
      const timeCodeEndX = promptStartX + cornerLabelWidth + spaceWidth + lessonNumberWidth + spaceWidth + timeCodeWidth + totalTimeWidth;
      const needleX = Math.floor((timeCodeEndX + playButtonX) / 2); // Centered between time code and play button
      
      // Calculate waveform offset so current position aligns with needle
      const waveformOffset = needleX - Math.floor(progress * progressBarWidth);
      
      const bottomY = barY + barHeight; // Bottom line for bottom-up waveform
      const topY = barY; // Top line for top-down waveform

      // Choose colors based on playing state
      const unplayedColor = isPlaying ? [48, 48, 48, 255] : [32, 32, 64, 255]; // Blue tint when paused
      const playedColor = isPlaying ? [64, 96, 64, 255] : [64, 64, 96, 255]; // Purple tint when paused

      // Draw entire waveform with offset - waveform moves, needle stays static
      for (let x = 0; x < progressBarWidth; x++) {
        // Calculate the actual waveform position accounting for offset
        let waveformX = x - waveformOffset;
        
        // Handle wrapping to ensure waveform always fills the screen
        while (waveformX < 0) waveformX += progressBarWidth;
        waveformX = waveformX % progressBarWidth;
        
        // Map waveform position to data array index with proper wrapping
        const normalizedProgress = waveformX / progressBarWidth; // 0 to 1
        const dataIndex = Math.floor(normalizedProgress * waveformData.length) % waveformData.length;
        const amplitude = waveformData[dataIndex] || 0;

        // Simple static color mapping - no rainbow, just amplitude-based brightness
        const baseColor = isPlaying ? [80, 120, 160] : [60, 60, 80]; // Blue when playing, gray when paused
        const amplitudeIntensity = Math.min(1, amplitude / barHeight);
        
        // Modulate brightness based on amplitude only
        const brightness = 0.4 + amplitudeIntensity * 0.6;
        const r = Math.round(baseColor[0] * brightness);
        const g = Math.round(baseColor[1] * brightness);
        const b = Math.round(baseColor[2] * brightness);

        // Simple consistent rendering - just draw vertical lines from center
        const waveHeight = Math.max(1, Math.floor(amplitude));
        const centerY = barY + Math.floor(barHeight / 2);
        const halfHeight = Math.floor(waveHeight / 2);
        
        // Draw single vertical line from center with color
        const lineTop = centerY - halfHeight;
        const lineBottom = centerY + halfHeight;
        
        ink(r, g, b, 255).line(progressBarStartX + x, lineTop, progressBarStartX + x, lineBottom);
      }
      
      // Draw vertical bars at start and end of track to show track boundaries
      const trackStartX = needleX - Math.floor(progress * progressBarWidth);
      const trackEndX = trackStartX + progressBarWidth;
      
      // Start of track marker (left boundary)
      if (trackStartX >= 0 && trackStartX < progressBarWidth) {
        ink(255, 255, 255, 128).line(trackStartX, barY, trackStartX, barY + barHeight);
        ink(255, 255, 255, 64).line(trackStartX + 1, barY, trackStartX + 1, barY + barHeight);
      }
      
      // End of track marker (right boundary)
      if (trackEndX >= 0 && trackEndX < progressBarWidth) {
        ink(255, 255, 255, 128).line(trackEndX, barY, trackEndX, barY + barHeight);
        ink(255, 255, 255, 64).line(trackEndX - 1, barY, trackEndX - 1, barY + barHeight);
      }
    } else {
      // If no waveform data, just show the subtle background - no additional lines needed
    }

    // Draw static needle between time code and play/pause button - always draw this after waveform
    if (progress > 0 && (isPlaying || playbackAttempted)) {
      // Calculate the position for the white needle - between time code and play/pause button
      const glyphWidth = 6; // Each glyph is 6 pixels wide
      const promptStartX = 6; // Prompt starts at x=6
      const cornerLabelText = "prutti"; // The corner label text
      const cornerLabelWidth = cornerLabelText.length * glyphWidth;
      const lessonNumberWidth = lesson.toString().length * glyphWidth;
      const spaceWidth = glyphWidth;
      const timeCodeWidth = 5 * glyphWidth; // "00:00" is 5 characters
      const totalTimeWidth = 7 * glyphWidth; // "/ 00:00" is 7 characters
      const playButtonX = screen.width - 14 - 4; // Play button is 14px + 4px margin from right
      
      // Position needle between time code and play button
      const timeCodeEndX = promptStartX + cornerLabelWidth + spaceWidth + lessonNumberWidth + spaceWidth + timeCodeWidth + totalTimeWidth;
      const needleX = Math.floor((timeCodeEndX + playButtonX) / 2); // Centered between time code and play button
      
      // Draw a bright vertical line as static needle
      const needleAlpha = 255;
      const glowAlpha = 128;
      
      ink(255, 255, 255, needleAlpha) // Bright white needle
        .line(needleX, barY, needleX, barY + barHeight);
      // Add a subtle glow effect
      ink(255, 255, 255, glowAlpha)
        .line(needleX - 1, barY, needleX - 1, barY + barHeight)
        .line(needleX + 1, barY, needleX + 1, barY + barHeight);
    }

    // No visual scrubbing indicators needed - stample-style simplicity

    // Paint the scrub button if it exists
    if (scrubButton) {
      scrubButton.paint(() => {
        // Show different colors based on button state
        if (scrubButton.down) {
          // Draw a subtle highlight when button is pressed
          ink(255, 255, 255, 32).box(scrubButton.box); // Subtle white highlight when pressed
        } else if (scrubButton.over) {
          // Draw a very subtle overlay when hovering
          ink(255, 255, 255, 16).box(scrubButton.box); // Very subtle white overlay when hovering
        }
      });
    }

    // Update HUD label with current time and draw total time on right - only if actually playing
    // (Now integrated into HUD label, but keeping debug logs)
    if (isBuffering && playbackAttempted) {
      // Show "Buffering..." in place of the "/ --:--" text
      
      // Update the HUD label to show just "prutti 1" (no time while buffering)
      if (hud && hud.label) {
        hud.label(`prutti ${lesson + 1}`, undefined, 0);
      }

      // Draw "Buffering..." in the same position as the "/ --:--" text
      const bufferingText = "Buffering...";
      const promptString = `prutti ${lesson + 1} `;
      const promptWidth = promptString.length * glyphWidth;
      const bufferingX = promptStartX + promptWidth;
      const bufferingY = cornerLabelY - 4 - 2;

      // Draw shadow (offset by 1px down and right)
      ink(32, 32, 32, 255); // Dark gray shadow
      write(bufferingText, bufferingX + 1, bufferingY + 1);

      // Draw main text in gray (to match grayed out play button)
      ink(128, 128, 128, 255); // Gray text when buffering
      write(bufferingText, bufferingX, bufferingY);
    } else if (
      actualDuration &&
      progress !== undefined &&
      (isPlaying || playbackAttempted)
    ) {
      const currentTime = progress * actualDuration;

      // Format time as MM:SS
      const formatTime = (seconds) => {
        const mins = Math.floor(seconds / 60);
        const secs = Math.floor(seconds % 60);
        return `${mins}:${secs.toString().padStart(2, "0")}`;
      };

      const currentTimeText = formatTime(currentTime);
      const totalTimeText = formatTime(actualDuration);

      // Update the HUD label to show "prutti 1 0:01" (keeping lesson number)
      // Show real-time updates when scrubbing
      if (hud && hud.label) {
        const labelText = `prutti ${lesson + 1} ${currentTimeText}`;
        hud.label(labelText, undefined, 0);
      }

      // Measure actual prompt string width for better positioning
      const promptString = `prutti ${lesson + 1} `;
      const promptWidth = promptString.length * glyphWidth; // Character width calculation using correct glyph width
      const totalTimeX =
        promptStartX +
        promptWidth +
        currentTimeText.length * glyphWidth +
        glyphWidth; // Position after prompt and current time
      const totalTimeY = cornerLabelY - 4 - 2; // Match corner label y position, moved up 6px

      // Draw shadow (offset by 1px down and right)
      ink(32, 32, 32, 255); // Dark gray shadow
      write(`/ ${totalTimeText}`, totalTimeX + 1, totalTimeY + 1);

      // Draw main text in white - brighter when scrubbing
      const textAlpha = 255;
      ink(255, 255, 255, textAlpha);
      write(`/ ${totalTimeText}`, totalTimeX, totalTimeY);
    } else if (
      requestedTimeString &&
      lessons[lesson].sounds?.length > 0 &&
      (isPlaying || playbackAttempted)
    ) {
      // Show requested time only after playback has been attempted
      const currentTimeText = requestedTimeString;
      const totalTimeText = "-:--"; // Placeholder until duration is known

      // Update the HUD label to show "prutti 1 1:53" (keeping lesson number)
      if (hud && hud.label) {
        hud.label(`prutti ${lesson + 1} ${currentTimeText}`, undefined, 0);
      }

      // Measure actual prompt string width for better positioning
      const promptString = `prutti ${lesson + 1} `;
      const promptWidth = promptString.length * glyphWidth; // Character width calculation using correct glyph width
      const totalTimeX =
        promptStartX +
        promptWidth +
        currentTimeText.length * glyphWidth +
        glyphWidth; // Position after prompt and current time
      const totalTimeY = cornerLabelY - 4 - 2; // Match corner label y position, moved up 6px

      // Draw shadow (offset by 1px down and right)
      ink(32, 32, 32, 255); // Dark gray shadow
      write(`/ ${totalTimeText}`, totalTimeX + 1, totalTimeY + 1);

      // Draw main text in white
      ink(255, 255, 255, 255);
      write(`/ ${totalTimeText}`, totalTimeX, totalTimeY);
    } else if (lessons[lesson].sounds?.length > 0) {
      // Show default time display for lessons with sounds before playback is attempted
      const currentTimeText = "--:--";
      const totalTimeText = "--:--";

      // Update the HUD label to show "prutti 1 --:--" (keeping lesson number)
      if (hud && hud.label) {
        hud.label(`prutti ${lesson + 1} ${currentTimeText}`, undefined, 0);
      }

      // Measure actual prompt string width for better positioning
      const promptString = `prutti ${lesson + 1} `;
      const promptWidth = promptString.length * glyphWidth; // Character width calculation using correct glyph width
      const totalTimeX =
        promptStartX +
        promptWidth +
        currentTimeText.length * glyphWidth +
        glyphWidth; // Position after prompt and current time
      const totalTimeY = cornerLabelY - 4 - 2; // Match corner label y position, moved up 6px

      // Draw shadow (offset by 1px down and right)
      ink(32, 32, 32, 255); // Dark gray shadow
      write(`/ ${totalTimeText}`, totalTimeX + 1, totalTimeY + 1);

      // Draw main text in white
      ink(255, 255, 255, 255);
      write(`/ ${totalTimeText}`, totalTimeX, totalTimeY);
    }
  }

  // Draw vertical scroll bar on the left side (similar to chat.mjs)
  if (scrollMax > 0) {
    const scrollBarX = 0; // Flush left
    const scrollBarWidth = 4; // 4 pixels wide
    const scrollBarTop = lessons[lesson].sounds?.length > 0 ? 22 : 0; // Start at bottom of progress bar or top
    const scrollBarHeight = screen.height - scrollBarTop; // Flush to bottom

    // Draw scroll bar background
    ink(24, 24, 24, 255).box(
      scrollBarX,
      scrollBarTop,
      scrollBarWidth,
      scrollBarHeight,
    );

    // Calculate scroll indicator position and size
    const totalScrollRange = scrollMax + 6; // Total scroll range from -scrollMax to +6
    const viewportRatio = scrollBarHeight / (scrollBarHeight + scrollMax); // Ratio of viewport to total content
    const indicatorHeight = Math.max(
      4,
      Math.floor(scrollBarHeight * viewportRatio),
    );

    // Calculate position: scroll ranges from -scrollMax (top) to +6 (bottom)
    // Reverse the logic so indicator is at top when scroll is at top
    const scrollProgress = (scroll + scrollMax) / totalScrollRange; // 0 = top, 1 = bottom
    const indicatorY =
      scrollBarTop +
      Math.floor((1 - scrollProgress) * (scrollBarHeight - indicatorHeight));

    // Draw scroll indicator
    ink(128, 128, 128, 255).box(
      scrollBarX,
      indicatorY,
      scrollBarWidth,
      indicatorHeight,
    );
  }

  // Calculate scrollMax accounting for progress bar height and ensuring content isn't cut off
  const progressBarHeight = lessons[lesson].sounds?.length > 0 ? 16 : 0;
  const totalContentHeight =
    titleBox.box.height + 10 + textBox.box.height + lastHeight;
  const availableHeight = screen.height - progressBarHeight;
  scrollMax = Math.max(
    0,
    totalContentHeight - availableHeight + 6 + progressBarHeight,
  ); // Ensure last image is 6px from bottom

  // Show buffering indicator in top right corner only if user has attempted playback and audio is loading
  // (Disabled debug logging to reduce console spam)
  // if (isBuffering && playbackAttempted && lessons[lesson].sounds?.length > 0) {
  //   console.log("🐛 Paint DEBUG: Showing buffering indicator (in HUD label)");
  // }

  // Draw pause/play icon in top right corner within progress bar area - always visible if lesson has sounds
  // Drawing this last so it appears on top of all other elements
  if (lessons[lesson].sounds?.length > 0) {
    const iconSize = 14; // Smaller size to fit within progress bar height (22px)
    const iconX = screen.width - iconSize - 4; // 4px margin from right edge
    const iconY = 4; // 4px from top, centered within progress bar area

    if (isPlaying && playingSfx && !playingSfx.killed && !isBuffering) {
      // Draw pause icon - two vertical bars with shadow (only when actually playing and not buffering)
      const barWidth = 2;
      const barHeight = 8;
      const barSpacing = 2;

      // Draw black shadow (1px right and 1px down)
      ink(0, 0, 0, 255);
      ink().box(iconX + 1, iconY + 3 + 1, barWidth, barHeight);
      ink().box(
        iconX + barWidth + barSpacing + 1,
        iconY + 3 + 1,
        barWidth,
        barHeight,
      );

      // Draw pause bars - orange
      ink(255, 165, 0, 255);
      ink().box(iconX, iconY + 3, barWidth, barHeight);
      ink().box(iconX + barWidth + barSpacing, iconY + 3, barWidth, barHeight);
    } else {
      // Draw play icon - triangle pointing right
      const triangleSize = 8;
      const triangleX = iconX + 7; // Center the triangle in the icon area
      const triangleY = iconY + 7;

      // Draw black shadow (1px right and 1px down)
      ink(0, 0, 0, 255);
      shape([
        [triangleX - triangleSize / 2 + 1, triangleY - triangleSize / 2 + 1],
        [triangleX + triangleSize / 2 + 1, triangleY + 1],
        [triangleX - triangleSize / 2 + 1, triangleY + triangleSize / 2 + 1],
      ]);

      // Draw triangle - gray when buffering, yellow when ready
      if (isBuffering) {
        ink(128, 128, 128, 255); // Gray when buffering
      } else {
        ink(255, 255, 0, 255); // Bright yellow when ready
      }
      shape([
        [triangleX - triangleSize / 2, triangleY - triangleSize / 2],
        [triangleX + triangleSize / 2, triangleY],
        [triangleX - triangleSize / 2, triangleY + triangleSize / 2],
      ]);
    }
  }

  // Keep repainting when audio is playing, buffering, or scrubbing so progress bar can update
  if ((isPlaying && playingSfx && !playingSfx.killed) || isBuffering || isScrubbing) {
    return true;
  }
  
  // Don't return false - let normal repainting continue so content stays visible
  // (returning undefined allows the system to handle repainting naturally)
}
*/

// 🎪 Act// 🎪 Act
function act({ event: e, needsPaint, jump, sound, net, screen, api, ui }) {
  // Handle the scrub button if we have sounds for this lesson
  if (scrubButton && lessons[lesson].sounds?.length > 0) {
    scrubButton.act(e, {
      // Touch down on the button
      down: (btn) => {
        console.log("🟣 BUTTON: DOWN");
        scrubOccurred = false; // Reset scrub tracking for this interaction
        
        // Calculate offset between touch position and current playback position
        if (playingSfx && !playingSfx.killed && isPlaying) {
          const touchRelativeX = Math.max(0, Math.min(btn.box.w, e.x - btn.box.x));
          const touchProgress = touchRelativeX / btn.box.w;
          scrubOffset = touchProgress - progress; // Store the difference (flipped for correct direction)
          console.log("🎯 SCRUB OFFSET: touchProgress:", touchProgress.toFixed(4), "currentProgress:", progress.toFixed(4), "offset:", scrubOffset.toFixed(4));
        }
        
        return true;
      },
      
      // Button push (tap) - handle play/pause toggle
      push: (btn) => {
        // If scrubbing occurred, don't handle push - scrubbing should not trigger play/pause
        if (isScrubbing || scrubOccurred) {
          return true;
        }
        
        // Prevent any action when buffering is active
        if (isBuffering) {
          return true;
        }

        if (isPlaying && playingSfx && !playingSfx.killed) {
          // Currently playing - pause
          console.log("🐛 Audio DEBUG: Pausing playback");
          
          // Use current progress as the pause position (accounts for any scrubbing)
          // Stop audio and update position
          playingSfx.kill(0.1);
          playingSfx = null;
          isPlaying = false;
          pausedAt = progress; // Use current progress position
          
          console.log("🐛 Audio DEBUG: Paused at position:", (pausedAt * (actualDuration || 60)).toFixed(2) + "s");
          needsPaint();
        } else if (lessons[lesson].sounds?.length > 0) {
          // Not playing - start normal playback
          
          // Mark that user has attempted playback
          playbackAttempted = true;

          // Load audio if not already loaded
          if (!preloadedAudio) {
            console.log("🎵 PRUTTI: Loading audio on first play...");
            
            loadAudio(api, sound)
              .then(() => {
                console.log("🎵 PRUTTI: Audio loaded, starting normal playback");
                startNormalPlayback();
              })
              .catch((error) => {
                console.error("🎵 PRUTTI: Audio load failed:", error);
                isBuffering = false;
                needsPaint();
              });
            return true;
          }

          // Audio already loaded, start playback directly
          startNormalPlayback();

          function startNormalPlayback() {
            // Kill any existing audio before starting
            if (playingSfx && !playingSfx.killed) {
              playingSfx.kill(0.1);
            }
            playingSfx = null; // Clear reference

            // Set buffering state since we're about to attempt audio playback
            isBuffering = true;
            needsPaint();

            console.log(
              "🐛 Playback DEBUG: About to play audio with from:",
              progress,
            );

            // Use preloaded audio directly - start with normal speed for immediate playback
            playingSfx = sound.play(preloadedAudio, {
              speed: 1.0,
              from: progress,
              loop: true,
              volume: 1,
            });

            console.log(
              "🐛 Playback DEBUG: Audio play() called, playingSfx:",
              playingSfx,
            );

            if (playingSfx && !playingSfx.killed) {
              playStartTime = performance.now();
              pausedAt = progress; // Sync pausedAt with current progress
              isPlaying = true;

              // Get duration for accurate progress tracking
              if (!actualDuration && sound.getDuration) {
                sound
                  .getDuration(preloadedAudio)
                  .then((duration) => {
                    actualDuration = duration;
                    if (isPlaying && playingSfx && !playingSfx.killed) {
                      isBuffering = false;
                      needsPaint();
                    }
                  })
                  .catch(() => {
                    actualDuration = 60;
                    if (isPlaying && playingSfx && !playingSfx.killed) {
                      isBuffering = false;
                      needsPaint();
                    }
                  });
              } else {
                isBuffering = false;
              }

              needsPaint();
            } else {
              console.log("🐛 Playback DEBUG: Audio play() failed, stopping buffering");
              isBuffering = false;
              isPlaying = false;
              needsPaint();
            }
          }
        }
        
        return true;
      },
      
      // Button scrub - direct position mapping with offset (no jumping)
      scrub: (btn) => {
        // Direct position mapping with offset to prevent jumping
        if (playingSfx && !playingSfx.killed && isPlaying) {
          isScrubbing = true; // Pause normal time-based progress updates
          scrubOccurred = true; // Mark that scrubbing occurred during this interaction
          
          // Map current X position within the button to progress (0-1)
          const relativeX = Math.max(0, Math.min(btn.box.w, e.x - btn.box.x));
          const touchProgress = relativeX / btn.box.w;
          
          // Apply the offset to prevent jumping
          let newProgress = touchProgress - scrubOffset;
          
          // Allow wrapping around the track boundaries
          while (newProgress < 0) {
            newProgress += 1.0; // Wrap to end when going past beginning
          }
          while (newProgress > 1.0) {
            newProgress -= 1.0; // Wrap to beginning when going past end
          }
          
          // Update progress position
          progress = newProgress;
          
          needsPaint(); // Trigger visual update
        }
      },
      
      // Button up - restart audio from seeked position
      up: (btn) => {
        if (playingSfx && !playingSfx.killed && isPlaying && isScrubbing) {
          // Kill and restart audio from exact seeked position
          playingSfx.kill(0.01); // Quick fade to avoid click
          
          // Restart from seeked position
          playingSfx = sound.play(preloadedAudio, {
            speed: 1.0,
            from: progress,
            loop: true,
            volume: 1,
          });
          
          if (playingSfx && !playingSfx.killed) {
            playStartTime = performance.now();
            pausedAt = progress;
          }
        }
        
        isScrubbing = false; // Resume normal playback
        
        return true;
      },
      
      // Button cancel - restart audio from seeked position
      cancel: (btn) => {
        if (playingSfx && !playingSfx.killed && isScrubbing) {
          playingSfx.kill(0.01);
          playingSfx = sound.play(preloadedAudio, {
            speed: 1.0,
            from: progress,
            loop: true,
            volume: 1,
          });
          
          if (playingSfx && !playingSfx.killed) {
            playStartTime = performance.now();
            pausedAt = progress;
          }
        }
        
        isScrubbing = false;
      }
    });
  }

  // Handle touch/draw events for page scrolling in the content area (below progress bar)
  if (e.is("draw")) {
    // Check if the touch is in the content area (below progress bar)
    const progressBarHeight = lessons[lesson].sounds?.length > 0 ? 22 : 0;
    
    // Only handle scrolling if the touch is below the progress bar area
    if (e.y >= progressBarHeight) {
      scroll += e.delta.y;
      checkScroll();
      needsPaint();
    }
  }

  // Handle keyboard navigation
  if (e.is("keyboard:down:arrowdown")) {
    scroll -= 10;
    checkScroll();
    needsPaint();
  }

  if (e.is("keyboard:down:arrowup")) {
    scroll += 10;
    checkScroll();
    needsPaint();
  }

  if (e.is("keyboard:down:arrowright")) {
    if (lesson < lessons.length) jump(`prutti ${lesson + 2}`);
  }

  if (e.is("keyboard:down:arrowleft")) {
    if (lesson > 0) jump(`prutti ${lesson - 1 + 1}`);
  }

  // Handle scroll wheel events
  if (e.is("scroll")) {
    scroll -= e.y;
    checkScroll();
    needsPaint();
  }

  // Handle window/screen reframing - recreate scrub button with new dimensions
  if (e.is("reframed")) {
    // Recreate the scrub button if we have sounds for this lesson
    if (lessons[lesson].sounds?.length > 0) {
      const barHeight = 22;
      
      // Calculate positioning based on timecode layout (matching boot function calculations)
      const glyphWidth = 6; // Each glyph is 6 pixels wide (typeface blockWidth)
      const promptStartX = 6; // Prompt starts at x=6
      const cornerLabelText = "prutti"; // The corner label text
      const cornerLabelWidth = cornerLabelText.length * glyphWidth; // Character width
      const lessonNumberWidth = lesson.toString().length * glyphWidth; // Width of lesson number
      const spaceWidth = glyphWidth; // Space character width
      const timeCodeWidth = 5 * glyphWidth; // "00:00" is 5 characters
      const totalTimeWidth = 7 * glyphWidth; // "/ 00:00" is 7 characters
      
      // Calculate where the timecode starts and ends
      const timeCodeStartX = promptStartX + cornerLabelWidth + spaceWidth + lessonNumberWidth + spaceWidth;
      const timeCodeEndX = timeCodeStartX + timeCodeWidth + totalTimeWidth;
      
      // Start the scrub button from the middle of the timecode area
      const timecodeMiddleX = timeCodeStartX + Math.floor((timeCodeWidth + totalTimeWidth) / 2);
      const buttonStartX = timecodeMiddleX; // Start from middle of timecode
      const buttonWidth = screen.width - buttonStartX; // Extend to right edge of screen
      
      // Update existing button dimensions or create new one
      if (scrubButton) {
        scrubButton.box.x = buttonStartX;
        scrubButton.box.y = 0;
        scrubButton.box.w = buttonWidth;
        scrubButton.box.h = barHeight;
        console.log("🟣 BUTTON REFRAMED: Updated timecode-aware position (" + buttonStartX + ", 0) Size:", buttonWidth, "x", barHeight);
      } else {
        // Create new button if it doesn't exist
        scrubButton = new ui.Button(buttonStartX, 0, buttonWidth, barHeight);
        scrubButton.offScreenScrubbing = true;
        scrubButton.noRolloverActivation = false;
        console.log("🟣 BUTTON REFRAMED: Created new timecode-aware button position (" + buttonStartX + ", 0) Size:", buttonWidth, "x", barHeight);
      }
    } else {
      scrubButton = null; // Clear button if no sounds
      console.log("🟣 BUTTON REFRAMED: No button - lesson has no sounds");
    }
    
    needsPaint();
  }
    }

// 🧮 Sim
function sim({ sound }) {
  sound.speaker?.poll();

  // Only update position when not actively scrubbing
  if (isPlaying && playingSfx && !playingSfx.killed && !isScrubbing) {
    const currentTime = performance.now();
    const elapsedSeconds = (currentTime - playStartTime) / 1000;
    
    // Normal playback
    const trackDuration = actualDuration || 60;
    const totalElapsed = pausedAt * trackDuration + elapsedSeconds;
    const newPosition = Math.min(totalElapsed / trackDuration, 1.0);
    
    progress = newPosition;
    
    // Handle looping
    if (newPosition >= 1.0) {
      pausedAt = 0;
      progress = 0;
      playStartTime = performance.now();
    }
  }
}

// 🎵 Load audio on demand when scrub button is tapped
function loadAudio(api, sound) {
  // Check if audio is already loaded (but not currently loading)
  if (preloadedAudio) {
    console.log("🎵 PRUTTI: Audio already loaded - preloadedAudio:", !!preloadedAudio);
    return Promise.resolve();
  }

  // Check if currently loading to prevent double loading
  if (isBuffering) {
    console.log("🎵 PRUTTI: Audio is currently loading - isBuffering:", isBuffering);
    return Promise.resolve();
  }

  // Check if current lesson has audio
  if (!lessons[lesson].sounds?.length) {
    console.log("🎵 PRUTTI: No audio for this lesson");
    return Promise.resolve();
  }

  // Set buffering state
  isBuffering = true;
  api.needsPaint(); // Trigger repaint to show buffering indicator
  console.log("🎵 PRUTTI: Starting audio load...");

  const path = api.debug
    ? "/assets/pruttipal/lnl"
    : "https://assets.aesthetic.computer/pruttipal/lnl";

  const name = lessons[lesson].sounds[0]; // Use first sound for now
  const ext = api.platform.Safari ? "m4a" : "ogg";
  const soundUrl = `${path}/${name}.${ext}`;

  console.log("🎵 PRUTTI: Starting audio load for:", soundUrl);

  return api.net
    .preload(soundUrl)
    .then((sfx) => {
      console.log("🎵 PRUTTI: Preload result:", !!sfx);
      if (sfx) {
        preloadedAudio = sfx; // Store for instant playback
        console.log("🎵 PRUTTI: Audio loaded successfully");

        // Get duration and set start time if requested
        if (sound.getDuration) {
          console.log("🎵 PRUTTI: Getting duration...");
          return sound
            .getDuration(sfx)
            .then((duration) => {
              console.log("🎵 PRUTTI: Got duration:", duration);

              if (duration && startTimeSeconds !== null) {
                // Check if requested start time is within the audio duration
                if (startTimeSeconds < duration) {
                  // Convert seconds to fraction (0-1)
                  const newPausedAt = startTimeSeconds / duration;
                  pausedAt = newPausedAt;
                  progress = newPausedAt; // Set initial progress to match start position
                  console.log(
                    "🕐 Prutti: Start time set to",
                    startTimeSeconds + "s",
                    "(" + (pausedAt * 100).toFixed(1) + "% of track)",
                    "Duration:",
                    duration + "s",
                  );
                } else {
                  console.log(
                    "🕐 Prutti: Requested start time",
                    startTimeSeconds + "s",
                    "is beyond track duration",
                    duration + "s",
                    "- starting from beginning",
                  );
                  pausedAt = 0;
                  progress = 0;
                }
              } else if (startTimeSeconds === null) {
                // No start time requested - only reset to beginning if this is the first load
                // (maintain current position for subsequent play/pause cycles like stample.mjs)
                if (pausedAt === null || pausedAt === undefined) {
                  pausedAt = 0;
                  progress = 0;
                  console.log(
                    "🕐 Prutti: No start time requested, starting from beginning (first load)",
                  );
                } else {
                  console.log(
                    "🕐 Prutti: No start time requested, maintaining current position:",
                    pausedAt,
                  );
                }
              }
              // Clear the requested time string now that we have the actual duration
              requestedTimeString = null;
              actualDuration = duration;
              api.needsPaint(); // Trigger repaint to update display
            })
            .catch((error) => {
              console.error("🎵 PRUTTI: Failed to get duration:", error);
              // If we can't get duration but have a start time, ignore it and start from beginning
              if (startTimeSeconds !== null) {
                console.log(
                  "🕐 Prutti: Could not verify duration - starting from beginning",
                );
                pausedAt = 0;
                progress = 0;
              } else {
                // No start time requested - only reset to beginning if this is the first load
                // (maintain current position for subsequent play/pause cycles like stample.mjs)
                if (pausedAt === null || pausedAt === undefined) {
                  pausedAt = 0;
                  progress = 0;
                  console.log(
                    "🕐 Prutti: Error getting duration, starting from beginning (first load)",
                  );
                } else {
                  console.log(
                    "🕐 Prutti: Error getting duration, maintaining current position:",
                    pausedAt,
                  );
                }
              }
              // Clear the requested time string on error too
              requestedTimeString = null;
              actualDuration = 60; // Fallback duration
              api.needsPaint(); // Trigger repaint to update display
              return Promise.resolve(); // Make sure the promise chain continues
            });
        } else {
          // No getDuration method available - only reset to beginning if this is the first load
          // (maintain current position for subsequent play/pause cycles like stample.mjs)
          console.log(
            "🎵 PRUTTI: sound.getDuration method not available",
          );
          if (pausedAt === null || pausedAt === undefined) {
            pausedAt = 0;
            progress = 0;
            console.log(
              "🕐 Prutti: No getDuration method, starting from beginning (first load)",
            );
          } else {
            console.log(
              "🕐 Prutti: No getDuration method, maintaining current position:",
              pausedAt,
            );
          }
          requestedTimeString = null;
          actualDuration = 60; // Fallback duration
          return Promise.resolve();
        }
      } else {
        console.error(
          "🎵 PRUTTI: Preload returned null/undefined for:",
          soundUrl,
        );
        return Promise.reject(new Error("Audio preload failed"));
      }
    })
    .catch((preloadError) => {
      console.error("🎵 PRUTTI: Preload failed:", preloadError);
      throw preloadError; // Re-throw to continue down the chain
    })
    .then(() => {
      // Load waveform data for progress bar visualization
      if (sound.getSampleData && preloadedAudio) {
        console.log("🎵 PRUTTI: Loading waveform data...");
        return sound
          .getSampleData(preloadedAudio)
          .then((data) => {
            if (data) {
              // Compress to higher resolution for more detailed waveform
              const targetWidth = 1024; // Higher resolution for more detail
              const compressedData = api.num.arrCompress(data, targetWidth);

              // Normalize the waveform data to fit the progress bar height (24px)
              const maxValue = Math.max(...compressedData);
              const minValue = Math.min(...compressedData);
              const range = maxValue - minValue;

              if (range > 0) {
                // Normalize to 0-1 range, then scale to progress bar height
                const progressBarHeight = 24; // Match the bar height
                const maxWaveHeight = progressBarHeight / 2; // Use half the bar height for waves

                waveformData = compressedData.map(
                  (value) => ((value - minValue) / range) * maxWaveHeight,
                );
              } else {
                // If all values are the same, create a flat line
                waveformData = new Array(compressedData.length).fill(1);
              }

              console.log("🎵 PRUTTI: Waveform data loaded");
              api.needsPaint(); // Trigger repaint to show waveform
            } else {
              console.log("🎵 PRUTTI: No waveform data available");
              api.needsPaint(); // Trigger repaint anyway
            }
          })
          .catch((error) => {
            console.error(
              "🎵 PRUTTI: Failed to load waveform data:",
              error,
            );
            api.needsPaint(); // Trigger repaint anyway
          });
      } else {
        console.log(
          "🎵 PRUTTI: No getSampleData method, skipping waveform",
        );
        return Promise.resolve();
      }
    })
    .then(() => {
      // Clear buffering state when everything is loaded
      isBuffering = false;
      console.log("🎵 PRUTTI: Audio loading complete - isBuffering set to false");
      api.needsPaint(); // Trigger repaint to hide buffering indicator
    })
    .catch((error) => {
      console.error("🎵 PRUTTI ERROR: Audio loading failed:", error);
      isBuffering = false; // Stop buffering on error
      api.needsPaint(); // Trigger repaint to hide buffering indicator
      throw error; // Re-throw for caller to handle
    });
}

// Load lesson images
function loadLesson(api, sound) {
  // Load lesson images
  const path = api.debug
    ? "/assets/pruttipal/lnl"
    : "https://assets.aesthetic.computer/pruttipal/lnl";

  lessonPaintings.length = 0; // Clear previous paintings
  lessonPaintingsCache.clear(); // Clear cached dimensions for new lesson
  preRenderedPaintings.clear(); // Clear pre-rendered paintings for new lesson
  
  lessons[lesson].pictures?.forEach((pic, index) => {
    const picUrl = `${path}/${pic}.jpeg`;
    
    api.net.preload(picUrl).then((img) => {
      if (img) {
        lessonPaintings[index] = img;
        api.needsPaint();
      } else {
        console.error("❌ PRUTTI: Image", index + 1, "failed to load (returned null/undefined)");
      }
    }).catch((error) => {
      console.error("❌ PRUTTI: Image", index + 1, "loading error:", error);
    });
  });

  // Calculate scroll limits
  scrollMax = lessonPaintings.length * (picWidth + 10) + 100; // Approximate
}

// Function to create pre-rendered paintings for faster rendering
function createPreRenderedPainting(index, targetWidth, cachedInfo, api, painting) {
  try {
    const preRenderKey = `${index}-${targetWidth}`;
    
    // Don't re-create if already exists
    if (preRenderedPaintings.has(preRenderKey)) {
      return;
    }
    
    // Try different ways to access the painting function
    let paintingFunction = null;
    if (typeof painting === 'function') {
      paintingFunction = painting;
    } else if (api && typeof api.painting === 'function') {
      paintingFunction = api.painting;
    } else {
      console.error("❌ PRUTTI: Could not find painting function");
      return;
    }
    
    // Create a new painting with the scaled dimensions
    const preRenderedPainting = paintingFunction(targetWidth, Math.floor(cachedInfo.scaledHeight), (p) => {
      // Render the scaled image to the new painting using the callback pattern
      p.paste(cachedInfo.imageObj, 0, 0, {
        width: targetWidth,
        height: cachedInfo.scaledHeight,
      });
    });
    
    if (!preRenderedPainting) {
      console.error("❌ PRUTTI: Failed to create painting object");
      return;
    }
    
    // Cache the pre-rendered painting
    preRenderedPaintings.set(preRenderKey, {
      painting: preRenderedPainting,
      width: targetWidth,
      height: Math.floor(cachedInfo.scaledHeight)
    });
    
    // Trigger a repaint to use the new pre-rendered version
    api.needsPaint();
    
  } catch (error) {
    console.error("❌ PRUTTI: Failed to pre-render painting", index + 1, ":", error);
  }
}

// Check scroll boundaries
function checkScroll() {
  // Get screen height for scroll limit calculations
  const screenHeight = 256; // Default fallback
  
  // Calculate content height based on current lesson
  const { title, text } = lessons[lesson];
  const contentHeight = title.length + text.length + lessonPaintings.length * 200; // Approximate
  
  // Set scroll limits
  const maxScroll = Math.max(0, contentHeight - screenHeight + 100);
  
  if (scroll > 0) scroll = 0;
  if (scroll < -maxScroll) scroll = -maxScroll;
}

export { boot, sim, paint, act };
