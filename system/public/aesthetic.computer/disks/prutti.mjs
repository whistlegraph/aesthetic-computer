// Prutti, 2023.11.09.16.36.06.537
// Lessons NOT Learnt by pruttipal aka goodiepal

/* #region 📚 README
5
7
8
11
13
14
20
26
28
31
39
#endregion */

/* #region 🏁 TODO 
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
    title: `Lesson NOT Learnt 01 - The failure of academic identity politics`,
    text: `
    // Right outside the house were I live, > a Celtic churce-settlement has been found only a #few years ago#—§ This changes the whole idea of Faroish identity completly as most modern - inovativ self-brand-building youngsters on the island's now ses a opotunity to make up a new history around themself as: Ancient Celts and by that pushing aside, the Sagas of the Icelanders, there otherwise has been the concept of real Nordic - national - IDEAS OF HOME - now the most clever of us are Celtic and hates on Norway and Black Metal etc. - But this is down-right a fantasy - as most people here is a >>-bio-dna-product-<< of sailors and shipping trafic routes… 'nuff said here.

    Most identity politics is based on getting a place at the power and capitalist-table on the expense of any scientific or historical facts - YES - This is wonderfull and I can only bee-sing it and enjoy as I myself has changed my acent, history, origin etc. — many a time, in order to fit in and make myself intereting and doing so in a way there was playing whit fact the same way as the already mentioned the Saga's. (((Were is The Goodiepal from etc.)))

    ______Just as K-omputer hacking was inteligent and realativly harmless to begin whit,  it is now a days turned into only being about getting money out of old and lesser informed on-liner's - The Academic identity hacking and story building people use today could be said to share the same faith ((—- maybe?? —)) At least almost all Scandinavien academics only research in fields they are payed in Capital to researh and people in the needs of power ar desperate to build new world views and history's to fit into … just look at teh Balkan changes over the last 30 years for another example - and I guess it is more or less the same anywere else… Butthen agian  this is only some Celtic fantasy thought's - they have no relevance now, but give them a few years…
    `,
    pictures: ["lnl1"],
  },
  {
    title: "Lesson NOT Learnt 02 - Hang whit the wrong",
    text: `
    Let's go and in ART find the queer·ness of the left-wing >>>> and mash it ..into.. the outsider art of the modern right-wing ___ Here we could include all the social free thinkers dreams like: modular synth's, bright-colours etc. — & —gently blendt it whit things from the right'ers - that could includ re·li·gious heart-to-brush painting technics & nordic sing-a-song writer traditions etc.  — Do not hold back, but please go give it a try - § § § __ The hopefull conclution should be that, the so called right- and left-wing talk is something from the old France Revolution, that is of today, in no way, a good illustrative way to descrip the strugles and political mind-sets the youth is trying to navigate in…

    __ The workers on a Norwigien 'oil-platform' has a much better idea of what they everyday work destory's, then let's say a Ethereum-Space Karen-Dogecoin-investor, happy in the Berlin Cryto-Capital section of the ART-market, if a Rough-Neck on the Rig sings a song, please listen to it before you judge that person - there is always some things to learn,  don't just doubt ur vibe…

    If you are a Hawaii'en native working on: the Pituffik Space Base and is on leave in Thorshavn on the Faroe Islands, your ART— %% even if you call it: “just a hobby” %% might tell a intereting story about: in·dig·e·nous'nes and enviromental destruction — even thow that you are in the uniform…

    in ART so in life - trust your heart - not what your phone tells you... The only exception here is phone-ART...

    https://www.airforce-technology.com/wp-content/uploads/sites/6/2019/06/US-Air-Force-1.jpg

    Cryptic: KovBoyFilm Ref. 

    https://www.youtube.com/watch?v=GhIhJ8w4e2U
    `,
    pictures: ["lnl2-1", "lnl2-2"],
  },
  {
    title:
      "Lesson NOT Learnt 03 - Purano Sei Diner Kothis & The Guild of the Viking Klokkers",
    text: `
    I only like words in songs and not _general sytax—> as it is the destroy'er of EMO ideas. << For: Bananskolen I tryed NOT to be: Karin Hansson & Åse Andersson's: _TED (Beta 0.3)__ but was / is still iinternally in the gruppe sean as that person (& as I become of age, I really DO hate being in focus - - - -belive it or not), - - - - But all this brings something interesting to the table, a lot of current intellectualism and Academic discussion’s is actually not about the subject but more about hove the gruppe feels internaly.

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
    `,
    pictures: ["lnl3"],
  },
  {
    title: "Lesson NOT Learnt 04",
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
    `,
    pictures: ["lnl4"],
  },
  {
    title: "Lesson NOT Learnt 05",
    text: `
    Karlheinz - Fucking (Vinyl Rip) 
    we painted red paint on politikens boghal————————————————
    most of foods is dropped in the garbage disposal - why do you not dumpster dive
    try only to eat free food

    Mike Burrows, just joined the Guild the Viking Klokkers
    ————————————————
    `,
    pictures: ["lnl5"],
  },
  {
    title:
      "Lesson NOT Learnt 06 *Silence is the power of any overclass, anywere*",
    text: `
    & Fansy political ideas is all the same - just power structures waiting to be put into place... — well educated Euro's who jumps on a fashionable idea.. and talk to a lower class in order to highen them to another >>mille plateaux<<- But fails to interact whit the class whit's way of life the well educated Euro's certainly can't stand.. Just Like the fishers living in Vorupør and the bikers who en·gaged in a fight of 300 people around 30 years ago in that little town -  and the white institutional overclass only had silence as a comment - since silence is the way that intelectualisem deals whit ideas it dos not like... = silence is wrong & conceptual high modernism was a dead - NO it is / was not!
    `,
    pictures: ["lnl6"],
  },
  {
    title: "Lesson NOT Learnt 07",
    text: `
    ———————————————
    let's stretches the concept if life <Power>
    alone = is golden - youth = is wrong - age = is wrong - time = is a joke
    <Ghost>
    call me up as: We are the Viking KLOKKERS—
    <Rainbow>
    ———————————————
    `,
    pictures: ["lnl7"],
  },
  {
    title: "Lesson NOT Learnt 08 - w/ 02 variation",
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
    `,
    pictures: ["lnl8"],
  },
  {
    title: "Lesson NOT Learnt 09",
    text: `
    The modern art-critic of today, has huge problems acepting - reversed signals as a comunication form…'''
    $$$ But no Euro-industiral Culture - could ever had existed whitout possing in Euro facist-left owers and in that way critisising the modern, the new & the global world there was to come…

    Dungeon & Draogns cost-play and so called gay culture was pre-industrial framworks, lets line it up:

    Dungeon & Draogns cost-play and (gay) culture > Industrial - revesed signals culture > Cyber Punk > Internet > Tekno  > Global Village Dreams > Social Media & web 2.0  > The curret Politcal Art-scene…

    and that current art-scene would never accept a man·ly Dungeon & Dragons cost-play picture like this:
    `,
    pictures: ["lnl9"],
  },
  {
    title: "Lesson NOT Learnt 10",
    text: `
    When we come to your shores, people start to  see pyramids everywere…
    = The reaosn being that, we bring the paralel universiterium
    and the best place to leave ideas for the time to come is in pryramidiums

    We are the Viking Klokkers have trust a old goodiepal'ium
    `,
    pictures: ["lnl10"],
  },
  {
    title: "Lesson NOT Learnt 11 - what punk is not about.m4a",
    text: `[listen 2 the audio...]`,
    sounds: ["what punk is not about"],
  },
  {
    title: "Lesson NOT Learnt 12",
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
    `,
    pictures: ["lnl12"],
  },
  {
    title: "Lesson NOT Learnt 13",
    text: `
    .. clocks and music and life - is my speciality ..

    >ON rainy days - ( like most days out here)  - ROTOR's is all that I do, _ I'am definitely one of the most adventure's ROTOR make's around - & I puts great pride into this little craft - I mean I do rep. the guild OF THE VIKING KLOKKER'S after all, right? —- Let's just make thing's there is around us better, that's all that I would like to share whit my daughter and the people around me …

    >>> HAANDSKULPTUR…HAANDSKULPTUR..HAANDSKULPTUR.HAANDSKULPTR >>>

    N.B. now a days I hardly sign any of creation's — as I do think that my work should do the talk-ing & I have never really been a artist, in the first place -  I'am more like a UPdate'er - I take things and makes them better…

    If you stay around me just for a little while, things in your life improves drastically —YES - it is true, it is a mystical (also to me) gift that  i inherited when i was very small - - — in watche's people always talk about, unique thing's — but you, just as what I create is a PIECE UNIQUE 

    0 - ROTOR's —0 ROTOR's - 0 - ROTOR's - - 0 - ROTOR's - 0
    `,
    pictures: ["lnl13-1", "lnl13-2", "lnl13-3"],
  },
  {
    title: "Lesson NOT Learnt 14 - Distribution",
    text: `
    Do You Want To Know A Sick-Rat << TORA DALSENG is one of the BEST >> 
    yes yes - we have done some kissing in the distant past, but that is of the point, as I would KISS the whole world if it ever gave me the change!

    'NOW' better then the 'BEST' is: distribution,  most fine-loveley-fumbling art-works, first come's to life when they are distributed … ### Some individual's produce to much and distribute to little ### - If it was not for other people's story-telling of my craft's and life, my work would only be one and the same as “various crap-forms of old manliness”= not something the world would fined particularly HOT——— SO——— YOU, yes all of YOU distributer's continue to make me shine, by altering my output — Danke Schoen!
    -THE DALSENG-
    NOW back to: ms. DALSENG = When you meet the BEST, you should always UPDATE, whit-out destroy'ing the work, —— & by doing so taking the art and the life of the artist into your heart - - - You know that people actually did this for the first 3 - 4 seconds of social media web—2.0 before the distribution of ideas - was eschewed and replaced w. self-promotion & identity bla. bla. bla.
    I will try to still be all about distro, — like Atlantic- I got them works flyin- '-aesthetic computer'ly-' -cross the Atlantic (Woo!)  & -00- LOOK the DALSENG-work  is though it may seem strange, even better now, but no NOT for me flagging it up, no no = simply for context-flipping it - embrace: Stanley Brouwn
    `,
    pictures: ["lnl14-1", "lnl14-2", "lnl14-3"],
  },
  {
    title: "Lesson NOT Learnt 15 - The Drug of WAR destroys EVERYTHING!",
    text: `
    [under the drugs belvie me.m4a]

    §§ Ohh Prutti, §§ Ohh Pruttipal - this is the Kitchi-Man >' Vladimir Obradovic ' … The picture is a 1990's sel-fi - from when he was young & full of MTV-music knowledge & dreams… — —- §§ ohh young prutti, ohh young prutti  §§ > the Yugoslav Wars was hard on the Kitchi-Man, he never fully recovered - - the WAR-drug destroy's EVERYTHING, war-info-drugs destroy's all there is good - but I will carry Vladimir, in my heart and will forever sing his unwritten & unfinished songs - no further info is needed there...
    `,
    pictures: ["lnl15"],
    sounds: ["under the drugs belvie me"],
  },
  {
    title: "Lesson NOT Learnt 16 * a PAST not remembered *",
    text: `
    .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. 
    o0OoO0o Owls & BLACK fanthom helicopters = as replacement memory, Damascus steel & viking hands - for the viking times, when it is just around viking o’ klok — the guild of the Viking klo0OoO0okkers
    .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. 
    `,
    pictures: ["lnl16-1", "lnl16-2"],
  },
  {
    title: "Lesson NOT Learnt 17 - Das real IRDIAL's",
    text: `
    Most Euro-crafts is about: avoiding nature - but let me tell you I was painting one of my bicykle's the other day in: Velbastaður - I was using some wild & crazy chemical-paint that I had mixed from leftovers found at an abandoned construction site, and the rain was pouring down & interacting w. the paint - the result was so so so beautiful -
    
    I enjoy the act of painting bicykle's in the rain - BUT now > back to watch-making ... take a look at the unfinished experiments w. Damascus dial's here // - let nature - o0OoO0o rpl-memory and black phantom helicopters o0OoO0o - take part in your craft-making >--- what comes from the outside is often much better, then what come's from the inside - the outside world is THE real inside 'YES' & this unfinished dial's are the real irdial's - Me·chan·ics and interruption from nature is what's usually missing in the digital art's --- NOW go get a skeletonized pocket watch my friend, - - - they come fancy and they come cheap, BUT get 1 and start to think of civilization vs. nature - until you fall asleep…
    `,
    pictures: ["lnl17-1", "lnl17-2"],
  },
  {
    title: "Lesson NOT Learnt 18 __Euro-Bell__",
    text: `
    ————…………….
    Would you like to hear a a real electro-akustic-contemprary bell struck? - § made by my music-friend: V-Lars - Villads von Possel ?? §
    #WELL# here you go - IN - life & music I hardly, see myself as taking part anymore, I am just the one who is ringing the BELL… YES the same right now I just start and maintain the output of in this case the musical action . .  bla. BLA. bla. BLA. bla. 
    ————
    Ohh do you know the reason for this tintinnabulation - ? ? ? - §IT IS because§ —Stanley Brouwn— just joined: the GUILD of the viking KLOKKER's
    ………………§……….§……….§……….§:……..§:………………………………..

    [v-lars.m4a]
    `,
    pictures: ["lnl18"],
    sounds: ["v-lars"],
  },
  {
    title: "Lesson NOT Learnt 19 - 50 years - Energy & why Aesthetic.computer?",
    text: `
    [robotic.m4a]
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
    text: `
    §————————————————§
    …Some Classic Pruttipal -/- Goodiepal Talk…
    §————————————————§
    000 §§§ — if you do not have a house, then never should you have more things then, what you can store-away unnoticed in a middle-sized Euro-art-gallery -  
    Today SO much SPACE is used in handling and storing art - & Since the cost of living is getting higher and higher in most places - t.t.to hide your belongings among museum & gallery-things might not be a bad idea… — §§§ 000
    `,
    pictures: ["lnl20"],
  },
  {
    title: `Lesson NOT Learnt 21`,
    text: `
    $$——————————————————$$$$
    Best background music for boring lecture videos
    _______—______ peak mandolin ———§§$$$$
    ingen tid - Ingen saaar 
    WARNING - to not listen to this mu-sick, as it is a NO GO!

    [ingen tid - ingen mandolin.m4a]
    `,
    pictures: ["lnl21"],
    sounds: ["ingen tid - ingen mandolin"],
  },
  {
    title: `Lesson NOT Learnt 22`,
    text: `
    ——————————————————————————————————
    RAP-music & Roll-play Games
    ——————————————————————————————————
    good song, good for you… 
    ——————————————————————————————————
    [ingen tid - ingen saaar.m4a]
    `,
    pictures: ["lnl22-1", "lnl22-2", "lnl22-3"],
    sounds: ["ingen tid - ingen saaar"],
  },
  {
    title: `Lesson NOT Learnt 23`,
    text: `
    ——————————————————————————————————
    Mandolin Cafe
    ——————————————————————————————————
    Welcome - please do not listen…

    [life is mandolin.m4a]
    `,
    pictures: ["lnl23"],
    sounds: ["life is mandolin"],
  },
  {
    title: `Lesson NOT Learnt 24`,
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
    title: `Lesson NOT Learnt 26 - Pause`,
    text: `
    ————————————— 
    '//**Clip Talk**//'' 2 
    —<HERE you will have no audio - for this 2 wonders of this world>— - The %%world is full of good things… NO music is good - Good music is NO - often I find myself whit so much love inside, that even my space-friends can not hold me back .. & why should They ??? - you are my space friend - i'am your time friend & time never really existed in the first place %%
    `,
    pictures: ["lnl26-1", "lnl26-2"],
  },
  {
    title: `Lesson NOT Learnt 27`,
    text: `
    ———————————————
    ANGEL's mandolin-ium bum BuM
    [angel mandolinur.m4a]
    `,
    pictures: ["lnl27"],
    sounds: ["angel mandolinur"],
  },
  {
    title: `Lesson NOT Learnt 28`,
    text: `
    ———————————————-
    ANGEL EYES for angel's in the sky & angel's under the sea. . .
    [angel eyes.m4a]
    `,
    pictures: ["lnl28"],
    sounds: ["angel eyes"],
  },
  {
    title: `Lesson NOT Learnt 29`,
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
    ^'?^?^'?^'?^?^'?^'?^?^'?^'?^?^'?^'?^?^'?^'?^?^'?^'?^?^'?^'?^?^'?

    [new recording 16.m4a]
    `,
    pictures: ["lnl29"],
    sounds: ["new recording 16"],
  },
  {
    title: `Lesson NOT Learnt 30`,
    text: `
    ———————————————-
    L.O.V.E. RAP-rock : §§§—LOVE-YOU-POP—
    ^'?^?^'?^'?^?^'?^'?^?^'?^'?^?^'?^'?^?'?^'?^?^'?^'?^?^'?^'?^?^'?^'?^?^'?^'?^?^'?^'?^?^'?^'?^?^'?^'?^?^'?^'?^?^'?^'?^?^'?^'?^?^'?….:::::..:::……..
    En*Kat*Kat*Kat —som— er en*Kat*Kat*Kat —som—er en*Kat*Kat*Kat —som— er en* ——WAW—— Mandolin'er version
    ^'?^?^'?^'?^?^'?^'?^?^'?^'?^?^'?^'?^?^'?^'?^?^'?^'?^?^'?^'?^?^'?^'?^?^'?^'?^?^'?^'?^?^'?^'?^?^'?^'?^?^'?^'?^?^'?^'?^?^'?^'?^?^'?^'?^?^'?^'?^?^'?'
  
    [i love you kat - mandolin.m4a]
    `,
    pictures: ["lnl30-1", "lnl30-2"],
    sounds: ["i love you kat - mandolin"],
  },
  {
    title: `Lesson NOT Learnt 31`,
    text: `
    ———————————————-
    LOVE You dad
    Original Version - B-tar = very good…
    !—!—^*^_!0^0_*0—!—^*^!_!0^0_!*0_!
    *_:__*_:__*_:__*_:__*_:__*_:__*_:__*_:__*_:__*_:__*_:__*_:__*_:
    I still travel to places and record music with the young ones, to learn about passion-living art's-&-tech - people think that I make this super advanced music or musical-ideas, but in reality- I just help-out, listen-in and try to make things better-  whatever that means... since I was young and until today I have alway's just done so, because even after all this years i am still just: Pruttipal - 
    *_:__*_:__*_:__*_:__*_:__*_:__*_:__*_:__*_:__*_:__*_:__*_:__*_:

    [i love you pop.m4a]
    `,
    pictures: ["lnl31-1", "lnl31-2"],
    sounds: ["i love you pop"],
  },
  {
    title: `Lesson NOT Learnt 32`,
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
    title: `Lesson NOT Learnt 33 - Should I make some pornography?`,
    text: `
    In cold country's you will typically find coloured poles by the side of the road %%% - * These poles are typically used as markers for: - snow-plow-drivers- * to follow during snowstorms. They help the drivers stay on the road and avoid going too far to the side, where they might hit lets say a ditch or a tree… Additionally, ON-top they can also be used as guides for drivers in low visibility conditions. // //
    —bla. bla. bla. bla. bla. bla. bla.—
    now last-early evening I was going on my bicikel, downtown Torshavn, and there behind a interactive-Bus-info-map, was 3 lusty road poles engaing in xxx-activities, and I was just thinking that at my AGE, there is Not much to be seen, or for some poeple to much to be seen? ? — But maybe I should make some content, looking at a BEAR-like me, would be for the few, YES only for the people with the most distinct and perverted sexual DREAMS, — AND there should be something for them as well, ROAD POLES and OLD BEAR's - uhhh so perverted you are Pruttipal'ur… - WELL you just hit START in your head and, you will never come back…
    ———————————————
    `,
    pictures: ["lnl33"],
  },
  {
    title: "Lesson NOT Learnt 34 - Loin-El Glitchie-Banan",
    text: `
    WOW *** Glitch today has started to be  ——enjoyed as some fancy wine uhh uhh - and ALL the NFT losers tell funny refs. to the subject ! — NFT's was a BIG JOKE - do not NFT - BUT but let me share with you the real deal… yOu see ^^Real Glitch* takes the OUTput of malfunctioning technology and crafts it into adventurous new things & bla. bla. bla. bla. or so they say -%%%%% BUT — NO o no NO no -that is not the whole story, you need to live the Glitchie life - §§§§ YES Squelchy & glitshiiii §§§§ - In order to understand it - !!§§§ and some of the best to do so has been my heart-friend's:- Yaloopop, Rosa Menkman & Legacy Russell - there you have words and lives to dive into -WARNING - could be A bumpy ride…but output =  FANTASTIC . . ._:___ Now a days Computer people in the U.S. has a lot to say about acting Glitchie, in gender - and life and YES YES —But —People in: New York S.F. & the tech-valley took years and years to understand us Glitch'ers  ——   :_*:_*:_:*:*: — Well I guess if you are interested in function & composing it takes you years and years to learn to: DE-compose - and DE-function - now Rosa Menkman was very good at that especially in life - she is fantasic you should go and experience hear work . …&”/!&”!/“&! “&/“  All my Beo_Bio_Lystrup Syre-tapes & DAT*s & Mini-disc's was send to mailing lists in Japan -that is hove I got not know wUnders as  Toshiyuki Kobayashi - Timepoco - OHHH those recordings - they were all different renditions of my Glitchy Lystrup set-up . __ 

    Glitch Feminism §§ init ding §§ Glitch Studies Manifesto - || beyond resolution §§ Ten Minute Painting & maybe just maybe my lanscape paintings in my book: El Camino del Hardcore  rejsen TIL nordens INDRE! §§ ohh and everything  by: Yaloopop §§ — $$ N.B. When the academy's joined the fun was over real Glitchie's should never go for the PHD!

    [nice dvvvvice and glitch.m4a]
    `,
    pictures: ["lnl34"],
    sounds: ["nice dvvvvice and glitch"],
  },
  {
    title: `Lesson NOT Learnt 35`,
    text: `
    …. & raD & Mandolin'ur

    V54ttur, Nissar, Dv54rgir, Viking'ur, Picts ——— V54ttur, Nissar, Dv54rgir, Viking'ur, Picts ——— V54ttur, Nissar, Dv54rgir, Viking'ur, Picts  

    <<< Pict's …. & raD >>> <<< …. & raD >>> <<< Pict's …. & raD >>> <<< …. & raD >>> <<< …. & raD >>> <<< …. & raD >>> <<< Pict's …. & raD >>> 

    Sometimes it's just too hard to work in music-archaeology, YOU  are up at libraries & in the field day and night - YOU find things that is not listed anywhere, & if you are good as the Goodiepal / Pruttipal - you try to bring it back into life, because you know that only true bringing you finds on the road - people will unlock a interest in your most §§§cryptik and obscure artifacts§§§
    - NOW - Listen 
    THis is: def - N- RAD - an  VERY VERY early Pictish rap-duo-from the orkney islands - later, much much later one of the members would move to mainland Europe and become SUPER happening in the art's of Wien & München  the name was: MISS le BOMB — 

    The Pict's had it all and still has it & this is why you should care and research this fantastic duo…

    BIG BIG big LOVE TO the Orkney Islands for always being a safe-harbor for the pruttipal - …. & raD …. & raD …. & raD …. & raD …. & raD …. & raD …. & raD 
    V54ttur, Nissar, Dv54rgir, Viking'ur, Picts ——— V54ttur, Nissar, Dv54rgir, Viking'ur, Picts ——— V54ttur, Nissar, Dv54rgir, Viking'ur, Picts  

    <<< …. & raD >>> <<< Pict's …. & raD >>> <<< …. & raD >>> <<< …. & raD >>> <<<  Pict's …. & raD >>> <<< …. & raD >>> <<< Pict's …. & raD >>> 

    [pictish_dsc_str.m4a]
    `,
    pictures: ["lnl35-1", "lnl35-2", "lnl35-3"],
    sounds: ["pictish_dsc_str"],
  },
  {
    title: `Lesson NOT Learnt 36`,
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

    [snakkende_shadow_d0nni.m4a]
    `,
    pictures: ["lnl36-1", "lnl36-2"],
    sounds: ["snakkende_shadow_d0nni"],
  },
  {
    title: "Lesson NOT Learnt 37",
    text: `
    more on distro…

    take notes of almost everything… — but not all… 
    Trust: din Nisse - Trust: THE GUILD OF THE VIKIGN klokker's

    [09_distribution-manfler.m4a]
    `,
    pictures: ["lnl37"],
    sounds: ["09_distribution-manfler"],
  },
  {
    title: "Lesson NOT Learnt 38",
    text: `
    Hygge (Injected With a Poison)

    V-Lars-V-Lars-V-Lars-V-Lars-V-Lars-V-Lars-V-Lars-V-Lars-V-Lars-V-Lars-V-Lars-V-Lars-V-Lars

    My old old ancient friends of Cholchester, you want that V-lars London feeling - you wonna have that god old V-Lars - TIMMERs<>Drunkard feeling. §§§ Well also if you are living somewhere else you probably still wonna have the same feeling????  

    —THE time is here: 'Kick'Back'For'The'HYGGE and make the sounds from the Faro islands bring you here - and back to Colchester.

    >>>21th of January = Save this mystery date<<< 50 years old and stil living off the grid. * No Mortgage, No Utilities * ((no daughter (sadly) ))

    V-Lars-V-Lars-V-Lars-V-Lars-V-Lars-V-Lars-V-Lars-V-Lars-V-Lars-V-Lars-V-Lars-V-Lars-V-Lars
   
    [dsj.m4a]
    `,
    pictures: ["lnl38-1", "lnl38-2"],
    sounds: ["dsj"],
  },
  {
    title: "Lesson NOT Learnt 39 - Cosmic Entities of Pure Energy",
    text: `
    §_§ MAgisKE nisser hates WAR - The Viking Klokker's hates WAR & is down w. monsters, vættur & d.d.dragons = WAR_is_shit_!

    - It takes 5 UFO's to construct an: ANGEL…

    it takes 9 GOODIEBAGS to form 9 demonbags §_§_
    -come, come & join the guild of the viking klokkers!
    `,
    pictures: ["lnl39-1", "lnl39-2", "lnl39-3"],
  },
  {
    title: "Lesson NOT Learnt 40",
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

    [rider_G_las_prut_slip.m4a]
    `,
    pictures: ["lnl40-1", "lnl40-2"],
    sounds: ["rider_G_las_prut_slip"],
  },
];

let lesson = 0,
  scroll = 0,
  scrollMax;
const lessonPaintings = [],
  picWidth = 320;
let color,
  noiseTint = [255, 0, 0];

// 🥾 Boot
function boot({ api, wipe, params, hud, help, num }) {
  wipe(0);
  noiseTint = num.randIntArr(255, 3);
  lesson = params[0] ? parseInt(params[0]) - 1 : num.randInd(lessons);
  if (lesson > lessons.length - 1 || lesson < 0) lesson = 0;

  hud.label("prutti " + (lesson + 1));
  loadLesson(api);
  color = help.choose("white", "cyan", "yellow", "orange", undefined);
}

// 🎨 Paint
function paint({ wipe, ink, paste, screen, text: txt, help, noiseTinted }) {
  let { title, text } = lessons[lesson];
  text = text.trim();
  const titleBox = txt.box(title, { x: 6, y: 24 + scroll }, screen.width - 8);
  const textBox = txt.box(
    text,
    { x: 6, y: 24 + scroll + 10 + titleBox.box.height },
    screen.width - 8,
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
    .write(title, titleBox.pos, noiseTint, screen.width - 8);
  ink(color).write(text, textBox.pos, 0, screen.width - 8);

  let lastHeight = 0;
  lessonPaintings.forEach((painting) => {
    // if (!painting) return;
    const width = min(picWidth, screen.width - 12);
    const height = (painting.height / painting.width) * width;
    paste(painting, 6, 10 + textBox.pos.y + textBox.box.height + lastHeight, {
      width,
      height,
    });
    lastHeight += height + 10;
  });

  scrollMax = titleBox.box.height + 10 + textBox.box.height + lastHeight;
  return false;
}

// 🎪 Act
function act({ event: e, needsPaint, jump }) {
  if (e.is("draw")) {
    scroll += e.delta.y;
    checkScroll();
    needsPaint();
  }

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

  if (e.is("scroll")) {
    scroll -= e.y;
    checkScroll();
    needsPaint();
  }
}

// 🧮 Sim
// function sim() {
//  // Runs once per logic frame. (120fps locked.)
// }

// 🥁 Beat
// function beat() {
//   // Runs once per metronomic BPM.
// }

// 👋 Leave
// function leave() {
//  // Runs once before the piece is unloaded.
// }

// 📰 Meta
function meta() {
  return {
    title: "Prutti",
    desc: "Lessons NOT Learnt by pruttipal aka goodiepal",
  };
}

// 🖼️ Preview
// function preview({ ink, wipe }) {
// Render a custom thumbnail image.
// }

// 🪷 Icon
// function icon() {
// Render an application icon, aka favicon.
// }

export { boot, paint, act, meta };

// 📚 Library
//   (Useful functions used throughout the piece)

function loadLesson(api) {
  const path = api.debug
    ? "/assets/pruttipal/lnl"
    : "https://assets.aesthetic.computer/pruttipal/lnl";

  lessons[lesson].pictures?.forEach((name, index) => {
    api.net.preload(`${path}/${name}.jpeg`).then((file) => {
      lessonPaintings[index] = api.resize(
        file.img,
        picWidth,
        (file.img.height / file.img.width) * picWidth,
      );
      api.needsPaint();
    });
  });

  // Load and play sample for lesson 11.
  lessons[lesson].sounds?.forEach((name) => {
    api.net.preload(`${path}/${name}.m4a`).then((sfx) => {
      api.sound.play(sfx, { loop: true }); // Immediately play '.m4a' after loading it, and looping it.
    });
  });
}

function checkScroll() {
  if (scroll < -scrollMax) scroll = -scrollMax;
  if (scroll > 16) scroll = 16;
}
