// 🐟 Ordfish, 23.05.06.13.15
// Viewer and tracker of `ordfish`.

/* #region ✏️ todo
  + Later
  - [❤️‍🔥] Background music that runs consistently across ordfish.
  - [] Special effects... other data.
  + Done
  - [x] Add hover to TextButton.
  - [x] Add sound.
  - [x] Tap the screen to change the fish. 
  - [x] Fix iOS tap bug.
  - [x] Add default load delay.
#endregion */

import { anyKey } from "../lib/help.mjs";
const { sin, min, max } = Math;

const ordfish = {
  afajydklwun:
    "5b182278c42d5a74ab4dd9f43826a0a2fce0158000001ed3bdf44873f1378d93i0",
  bhouvfjcwjv:
    "ffea6caf389f625cb58aadcce4e6d59c0ba79e9328fb550df7d7773085b7c2f9i0",
  bsoehhcnrdk:
    "ea356f87bda9f6a2a2f63e2a7aa8182bbd43a0e7ffb42a0db6669a255f90dfb8i0",
  bsoehhdbpto:
    "ebec9d350dbe923cf808f07ac2a98f66d1a5c8fbeada617dc5da0c5b8fe21298i0",
  ewqzayivxuy:
    "9b60a92316a0e1472d1e434c775636e45e9173084c99615bb38eb9e2fd83c1c5i0",
  hqdekuuuyhb:
    "47ce3cac5662cde8d2c228a7d186b2be92ad7a4e00069b9735af84409f96d4d3i0",
  atbfflnurtx:
    "fd29ec8b749dee828bc170c62533bd541d2084039ff6619c5efa1d135d1700d0i0",
  ewqzayirtoi:
    "2e6690811e32b9712d7ae2231f90807a6963938dc6377a544be8212fb80f491ci0",
  atbfflnswvx:
    "d52041661ef0ee12d8dcc784f1274a7a4995b1753d8b37f11a603aa23ee56a24i0",
  ewqzayiqitk:
    "0c369e39812f83d8f1274246f45f5de41cfc1fd008135935aed7bb77dbdb8919i0",
  afajydkiabh:
    "bb31335aa76fab43f19c36bf76923168324d0c17b8cbbf7e083fe8b383120812i0",
  atbfflnrapr:
    "e0f3c237c46dcd5224feae2442f48929e13e0a0589436eca6e65caeaa1941c48i0",
  czvjqrsrfzg:
    "20a439eb33cdc4800c080f71d5848591621687d2c7873605fc62aa59336b1c44i0",
  exgjjlbpmuf:
    "fd0fa116569856df3a22719d8ace732662210ab86440afb20ae3b38cc8ace262i0",
  bsoehhcydbw:
    "e4073a8cb6a5c50b34f1639adcb4ebb2d7c15be29e6bdb3f5b23167a892d1549i0",
  atbfflnofpf:
    "8ecfd56cf0cb086eafafb7c27fa3b66c42d445408bf0d982d94c5163b8c734bdi0",
  cyizglvvggw:
    "7a17276207151786d73149ef59fd197c2df6c460ed13c23d912ed1268813eaf9i0",
  cyizglvsoki:
    "11f894b119f9aa495bc7406d6522ad83a521e1ea86edb89e3909ee365bd609f5i0",
  hqdekuuqjit:
    "762c294f52fb25e0696385ca71061f0d7f6fcfc77e99a0fbdd8c3c714e9deac7i0",
  gxlbncjokam:
    "055777368ab2af60774ce11310b861c241628e775c3d9b5550b1171365a6744fi0",
  exgjjlbleox:
    "452dd0688a9752fd39224e0ea49c2a3b2b04de8b125cff509593ceb962214a9ai0",
  gxlbncjkabl:
    "b7de80e8363791d46f5094ee903c53eed6cc25ba7b21cc68462b31430dadbcfdi0",
  afajydkdnav:
    "3c772118cce5265822adeb7bb768d9d7272a327a9ed1c85e991373bb6c9f3bffi0",
  atbfflnmnrr:
    "15d09ed178c442299e22737dd25cc93036f67b500f64baced2c51b644dbfd16bi0",
  afajydjybwq:
    "eaf0e999a61728d02af1fbed32d72df0020ee2759e1262e760346510bc3d8a5bi0",
  afajydjubgq:
    "d4a3d5ec2f867dedd6c33118830ff61eb2816df18ef18023cc0bc3f8c8d782c1i0",
  atbfflnkhzr:
    "0fa12802efd733c4059fd66ee2d8ee7d9fbc8e460b80991a2fedfdd40115b7d4i0",
  cyizglvotma:
    "d58eb96234dcc2409e01f73a3a3f3176f23a097036c6d9648a407efc04314687i0",
  ddqccmdnoth:
    "3ee68b971f1d52e054bfb8d7a2b414e1d91d7e53c3ce31deb3b835ac9c424359i0",
  afajydjpkxa:
    "c5b97ff9749d3bb59cb0864f6e903433fc9a2101e9299934b8ccfeb57b05d875i0",
  gxlbncizwhg:
    "80dcb20cc0f72ece2788a196ff1d72b287a9bb3174b31d8a63d27b0fed98b946i0",
  atbfflnbxxm:
    "b24edd0ade77f84584427c85b94cf46c391600d4224c7b8309ba0b9ce66a9895i0",
  bikdtxjurmm:
    "1575cf2d9b689f4ad4004c7b02a2d899904b5ea42d5416503d9ddfac1d635a6ei0",
  ewqzayimcig:
    "2efb094b7c44220b660eabd3269812c85181bd52a21a37f7778507ffc1bb543ai0",
  bhouvfiukft:
    "129dc4ba01b40cd91d4703ef451bbd22a42bb90f07cbbc42e37e6d91b9d9ea9ci0",
  atbfflmwhua:
    "2721164e0160d4feb1e92030ee747d01159a650737df52778d109f539154f2fei0",
  czvjqrsmtio:
    "56ee4746d25caf6d59a3467fd6f036287db68a51ae7c75f66aacba23ec82a574i0",
  ddqccmegpuz:
    "5973583f8955695ec17b5b634540f500feb585230a8d98aa7a06ec9009c489c7i0",
  atbfflmszgu:
    "c87d1ae363e7812deb8fadf0c54dfabdd6cedb9e09f06a08485b1adeb979b6f8i0",
  blnwvwcksbj:
    "f9dda6d9cc426de067f5374ef9254022a32e3cbd31296d0a6a63a816786be672i0",
  cggizjswstp:
    "a63313ddad09cbe4f8cd35f876c93db6a0dc92f9c8f8541697b74a255bccf8e0i0",
  empraaozdzg:
    "cd86563c0330863252b9bb498034f85371c9d99e5ae0a13338a5a6c3f95a546ci0",
  afajydjjxqz:
    "9e5f39181d7b2cbb18c2000ae5196bcfe6109d3498beca65164b8ab35f034f1di0",
  jahkarqnepw:
    "718deb8a4a2f18a230b5da19cba28780cf768aedb0c97afe6b0cd022ea1f04dei0",
  blnwvwchezz:
    "b8a12a3c23b17234fec5ae5a475041d6113351ab3f48f4e7cb0ebde0db3f19a1i0",
  dphnonhypod:
    "5f25fa2b3604e55352db003b106858807479397536cbe8a40a557ce2b0d3d194i0",
  cggizjsqzhk:
    "e66aae9db6ccf2049ddca95bbec86dfb16d967b0f0935f95f306927ce7e3e962i0",
  empraapnasc:
    "cf33fed2a637b9cf6640f35aa7329bf0792669a74e44a618d3235db16e0978cbi0",
  bcfqtpqefzb:
    "3290fc083f787cb77d9049e246e1f307b0c029e596c6e7993992ada4ba6f1b43i0",
  jahkarpbgus:
    "b32733504fd221006763ee969f113f1b0f4482c4a56b56d0557d3aac3b475d46i0",
  jmnmofemhyx:
    "ee2490d85792d156fe2f7eb40daf1a861295bef0cfff1094e06f555859560007i0",
  aqdaszimniv:
    "634e525f7bafa9d48bd8223deb2e598b0f950df5b7a2966c169f13d8eeacf846i0",
  ddqccmdsbqf:
    "a997f335430dc852e42dddeca28ddb4b915cc09d07ed16794bee6d25aeee9db1i0",
  aqdasziiajw:
    "f704a6b279a766269c19662a314dd68077ac10a46a3edead88b568788cb2a8bdi0",
  jahkarovszw:
    "153445406160c34151ff69dcc47fafca66765daa7ba571d5002435c8f7e719fdi0",
  aqdaszhurwg:
    "82cda410ec2711eaa67fe90034e7e816d6c94bdebf218a7172098299c10edd6ci0",
};

const fishCount = Object.keys(ordfish).length;
const GO = 7;
let pix,
  counter,
  chain,
  path,
  dir,
  code,
  ordf,
  ready = 0;

const baseUrl = `https://assets.aesthetic.computer/ordfish`;
//const baseUrl = `https://assets.aesthetic.computer.sfo3.cdn.digitaloceanspaces.com`;
// Or `https://cdn.ordinalswallet.com/inscription/content`;

let needsWipe = false;

async function boot({ params, wipe, ink, help, resize, screen, hud, net }) {
  // Look up an ordfish code from the first param.
  parseParams(params);
  if (!ordf) return;
  hud.label(`ordfish ${code}`);
  net.rewrite(`ordfish~${code}`);
  // Get url of ordfish image.
  path = `${baseUrl}/${code}.webp`;
  try {
    // Preload ordfish image from the internet and downsize its bitmap.
    pix = resize(
      (await net.preload({ path, extension: "webp" })).img,
      128,
      128,
    );
  } catch (err) {
    console.error("Failed to load ordfish image:", err);
  }

  wipe();

  /*
  counter = (gap = 13) => {
    const msg = `swimming: ${fishCount}/100`;
    ink(0).write(msg, { x: 4 + 1, y: screen.height - gap + 1 });
    ink(255).write(msg, { x: 4, y: screen.height - gap });
  };
  counter(); // Paint the total ordfish count onto the screen.
  */

  dir = help.choose(1, -1);
}

function paint({ screen, ink, wipe, paste, paintCount, ui, noise16 }) {
  if (needsWipe) {
    wipe();
    needsWipe = false;
  }
  if (pix && ready >= GO + 1) {
    const osc = sin(paintCount / 60);
    const osc2 = sin(paintCount / 40);
    const scaleRat =
      min(screen.width, screen.height) / max(pix.width, pix.height);
    const scale = scaleRat / 1.1 + osc / 3; // Bounce in and out
    const angle = 0 + osc2 * dir * 10; // Slowly rotate
    const x = screen.width / 2 - (pix.width * scale) / 2; // Center
    const y = screen.height / 2 - (pix.height * scale) / 2;
    ink(undefined, undefined, undefined, 1).box(
      0,
      0,
      screen.width,
      screen.height,
    );
    paste(pix, x, y, { scale, angle });
    // counter();

    if (!chain)
      chain = new ui.TextButton("chain", { right: 6, bottom: 6, screen });
    chain.reposition({ right: 6, bottom: 6, screen });
    chain.paint({ ink }, [0, 255, 255, 0]);
  } else if (ready <= GO) {
    ready += 1;
    ready <= GO ? noise16(0) : wipe();
  }
}

function act({ event: e, jump, help, screen }) {
  // Randomize button. (Takes up most of the screen.)
  const strip = 30; // Strip of pixels to ignore for the big randomizer.
  if (
    ((e.device === "touch" && e.is("lift")) || // Act on lift if using a finger.
      (e.device === "mouse" && e.is("touch"))) && // Or mouse down on a laptop.
    e.y > strip &&
    e.y < screen.height - strip
  ) {
    let newCode = code; // Pick any fish other than this one...
    while (code === newCode) newCode = help.anyKey(ordfish);
    bip = true;
    jump(`ordfish~${newCode}`);
  }

  if (e.is("reframed")) needsWipe = true;

  chain?.btn.act(e, () => {
    jump(`https://ordinalswallet.com/inscription/${ordf}`);
    bap = true;
  });
}

let beatCount = 0n; // TODO: This should REALLY go into the main API at this point... 23.05.08.17.32
let bap, bip;

function beat({ num, sound: { bpm, synth } }) {
  if (beatCount === 0n) {
    bap = bip = false; // Clear any existing signals.
    bpm(1800); // Set bpm to 1800 ~ 30fps }
  }
  beatCount += 1n; // TODO: This should go into the main API. 22.11.01.17.43

  if (bap) {
    synth({
      tone: num.randIntRange(100, 800),
      beats: 1.5,
      attack: 0.02,
      decay: 0.97,
      volume: 0.35,
    });
    bap = false;
  }

  if (bip) {
    synth({
      tone: num.randIntRange(1000, 1600),
      beats: 1,
      attack: 0.02,
      decay: 0.97,
      volume: 0.1,
    });
    bip = false;
  }
}

function meta({ params }) {
  let title;
  if (!params[0]) {
    title = "Ordfish · Aesthetic Computer";
  } else {
    parseParams(params);
    title = `Ordfish - ${code} · Aesthetic Computer`;
  }
  return {
    title,
    desc: `There are ${fishCount} ordfish swimming right now.`,
    image_url: `${baseUrl}/${code}.webp`,
  };
}

export { boot, paint, act, ordfish, beat, meta };

// 📚 Library (Useful functions used throughout the piece)

function parseParams(params) {
  code = params[0] || anyKey(ordfish);
  ordf = ordfish[code];
}

// Build collection JSON.
/*
const ordfishMeta = [];
Object.keys(ordfish).forEach((code) => {
  ordfishMeta.push({
    id: ordfish[code], // inscription id
    meta: {
      name: code
    }
  })
});
console.log(ordfishMeta);
*/
