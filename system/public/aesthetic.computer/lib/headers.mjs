export function headers() {
  // Title
  console.log(
    "%cAesthetic Computer",
    `background: rgba(10, 20, 40);
     color: rgb(200, 200, 250);
     font-size: 18px;
     padding: 0 0.25em;
     border-radius: 0.15em;
     border-bottom: 0.75px solid rgb(120, 120, 170);
     border-right: 0.75px solid rgb(120, 120, 170);`
  ); // Print a pretty title in the console.
  
  // Show TEIA mode in TEIA environments
  if (window.acTEIA_MODE) {
    console.log(
      "%cTEIA",
      `background: rgba(0, 0, 0, 0.9);
       color: rgb(255, 255, 255);
       font-size: 16px;
       padding: 0 0.35em;
       border-radius: 0.15em;
       border: 0.75px solid rgb(120, 120, 120);`
    );
  }

  // Global Keyboard Shortcuts
  // console.log(
  //   `%cFullscreen: C-x, Prompt: ~`,
  //   `background-color: black;
  //    color: rgb(200, 200, 200);
  //    padding: 0 0.25em;
  //    border-left: 0.75px solid rgb(60, 60, 60);
  //    border-right: 0.75px solid rgb(60, 60, 60);`
  // );

  // Source code URL.
  // console.log(
  //   "%chttps://github.com/whistlegraph/aesthetic-computer",
  //   `color: rgb(200, 200, 200);
  //    background-color: black;
  //    padding: 0 0.25em;
  //    border-left: 0.75px solid rgb(60, 60, 60);
  //    border-right: 0.75px solid rgb(60, 60, 60);`
  // );
}