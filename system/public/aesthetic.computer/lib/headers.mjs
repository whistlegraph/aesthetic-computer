export function headers() {
  // Title
  console.log(
    "%caesthetic.computer",
    `background: rgba(10, 20, 40);
     color: rgb(200, 200, 250);
     font-size: 120%;
     padding: 0 0.25em;
     border-radius: 0.15em;
     border-bottom: 0.75px solid rgb(120, 120, 170);
     border-right: 0.75px solid rgb(120, 120, 170);`
  ); // Print a pretty title in the console.

  // Global Keyboard Shortcuts
  console.log(
    `%cFullscreen: C-x, Prompt: ~`,
    `background-color: black;
     color: rgb(200, 200, 200);
     padding: 0 0.25em;
     border-left: 0.75px solid rgb(60, 60, 60);
     border-right: 0.75px solid rgb(60, 60, 60);`
  );

  // Source code URL.
  console.log(
    "%cgithub.com/digitpain/aesthetic.computer",
    `color: rgb(200, 200, 200);
     background-color: black;
     padding: 0 0.25em;
     border-left: 0.75px solid rgb(60, 60, 60);
     border-right: 0.75px solid rgb(60, 60, 60);`
  );
}