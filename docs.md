# Aesthetic Computer Programming Basics

Writing and publishing a `piece` on aesthetic.computer involves
programming against an easy to learn JavaScript API while using
the <a href="https://marketplace.visualstudio.com/items?itemName=aesthetic-computer.aesthetic-computer-code">VS Code</a> extension.
<br>
<br> Follow the <a href="#">setup guide</a> to get started. 

## Graphics

<!-- wipe() -->
<details>
<summary class="fun">wipe()</summary>
<ul>
  <li>wipe() <em>Yields a random color</em></li>
  <li>wipe(<a href="https://www.w3.org/wiki/CSS/Properties/color/keywords">colorName</a>) <em>Such as "red" or "hotpink" (CSS)</em></li>
  <li>wipe(red, green, blue) <em>Each from 0-255</em></li>
  <li>wipe(gray) <em>Grayscale from 0-255</em></li>
  <li>wipe(paletteName) <em>Try "rainbow"</em></li>
</details>
<span class="desc">Clear the screen with a given color.</span>

<!-- ink() -->
<details>
<summary class="fun">ink()</summary>
<ul>
  <li>ink() <em>Chooses a random color</em></li>
  <li>wipe(<a href="https://www.w3.org/wiki/CSS/Properties/color/keywords">colorName</a>) <em>Such as "red" or "hotpink" (CSS)</em></li>
  <li>ink(red, green, blue) <em>Each from 0-255</em></li>
  <li>ink(red, green, blue, trans) <em>Including transparency</em></li>
  <li>ink(gray) <em>Grayscale from 0-255</em></li>
  <li>ink(gray, trans) <em>Grayscale with transparency</em></li>
  <li>ink(paletteName) <em>Try "rainbow"</em></li>
</ul>
</details>
<span class="desc">Set the color for future graphics functions like <code>line</code> or <code>box</code>.</span>

<!-- point() -->
<details>
<summary class="fun">point()</summary>
<ul>
  <li>point() <em>Paint a random point</em></li>
  <li>point(x, y) <em>At specific coordinates</em></li>
</ul>
</details>
<span class="desc">Plot a pixel at position <code>x</code>, <code>y</code>.</span>

<!-- line() -->
<details>
<summary class="fun">line()</summary>
<ul>
  <li>line() <em>Paints a random line</em></li>
  <li>line(Ax, Ay, Bx, By) <em>From 0 on up starting at top left</em></li>
</ul>
</details>
<span class="desc">Paint a 1px thick line from point <code>A</code> to <code>B</code>.</span>

<!-- box() -->
<details>
<summary class="fun">box()</summary>
<ul>
  <mark><li>box() <em>A random box</em></li></mark>
  <li>box(x, y, size) <em>Square from top left corner</em></li>
  <li>box(x, y, w, h) <em>Rectangle from top left corner</em></li>
  <li>box(x, y, size, mode) <em>Square with <code>mode</code></em></li>
  <li>box(x, y, w, h, mode) <em>Rectangle with <code>mode</code></em></li>
  <li class="subsection"><code>mode</code></li>
  <em class="small">
  center &nbsp;- paints a box from the center<br>
  <hr>
  outline - paints the outline of a box<br>
  inline &nbsp;- the opposite of outline<br>
  <em class="tiny">(thicken with <code>:</code> like <code>outline:4</code>)</em>

  <br>
  combine modes with <code>*</code> like <code>outline*center</code> or <code>inline:3*center</code> 
  </em>
</ul>

</details>
<span class="desc">Paint a rectangle.</span>

<!-- circle() -->
<details>
<summary class="fun">circle()</summary>
<ul>
  <mark><li>circle() <em>A random circle</em></li></mark>
  <li>circle(x, y, radius) <em>Circle from center</em></li>
  <li>circle(x, y, radius, filled) <em>Set to <code>false</code> for outline</em></li>
  <li>circle(x, y, radius, filled, thickness) <em>Set outline thickness</em></li>
</ul>
</details>
<span class="desc">Paint a circle.</span>

<!-- oval() -->
<details>
<summary class="fun">oval()</summary>
<ul>
  <mark><li>oval() <em>A random oval</em></li></mark>
  <li>oval(x, y, w, h) <em>Oval from center with width & height</em></li>
  <li>oval(x, y, w, h, filled) <em>Set to <code>false</code> for outline</em></li>
  <li>oval(x, y, radius, filled, thickness) <em>Set outline thickness</em></li>
</ul>
</details>
<span class="desc">Paint an oval.</span>

<!-- shape() -->
<details>
<summary class="fun">shape()</summary>
<ul>
  <mark><li>shape() <em>A random shape</em></li></mark>
  <li>shape(x, y, x, y, ...) <em>Shape from a list of points</em></li>
  <li>shape({ points: [x, y, ...], filled: false }) <em>Outline only</em></li>
</ul>
</details>
<span class="desc">Paint any shape with points.</span>

<!-- poly() -->
<details>
<summary class="fun">poly()</summary>
<ul>
  <mark><li>poly() <em>A random scribble</em></li></mark>
  <li>poly([[x, y], [x, y], ...]) <em>Line from a list of points</em></li>
</ul>
</details>
<span class="desc">Paint connected lines with points.</span>

<!-- flood() -->
<details>
<summary class="fun">flood()</summary>
<ul>
  <mark><li>flood() <em>Fill a random location.</em></li></mark>
  <li>flood(x, y) <em>Line from a list of points</em></li>
</ul>
</details>
<span class="desc">Fill connected pixels of the same color and return a list of them.</span>

<!-- write() -->
<details>
<summary class="fun">write()</summary>
<ul>
  <li>write("text") <em>A word in a random location</em></li>
  <li>write("text", x, y) <em>Or at specific coordinates</em></li>
</ul>
</details>
<span class="desc">Write text on the screen.</span>

## Sound
## Action
## Number

<style>
  h2 {
    border-bottom: 1px solid rgba(255, 255, 255, 0.25) !important;
  }
  body {
    background: rgba(32, 32, 32, 1);
    color: white;
  }
  code {
    color: yellow;
  }
  mark {
    color: white;
    opacity: 0.25;
    text-decoration: line-through;
  }
  .fun {
    font-size: 1.25em;
    font-weight: bold;
    user-select: none;
    cursor: pointer;
    color: yellow;
  }
  details ul {
    font-family: monospace;
    list-style-type: none;
    margin-left: 1.1em;
    padding-left: 0;
  }
  details[open] ul {
    padding-bottom: 0.75em;
    margin-bottom: 0.75em;
    border-bottom: 1px solid rgba(255, 255, 255, 0.25);
  }
  .subsection, .small {
    margin-left: 1em;
  }
  .subsection {
    padding-top: 0.5em;
    opacity: 0.5;
  }
  .small {
    display: block;
    font-size: 0.85em;
    font-style: normal;
    padding-left: 0.4em;
    padding-top: 0.35em;
  }
  .tiny {
    opacity: 1;
    font-style: normal;
    display: block;
    padding-top: 0.3em;
  }
  .small code, .tiny code {
    color: white;
  }
  details ul li em { opacity: 0.5; }
  details ul em { opacity: 0.5; }
  a { color: pink; }
  .desc {
    padding-left: 1.1em;
    font-size: 1.1em;
    margin-bottom: 1em;
    display: block;
  }
</style>
