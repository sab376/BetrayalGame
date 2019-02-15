# Betrayal at Home on the Heap

## Overview

  Betrayal at Home on the Heap is an OCaml implementation of the dynamic
exploration game Betrayal at House on the Hill. Initially, players take
turns creating an endless mansion by exploring outwards from the start
room. Room tiles can have special effects tied to them that can influence
players' stats positively or negatively. Some rooms also require players
to draw a card upon discovery. If a player happens to draw an Omen card,
they must roll to determine whether or not the haunt begins.
  The second phase of the game activates when the haunt begins. One of
several creative haunts can occur, in which players will be split up
into teams in order to complete some objective. At this stage of the
game, players can attack one another, and can die if their stats fall
too low. The first team to complete the haunt objective wins the game.
  For more information, read a more complete description of Betrayal at
House on the Hill at:
(https://en.wikipedia.org/wiki/Betrayal_at_House_on_the_Hill)

![Alt text](screenshots/screenshot2.png?raw=true "Title")

## Installation

As this is the project for CS 3110 at Cornell University, we will assume that you already have ocaml and opam installed. If not, please follow the [instructions here] (http://www.cs.cornell.edu/courses/cs3110/2016fa/install.html).

Additionally, you will need to install X11/XQuartz for Graphics support.
(https://www.xquartz.org/)

If you installed ocaml with homebrew, this can also be done by running
<dl>
  <dd> brew install Caskroom/cask/xquartz </dd>
  <dd> brew reinstall ocaml --with-x11 </dd>
</dl>

Then, map opam to use the system installation instead of the currently bound one: opam switch sys. Then run eval `opam config env` as instructed.

You might also need to install the Graphics module, which can be done by running
<dl>
  <dd> opam install graphics </dd>
</dl>

If this does not work, you may need to reinstall OCaml with a version that includes Graphics. We recommend:
<dl>
  <dd> opam switch reinstall 4.06.0 </dd>
</dl>

## Gameplay

After successfully installing the Graphics library, you can play the game by running the Terminal command
<dl>
  <dd> make play </dd>
</dl>
Note that if running natively on macOS, it might take a few seconds to power up XQuartz.

The Terminal is not needed for gameplay beyond launching the game.

![Alt text](screenshots/screenshot1.png?raw=true "Title")
![Alt text](screenshots/screenshot3.png?raw=true "Title")
![Alt text](screenshots/screenshot4.png?raw=true "Title")
