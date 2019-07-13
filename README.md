# Pac-Man animation demo for C64

Short Pac-Man animation demo written in 6510 assembly for the C64 as can be seen <a href="https://t.co/7PtWfYISMg">here</a> .

The project was developed in <a href="https://www.ajordison.co.uk/">CBMPrg Studio</a> and can be imported in it easily and ran on a <a href="http://vice-emu.sourceforge.net/">VICE emulator</a>. 

Once loaded into the RAM, program can be executed from the BASIC prompt by pointing to the starting address ($1001 = 4097) using the command <b>SYS 4097</b>

The demo uses some techniques such as:
<ul>
<li><a href="https://www.dbfinteractive.com/forum/index.php?topic=4477.0">Top & bottom border hack</a></li>
<li>Detecting sprite collisions in VIC-II's Sprite to Sprite Collision Detect Register</li>
</ul>

Pac-Man and Blinky characters used as basis for the animation are property of Bandai Namco corporation.
