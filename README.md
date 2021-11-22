# corpsfini 
Code for experimenting with finite fields and pseudorandom number 
generation using them.  ("Corps fini" is French for "finite field".)

The code here is designed for learning and exploration.

If you're looking for a high-quality, efficient pseudorandom number
generator (PRNG), this is definitely not the place to look.  If you're
looking for a PRNG for use in simulations, statistics, or numerical
estimation, I'd suggest using one of WELL generators developed by
Panneton, L'Ecuyer, and Matsumoto (Pierre L'Ecuyer's website is a good
startig point), or an xoshiro generator from Sebastiano Vigna.  There
are other recent generators worth considering, and a Mersenne Twister
should be OK if it's initialized properly, as it probably is in most
popular libraries.  If you're looking for a cryptographic pseudorandom
number generator, I don't have advice, but good advice should be easy to
find.



## License

This software and text is copyright 2021 by [Marshall
Abrams](http://members.logical.net/~marshall/), and is distributed under
the [Gnu Lesser General Public License version
3.0](https://www.gnu.org/licenses/lgpl.html) as specified in the file
LICENSE, except where noted, or where code has been included that was
released under a different license.
