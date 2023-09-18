s.boot

"b1".namecps;

~f = ("bb1".namecps +2); // 112.0

// cuerda 6
{Pan2.ar(SinOsc.ar(~f))}.play

// cuerda 5
{Pan2.ar(SinOsc.ar(~f*3/2))}.play

// cuerda 4
{Pan2.ar(SinOsc.ar(~f*5/2))}.play

// cuerda 3
{Pan2.ar(Saw.ar(~f*7/2))}.play

// cuerda 2
(~f*11/2).cpsname;
{Pan2.ar(Saw.ar(~f*11/2))}.play


// cuerda 1
(~f*13/2).cpsmidi- (~f*12/2).cpsmidi;
(~f*13/2).cpsname;
{Pan2.ar(Saw.ar(~f*12/2))}.play
{Pan2.ar(Saw.ar(~f*25/4))}.play

