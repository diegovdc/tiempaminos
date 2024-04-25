

(
SynthDef("amp-regulator-replier",
	{
		|in=0, replyRate=10, peakLag=3|
		SendPeakRMS.kr(In.ar(in, 4), replyRate, peakLag, "/amp-regulator")
	}
).store(dir: "/Users/diego/Music/code/tieminos/src/tieminos/habitat/extended_sections/hacia_un_nuevo_universo/scsyndefs/")
)


(
SynthDef("sini",
	{
		|freq 0|
		Out.ar(0, (SinOsc.ar(freq)*0.2)!2)
	}
).store(dir: "/Users/diego/Music/code/tieminos/src/tieminos/habitat/extended_sections/hacia_un_nuevo_universo/scsyndefs/")
)



(
SynthDef("arr",
	{
		|freq 0|
		SendPeakRMS.kr(Dust.ar(20)!4, 10, 3, "/amp-regulator")
	}
).add
)

Synth('arr')




(
o = OSCFunc({ |msg|
	msg.postln
}, '/amp-regulator');
)
o.free;
