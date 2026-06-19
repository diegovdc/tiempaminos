s.options.memSize = 2**16
s.options.numInputBusChannels = 32;
s.options.numOutputBusChannels = 84;
s.boot



(
SynthDef(\twotube,{
	arg delay1=100, delay2=50, k=0.0, loss=0.999, dur_=0.5, pan=0.0, psratio1 = 1, psratio2 = 1, psamp=0.7;
	var source, sig, ps;

	source= BrownNoise.ar(0.5)*EnvGen.ar(Env([1,1,0,0],[(delay1+delay2)/SampleRate.ir,0.0,1.0]));

	sig = TwoTube.ar(source,k,loss,delay1,delay2)
			* EnvGen.ar(
				Env([0,1,1,0],[0.001]
				++ ((dur_-0.001)*[0.4,0.6])),
				doneAction:2
			);
	sig = (0.9*sig);
	ps =  (psamp * PitchShift.ar(sig, windowSize: 0.3, pitchRatio: [psratio1, psratio2]));
	sig = FreeVerb.ar(sig);
	sig = Pan2.ar(sig+ps,pan);
	sig = sig*0.1;
	//sig = Limiter.ar(sig, 0.95);
	Out.ar(20,sig);
}).add;
)

(
Pdef(\cholé,
	Pbind(
		\instrument, \twotube,
		\dur, Pseq([0.2, Pseq([0.1], Pseq([2, 4]))], inf),
		\dur_, Pseq([1, 2, 1, 2, 1, [1,1,1,5] ]/[2, 1,3], inf)+Pwhite(0, 2),
		\amp, Pseq([2,1,2,1,1]/3)++Pwhite(-0.2, 0.5),
		\k, Pseq((0.1!20)++([0.1, 0]!10)+[0, 0.01, 0], inf)+Pwhite(-0.01, 0.01),
		\loss, Pseq([0.999, [0.8, 0.9, 0.8, 0.95]], inf),
		\pan, Pbrown(-1, 1),
		\delay1, Pseq(
			[
				20,
				Pseq([40, 100]),
				30,
				Pseq([1, 20, 40, 30])
			],
			inf) + Pseq([0,0,[0, ([0, 2]!10)++[80]],0,10], inf),
		\delay2, Pseq([
			20, 10, [30!10, 60], 10,
			[50, Pseq([70, 100, 60]),
		]],
		inf),
		\psamp, Pseq([0.5, 0.4, 0.6], inf)+Pwhite(0, 0.4),
		\psratio1, Pseq((1!20)++((1/2)!50)++([1,2,3]!20).flatten++(([2,1/3,2, 3, 2])!25).flatten, inf),
		\psratio2, Pseq((1!50)++((3)!50)++(([1/3, 1, 4])!25).flatten, inf)

	)
);
)

Pdef(\cholé).play
Pdef(\cholé).stop
