Can.init; s.boot;

Env

MIDIClient.init

~overtone = NetAddr("127.0.0.1", 7777)
~overtone.sendMsg(\note_on, 2000, \gate, 0);

MIDIIn.connectAll
(
MIDIdef.noteOn(\note_on, {|vel,note,chan|
	~overtone.sendMsg("/note-on", note, vel, chan);
});
)

(
MIDIdef.noteOff(\note_off, {|vel,note,chan|
	~overtone.sendMsg("/note-off", note, vel, chan);
});
)
