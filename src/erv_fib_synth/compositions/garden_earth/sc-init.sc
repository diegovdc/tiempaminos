//https://doc.sccode.org/Reference/EmacsEditor.html
(
Can.init;
o = Server.default.options;
o.numOutputBusChannels = 10;
s.waitForBoot({
	().play;
	s.makeGui;
});
)
