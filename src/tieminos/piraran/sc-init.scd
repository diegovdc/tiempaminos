//https://doc.sccode.org/Reference/EmacsEditor.html
(
Can.init;
o = Server.default.options;
o.numOutputBusChannels = 14;
s.waitForBoot({
	().play;
	s.makeGui;
});
)
