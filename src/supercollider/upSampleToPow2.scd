// Used for upsampling the wavetables of AKWF
(
~upSampleToPow2 = {|filePath, outPath|
	var file, data, n, newData, outFile;

	// 'protect' guarantees the file objects will be closed in case of error
	protect {
		// Read original size of data
		file = SoundFile.openRead(filePath);
		data = Signal.newClear(file.numFrames);
		file.readData(data);

		// Convert to n = some power of 2 samples.
        n = data.size.nextPowerOfTwo.postln;
        newData = data.resamp1(n);

        // Convert the resampled signal into a Wavetable.
        // resamp1 outputs an Array, so we have to reconvert to Signal
        newData = newData.as(Signal).asWavetable;

        // Load this into a buffer, or save to disk.
        outFile = SoundFile(outPath)
        .headerFormat_("WAV")
        .sampleFormat_("float")
        .numChannels_(1)
        .sampleRate_(44100);
        if(outFile.openWrite.notNil) {
			outFile.writeData(newData);
			("file created"+outPath).postln;
        } {
			"Couldn't write output file".warn;
		};
	} {
		file.close;
		if(outFile.notNil) { outFile.close };
	};
};
)

(
~upSampleDir = {
	|dir, outPath|
	var files = dir.files;
	var dirName = dir.folderName;
	files
	.select({|file_| file_.extension.asString == "wav"})
	.do{|file_|
		var parentDir = outPath +/+dirName;
		var outPath_ =parentDir+/+file_.fileName;
		file_.fullPath.postln;
		parentDir.mkdir;
		~upSampleToPow2.(file_.fullPath,outPath_);
	};
};
)

(
var path = "/Users/diego/Music/samples/wavetables/AKWF-FREE/AKWF";
var outPath = "/Users/diego/Music/samples/wavetables/AKWF-SuperCollider/";
var dirs = PathName.new(path).folders;
dirs.do(~upSampleDir.(_, outPath));
)

