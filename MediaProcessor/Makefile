VPATH = media_processor/

FFMPEG_CFLAGS = $(shell pkg-config --cflags libavformat libavcodec libswresample libavutil)

CFLAGS = -std=c++14 $(FFMPEG_CFLAGS) -I. -O2

OBJECT_FILES = .ffmpeg_error.o .keyframe_extractor.o .media_file.o .media_processor.o .peak_extractor.o .stream_processor.o .MediaProc.o

libMediaProc.a: $(OBJECT_FILES) 
	ar rvs $@ $^
	
.MediaProc.o: MediaProc.cpp MediaProc.h
	g++ -c $(CFLAGS) $< -o $@

.ffmpeg_error.o: ffmpeg_error.cpp ffmpeg_error.hpp
	g++ -c $(CFLAGS) $< -o $@

.keyframe_extractor.o: keyframe_extractor.cpp keyframe_extractor.hpp ffmpeg_error.hpp stream_processor.hpp
	g++ -c $(CFLAGS) $< -o $@
	
.media_file.o: media_file.cpp media_file.hpp ffmpeg_error.hpp stream_processor.hpp
	g++ -c $(CFLAGS) $< -o $@

.media_processor.o: media_processor.cpp media_processor.hpp media_file.hpp stream_processor.hpp
	g++ -c $(CFLAGS) $< -o $@

.peak_extractor.o: peak_extractor.cpp peak_extractor.hpp stream_processor.hpp peak.hpp
	g++ -c $(CFLAGS) $< -o $@

.stream_processor.o: stream_processor.cpp stream_processor.hpp ffmpeg_error.hpp
	g++ -c $(CFLAGS) $< -o $@

.PHONY: clean

clean:
	rm $(OBJECT_FILES) 
	rm libMediaProc.a
