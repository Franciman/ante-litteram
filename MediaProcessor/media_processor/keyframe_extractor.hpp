#ifndef keyframe_extractor_hpp_INCLUDED
#define keyframe_extractor_hpp_INCLUDED

#ifdef __cplusplus

#include <media_processor/stream_processor.hpp>
#include <vector>

struct AVFrame;

class KeyFrameExtractor : public StreamDecoder
{
    std::vector<int> TimePoints;
    AVFrame *Frame;
    double TimeBase;
public:
    KeyFrameExtractor(AVStream *stream);

    virtual ~KeyFrameExtractor();

    virtual void push_packet(const AVPacket &pkt) override;

    virtual void processing_finished() override;

    const std::vector<int> &timePointsMs() const
    {
        return TimePoints;
    }
};

#endif

#endif // keyframe_extractor_hpp_INCLUDED

