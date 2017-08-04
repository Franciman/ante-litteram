#ifndef MediaProc_h_INCLUDED
#define MediaProc_h_INCLUDED

#include <media_processor/peak.hpp>
#include <stdlib.h>

#ifdef __cplusplus
extern "C"
{
#endif

// Up to the caller to free Peak array's memory

#ifndef __cplusplus
struct
#endif
Peak *extractPeaks(const char *filename, void (*tracker)(int64_t, int64_t)
                   , size_t *peaksNum, int *sampleRate, int *samplesPerPeak);

#ifdef __cplusplus
}
#endif

#endif // MediaProc_h_INCLUDED

