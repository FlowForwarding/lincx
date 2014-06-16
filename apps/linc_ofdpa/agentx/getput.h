#ifndef GETPUT_H
#define GETPUT_H

#define GET16(p) \
	 (uint16_t)((uint8_t *)(p))[0] | \
	((uint16_t)((uint8_t *)(p))[1] << 8)

#define GET32(p) \
	 (uint32_t)((uint8_t *)(p))[0] | \
	((uint32_t)((uint8_t *)(p))[1] << 8) | \
	((uint32_t)((uint8_t *)(p))[2] << 16) | \
	((uint32_t)((uint8_t *)(p))[3] << 24)

#define GET64(p) \
	 (uint64_t)((uint8_t *)(p))[0] | \
	((uint64_t)((uint8_t *)(p))[1] << 8) | \
	((uint64_t)((uint8_t *)(p))[2] << 16) | \
	((uint64_t)((uint8_t *)(p))[3] << 24) | \
	((uint64_t)((uint8_t *)(p))[4] << 32) | \
	((uint64_t)((uint8_t *)(p))[5] << 40) | \
	((uint64_t)((uint8_t *)(p))[6] << 48) | \
	((uint64_t)((uint8_t *)(p))[7] << 56) 

#define PUT16(p, v) do { \
	((uint8_t *)(p))[0] = (v) & 255; \
	((uint8_t *)(p))[1] = ((v) >> 8) & 255; \
} while (0)

#define PUT32(p, v) do { \
	((uint8_t *)(p))[0] = (v) & 255; \
	((uint8_t *)(p))[1] = ((v) >> 8) & 255; \
	((uint8_t *)(p))[2] = ((v) >> 16) & 255; \
	((uint8_t *)(p))[3] = ((v) >> 24) & 255; \
} while (0)

#define PUT64(p, v) do { \
	((uint8_t *)(p))[0] = (v) & 255; \
	((uint8_t *)(p))[1] = ((v) >> 8) & 255; \
	((uint8_t *)(p))[2] = ((v) >> 16) & 255; \
	((uint8_t *)(p))[3] = ((v) >> 24) & 255; \
	((uint8_t *)(p))[4] = ((v) >> 32) & 255; \
	((uint8_t *)(p))[5] = ((v) >> 40) & 255; \
	((uint8_t *)(p))[6] = ((v) >> 48) & 255; \
	((uint8_t *)(p))[7] = (v) >> 56; \
} while (0)

#endif
