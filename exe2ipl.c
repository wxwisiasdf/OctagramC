#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define b16_to_be(x) __bswap_16(x)
#define b32_to_be(x) __bswap_32(x)
int main(int argc, char** argv)
{
    static char zero[80] = { 0 };
    static uint32_t max_rec_size = 2048;
    static uint32_t addr = 0;
    uint16_t len = b16_to_be(56);

    if (argc <= 2) {
        fprintf(stderr, "Usage: bin2txt [in] [out]\n");
        exit(EXIT_FAILURE);
    }

    FILE* in = fopen(argv[1], "rb");
    if (in == NULL) {
        fprintf(stderr, "Cannot open file %s: %s\n", argv[1], strerror(errno));
        exit(EXIT_FAILURE);
    }

    FILE* out = fopen(argv[2], "wb");
    if (out == NULL) {
        fprintf(
            stderr, "Cannot create file %s: %s\n", argv[2], strerror(errno));
        exit(EXIT_FAILURE);
    }

    while (!feof(in)) {
        static char tmp[56];
        fread(&tmp[0], 1, 56, in); /* Read from binary */
        size_t w_len = 0, line_len = 0; /* New punchcard line */
        w_len = 1; /* COLUMN 0,0 */
        fwrite("\x02", 1, w_len, out);
        line_len += w_len;
        w_len = 3; /* COLUMN 1,3 */
        if (addr >= max_rec_size || feof(in)) {
            fwrite("\xC5\xD5\xC4", 1, w_len, out); /* END */
            w_len = 80 - line_len; /* COLUMN 4,80 */
            fwrite(&zero, 1, w_len, out);
            line_len += w_len;
            break;
        } else {
            fwrite("\xE3\xE7\xE3", 1, w_len, out); /* TXT */
        }
        line_len += w_len;
        w_len = 1; /* COLUMN 4,4 */
        fwrite(&zero, 1, w_len, out);
        line_len += w_len;
        w_len = 3; /* COLUMN 5,7 */
        uint32_t b_addr = b32_to_be(addr);
        fwrite((char*)&b_addr + 1, 1, w_len, out);
        line_len += w_len;
        w_len = 2; /* COLUMN 8,9 */
        fwrite(&zero, 1, w_len, out);
        line_len += w_len;
        w_len = sizeof(uint16_t); /* COLUMN 10,11 */
        fwrite(&len, 1, w_len, out);
        line_len += w_len;
        w_len = 16 - line_len; /* COLUMN 12,15 */
        fwrite(&zero, 1, w_len, out);
        line_len += w_len;
        w_len = 56; /* COLUMN 16,68 */
        fwrite(&tmp[0], 1, w_len, out);
        line_len += w_len;
        w_len = 80 - line_len; /* COLUMN 68,80 */
        fwrite(&zero, 1, w_len, out);
        line_len += w_len;
        addr += 56;
        if (line_len > 80) {
            printf("Line is bigger than 80 (%u)\n", line_len);
            exit(EXIT_FAILURE);
        }
    }

    fclose(in);
    fclose(out);
    return 0;
}
