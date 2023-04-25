char c[100];

extern int printf(const char *, ...);

static thread_local char st_tls_test;
static char st_test;
thread_local tls_test;

int callee(void) {
    return (callee)();
}

constexpr int eval_test(int x, int y) {
    return x;
}

typedef unsigned int size_t;

int vla_size = 16;
size_t vla_dynsize(size_t n) {
    return n * n;
}
size_t vla_dynoffset(size_t n) {
    return n + 1;
}
int vla_test1(char vla[vla_dynsize(vla_size)]) {
    return sizeof(vla);
}
int vla_test(char vla[vla_dynsize(vla_size)], size_t offset) {
    c[0] = sizeof(vla);
    return vla[vla_dynoffset(offset / 4)];
}
void array_assign(int a) {
    c[0] = a;
}

struct [[packed]] [[align(eval_test(100, 100))]] [[max_align(16)]] test {
    int memb1;
    short memb2;
    long memb3;
    float memb4;
} t;

enum sus {
    BANANA,
    APPLE,
    ORANGE,
    KINNOW
};

[[nodiscard]] [[unknown_attr]] int dl_limit(int argc, char **argv) {
    enum sus fruit_type = 50;

    if (fruit_type > 10)
        return fruit_type;

    switch (fruit_type) {
    case BANANA:
        break;
    case APPLE:
        break;
    }

    fruit_type = alignof(int);
    fruit_type = sizeof(int);

    c[-1] = 10;
    c[2] = c[5];
    printf("Hello world!\n");
    return eval_test(10, 5);
}

int mdim[4][4];
int a1(int n) {
    return mdim[n];
}

int main(int argc, char **argv) {
    c[-1] = 10;
    dl_limit(argc, argv);
    return 0;
}
