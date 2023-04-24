char c[100];
int main(int argc, char **argv) {
    c[-1] = 10;
    return 0;
}

extern int printf(const char *, ...);

constexpr int eval_test(int x) {
    return x;
}

struct [[packed]] [[align(eval_test(100))]] [[max_align(16)]] test {
    int memb1;
    short memb2;
    long memb3;
    float memb4;
} t;

enum sus {
    BANANA,
    APPLE,
    ORANGE,
    KINNOW,
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
    return (char)eval_test(10);
}

int mdim[4][4];
int a1(int n) {
    return mdim[n];
}
