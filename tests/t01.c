const char *itostr(int v) {
    return v ? "Not zero!" : "Zero!";
}

extern int printf(const char *fmt, ...);
int main(void) {
    printf("%s & %s & %s\n", itostr(0), itostr(1), itostr(2));
}
