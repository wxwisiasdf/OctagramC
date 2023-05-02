struct token;
struct {
    struct token* tokens;
    int n_tokens;
} ctx;
struct token {
    int a;
};
int f() {
    return ctx.tokens[ctx.n_tokens].a;
}
