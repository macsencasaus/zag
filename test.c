#define TESTING
#include "utest.h"
#include "vector.c"

static char scratch[1024 * 1024];
const char *temp_sprintf(const char *fmt, ...) {
    va_list vargs;
    va_start(vargs, fmt);
    vsprintf(scratch, fmt, vargs);
    va_end(vargs);
    return scratch;
}

UTEST(lexer, correctness) {
    const char input_file_path[] = "test.v";
    const char tt[] = "i32 x;";

    Token expected[] = {
        {TOKEN_TYPE_IDENT, "i32", 3, {1, 1, "test.v"}},
        {TOKEN_TYPE_IDENT, "x", 1, {1, 5, "test.v"}},
        {TOKEN_TYPE_SEMICOLON, ";", 1, {1, 6, "test.v"}},
        {TOKEN_TYPE_EOF, "", 1, {1, 7, "test.v"}},
    };

    usize expected_size = sizeof(expected) / sizeof(Token);

    Lexer l = {0};
    lexer_init(&l, input_file_path, tt, sizeof(tt));

    for (usize i = 0; i < expected_size; ++i) {
        Token actual = lexer_next_token(&l);

        ASSERT_EQ(actual.type, expected[i].type);
        ASSERT_STRNEQ(actual.literal, expected[i].literal, expected[i].length);
        ASSERT_EQ(actual.length, expected[i].length);
        ASSERT_EQ(actual.loc.line, expected[i].loc.line);
        ASSERT_EQ(actual.loc.col, expected[i].loc.col);
        ASSERT_STRNEQ(actual.loc.input_file_path, expected[i].loc.input_file_path, expected[i].length);
    }
}

typedef struct {
    const char *stmts;
    usize stack_size;
} Decl_Test_Case;

UTEST(ir_gen, decl) {
    const char input_file_path[] = "test.v";
    Decl_Test_Case tt[] = {
        {
            .stmts = "i8 a;"
                     "u8 b;"
                     "i16 c;"
                     "u16 d;"
                     "i32 e;"
                     "u32 f;"
                     "i64 g;"
                     "u64 h;",
            .stack_size = 30,
        },
        {
            .stmts = "f32 a;"
                     "f64 b;",
            .stack_size = 12,
        },
        {
            .stmts = "i32 a, b;",
            .stack_size = 8,
        },
    };

    usize n = sizeof(tt) / sizeof(Decl_Test_Case);
    for (usize i = 0; i < n; ++i) {
        const Decl_Test_Case *t = tt + i;

        Lexer l = {0};
        lexer_init(&l, input_file_path, t->stmts, strlen(t->stmts) + 1);
        Compiler c = {0};
        compiler_init(&c, &l);

        ASSERT_TRUE_MSG(compile_program(&c), c.err_msg);

        ASSERT_EQ(c.ops.size, 0);

        ASSERT_EQ(c.stack_index, t->stack_size);
    }
}

typedef struct {
    const char *stmts;
    const char *err_msg;
    Location err_loc;
} Decl_Err_Test_Case;

UTEST(ir_gen, decl_err) {
    const char input_file_path[] = "test.v";
    Decl_Err_Test_Case tt[] = {
        {
            .stmts = "x8 x;",
            .err_msg = "Unknown type x8",
            .err_loc = {1, 1},
        },
        {
            .stmts = "i8 x;\ni8 x;",
            .err_msg = "Redefinition of x",
            .err_loc = {2, 4},
        },
        {
            .stmts = "i8 x",
            .err_msg = "Expected token type ';', but got EOF",
            .err_loc = {1, 5},
        },
    };

    usize n = sizeof(tt) / sizeof(*tt);

    for (usize i = 0; i < n; ++i) {
        Decl_Err_Test_Case *t = tt + i;

        Lexer l = {0};
        lexer_init(&l, input_file_path, t->stmts, strlen(t->stmts) + 1);
        Compiler c = {0};
        compiler_init(&c, &l);

        ASSERT_FALSE(compile_program(&c));
        ASSERT_STREQ(t->err_msg, c.err_msg);
        ASSERT_EQ(t->err_loc.line, c.err_loc.line);
        ASSERT_EQ_MSG(t->err_loc.col, c.err_loc.col, temp_sprintf("Test case: %zu", i));
    }
}

UTEST_MAIN()
