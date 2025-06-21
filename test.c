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
    const char tt[] =
        "i32 x;\n"
        "fn i32 main(i32 argc, i8 **argv) {}";

    Token expected[] = {
        {TOKEN_TYPE_IDENT, "i32", 3, {1, 1}},
        {TOKEN_TYPE_IDENT, "x", 1, {1, 5}},
        {TOKEN_TYPE_SEMICOLON, ";", 1, {1, 6}},
        {TOKEN_TYPE_FN, "fn", 2, {2, 1}},
        {TOKEN_TYPE_IDENT, "i32", 3, {2, 4}},
        {TOKEN_TYPE_IDENT, "main", 4, {2, 8}},
        {TOKEN_TYPE_LPAREN, "(", 1, {2, 12}},
        {TOKEN_TYPE_IDENT, "i32", 3, {2, 13}},
        {TOKEN_TYPE_IDENT, "argc", 4, {2, 17}},
        {TOKEN_TYPE_COMMA, ",", 1, {2, 21}},
        {TOKEN_TYPE_IDENT, "i8", 2, {2, 23}},
        {TOKEN_TYPE_ASTERISK, "*", 1, {2, 26}},
        {TOKEN_TYPE_ASTERISK, "*", 1, {2, 27}},
        {TOKEN_TYPE_IDENT, "argv", 4, {2, 28}},
        {TOKEN_TYPE_RPAREN, ")", 1, {2, 32}},
        {TOKEN_TYPE_LBRACE, "{", 1, {2, 34}},
        {TOKEN_TYPE_RBRACE, "}", 1, {2, 35}},
        {TOKEN_TYPE_EOF, "", 1, {2, 36}},
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
    }
}

typedef struct {
    const char *stmts;
    usize stack_size;
} Global_Decl_Test_Case;

UTEST(ir_gen, global_decl) {
    const char input_file_path[] = "test.v";
    Global_Decl_Test_Case tt[] = {
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

    usize n = sizeof(tt) / sizeof(*tt);
    for (usize i = 0; i < n; ++i) {
        const Global_Decl_Test_Case *t = tt + i;

        Lexer l = {0};
        lexer_init(&l, input_file_path, t->stmts, strlen(t->stmts) + 1);
        Compiler c = {0};
        compiler_init(&c, &l);

        ASSERT_TRUE_MSG(compile_program(&c), c.err_msg);

        ASSERT_EQ(c.stack_index, t->stack_size);
    }
}

typedef struct {
    const char *stmts;
    const char *err_msg;
    Location err_loc;
} Global_Decl_Err_Test_Case;

UTEST(ir_gen, global_decl_err) {
    const char input_file_path[] = "test.v";
    Global_Decl_Err_Test_Case tt[] = {
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
        Global_Decl_Err_Test_Case *t = tt + i;

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

struct Func_Test_Case {
    const char *name;
    Type_Id return_type;
    Type_Id *param_types;
    usize params;
};

typedef struct {
    const char *stmts;
    struct Func_Test_Case *func;
    usize funcs;
} Func_Decl_Test_Case;

UTEST(ir_gen, func_decl) {
    const char input_file_path[] = "test.v";
    Func_Decl_Test_Case tt[] = {
        {
            .stmts = "fn i32 id(i32 argc);",
            .func = (struct Func_Test_Case[]){
                {"id", TYPE_I32, (Type_Id[]){TYPE_I32}, 1},
            },
            .funcs = 1,
        },
    };

    usize n = sizeof(tt) / sizeof(*tt);
    for (usize i = 0; i < n; ++i) {
        const Func_Decl_Test_Case *t = tt + i;

        Lexer l = {0};
        lexer_init(&l, input_file_path, t->stmts, strlen(t->stmts) + 1);
        Compiler c = {0};
        compiler_init(&c, &l);

        ASSERT_TRUE_MSG(compile_program(&c), c.err_msg);

        for (usize j = 0; j < t->funcs; ++j) {
            const struct Func_Test_Case *f = t->func + j;
            const Func *func = sht_try_get(&c.funcs, f->name, strlen(f->name));

            ASSERT_NE(func, NULL);

            ASSERT_EQ(f->return_type, func->return_type->id);
            ASSERT_EQ(f->params, func->params.size);
            for (usize k = 0; k < f->params; ++k) {
                ASSERT_EQ(f->param_types[k], func->params.store[k].type->id);
            }
        }
    }
}

UTEST_MAIN()
