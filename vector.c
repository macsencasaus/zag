#include <stdio.h>

#include "da.h"
#include "types.h"

#define UNIMPLEMENTED()                                                                  \
    do {                                                                                 \
        fprintf(stderr, "UNIMPLEMENTED: %s:%d in %s()\n", __FILE__, __LINE__, __func__); \
        abort();                                                                         \
    } while (0)

#define UNREACHABLE()                                                                               \
    do {                                                                                            \
        fprintf(stderr, "UNREACHABLE CODE REACHED: %s:%d in %s()\n", __FILE__, __LINE__, __func__); \
        abort();                                                                                    \
    } while (0)

#define INLINE static inline

#define MAX(a, b) ((a) > (b)) ? (a) : (b)

#define SB_IMPLEMENTATION
#include "sb.h"

#define SHT_IMPLEMENTATION
#include "sht.h"

typedef struct {
    u32 line;
    u32 col;
    const char *input_file_path;
} Location;

typedef enum {
    TOKEN_TYPE_ILLEGAL,
    TOKEN_TYPE_EOF,
    TOKEN_TYPE_INT_LITERAL,

    TOKEN_TYPE_IDENT,

    TOKEN_TYPE_PLUS,
    TOKEN_TYPE_MINUS,
    TOKEN_TYPE_ASTERISK,
    TOKEN_TYPE_SLASH,

    TOKEN_TYPE_COMMA,
    TOKEN_TYPE_SEMICOLON,

    TOKEN_TYPE_ASSIGN,

    // keywords
    TOKEN_TYPE_FN,

    TOKEN_TYPE_COUNT,
} Token_Type;

static const char *tt_str[TOKEN_TYPE_COUNT] = {
    [TOKEN_TYPE_ILLEGAL] = "ILLEGAL",
    [TOKEN_TYPE_EOF] = "EOF",
    [TOKEN_TYPE_INT_LITERAL] = "int literal",
    [TOKEN_TYPE_IDENT] = "identifier",
    [TOKEN_TYPE_PLUS] = "'+'",
    [TOKEN_TYPE_MINUS] = "'-'",
    [TOKEN_TYPE_ASTERISK] = "'*'",
    [TOKEN_TYPE_SLASH] = "'/'",
    [TOKEN_TYPE_COMMA] = "','",
    [TOKEN_TYPE_SEMICOLON] = "';'",
    [TOKEN_TYPE_ASSIGN] = "'='",
    [TOKEN_TYPE_FN] = "fn",
};

typedef struct {
    Token_Type type;

    const char *literal;
    usize length;

    Location loc;
} Token;

void inspect_token(const Token *token) {
    printf("TOKEN Type: %13s, %.*s\n", tt_str[token->type], (int)token->length, token->literal);
}

typedef struct {
    const char *input;
    usize input_length;

    usize pos;
    usize read_pos;
    char ch;

    Location loc;
} Lexer;

INLINE void read_char(Lexer *l) {
    if (l->read_pos >= l->input_length) {
        l->ch = 0;
    } else {
        l->ch = l->input[l->read_pos];
    }

    if (l->ch == '\n') {
        ++l->loc.line;
        l->loc.col = 0;
    } else {
        ++l->loc.col;
    }

    l->pos = l->read_pos;
    ++l->read_pos;
}

INLINE void lexer_init(Lexer *l, const char *input_file_path, const char *input, usize length) {
    *l = (Lexer){
        .input = input,
        .input_length = length,
        .read_pos = 0,
        .loc = (Location){
            .line = 1,
            .input_file_path = input_file_path,
        }};

    read_char(l);
}

INLINE bool is_whitespace(char ch) { return ch == ' ' || ch == '\n' || ch == '\t' || ch == 'r'; }
INLINE bool is_digit(char ch) { return ch >= '0' && ch <= '9'; }
INLINE bool valid_ident_char(char ch) { return (ch >= 'A' && ch <= 'Z') ||
                                               (ch >= 'a' && ch <= 'z') ||
                                               ch == '_'; }

INLINE void eat_whitespace(Lexer *l) { for (; is_whitespace(l->ch); read_char(l)); }

Token_Type lookup_keyword(const char *literal, usize n) {
    if (strncmp("fn", literal, MAX(2, n)) == 0) {
        return TOKEN_TYPE_FN;
    } else {
        return TOKEN_TYPE_IDENT;
    }
}

Token lexer_next_token(Lexer *l) {
    eat_whitespace(l);

    Token tok = {
        .literal = l->input + l->pos,
        .length = 1,
        .loc = l->loc,
    };

    switch (l->ch) {
    case '+': {
        tok.type = TOKEN_TYPE_PLUS;
    } break;
    case '-': {
        tok.type = TOKEN_TYPE_MINUS;
    } break;
    case '*': {
        tok.type = TOKEN_TYPE_ASTERISK;
    } break;
    case '/': {
        tok.type = TOKEN_TYPE_SLASH;
    } break;
    case '=': {
        tok.type = TOKEN_TYPE_ASSIGN;
    } break;
    case ',': {
        tok.type = TOKEN_TYPE_COMMA;
    } break;
    case ';': {
        tok.type = TOKEN_TYPE_SEMICOLON;
    } break;
    case 0: {
        tok.type = TOKEN_TYPE_EOF;
    } break;
    default: {
        if (is_digit(l->ch)) {
            read_char(l);
            for (; is_digit(l->ch); read_char(l));
            tok.length = (l->input + l->pos) - tok.literal;
            tok.type = TOKEN_TYPE_INT_LITERAL;
        } else if (valid_ident_char(l->ch)) {
            read_char(l);
            for (; valid_ident_char(l->ch) || is_digit(l->ch); read_char(l));
            tok.length = (l->input + l->pos) - tok.literal;
            tok.type = lookup_keyword(tok.literal, tok.length);
        } else {
            tok.type = TOKEN_TYPE_ILLEGAL;
        }
        return tok;
    }
    }

    read_char(l);
    return tok;
}

typedef usize Type_Id;

typedef enum {
    TYPE_I8,
    TYPE_U8,
    TYPE_I16,
    TYPE_U16,
    TYPE_I32,
    TYPE_U32,
    TYPE_I64,
    TYPE_U64,

    TYPE_F32,
    TYPE_F64,
} Builtin_Type_Id;

typedef enum {
    FLAG_INT_SIGNED,
    FLAG_INT_UNSIGNED,

    FLAG_FLOAT,
} Type_Flag;

typedef struct {
    const char *name;
    Type_Id id;

    usize size;
    u8 alignment;
    Type_Flag flag : 2;
} Type;

INLINE bool strict_type_cmp(const Type *t1, const Type *t2) {
    return t1->id == t2->id;
}

INLINE bool lenient_type_cmp(const Type *t1, const Type *t2) {
    return t1->size == t2->size && t1->alignment == t2->alignment && t1->flag == t2->flag;
}

INLINE bool lenient_sign_type_cmp(const Type *t1, const Type *t2) {
    return t1->size == t2->size && t1->alignment == t2->alignment;
}

static const Type builtin_types[] = {
    {"i8", TYPE_I8, 1, 1, FLAG_INT_SIGNED},
    {"u8", TYPE_U8, 1, 1, FLAG_INT_UNSIGNED},
    {"i16", TYPE_I16, 2, 2, FLAG_INT_SIGNED},
    {"u16", TYPE_U16, 2, 2, FLAG_INT_UNSIGNED},
    {"i32", TYPE_I32, 4, 4, FLAG_INT_SIGNED},
    {"u32", TYPE_U32, 4, 4, FLAG_INT_UNSIGNED},
    {"i64", TYPE_I64, 8, 8, FLAG_INT_SIGNED},
    {"u64", TYPE_U64, 8, 8, FLAG_INT_UNSIGNED},

    {"f32", TYPE_F32, 4, 4, FLAG_FLOAT},
    {"f64", TYPE_F64, 8, 8, FLAG_FLOAT},
};

typedef enum {
    ARG_TYPE_LITERAL,
    ARG_TYPE_VAR,
} Arg_Type;

typedef struct {
    Arg_Type type;

    union {
        u64 value;

        // stack index
        usize index;
    };
} Arg;

typedef struct {
    const Type *type;
    usize stack_index;
} Var;

DYNAMIC_ARRAY_TEMPLATE(Array_Type, Type);

typedef struct {
    Type return_type;
    Array_Type param_types;
} Func;

typedef enum {
    OP_TYPE_ASSIGN,
    OP_TYPE_NEGATE,
    OP_TYPE_BINARY,

    OP_TYPE_COUNT,
} Op_Type;

typedef struct {
    Op_Type type;

    union {
        // decl
        usize size;

        // set
        struct {
            usize index;

            // negate
            Arg arg;
        };

        // binary
        struct {
            Token op;
            Arg lhs;
            Arg rhs;
        };
    };
} Op;

#define OP_DECL(__size) ((Op){.type = OP_TYPE_DECL, .size = __size})

DYNAMIC_ARRAY_TEMPLATE(Op_Buf, Op);
DYNAMIC_ARRAY_TEMPLATE(Scope_Buf, String_Hash_Table);

typedef struct {
    Lexer *l;

    Token cur_token;
    Token peek_token;

    Op_Buf ops;
    usize stack_index;

    String_Hash_Table types;

    // array of string hash tables
    Scope_Buf vars;
    usize scope;

    String_Hash_Table funcs;

    const char *err_msg;
    Location err_loc;
} Compiler;

INLINE void compiler_error(Compiler *c, Location *loc, const char *fmt, ...) {
    va_list vargs;
    va_start(vargs, fmt);

    String_Builder sb = {0};
    sb_vappendf(&sb, fmt, vargs);
    va_end(vargs);

    c->err_msg = sb.store;
    c->err_loc = *loc;
}

INLINE void next_token(Compiler *p) {
    p->cur_token = p->peek_token;
    p->peek_token = lexer_next_token(p->l);
}

INLINE bool cur_tok_is(const Compiler *p, Token_Type tt) { return p->cur_token.type == tt; }
INLINE bool peek_tok_is(const Compiler *p, Token_Type tt) { return p->peek_token.type == tt; }
INLINE bool next_if_peek_tok_is(Compiler *p, Token_Type tt) {
    if (peek_tok_is(p, tt)) {
        next_token(p);
        return true;
    }
    return false;
}
INLINE bool expect_peek(Compiler *p, Token_Type tt) {
    if (!peek_tok_is(p, tt)) {
        compiler_error(p, &p->peek_token.loc, "Expected token type %s, but got %s",
                       tt_str[tt], tt_str[p->peek_token.type]);
        return false;
    }
    next_token(p);
    return true;
}
INLINE void unexpected_token(Compiler *p) {
    compiler_error(p, &p->cur_token.loc, "Unexpected token %s", tt_str[p->cur_token.type]);
}

#define CUR_TOKEN_FMT(p) (int)(p)->cur_token.length, (p)->cur_token.literal

void compiler_init(Compiler *c, Lexer *l) {
    *c = (Compiler){
        .l = l,
    };
    c->cur_token = lexer_next_token(l);
    c->peek_token = lexer_next_token(l);

    sht_init(&c->types, sizeof(Type), 0);

    usize builtin_type_size = sizeof(builtin_types) / sizeof(Type);
    for (usize i = 0; i < builtin_type_size; ++i) {
        const Type *builtin = builtin_types + i;
        Type *t = sht_get(&c->types, builtin->name, strlen(builtin->name));
        assert(t != NULL);
        memcpy(t, builtin, sizeof(Type));
    }

    da_append(&c->vars, (String_Hash_Table){0});
    sht_init(&c->vars.store[0], sizeof(Var), 0);
}

INLINE const Type *lookup_type(const Compiler *p, const Token *token) {
    assert(token->type == TOKEN_TYPE_IDENT);
    return sht_try_get(&p->types, token->literal, token->length);
}

Var *find_scoped_var(const Compiler *c, usize scope, const char *name, usize n) {
    const String_Hash_Table *scope_vars = c->vars.store + scope;
    return (Var *)sht_try_get(scope_vars, name, n);
}

INLINE Var *find_var_near(const Compiler *c, const char *name, usize n) {
    return find_scoped_var(c, c->vars.size - 1, name, n);
}

Var *find_var_far(const Compiler *c, const char *name, usize n) {
    Var *v;
    for (usize i = c->vars.size - 1; i >= 0; --i) {
        if ((v = find_scoped_var(c, i, name, n)) != NULL) {
            return v;
        }
    }
    return NULL;
}

INLINE const Var *declare_var(Compiler *c, const char *name, usize n, const Type *type, usize stack_index) {
    if (find_var_near(c, name, n) != NULL)
        return NULL;
    Var *var = sht_get(c->vars.store + c->vars.size - 1, name, n);
    *var = (Var){type, stack_index};
    return var;
}

INLINE usize alloc_scoped_var(Compiler *c, const Type *type) {
    usize frame = c->stack_index;
    c->stack_index += type->size;
    return frame;
}

typedef bool Compile_Stmt_Fn(Compiler *);
typedef bool Parse_Expr_Fn(Compiler *, Type *);

typedef usize Emit_Fn(Compiler *p, va_list vargs);

static Emit_Fn *const emit_fns[OP_TYPE_COUNT] = {};

usize push_opcode(Compiler *p, Op_Type type, ...) {
    va_list vargs;
    va_start(vargs, type);

    Emit_Fn *emit_fn = emit_fns[type];
    if (emit_fn == NULL)
        UNIMPLEMENTED();
    usize frame = emit_fn(p, vargs);

    va_end(vargs);

    return frame;
}

bool compile_ident(Compiler *c) {
    switch (c->peek_token.type) {
    case TOKEN_TYPE_IDENT: {
        const Type *decl_type = lookup_type(c, &c->cur_token);
        if (decl_type == NULL) {
            compiler_error(c, &c->cur_token.loc, "Unknown type %.*s", CUR_TOKEN_FMT(c));
            return false;
        }

        do {
            usize stack_index = alloc_scoped_var(c, decl_type);

            next_token(c);

            const Token *token = &c->cur_token;
            if (declare_var(c, token->literal, token->length, decl_type, stack_index) == NULL) {
                compiler_error(c, &c->cur_token.loc, "Redefinition of %.*s", CUR_TOKEN_FMT(c));
                return false;
            }

            if (peek_tok_is(c, TOKEN_TYPE_ASSIGN)) {
                next_token(c);
                next_token(c);

                Type expr_type;
                // TODO: parse expression

                if (strict_type_cmp(&expr_type, decl_type)) {
                    compiler_error(c, &c->cur_token.loc, "Type Error: assigning expression of type %s to variable of type %s",
                                   expr_type.name, decl_type->name);
                }
            }
        } while (next_if_peek_tok_is(c, TOKEN_TYPE_COMMA));

        if (!expect_peek(c, TOKEN_TYPE_SEMICOLON))
            return false;

        return true;
    } break;

    default:
        // TODO: expression statement
        return false;
    }
}

static Compile_Stmt_Fn *compile_stmt_fns[TOKEN_TYPE_COUNT] = {
    [TOKEN_TYPE_IDENT] = compile_ident,
};

bool compile_stmt(Compiler *c) {
    Compile_Stmt_Fn *compile_fn = compile_stmt_fns[c->cur_token.type];
    if (compile_fn == NULL) {
        unexpected_token(c);
        return false;
    }
    return compile_fn(c);
}

bool compile_program(Compiler *c) {
    while (!cur_tok_is(c, TOKEN_TYPE_EOF)) {
        if (!compile_stmt(c))
            return false;

        // The compiler ends on the last token of the statement.
        // Must be advanced forward by one to start at the next statement.
        next_token(c);
    }
    return true;
}

#ifndef TESTING

int main(void) {
    char test[] = "i32 x;";

    Lexer l = {0};
    lexer_init(&l, "test", test, sizeof(test));

    Compiler c = {0};
    compiler_init(&c, &l);

    if (!compile_program(&c)) {
        const Location *loc = &c.err_loc;
        fprintf(stderr, "%s:%u:%u: error: %s\n", loc->input_file_path, loc->line, loc->col, c.err_msg);
        return 1;
    }

    return 0;
}

#endif
