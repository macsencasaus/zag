#define ZAG_C

#define _POSIX_C_SOURCE 200809L
#include <libgen.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <wait.h>

#ifndef NDEBUG
#define DA_INIT_CAPACITY 1
#endif
#include "da.h"
#include "types.h"

#ifdef NDEBUG
#define ZAG_ASSERT(expr) ((void)(expr))
#else
#include <assert.h>
#define ZAG_ASSERT(expr) assert(expr)
#endif

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
#define MIN(a, b) ((a) < (b)) ? (a) : (b)

#define CHECK(__expr) \
    if (!(__expr)) return 0

#ifndef NDEBUG
#define SB_INIT_CAPACITY 1
#endif
#define SB_IMPLEMENTATION
#include "sb.h"

#define BUMPALLOC_IMPLEMENTATION
#include "bumpalloc.h"

#define SV_SPREAD(__sv) (__sv).store, (__sv).len
#define SV_FMT(__sv) (int)((__sv).len), (__sv).store

#define SHT_IMPLEMENTATION
#include "sht.h"

#define ARGPARSE_IMPLEMENTATION
#include "argparse.h"

typedef String_View sv;

typedef struct {
    u32 line;
    u32 col;
    const char *input_file_path;
} Location;

typedef enum {
    TOKEN_TYPE_ILLEGAL,
    TOKEN_TYPE_EOF,
    TOKEN_TYPE_INT_LITERAL,

    TOKEN_TYPE_CHAR_LITERAL,
    TOKEN_TYPE_STRING_LITERAL,

    TOKEN_TYPE_IDENT,

    TOKEN_TYPE_LPAREN,
    TOKEN_TYPE_RPAREN,
    TOKEN_TYPE_LBRACE,
    TOKEN_TYPE_RBRACE,
    TOKEN_TYPE_LBRACKET,
    TOKEN_TYPE_RBRACKET,

    TOKEN_TYPE_PLUS,
    TOKEN_TYPE_MINUS,
    TOKEN_TYPE_ASTERISK,
    TOKEN_TYPE_SLASH,
    TOKEN_TYPE_PERCENT,

    TOKEN_TYPE_LNOT,
    TOKEN_TYPE_BNOT,

    TOKEN_TYPE_LT,
    TOKEN_TYPE_LTEQ,
    TOKEN_TYPE_GT,
    TOKEN_TYPE_GTEQ,
    TOKEN_TYPE_EQ,
    TOKEN_TYPE_NEQ,

    TOKEN_TYPE_BAND,
    TOKEN_TYPE_BOR,
    TOKEN_TYPE_XOR,

    TOKEN_TYPE_SHL,
    TOKEN_TYPE_SHR,

    TOKEN_TYPE_PLUSPLUS,
    TOKEN_TYPE_MINUSMINUS,

    TOKEN_TYPE_COMMA,
    TOKEN_TYPE_ELLIPSES,
    TOKEN_TYPE_COLON,
    TOKEN_TYPE_SEMICOLON,

    TOKEN_TYPE_ASSIGN,

    // keywords
    TOKEN_TYPE_FN,
    TOKEN_TYPE_RETURN,
    TOKEN_TYPE_VAR,
    TOKEN_TYPE_IF,
    TOKEN_TYPE_ELSE,

    TOKEN_TYPE_WHILE,
    TOKEN_TYPE_BREAK,
    TOKEN_TYPE_CONTINUE,

    TOKEN_TYPE_EXTERN,

    TOKEN_TYPE_COUNT,
} Token_Type;

static const char *tt_str[TOKEN_TYPE_COUNT] = {
    [TOKEN_TYPE_ILLEGAL] = "ILLEGAL",
    [TOKEN_TYPE_EOF] = "EOF",
    [TOKEN_TYPE_INT_LITERAL] = "int literal",
    [TOKEN_TYPE_CHAR_LITERAL] = "char literal",
    [TOKEN_TYPE_STRING_LITERAL] = "string literal",
    [TOKEN_TYPE_IDENT] = "identifier",
    [TOKEN_TYPE_LPAREN] = "'('",
    [TOKEN_TYPE_RPAREN] = "')'",
    [TOKEN_TYPE_LBRACE] = "'{'",
    [TOKEN_TYPE_RBRACE] = "'}'",
    [TOKEN_TYPE_LBRACKET] = "'['",
    [TOKEN_TYPE_RBRACKET] = "']'",
    [TOKEN_TYPE_PLUS] = "'+'",
    [TOKEN_TYPE_MINUS] = "'-'",
    [TOKEN_TYPE_ASTERISK] = "'*'",
    [TOKEN_TYPE_SLASH] = "'/'",
    [TOKEN_TYPE_PERCENT] = "'%'",
    [TOKEN_TYPE_LNOT] = "'!'",
    [TOKEN_TYPE_BNOT] = "'~'",
    [TOKEN_TYPE_LT] = "'<'",
    [TOKEN_TYPE_LTEQ] = "'<='",
    [TOKEN_TYPE_GT] = "'>'",
    [TOKEN_TYPE_GTEQ] = "'>='",
    [TOKEN_TYPE_EQ] = "'=='",
    [TOKEN_TYPE_NEQ] = "'!='",
    [TOKEN_TYPE_BAND] = "'&'",
    [TOKEN_TYPE_BOR] = "'|'",
    [TOKEN_TYPE_XOR] = "'^'",
    [TOKEN_TYPE_SHL] = "'<<'",
    [TOKEN_TYPE_SHR] = "'>>'",
    [TOKEN_TYPE_PLUSPLUS] = "'++'",
    [TOKEN_TYPE_MINUSMINUS] = "'--'",
    [TOKEN_TYPE_COMMA] = "','",
    [TOKEN_TYPE_COLON] = "':'",
    [TOKEN_TYPE_ELLIPSES] = "'...'",
    [TOKEN_TYPE_SEMICOLON] = "';'",
    [TOKEN_TYPE_ASSIGN] = "'='",
    [TOKEN_TYPE_FN] = "fn",
    [TOKEN_TYPE_RETURN] = "return",
    [TOKEN_TYPE_VAR] = "var",
    [TOKEN_TYPE_IF] = "if",
    [TOKEN_TYPE_ELSE] = "else",
    [TOKEN_TYPE_WHILE] = "while",
    [TOKEN_TYPE_BREAK] = "break",
    [TOKEN_TYPE_CONTINUE] = "continue",
    [TOKEN_TYPE_EXTERN] = "extern",
};

typedef struct {
    Token_Type type;

    // literal
    sv lit;

    Location loc;
} Token;

#define TOKEN_FMT(t) SV_FMT((t)->lit)
#define CUR_TOKEN_FMT(c) TOKEN_FMT(&(c)->cur_token)

void inspect_token(const Token *token) {
    printf("TOKEN Type: %13s, %.*s\n", tt_str[token->type], TOKEN_FMT(token));
}

typedef struct {
    const char *input;
    usize input_length;

    const char *input_file;

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
        .input_file = input_file_path,
        .read_pos = 0,
        .loc = (Location){
            .line = 1,
            .input_file_path = input_file_path,
        }};

    read_char(l);
}

INLINE bool is_whitespace(char ch) { return ch == ' ' || ch == '\n' || ch == '\t' || ch == '\r'; }
INLINE bool is_digit(char ch) { return ch >= '0' && ch <= '9'; }
INLINE bool valid_ident_char(char ch) { return (ch >= 'A' && ch <= 'Z') ||
                                               (ch >= 'a' && ch <= 'z') ||
                                               ch == '_'; }

INLINE void eat_whitespace(Lexer *l) { for (; is_whitespace(l->ch); read_char(l)); }

Token_Type lookup_keyword(sv lit) {
    if (sveq(lit, sv_from_cstr("fn")))
        return TOKEN_TYPE_FN;
    else if (sveq(lit, sv_from_cstr("return")))
        return TOKEN_TYPE_RETURN;
    else if (sveq(lit, sv_from_cstr("var")))
        return TOKEN_TYPE_VAR;
    else if (sveq(lit, sv_from_cstr("if")))
        return TOKEN_TYPE_IF;
    else if (sveq(lit, sv_from_cstr("else")))
        return TOKEN_TYPE_ELSE;
    else if (sveq(lit, sv_from_cstr("while")))
        return TOKEN_TYPE_WHILE;
    else if (sveq(lit, sv_from_cstr("break")))
        return TOKEN_TYPE_BREAK;
    else if (sveq(lit, sv_from_cstr("continue")))
        return TOKEN_TYPE_CONTINUE;
    else if (sveq(lit, sv_from_cstr("extern")))
        return TOKEN_TYPE_EXTERN;
    else
        return TOKEN_TYPE_IDENT;
}

INLINE void read_comment(Lexer *l) {
    for (; l->ch != '\n' && l->ch != 0 && l->ch != (char)EOF; read_char(l));
    eat_whitespace(l);
}

INLINE char peek_char(Lexer *l) {
    if (l->read_pos >= l->input_length) return 0;
    return l->input[l->read_pos];
}

Token lexer_next_token(Lexer *l) {
    eat_whitespace(l);

    while (l->ch == '/' && peek_char(l) == '/') {
        read_comment(l);
        eat_whitespace(l);
    }

    Token tok = {
        .lit = {
            .store = l->input + l->pos,
            .len = 1,
        },
        .loc = l->loc,
    };

    switch (l->ch) {
    case '(': {
        tok.type = TOKEN_TYPE_LPAREN;
    } break;
    case ')': {
        tok.type = TOKEN_TYPE_RPAREN;
    } break;
    case '{': {
        tok.type = TOKEN_TYPE_LBRACE;
    } break;
    case '}': {
        tok.type = TOKEN_TYPE_RBRACE;
    } break;
    case '[': {
        tok.type = TOKEN_TYPE_LBRACKET;
    } break;
    case ']': {
        tok.type = TOKEN_TYPE_RBRACKET;
    } break;
    case '+': {
        if (peek_char(l) == '+') {
            read_char(l);
            tok.lit.len = 2;
            tok.type = TOKEN_TYPE_PLUSPLUS;
        } else {
            tok.type = TOKEN_TYPE_PLUS;
        }
    } break;
    case '-': {
        if (peek_char(l) == '-') {
            read_char(l);
            tok.lit.len = 2;
            tok.type = TOKEN_TYPE_MINUSMINUS;
        } else {
            tok.type = TOKEN_TYPE_MINUS;
        }
    } break;
    case '*': {
        tok.type = TOKEN_TYPE_ASTERISK;
    } break;
    case '/': {
        tok.type = TOKEN_TYPE_SLASH;
    } break;
    case '%': {
        tok.type = TOKEN_TYPE_PERCENT;
    } break;
    case '~': {
        tok.type = TOKEN_TYPE_BNOT;
    } break;
    case '<': {
        if (peek_char(l) == '=') {
            read_char(l);
            tok.lit.len = 2;
            tok.type = TOKEN_TYPE_LTEQ;
        } else if (peek_char(l) == '<') {
            read_char(l);
            tok.lit.len = 2;
            tok.type = TOKEN_TYPE_SHL;
        } else {
            tok.type = TOKEN_TYPE_LT;
        }
    } break;
    case '>': {
        if (peek_char(l) == '=') {
            read_char(l);
            tok.lit.len = 2;
            tok.type = TOKEN_TYPE_GTEQ;
        } else if (peek_char(l) == '>') {
            read_char(l);
            tok.lit.len = 2;
            tok.type = TOKEN_TYPE_SHR;
        } else {
            tok.type = TOKEN_TYPE_GT;
        }
    } break;
    case '=': {
        if (peek_char(l) == '=') {
            read_char(l);
            tok.lit.len = 2;
            tok.type = TOKEN_TYPE_EQ;
        } else {
            tok.type = TOKEN_TYPE_ASSIGN;
        }
    } break;
    case '!': {
        if (peek_char(l) == '=') {
            read_char(l);
            tok.lit.len = 2;
            tok.type = TOKEN_TYPE_NEQ;
        } else {
            tok.type = TOKEN_TYPE_LNOT;
        }
    } break;
    case '&': {
        tok.type = TOKEN_TYPE_BAND;
    } break;
    case '|': {
        tok.type = TOKEN_TYPE_BOR;
    } break;
    case '^': {
        tok.type = TOKEN_TYPE_XOR;
    } break;
    case ',': {
        tok.type = TOKEN_TYPE_COMMA;
    } break;
    case '.': {
        if (strncmp(tok.lit.store, "...", 3) == 0) {
            read_char(l);
            read_char(l);
            tok.lit.len = 3;
            tok.type = TOKEN_TYPE_ELLIPSES;
        } else {
            tok.type = TOKEN_TYPE_ILLEGAL;
        }
    } break;
    case ':': {
        tok.type = TOKEN_TYPE_COLON;
    } break;
    case ';': {
        tok.type = TOKEN_TYPE_SEMICOLON;
    } break;
    case '\'': {
        tok.type = TOKEN_TYPE_CHAR_LITERAL;
        read_char(l);
        for (; l->ch != '\'' && l->ch != 0; read_char(l)) {
            ++tok.lit.len;
            if (l->ch == '\\') {
                read_char(l);
                ++tok.lit.len;
            }
        }
        ++tok.lit.len;
        if (l->ch == 0)
            tok.type = TOKEN_TYPE_ILLEGAL;
    } break;
    case '"': {
        tok.type = TOKEN_TYPE_STRING_LITERAL;
        read_char(l);
        for (; l->ch != '"' && l->ch != 0; read_char(l)) {
            ++tok.lit.len;
            if (l->ch == '\\') {
                read_char(l);
                ++tok.lit.len;
            }
        }
        ++tok.lit.len;
        if (l->ch == 0)
            tok.type = TOKEN_TYPE_ILLEGAL;
    } break;
    case 0: {
        tok.type = TOKEN_TYPE_EOF;
    } break;
    default: {
        if (is_digit(l->ch)) {
            read_char(l);
            for (; is_digit(l->ch); read_char(l));
            tok.lit.len = (l->input + l->pos) - tok.lit.store;
            tok.type = TOKEN_TYPE_INT_LITERAL;
        } else if (valid_ident_char(l->ch)) {
            read_char(l);
            for (; valid_ident_char(l->ch) || is_digit(l->ch); read_char(l));
            tok.lit.len = (l->input + l->pos) - tok.lit.store;
            tok.type = lookup_keyword((sv){tok.lit.store, tok.lit.len});
        } else {
            tok.type = TOKEN_TYPE_ILLEGAL;
        }
        return tok;
    }
    }

    read_char(l);
    return tok;
}

#define MAX_PARAM_COUNT 16

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

    BUILTIN_TYPE_ID_COUNT,
} Builtin_Type_Id;

#define DEFAULT_INT_LITERAL_TYPE_ID TYPE_I32

typedef enum {
    TYPE_KIND_INT_SIGNED,
    TYPE_KIND_INT_UNSIGNED,

    TYPE_KIND_FLOAT,

    TYPE_KIND_FN,
    TYPE_KIND_PTR,

    TYPE_KIND_ARRAY,
} Type_Kind;

typedef struct Type Type;

Array_Template(const Type *, MAX_PARAM_COUNT, Param_Type_Array);

struct Type {
    Type_Id id;

    usize size;
    u8 alignment;

    Type_Kind kind;
    union {
        // function
        struct {
            Param_Type_Array *params;
            const Type *ret;
            bool is_variadic;
        };

        // array
        struct {
            // ptr
            const Type *internal;
            usize len;
        };

        // builtin type
        const char *name;
    };
};

static const Type *default_int_literal_type;

typedef struct {
    const char *name;
    Type type;
} Builtin_Type;

#define BUILTIN_TYPE(__name, __id, __size, __alignment, __kind) \
    {                                                           \
        .name = (__name), .type = {.id = (__id),                \
                                   .size = (__size),            \
                                   .alignment = (__alignment),  \
                                   .kind = (__kind) }           \
    }

static const Builtin_Type builtin_types[] = {
    BUILTIN_TYPE("i8", TYPE_I8, 1, 1, TYPE_KIND_INT_SIGNED),
    BUILTIN_TYPE("u8", TYPE_U8, 1, 1, TYPE_KIND_INT_UNSIGNED),
    BUILTIN_TYPE("i16", TYPE_I16, 2, 2, TYPE_KIND_INT_SIGNED),
    BUILTIN_TYPE("u16", TYPE_U16, 2, 2, TYPE_KIND_INT_UNSIGNED),
    BUILTIN_TYPE("i32", TYPE_I32, 4, 4, TYPE_KIND_INT_SIGNED),
    BUILTIN_TYPE("u32", TYPE_U32, 4, 4, TYPE_KIND_INT_UNSIGNED),
    BUILTIN_TYPE("i64", TYPE_I64, 8, 8, TYPE_KIND_INT_SIGNED),
    BUILTIN_TYPE("u64", TYPE_U64, 8, 8, TYPE_KIND_INT_UNSIGNED),

    BUILTIN_TYPE("f32", TYPE_F32, 4, 4, TYPE_KIND_FLOAT),
    BUILTIN_TYPE("f64", TYPE_F64, 8, 8, TYPE_KIND_FLOAT),
};

typedef struct Op Op;

typedef enum {
    VAR_TYPE_STACK,
    VAR_TYPE_FN,
} Var_Type;

typedef struct {
    sv name;
    usize index;
} Func_Param;

typedef enum {
    VALUE_TYPE_INT_LITERAL,
    VALUE_TYPE_VAR,
    VALUE_TYPE_FUNC,
    VALUE_TYPE_EXTERN_FUNC,

    VALUE_TYPE_INIT_LIST,

    VALUE_TYPE_DEREF,

    VALUE_TYPE_DATA_OFFSET,

    VALUE_TYPE_COUNT,
} Value_Type;

Array_Template(Func_Param, MAX_PARAM_COUNT, Param_Array);

typedef struct Value Value;

struct Value {
    Value_Type value_type;
    const Type *type;

    union {
        // int literal
        u64 int_value;

        // variable, deref
        usize stack_index;

        // function, extern function
        struct {
            sv name;
            Param_Array *params;
            Dynamic_Array(const Op *) ops;
            usize stack_size;
        };

        // initializer list
        Dynamic_Array(const Value *) elems;

        // data offset
        usize offset;
    };
};

typedef enum {
    BINOP_ADD,
    BINOP_SUB,
    BINOP_IMUL,  // signed
    BINOP_MUL,
    BINOP_IDIV,  // signed
    BINOP_DIV,
    BINOP_IMOD,  // signed
    BINOP_MOD,

    BINOP_ULT,
    BINOP_SLT,
    BINOP_ULE,
    BINOP_SLE,
    BINOP_UGT,
    BINOP_SGT,
    BINOP_UGE,
    BINOP_SGE,
    BINOP_EQ,
    BINOP_NE,

    BINOP_AND,
    BINOP_OR,
    BINOP_XOR,

    BINOP_SHL,
    BINOP_ASHR,
    BINOP_LSHR,

    BINOP_COUNT,
} Binop;

Binop lookup_binop(Token_Type tt, bool is_signed) {
    switch (tt) {
    case TOKEN_TYPE_PLUS: return BINOP_ADD;
    case TOKEN_TYPE_MINUS: return BINOP_SUB;
    case TOKEN_TYPE_ASTERISK: return is_signed ? BINOP_IMUL : BINOP_MUL;
    case TOKEN_TYPE_SLASH: return is_signed ? BINOP_IDIV : BINOP_DIV;
    case TOKEN_TYPE_PERCENT: return is_signed ? BINOP_IMOD : BINOP_MOD;
    case TOKEN_TYPE_LT: return is_signed ? BINOP_SLT : BINOP_ULT;
    case TOKEN_TYPE_LTEQ: return is_signed ? BINOP_SLE : BINOP_ULE;
    case TOKEN_TYPE_GT: return is_signed ? BINOP_SGT : BINOP_UGT;
    case TOKEN_TYPE_GTEQ: return is_signed ? BINOP_SGE : BINOP_UGE;
    case TOKEN_TYPE_EQ: return BINOP_EQ;
    case TOKEN_TYPE_NEQ: return BINOP_NE;
    case TOKEN_TYPE_BAND: return BINOP_AND;
    case TOKEN_TYPE_BOR: return BINOP_OR;
    case TOKEN_TYPE_XOR: return BINOP_XOR;
    case TOKEN_TYPE_SHL: return BINOP_SHL;
    case TOKEN_TYPE_SHR: return is_signed ? BINOP_ASHR : BINOP_LSHR;
    default: UNREACHABLE();
    };
}

typedef enum {
    OP_TYPE_ASSIGN,
    OP_TYPE_STORE,
    OP_TYPE_NEG,
    OP_TYPE_BINOP,
    OP_TYPE_RET,
    OP_TYPE_BNOT,
    OP_TYPE_LNOT,
    OP_TYPE_REF,

    OP_TYPE_LABEL,
    OP_TYPE_JMP,
    OP_TYPE_JMPZ,

    OP_TYPE_CALL,

    OP_TYPE_COUNT,
} Op_Type;

Array_Template(const Value *, MAX_PARAM_COUNT, Param_Value_Array);

struct Op {
    Op_Type type;

    // used in almost all Ops to store its result
    usize result;

    union {
        // jmpz
        struct {
            // assign, store, prefix expr, return, ref
            const Value *val;

            // label, jmp
            usize label_id;
        };

        // binary
        struct {
            Binop op;
            const Value *lhs;
            const Value *rhs;
        };

        // call
        struct {
            const Value *func;
            Param_Value_Array *params;
        };
    };
};

typedef enum {
    PREC_LOWEST,
    PREC_ASSIGN,
    PREC_BOR,
    PREC_XOR,
    PREC_BAND,
    PREC_EQ,
    PREC_CMP,
    PREC_SHIFT,
    PREC_ADD,
    PREC_MULT,
    PREC_PREFIX,
} Prec;

static Prec prec_lookup[TOKEN_TYPE_COUNT] = {
    [TOKEN_TYPE_ASSIGN] = PREC_ASSIGN,
    [TOKEN_TYPE_BOR] = PREC_BOR,
    [TOKEN_TYPE_XOR] = PREC_XOR,
    [TOKEN_TYPE_BAND] = PREC_BAND,
    [TOKEN_TYPE_EQ] = PREC_EQ,
    [TOKEN_TYPE_NEQ] = PREC_EQ,
    [TOKEN_TYPE_LT] = PREC_CMP,
    [TOKEN_TYPE_LTEQ] = PREC_CMP,
    [TOKEN_TYPE_GT] = PREC_CMP,
    [TOKEN_TYPE_GTEQ] = PREC_CMP,
    [TOKEN_TYPE_SHL] = PREC_SHIFT,
    [TOKEN_TYPE_SHR] = PREC_SHIFT,
    [TOKEN_TYPE_PLUS] = PREC_ADD,
    [TOKEN_TYPE_MINUS] = PREC_ADD,
    [TOKEN_TYPE_ASTERISK] = PREC_MULT,
    [TOKEN_TYPE_SLASH] = PREC_MULT,
    [TOKEN_TYPE_PERCENT] = PREC_MULT,
};

typedef struct {
    Bump_Alloc type_alloc;
    Dynamic_Array(Type *) types;
    String_Hash_Table named_types;
    usize next_id;
} Types_Ctx;

typedef struct {
    bool in_loop;
    usize loop_label_id;
    usize leave_label_id;
} Loop_Ctx;

typedef struct {
    Lexer *l;

    Token cur_token;
    Token peek_token;

    Types_Ctx ty_ctx;
    Loop_Ctx loop_ctx;

    // Name (string) -> Value
    Dynamic_Array(String_Hash_Table) vars;
    usize scope;

    // current func compiler is working on
    usize stack_index;
    Value *func;

    Dynamic_Array(const Value *) funcs;
    Dynamic_Array(const Value *) extern_funcs;
    String_Builder data;

    Bump_Alloc value_alloc;
    Bump_Alloc op_alloc;

    // fixed arrays not kept in Value and Type
    Bump_Alloc arrays;

    usize next_label_id;

    const char *err_msg;
    Location err_loc;
} Compiler;

INLINE void compiler_error(Compiler *c, const Location *loc, const char *fmt, ...) {
    va_list vargs;
    va_start(vargs, fmt);

    String_Builder sb = {0};
    sb_vappendf(&sb, fmt, vargs);
    va_end(vargs);

    c->err_msg = sb.store;
    c->err_loc = *loc;
}

INLINE const Location *cur_loc(const Compiler *c) {
    return &c->cur_token.loc;
}

INLINE void next_token(Compiler *p) {
    p->cur_token = p->peek_token;
    p->peek_token = lexer_next_token(p->l);
}

INLINE bool cur_tok_is(const Compiler *p, Token_Type tt) { return p->cur_token.type == tt; }
INLINE bool peek_tok_is(const Compiler *p, Token_Type tt) { return p->peek_token.type == tt; }
INLINE bool try_peek_tok(Compiler *p, Token_Type tt) {
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
INLINE bool expect_cur(Compiler *c, Token_Type tt) {
    if (!cur_tok_is(c, tt)) {
        compiler_error(c, &c->peek_token.loc, "Expected token type %s, but got %s",
                       tt_str[tt], tt_str[c->peek_token.type]);
        return false;
    }
    return true;
}
INLINE void unexpected_token(Compiler *p) {
    compiler_error(p, &p->cur_token.loc, "Unexpected token %s", tt_str[p->cur_token.type]);
}
INLINE void unknown_type(Compiler *c) {
    compiler_error(c, &c->cur_token.loc, "Unknown type %.*s", SV_SPREAD(c->cur_token.lit));
}
INLINE void max_param_count_exceeded(Compiler *c) {
    compiler_error(c, cur_loc(c),
                   "Function may only have up to %d parameters",
                   MAX_PARAM_COUNT);
}

void compiler_init(Compiler *c, Lexer *l) {
    *c = (Compiler){
        .l = l,
    };
    c->cur_token = lexer_next_token(l);
    c->peek_token = lexer_next_token(l);

    Types_Ctx *ty_ctx = &c->ty_ctx;
    sht_init(&ty_ctx->named_types, sizeof(const Type *), 0);

    for (usize i = 0; i < ARRAY_SIZE(builtin_types); ++i) {
        const Builtin_Type *builtin = builtin_types + i;

        Type *type = ba_alloc_aligned(&ty_ctx->type_alloc, sizeof(Type), _Alignof(Type));
        da_append(&ty_ctx->types, type);

        *type = builtin->type;
        if (type->id == DEFAULT_INT_LITERAL_TYPE_ID)
            default_int_literal_type = type;
        type->name = builtin->name;

        const Type **t = sht_get(&ty_ctx->named_types, builtin->name, strlen(builtin->name));
        ZAG_ASSERT(t != NULL);

        *t = type;
    }
    ty_ctx->next_id = BUILTIN_TYPE_ID_COUNT;

    da_append(&c->vars, (String_Hash_Table){0});
    sht_init(&c->vars.store[0], sizeof(const Value *), 0);
}

void compiler_destroy(Compiler *c) {
    ba_free(&c->ty_ctx.type_alloc);
    da_delete(&c->ty_ctx.types);
    sht_free(&c->ty_ctx.named_types);

    for (usize i = 0; i < c->vars.size; ++i) {
        String_Hash_Table *sht = da_at(&c->vars, i);
        sht_free(sht);
    }
    da_delete(&c->vars);

    for (usize i = 0; i < c->funcs.size; ++i) {
        Value *f = *(Value **)da_at(&c->funcs, i);
        ZAG_ASSERT(f->value_type == VALUE_TYPE_FUNC);
        da_delete(&f->ops);
    }
    da_delete(&c->funcs);
    da_delete(&c->extern_funcs);
    sb_free(&c->data);

    ba_free(&c->value_alloc);
    ba_free(&c->op_alloc);
    ba_free(&c->arrays);
}

INLINE void push_op(Compiler *c, const Op *op) {
    da_append(&c->func->ops, op);
}

INLINE Type init_fn_type(Compiler *c) {
    Param_Type_Array *params = ba_alloc_aligned(&c->arrays, sizeof(Param_Type_Array), _Alignof(Param_Type_Array));
    return (Type){
        .size = 8,
        .alignment = 8,
        .kind = TYPE_KIND_FN,
        .params = params,
    };
}

INLINE Type init_ptr_type(void) {
    return (Type){
        .size = 8,
        .alignment = 8,
        .kind = TYPE_KIND_PTR,
    };
}

INLINE Value *new_int_literal(Compiler *c, const Type *type, u64 value) {
    Value *v = ba_alloc_aligned(&c->value_alloc, sizeof(Value), _Alignof(Value));
    *v = (Value){
        .value_type = VALUE_TYPE_INT_LITERAL,
        .type = type,
        .int_value = value,
    };
    return v;
}

INLINE Value *new_var(Compiler *c, const Type *type, usize stack_index) {
    Value *v = ba_alloc_aligned(&c->value_alloc, sizeof(Value), _Alignof(Value));
    *v = (Value){
        .value_type = VALUE_TYPE_VAR,
        .type = type,
        .stack_index = stack_index,
    };
    return v;
}

INLINE Value *new_func(Compiler *c) {
    Value *v = ba_alloc_aligned(&c->value_alloc, sizeof(Value), _Alignof(Value));
    *v = (Value){
        .value_type = VALUE_TYPE_FUNC,
        .params = ba_alloc_aligned(&c->arrays, sizeof(Param_Array), _Alignof(Param_Array)),
    };
    return v;
}

INLINE Value *new_extern_func(Compiler *c) {
    Value *v = ba_alloc_aligned(&c->value_alloc, sizeof(Value), _Alignof(Value));
    *v = (Value){
        .value_type = VALUE_TYPE_EXTERN_FUNC,
    };
    return v;
}

INLINE Value *new_deref(Compiler *c, const Type *type, usize stack_index) {
    Value *v = ba_alloc_aligned(&c->value_alloc, sizeof(Value), _Alignof(Value));
    *v = (Value){
        .value_type = VALUE_TYPE_DEREF,
        .type = type,
        .stack_index = stack_index,
    };
    return v;
}

INLINE Value *new_init_list(Compiler *c) {
    Value *v = ba_alloc_aligned(&c->value_alloc, sizeof(Value), _Alignof(Value));
    *v = (Value){
        .value_type = VALUE_TYPE_INIT_LIST,
    };
    return v;
}

INLINE Value *new_data_offset(Compiler *c, const Type *type, usize offset) {
    Value *v = ba_alloc_aligned(&c->value_alloc, sizeof(Value), _Alignof(Value));
    *v = (Value){
        .value_type = VALUE_TYPE_DATA_OFFSET,
        .type = type,
        .offset = offset,
    };
    return v;
}

INLINE Op *new_assign_op(Compiler *c, usize result, const Value *val) {
    Op *o = ba_alloc_aligned(&c->op_alloc, sizeof(Op), _Alignof(Op));
    *o = (Op){
        .type = OP_TYPE_ASSIGN,
        .result = result,
        .val = val,
    };
    return o;
}

INLINE Op *new_store_op(Compiler *c, usize result_addr, const Value *val) {
    Op *o = ba_alloc_aligned(&c->op_alloc, sizeof(Op), _Alignof(Op));
    *o = (Op){
        .type = OP_TYPE_STORE,
        .result = result_addr,
        .val = val,
    };
    return o;
}

INLINE Op *new_prefix_op(Compiler *c, Op_Type type, usize result, const Value *arg) {
    Op *o = ba_alloc_aligned(&c->op_alloc, sizeof(Op), _Alignof(Op));
    *o = (Op){
        .type = type,
        .result = result,
        .val = arg,
    };
    return o;
}

INLINE Op *new_binop(Compiler *c, Binop op, usize result,
                     const Value *lhs, const Value *rhs) {
    Op *o = ba_alloc_aligned(&c->op_alloc, sizeof(Op), _Alignof(Op));
    *o = (Op){
        .type = OP_TYPE_BINOP,
        .result = result,
        .op = op,
        .lhs = lhs,
        .rhs = rhs,
    };
    return o;
}

INLINE Op *new_ret_op(Compiler *c, const Value *val) {
    Op *o = ba_alloc_aligned(&c->op_alloc, sizeof(Op), _Alignof(Op));
    *o = (Op){
        .type = OP_TYPE_RET,
        .val = val,
    };
    return o;
}

INLINE Op *new_label(Compiler *c) {
    Op *o = ba_alloc_aligned(&c->op_alloc, sizeof(Op), _Alignof(Op));
    *o = (Op){
        .type = OP_TYPE_LABEL,
        .label_id = c->next_label_id++,
    };
    return o;
}

INLINE Op *new_jmp(Compiler *c, usize label_id) {
    Op *o = ba_alloc_aligned(&c->op_alloc, sizeof(Op), _Alignof(Op));
    *o = (Op){
        .type = OP_TYPE_JMP,
        .label_id = label_id,
    };
    return o;
}

INLINE Op *new_jmpz(Compiler *c, usize label_id, const Value *val) {
    Op *o = ba_alloc_aligned(&c->op_alloc, sizeof(Op), _Alignof(Op));
    *o = (Op){
        .type = OP_TYPE_JMPZ,
        .val = val,
        .label_id = label_id,
    };
    return o;
}

INLINE Op *new_call(Compiler *c, usize result) {
    Op *o = ba_alloc_aligned(&c->op_alloc, sizeof(Op), _Alignof(Op));
    *o = (Op){
        .type = OP_TYPE_CALL,
        .result = result,
        .params = ba_alloc_aligned(&c->arrays, sizeof(Param_Value_Array), _Alignof(Param_Value_Array)),
    };
    return o;
}

INLINE const Type *lookup_named_type(const Types_Ctx *ty_ctx, sv name) {
    return *(const Type **)sht_try_get(&ty_ctx->named_types, SV_SPREAD(name));
}

bool type_cmp(const Type *t1, const Type *t2) {
    if (t1->kind != t2->kind ||
        t1->size != t2->size ||
        t1->alignment != t2->alignment)
        return false;

    switch (t1->kind) {
    case TYPE_KIND_FN: {
        if (!type_cmp(t1->ret, t2->ret))
            return false;

        if (t1->params->len != t2->params->len)
            return false;

        if (t1->is_variadic != t2->is_variadic)
            return false;

        for (usize i = 0; i < t1->params->len; ++i) {
            const Type *param1 = *at(t1->params, i);
            const Type *param2 = *at(t2->params, i);

            if (!type_cmp(param1, param2))
                return false;
        }
    } break;

    case TYPE_KIND_PTR:
        return type_cmp(t1->internal, t2->internal);

    case TYPE_KIND_ARRAY:
        return t1->len == t2->len && type_cmp(t1->internal, t2->internal);

    default: {
    }
    }

    return true;
}

INLINE bool is_signed(const Type *t) {
    return t->kind == TYPE_KIND_INT_SIGNED || t->kind == TYPE_KIND_FLOAT;
}

const Type *lookup_interned_type(const Types_Ctx *ty_ctx, const Type *local_ty) {
    usize n = ty_ctx->types.size;
    for (usize i = 0; i < n; ++i) {
        const Type *type = *da_at(&ty_ctx->types, i);
        if (type_cmp(type, local_ty))
            return type;
    }

    return NULL;
}

INLINE const Type *new_type(Types_Ctx *ty_ctx, const Type *local_ty) {
    const Type *existing = lookup_interned_type(ty_ctx, local_ty);
    if (existing) return existing;

    Type *t = (Type *)ba_alloc_aligned(&ty_ctx->type_alloc, sizeof(Type), _Alignof(Type));
    *t = *local_ty;
    t->id = ty_ctx->next_id++;
    da_append(&ty_ctx->types, t);
    return t;
}

INLINE const Type *get_ptr_type(Types_Ctx *ty_ctx, const Type *internal) {
    Type local = init_ptr_type();
    local.internal = internal;
    return new_type(ty_ctx, &local);
}

INLINE const Type *make_array_type_with(Types_Ctx *ty_ctx, const Type *internal, usize len) {
    Type local = {
        .kind = TYPE_KIND_ARRAY,
        .size = internal->size * len,
        .alignment = internal->alignment,
        .internal = internal,
        .len = len,
    };
    return new_type(ty_ctx, &local);
}

char *get_type_name(const Type *t) {
    String_Builder type_name = {0};

    if (t->kind != TYPE_KIND_PTR && t->kind != TYPE_KIND_FN) {
        sb_append_cstr(&type_name, t->name);
    }

    else if (t->kind == TYPE_KIND_PTR) {
        sb_append(&type_name, '*');

        char *internal_type_name = get_type_name(t->internal);
        sb_append_cstr(&type_name, internal_type_name);
        free(internal_type_name);
    }

    else if (t->kind == TYPE_KIND_FN) {
        sb_append_cstr(&type_name, "fn(");

        for (usize i = 0; i + 1 < t->params->len; ++i) {
            const Type *param = *at(t->params, i);
            char *param_type_name = get_type_name(param);
            sb_append_cstr(&type_name, param_type_name);
            sb_append_cstr(&type_name, ", ");
            free(param_type_name);
        }

        if (t->params->len) {
            const Type *param = *last(t->params);
            char *param_type_name = get_type_name(param);
            sb_append_cstr(&type_name, param_type_name);
            free(param_type_name);
        }

        sb_append_cstr(&type_name, "): ");

        char *ret_type_name = get_type_name(t->ret);
        sb_append_cstr(&type_name, ret_type_name);
        free(ret_type_name);
    }

    sb_append_null(&type_name);
    return type_name.store;
}

const Type *parse_type(Compiler *c) {
    Types_Ctx *ty_ctx = &c->ty_ctx;

    switch (c->cur_token.type) {
    case TOKEN_TYPE_IDENT: {
        const Type *t;
        if (!(t = lookup_named_type(&c->ty_ctx, c->cur_token.lit))) {
            unknown_type(c);
            return NULL;
        }
        return t;
    } break;

    case TOKEN_TYPE_FN: {
        Type fn_type = init_fn_type(c);

        if (!expect_peek(c, TOKEN_TYPE_LPAREN))
            return NULL;

        next_token(c);

        while (!cur_tok_is(c, TOKEN_TYPE_RPAREN)) {
            const Type *param = parse_type(c);
            if (!param) {
                if (!expect_peek(c, TOKEN_TYPE_COLON))
                    goto parse_type_fn_cleanup;

                next_token(c);
                if (!(param = parse_type(c)))
                    goto parse_type_fn_cleanup;
            }

            if (!can_append(fn_type.params)) {
                max_param_count_exceeded(c);
                return NULL;
            }
            append(fn_type.params, param);

            if (peek_tok_is(c, TOKEN_TYPE_COMMA)) {
                next_token(c);
                next_token(c);
            } else if (!expect_peek(c, TOKEN_TYPE_RPAREN)) {
                goto parse_type_fn_cleanup;
            }
        }

        next_token(c);

        const Type *ret = parse_type(c);
        if (!ret)
            goto parse_type_fn_cleanup;

        fn_type.ret = ret;

        const Type *t;
        if (!(t = lookup_interned_type(ty_ctx, &fn_type)))
            t = new_type(ty_ctx, &fn_type);

        return t;

    parse_type_fn_cleanup:

        return NULL;
    } break;

    case TOKEN_TYPE_ASTERISK: {
        Type ptr_type = init_ptr_type();

        next_token(c);
        const Type *internal = parse_type(c);
        if (!internal) return NULL;
        ptr_type.internal = internal;

        return new_type(ty_ctx, &ptr_type);
    } break;

    case TOKEN_TYPE_LBRACKET: {
        CHECK(expect_peek(c, TOKEN_TYPE_INT_LITERAL));
        usize len = atoll(c->cur_token.lit.store);
        CHECK(expect_peek(c, TOKEN_TYPE_RBRACKET));

        next_token(c);
        const Type *internal = parse_type(c);

        Type array_type = {
            .size = len * internal->size,
            .alignment = internal->alignment,
            .kind = TYPE_KIND_ARRAY,
            .internal = internal,
            .len = len,
        };

        return new_type(ty_ctx, &array_type);
    } break;

    default: {
        compiler_error(c, &c->cur_token.loc, "Unable to parse type, expected %s, %s, or %s",
                       tt_str[TOKEN_TYPE_IDENT], tt_str[TOKEN_TYPE_FN], tt_str[TOKEN_TYPE_ASTERISK]);
        return NULL;
    }
    }

    return NULL;
}

INLINE Prec peek_prec(const Compiler *c) {
    return prec_lookup[c->peek_token.type];
}

Value *find_scoped_var(const Compiler *c, usize scope, sv name) {
    const String_Hash_Table *scope_vars = c->vars.store + scope;
    Value **val = sht_try_get(scope_vars, name.store, name.len);
    if (!val) return NULL;
    return *val;
}

INLINE Value *find_var_near(const Compiler *c, sv name) {
    return find_scoped_var(c, c->vars.size - 1, name);
}

Value *find_var_far(const Compiler *c, sv name) {
    Value *v;
    for (i64 i = (i64)c->vars.size - 1; i >= 0; --i) {
        if ((v = find_scoped_var(c, i, name)) != NULL) {
            return v;
        }
    }
    return NULL;
}

INLINE const Value *declare_var(Compiler *c, sv name, usize offset, const Type *type, bool top_level) {
    if (find_var_near(c, name) != NULL)
        return NULL;
    Value *var = top_level ? new_data_offset(c, type, offset)
                           : new_var(c, type, offset);
    *(Value **)sht_get(c->vars.store + c->vars.size - 1, SV_SPREAD(name)) = var;
    return var;
}

INLINE Value *declare_func(Compiler *c, sv name, const Type *type) {
    if (find_var_near(c, name) != NULL)
        return NULL;
    Value *func = new_func(c);
    func->type = type;
    func->name = name;
    *(Value **)sht_get(c->vars.store, SV_SPREAD(name)) = func;
    da_append(&c->funcs, func);
    return func;
}

INLINE Value *declare_extern_func(Compiler *c, sv name, const Type *type) {
    if (find_var_near(c, name) != NULL)
        return NULL;
    Value *func = new_extern_func(c);
    func->type = type;
    func->name = name;
    *(Value **)sht_get(c->vars.store, SV_SPREAD(name)) = func;
    da_append(&c->extern_funcs, func);
    return func;
}

INLINE const Func_Param *get_param_const(const Value *func, usize param_idx) {
    return at(func->params, param_idx);
}

INLINE Func_Param *get_param(Value *func, usize param_idx) {
    return at(func->params, param_idx);
}

INLINE const Type *get_param_type(const Value *func, usize param_idx) {
    return func->type->params->store[param_idx];
}

INLINE usize alloc_scoped_var(Compiler *c, const Type *type) {
    usize alignment = type->alignment;
    ZAG_ASSERT((alignment > 0) && ((alignment & (alignment - 1)) == 0));

    usize size = type->size;

    c->stack_index = (c->stack_index + alignment - 1) & ~(alignment - 1);

    usize frame = c->stack_index;
    c->stack_index += size;
    c->func->stack_size = MAX(c->stack_index, c->func->stack_size);
    return frame;
}

INLINE usize alloc_data_var(Compiler *c, const Type *type) {
    usize alignment = type->alignment;
    ZAG_ASSERT((alignment > 0) && ((alignment & (alignment - 1)) == 0));

    usize size = type->size;

    usize aligned_data_size = (c->data.size + alignment - 1) & ~(alignment - 1);
    for (usize i = 0; i < aligned_data_size - c->data.size; ++i)
        sb_append_null(&c->data);
    usize frame = c->data.size;

    for (usize i = 0; i < size; ++i)
        sb_append_null(&c->data);

    return frame;
}

INLINE bool is_constant_int(const Value *arg) {
    return arg->value_type == VALUE_TYPE_INT_LITERAL;
}

INLINE bool is_constant(const Value *arg) {
    if (arg->value_type == VALUE_TYPE_INIT_LIST) {
        for (usize i = 0; i < arg->elems.size; ++i) {
            const Value *elem = *da_at(&arg->elems, i);
            if (!is_constant(elem))
                return false;
        }
        return true;
    }
    return is_constant_int(arg);
}

INLINE bool is_integer_type(const Type *t) {
    return t->kind == TYPE_KIND_INT_SIGNED || t->kind == TYPE_KIND_INT_UNSIGNED;
}

INLINE bool coerce_constant_type(Value *arg, const Type *t) {
    if (!t) return true;
    arg->type = t;
    return true;
}

INLINE bool last_op_ret(const Compiler *c) {
    return (*da_last(&c->func->ops))->type == OP_TYPE_RET;
}

INLINE void push_scope(Compiler *c) {
    da_append(&c->vars, (String_Hash_Table){0});
    ++c->scope;
    sht_init(c->vars.store + c->scope, sizeof(const Value *), 0);
}

INLINE void pop_scope(Compiler *c) {
    --c->scope;
    sht_free(da_last(&c->vars));
    da_pop(&c->vars);
}

static u8 one_char_esc_lookup[UINT8_MAX] = {
    ['a'] = '\a',
    ['b'] = '\b',
    ['f'] = '\f',
    ['n'] = '\n',
    ['r'] = '\r',
    ['t'] = '\t',
    ['v'] = '\v',
    ['\\'] = '\\',
    ['\''] = '\'',
    ['"'] = '\"',
};

Value *compile_expr(Compiler *c, Prec prec, const Type *hint, bool *is_lvalue)
    __attribute__((warn_unused_result));

Value *compile_primary_expr(Compiler *c, const Type *hint, bool *is_lvalue) {
    Value *val;
    bool lval;

    Op_Type op_type;
    Token_Type tt;
    switch (tt = c->cur_token.type) {
    case TOKEN_TYPE_INT_LITERAL: {
        const Type *type = hint && is_integer_type(hint)
                               ? hint
                               : default_int_literal_type;

        val = new_int_literal(c, type, atoll(c->cur_token.lit.store));
        lval = false;
    } break;

    case TOKEN_TYPE_CHAR_LITERAL: {
        const Type *type = hint && is_integer_type(hint)
                               ? hint
                               : default_int_literal_type;

        Token *tok = &c->cur_token;
        usize len = tok->lit.len;
        ZAG_ASSERT(len >= 2);

        usize value;

        char ch = tok->lit.store[1];
        switch (ch) {
        case '\'': {
            value = 0;
        } break;
        case '\\': {
            value = 0;
            ZAG_ASSERT(len >= 4);
            char esc = tok->lit.store[2];

            if ((value = one_char_esc_lookup[(usize)esc])) {
                if (len != 4) {
                    printf("len: %zu\n", len);
                    compiler_error(c, &tok->loc, "Multi-character character constant");
                    return NULL;
                }
                break;
            }

            if (len < 5) {
                compiler_error(c, &tok->loc, "Escape sequence \\%c used with no following digits", esc);
                return NULL;
            }

            switch (esc) {
            case 'x': {
                for (usize i = 3; i < len - 1; ++i) {
                    value <<= 4;
                    char v = tok->lit.store[i];
                    switch (v) {
                    case 'A':
                    case 'B':
                    case 'C':
                    case 'D':
                    case 'E':
                    case 'F': {
                        value += v - 'A' + 10;
                    } break;

                    case 'a':
                    case 'b':
                    case 'c':
                    case 'd':
                    case 'e':
                    case 'f': {
                        value += v - 'a' + 10;
                    } break;

                    default: {
                        if (is_digit(v)) {
                            value += v - '0';
                        } else {
                            compiler_error(c, &tok->loc, "Invalid hex digit");
                            return NULL;
                        }
                    }
                    }
                }
            } break;

            default: {
                if (is_digit(esc)) {
                    for (usize i = 2; i < len - 1; ++i) {
                        value <<= 3;
                        char v = tok->lit.store[i];
                        if (!is_digit(v) || v > '7') {
                            compiler_error(c, &tok->loc, "Invalid octal digit");
                            return NULL;
                        }
                        value += v - '0';
                    }
                } else {
                    compiler_error(c, &tok->loc, "Unknown escape character");
                    return NULL;
                }
            }
            }
        } break;
        default: {
            value = ch;
            if (len != 3) {
                compiler_error(c, &tok->loc, "Multi-character character constant");
                return NULL;
            }
        }
        };

        val = new_int_literal(c, type, value);
        lval = false;
    } break;

    case TOKEN_TYPE_STRING_LITERAL: {
        usize offset = c->data.size;
        for (usize i = 1; i < c->cur_token.lit.len - 1; ++i) {
            u8 ch = *da_at(&c->cur_token.lit, i);

            if (ch == '\\') {
                ++i;
                u8 esc = *da_at(&c->cur_token.lit, i);
                u8 value = one_char_esc_lookup[(usize)esc];
                if (value) {
                    sb_append(&c->data, value);
                } else {
                    compiler_error(c, cur_loc(c), "Unkown escape character");
                    return NULL;
                }
            } else {
                sb_append(&c->data, ch);
            }
        }
        sb_append_null(&c->data);
        usize size = c->data.size - offset;

        const Type *u8_type = lookup_named_type(&c->ty_ctx, sv_from_cstr("u8"));
        const Type *array_type = make_array_type_with(&c->ty_ctx, u8_type, size);
        val = new_data_offset(c, array_type, offset);

        lval = true;
    } break;

    case TOKEN_TYPE_IDENT: {
        CHECK(val = find_var_far(c, c->cur_token.lit));
        lval = true;
    } break;

    case TOKEN_TYPE_LBRACKET:
    case TOKEN_TYPE_LBRACE: {
        Location loc = c->cur_token.loc;

        bool has_type = false;

        bool has_len = false;
        usize len;
        const Type *internal;

        if ((has_type = tt == TOKEN_TYPE_LBRACKET)) {
            if ((has_len = try_peek_tok(c, TOKEN_TYPE_INT_LITERAL)))
                len = atoll(c->cur_token.lit.store);
            CHECK(expect_peek(c, TOKEN_TYPE_RBRACKET));
            next_token(c);
            internal = parse_type(c);
            CHECK(expect_peek(c, TOKEN_TYPE_LBRACE));
        }

        if (!has_type && (!hint || hint->kind != TYPE_KIND_ARRAY)) {
            compiler_error(c, cur_loc(c), "Insufficent context to infer type of array initializer");
            return NULL;
        }

        if (!has_type)
            internal = hint->internal;

        val = new_init_list(c);

        next_token(c);
        while (!cur_tok_is(c, TOKEN_TYPE_RBRACE)) {
            const Value *elem = compile_expr(c, PREC_LOWEST, internal, NULL);
            CHECK(elem);

            if (!type_cmp(internal, elem->type)) {
                char *expected_type_name = get_type_name(internal);
                char *actual_type_name = get_type_name(elem->type);

                compiler_error(c, cur_loc(c), "Type Error: expected internal type to be %s, got %s",
                               expected_type_name, actual_type_name);

                free(actual_type_name);
                free(expected_type_name);
                return NULL;
            }

            da_append(&val->elems, elem);

            if (peek_tok_is(c, TOKEN_TYPE_COMMA)) {
                next_token(c);
                next_token(c);
            } else if (!expect_peek(c, TOKEN_TYPE_RBRACE)) {
                return NULL;
            }
        }

        usize n = val->elems.size * sizeof(const Value *);
        void *arr_mem = ba_alloc_aligned(&c->arrays, n, _Alignof(const Value *));
        memcpy(arr_mem, val->elems.store, n);

        da_delete(&val->elems);
        val->elems.store = arr_mem;

        if (has_len && val->elems.size > len) {
            compiler_error(c, &loc, "Initializer list longer than specified array type");
            return NULL;
        }

        if (!has_len)
            len = val->elems.size;

        Type local_type = {
            .size = len * internal->size,
            .alignment = internal->alignment,
            .kind = TYPE_KIND_ARRAY,
            .internal = internal,
            .len = len,
        };

        val->type = new_type(&c->ty_ctx, &local_type);
        lval = false;
    } break;

    case TOKEN_TYPE_LPAREN: {
        next_token(c);
        CHECK(val = compile_expr(c, PREC_LOWEST, hint, &lval));
        CHECK(expect_peek(c, TOKEN_TYPE_RPAREN));
    } break;

    case TOKEN_TYPE_PLUSPLUS: {
        Location loc = c->cur_token.loc;
        next_token(c);

        bool val_is_lval;
        CHECK(val = compile_expr(c, PREC_PREFIX, hint, &val_is_lval));
        if (!val_is_lval) {
            compiler_error(c, &loc, "Cannot increment an rvalue");
            return NULL;
        }

        // TODO: intern constants mayhaps
        Value *one = new_int_literal(c, val->type, 1);

        push_op(c, new_binop(c, BINOP_ADD, val->stack_index, val, one));

        lval = false;
    } break;

    case TOKEN_TYPE_MINUSMINUS: {
        Location loc = c->cur_token.loc;
        next_token(c);

        bool val_is_lval;
        CHECK(val = compile_expr(c, PREC_PREFIX, hint, &val_is_lval));
        if (!val_is_lval) {
            compiler_error(c, &loc, "Cannot decrement an rvalue");
            return NULL;
        }

        Value *one = new_int_literal(c, val->type, 1);

        push_op(c, new_binop(c, BINOP_SUB, val->stack_index, val, one));

        lval = false;
    } break;

    case TOKEN_TYPE_BAND: {
        Location loc = c->cur_token.loc;

        next_token(c);
        Value *arg;
        bool arg_lval;
        CHECK(arg = compile_expr(c, PREC_PREFIX, NULL, &arg_lval));

        if (!arg_lval) {
            compiler_error(c, &loc, "Cannot take the address of an rvalue");
            return NULL;
        }

        const Type *internal = arg->type->kind == TYPE_KIND_ARRAY
                                   ? arg->type->internal
                                   : arg->type;

        const Type *ptr_type = get_ptr_type(&c->ty_ctx, internal);

        usize result = alloc_scoped_var(c, ptr_type);

        if (arg->value_type == VALUE_TYPE_DEREF) {
            val = new_var(c, ptr_type, arg->stack_index);
            lval = false;
            break;
        }

        push_op(c, new_prefix_op(c, OP_TYPE_REF, result, arg));

        val = new_var(c, ptr_type, result);
        lval = false;
    } break;

    case TOKEN_TYPE_ASTERISK: {
        next_token(c);

        Location loc = c->cur_token.loc;
        Value *arg;
        CHECK(arg = compile_expr(c, PREC_PREFIX, NULL, NULL));

        if (arg->type->kind == TYPE_KIND_FN) {
            compiler_error(c, &loc, "Cannot dereference function pointer");
            return NULL;
        } else if (arg->type->kind != TYPE_KIND_PTR) {
            compiler_error(c, &loc, "Cannot dereference non pointer type");
            return NULL;
        }

        usize result = alloc_scoped_var(c, arg->type->internal);
        push_op(c, new_assign_op(c, result, arg));

        val = new_deref(c, arg->type->internal, result);
        lval = true;
    } break;

    // prefix tokens
    case TOKEN_TYPE_LNOT:
        if (tt == TOKEN_TYPE_LNOT) op_type = OP_TYPE_LNOT;
    case TOKEN_TYPE_BNOT:
        if (tt == TOKEN_TYPE_BNOT) op_type = OP_TYPE_BNOT;
    case TOKEN_TYPE_MINUS: {
        if (tt == TOKEN_TYPE_MINUS) op_type = OP_TYPE_NEG;

        next_token(c);

        Value *arg;
        CHECK(arg = compile_expr(c, PREC_PREFIX, hint, NULL));

        if (arg->value_type == VALUE_TYPE_INT_LITERAL) {
            if (op_type == OP_TYPE_LNOT) {
                arg->int_value = !arg->int_value;
            } else if (op_type == OP_TYPE_BNOT) {
                arg->int_value = ~arg->int_value;
            } else {
                arg->int_value = (~arg->int_value) + 1;
            }
            val = arg;
            lval = false;
            break;
        }

        usize result = alloc_scoped_var(c, arg->type);

        push_op(c, new_prefix_op(c, op_type, result, arg));

        val = new_var(c, arg->type, result);
        lval = false;
    } break;

    default: {
        unexpected_token(c);
        return NULL;
    }
    }

    for (;;) {
        switch (c->peek_token.type) {
        case TOKEN_TYPE_PLUSPLUS: {
            next_token(c);
            if (!lval) {
                compiler_error(c, &c->cur_token.loc, "Cannot increment rvalue");
                return NULL;
            }

            usize pre = alloc_scoped_var(c, val->type);
            push_op(c, new_assign_op(c, pre, val));

            Value *one = new_int_literal(c, val->type, 1);

            push_op(c, new_binop(c, BINOP_ADD, val->stack_index, val, one));

            val = new_var(c, val->type, pre);
            lval = false;
        } break;

        case TOKEN_TYPE_MINUSMINUS: {
            next_token(c);
            if (!lval) {
                compiler_error(c, &c->cur_token.loc, "Cannot decrement rvalue");
                return NULL;
            }

            Value *one = new_int_literal(c, val->type, 1);

            usize pre = alloc_scoped_var(c, val->type);
            push_op(c, new_assign_op(c, pre, val));

            push_op(c, new_binop(c, BINOP_SUB, val->stack_index, val, one));

            val = new_var(c, val->type, pre);
            lval = false;
        } break;

        case TOKEN_TYPE_LPAREN: {
            next_token(c);
            if (val->type->kind != TYPE_KIND_FN) {
                compiler_error(c, &c->cur_token.loc, "Called value is not a function");
                return NULL;
            }

            const Type *func_type = val->type;
            Location loc = c->cur_token.loc;

            usize result = alloc_scoped_var(c, val->type->ret);
            Op *call = new_call(c, result);
            call->func = val;

            next_token(c);

            for (usize i = 0; !cur_tok_is(c, TOKEN_TYPE_RPAREN); ++i) {
                const Type *hint;
                bool variadic_param = i >= func_type->params->len;
                if (variadic_param) {
                    if (!func_type->is_variadic) {
                        compiler_error(c, &loc,
                                       "Too many arguments to function call"
                                       ", expected %zu",
                                       func_type->params->len);
                        return NULL;
                    }
                    hint = NULL;
                } else {
                    hint = *da_at(func_type->params, i);
                }

                const Value *val = compile_expr(c, PREC_LOWEST, hint, NULL);

                if (!variadic_param) {
                    const Type *expected_type = *da_at(func_type->params, i);
                    if (!type_cmp(expected_type, val->type)) {
                        char *expected_type_name = get_type_name(expected_type);
                        char *actual_type_name = get_type_name(val->type);

                        compiler_error(c, &loc,
                                       "Type Error: expected argument %zu of "
                                       "function call to be %s, got %s",
                                       i + 1, expected_type_name, actual_type_name);

                        free(actual_type_name);
                        free(expected_type_name);
                        return NULL;
                    }
                }

                append(call->params, val);

                if (peek_tok_is(c, TOKEN_TYPE_COMMA)) {
                    next_token(c);
                    next_token(c);
                } else {
                    CHECK(expect_peek(c, TOKEN_TYPE_RPAREN));
                }
            }

            if (call->params->len < func_type->params->len) {
                if (func_type->is_variadic) {
                    compiler_error(c, &loc,
                                   "Too few arguments to function call"
                                   ", requires at least %zu",
                                   val->params->len);
                } else {
                    compiler_error(c, &loc,
                                   "Too few arguments to function call"
                                   ", expected %zu",
                                   val->params->len);
                }
                return NULL;
            }

            push_op(c, call);

            val = new_var(c, val->type->ret, result);
            lval = false;
        } break;

        case TOKEN_TYPE_LBRACKET: {
            const Type *u64_type = lookup_named_type(&c->ty_ctx, sv_from_cstr("u64"));
            Location loc = c->cur_token.loc;

            next_token(c);
            next_token(c);
            if (val->type->kind != TYPE_KIND_PTR && val->type->kind != TYPE_KIND_ARRAY) {
                compiler_error(c, &c->cur_token.loc, "Indexed value is not a pointer nor an array");
                return NULL;
            }

            Value *index = compile_expr(c, PREC_LOWEST, u64_type, NULL);
            CHECK(index);

            CHECK(expect_peek(c, TOKEN_TYPE_RBRACKET));

            if (!is_integer_type(index->type)) {
                compiler_error(c, &loc, "Type Error: invalid index type, expected an integer type");
                return NULL;
            }

            const Type *result_type = val->type->internal;

            if (val->type->kind == TYPE_KIND_ARRAY) {
                if (!lval) {
                    compiler_error(c, &loc, "Cannot a rvalue");
                    return NULL;
                }
                usize temp = alloc_scoped_var(c, get_ptr_type(&c->ty_ctx, result_type));
                push_op(c, new_prefix_op(c, OP_TYPE_REF, temp, val));
                val = new_var(c, get_ptr_type(&c->ty_ctx, result_type), temp);
            }

            usize add = alloc_scoped_var(c, val->type);

            if (index->value_type == VALUE_TYPE_INT_LITERAL) {
                index->int_value *= result_type->size;
                push_op(c, new_binop(c, BINOP_ADD, add, val, index));
            } else {
                usize temp = alloc_scoped_var(c, val->type);
                push_op(c, new_binop(c, BINOP_MUL, temp, index,
                                     new_int_literal(c, index->type, result_type->size)));
                push_op(c, new_binop(c, BINOP_ADD, add, val, new_var(c, index->type, temp)));
            }

            val = new_deref(c, result_type, add);
            lval = true;
        } break;

        default: goto compile_primary_expr_end_loop;
        }
    }

compile_primary_expr_end_loop:

    if (is_lvalue)
        *is_lvalue = lval;

    return val;
}

Value *compile_binary_expr(Compiler *c, const Value *left, bool left_is_lval) {
    Value *val;

    Token op = c->cur_token;
    next_token(c);

    switch (op.type) {
    case TOKEN_TYPE_ASSIGN: {
        if (!left_is_lval) {
            compiler_error(c, &op.loc, "Cannot assign rvalue");
            return NULL;
        }

        // - 1 on precedence to make assign right associative
        CHECK(val = compile_expr(c, PREC_ASSIGN - 1, left->type, NULL));

        if (!type_cmp(left->type, val->type)) {
            char *left_type_name = get_type_name(left->type),
                 *right_type_name = get_type_name(val->type);

            compiler_error(c, &op.loc, "Type Error: assigning expression of type '%s' to value of type '%s'",
                           right_type_name, left_type_name);

            free(right_type_name);
            free(left_type_name);
            return NULL;
        }

        if (left->value_type == VALUE_TYPE_DEREF) {
            push_op(c, new_store_op(c, left->stack_index, val));
        } else {
            push_op(c, new_assign_op(c, left->stack_index, val));
        }
    } break;
    case TOKEN_TYPE_PLUS:
    case TOKEN_TYPE_MINUS: {
        if (left->type->kind == TYPE_KIND_PTR) {
            const Type *u64_type = lookup_named_type(&c->ty_ctx, sv_from_cstr("u64"));

            Value *right;
            CHECK(right = compile_expr(c, prec_lookup[op.type], u64_type, NULL));

            usize ptr_internal_type_size = left->type->internal->size;

            // TODO: implement type promotion for all number types
            if (!type_cmp(right->type, u64_type)) {
                compiler_error(c, &op.loc, "Type Error: invalid operand to pointer arithmetic");
                return NULL;
            }

            if (type_cmp(right->type, u64_type)) {
                if (right->value_type == VALUE_TYPE_INT_LITERAL) {
                    right->int_value *= ptr_internal_type_size;

                    usize result = alloc_scoped_var(c, left->type);
                    Binop binop = lookup_binop(op.type, is_signed(left->type));
                    push_op(c, new_binop(c, binop, result, left, right));

                    val = new_var(c, left->type, result);
                } else {
                    usize temp = alloc_scoped_var(c, u64_type);

                    Value *m = new_int_literal(c, u64_type, ptr_internal_type_size);

                    push_op(c, new_binop(c, BINOP_MUL, temp, right, m));

                    right = new_var(c, u64_type, temp);

                    usize result = alloc_scoped_var(c, left->type);
                    Binop binop = lookup_binop(op.type, is_signed(left->type));
                    push_op(c, new_binop(c, binop, result, left, right));

                    val = new_var(c, left->type, result);
                }
            } else if (op.type == TOKEN_TYPE_MINUS && type_cmp(left->type, right->type)) {
                usize temp = alloc_scoped_var(c, u64_type);

                push_op(c, new_binop(c, BINOP_SUB, temp, left, right));

                Value *d = new_var(c, u64_type, temp);
                Value *m = new_int_literal(c, u64_type, ptr_internal_type_size);

                usize result = alloc_scoped_var(c, u64_type);

                push_op(c, new_binop(c, BINOP_DIV, result, d, m));

                val = new_var(c, u64_type, result);
            } else {
                compiler_error(c, &op.loc, "Type Error: invalid operand to pointer arithmetic");
                return NULL;
            }

            return val;
        }
    }
    default: {
        Value *right;
        CHECK(right = compile_expr(c, prec_lookup[op.type], left->type, NULL));

        if (!type_cmp(left->type, right->type)) {
            compiler_error(c, &op.loc, "Type Error: invalid binary operation");
            return NULL;
        }

        usize result = alloc_scoped_var(c, left->type);
        Binop binop = lookup_binop(op.type, left->type);
        push_op(c, new_binop(c, binop, result, left, right));

        val = new_var(c, left->type, result);
    }
    }

    return val;
}

Value *compile_expr(Compiler *c, Prec prec, const Type *hint, bool *is_lvalue) {
    Value *val;
    bool left_is_lval;
    CHECK(val = compile_primary_expr(c, hint, &left_is_lval));

    if (is_lvalue)
        *is_lvalue = left_is_lval;

    while (peek_prec(c) > prec) {
        next_token(c);

        CHECK(val = compile_binary_expr(c, val, left_is_lval));

        if (is_lvalue)
            *is_lvalue = false;
    }

    return val;
}

Value *compile_expr_save_stack(Compiler *c, Prec prec, const Type *hint, bool *is_value) {
    usize saved_stack = c->stack_index;
    Value *res = compile_expr(c, prec, hint, is_value);
    c->stack_index = saved_stack;
    return res;
}

bool compile_stmt(Compiler *c)
    __attribute__((warn_unused_result));

bool compile_block(Compiler *c)
    __attribute__((warn_unused_result));

bool compile_var_stmt(Compiler *c, bool top_level) {
    do {
        CHECK(expect_peek(c, TOKEN_TYPE_IDENT));
        Token name = c->cur_token;

        const Type *decl_type = NULL;
        if (try_peek_tok(c, TOKEN_TYPE_COLON)) {
            next_token(c);

            decl_type = parse_type(c);
            if (decl_type == NULL) {
                compiler_error(c, &c->cur_token.loc, "Unknown type %.*s", CUR_TOKEN_FMT(c));
                return false;
            }
        }

        // can be either stack index or data offset depending on top_level
        usize offset;

        if (peek_tok_is(c, TOKEN_TYPE_ASSIGN)) {
            next_token(c);
            next_token(c);

            Value *arg;
            CHECK(arg = compile_expr_save_stack(c, PREC_LOWEST, decl_type, NULL));

            if (is_constant_int(arg) && !coerce_constant_type(arg, decl_type)) {
                char *decl_type_name = get_type_name(decl_type);
                compiler_error(c, &c->cur_token.loc, "Type Error: Unable to coerce constant to type '%s'",
                               decl_type_name);
                free(decl_type_name);
                return false;
            }

            if (decl_type && !type_cmp(decl_type, arg->type)) {
                char *arg_type_name = get_type_name(arg->type),
                     *decl_type_name = get_type_name(decl_type);

                compiler_error(c, &c->cur_token.loc, "Type Error: assigning expression of type '%s' to variable of type '%s'",
                               arg_type_name, decl_type_name);

                free(decl_type_name);
                free(arg_type_name);
                return false;
            } else {
                decl_type = arg->type;
            }

            if (top_level) {
                offset = alloc_data_var(c, decl_type);
                if (!is_constant(arg)) {
                    compiler_error(c, cur_loc(c), "Top level declarations must be a constant");
                    return false;
                }

                // TODO: make recursive
                if (is_integer_type(arg->type)) {
                    memcpy(da_at(&c->data, offset), &arg->int_value, arg->type->size);
                } else {
                    ZAG_ASSERT(arg->value_type == VALUE_TYPE_INIT_LIST);
                    for (usize i = 0; i < arg->elems.size; ++i) {
                        const Value *elem = *da_at(&arg->elems, i);
                        ZAG_ASSERT(elem->value_type == VALUE_TYPE_INT_LITERAL);
                        usize size = elem->type->size;
                        memcpy(da_at(&c->data, offset + (i * size)), &elem->int_value, size);
                    }
                }
            } else {
                offset = alloc_scoped_var(c, decl_type);
                if (decl_type->kind != TYPE_KIND_ARRAY) {
                    push_op(c, new_assign_op(c, offset, arg));
                } else {
                    const Type *internal = decl_type->internal;
                    for (usize i = 0; i < decl_type->len; ++i) {
                        usize s = offset + (decl_type->len - i - 1) * internal->size;
                        push_op(c, new_assign_op(c, s, *da_at(&arg->elems, i)));
                    }
                }
            }
        } else {
            if (!decl_type) {
                compiler_error(c, &c->cur_token.loc, "Variable declaration must have type");
                return false;
            }
            offset = alloc_scoped_var(c, decl_type);
        }

        if (declare_var(c, name.lit, offset, decl_type, top_level) == NULL) {
            compiler_error(c, &c->cur_token.loc, "Redefinition of %.*s", TOKEN_FMT(&name));
            return false;
        }
    } while (try_peek_tok(c, TOKEN_TYPE_COMMA));

    CHECK(expect_peek(c, TOKEN_TYPE_SEMICOLON));
    return true;
}

bool compile_return_stmt(Compiler *c) {
    next_token(c);
    Value *val;
    const Type *ret_type = c->func->type->ret;

    CHECK(val = compile_expr_save_stack(c, PREC_LOWEST, ret_type, NULL));

    if (is_constant(val) && !coerce_constant_type(val, ret_type)) {
        char *ret_type_name = get_type_name(ret_type);

        compiler_error(c, &c->cur_token.loc, "Type Error: Unable to coerce constant to type '%s'",
                       ret_type_name);

        free(ret_type_name);
        return false;
    }

    const Type *expr_type = val->type;
    if (!type_cmp(ret_type, expr_type)) {
        char *expr_type_name = get_type_name(expr_type),
             *ret_type_name = get_type_name(ret_type);

        compiler_error(c, &c->cur_token.loc,
                       "Type Error: returning expression of type '%s', expected '%s'",
                       expr_type_name, ret_type_name);

        free(ret_type_name);
        free(expr_type_name);
        return false;
    }

    CHECK(expect_peek(c, TOKEN_TYPE_SEMICOLON));

    push_op(c, new_ret_op(c, val));

    return true;
}

bool compile_if_stmt(Compiler *c) {
    next_token(c);
    Value *cond = compile_expr_save_stack(c, PREC_LOWEST, NULL, NULL);
    CHECK(cond);

    Op *else_label = new_label(c);
    push_op(c, new_jmpz(c, else_label->label_id, cond));

    next_token(c);
    CHECK(compile_stmt(c));
    bool last_ret = last_op_ret(c);

    if (try_peek_tok(c, TOKEN_TYPE_ELSE)) {
        Op *success_label = new_label(c);
        if (!last_ret)
            push_op(c, new_jmp(c, success_label->label_id));
        push_op(c, else_label);
        next_token(c);
        CHECK(compile_stmt(c));
        if (!last_ret)
            push_op(c, success_label);
    } else {
        push_op(c, else_label);
    }

    return true;
}

bool compile_block_stmt(Compiler *c) {
    push_scope(c);
    CHECK(compile_block(c));
    pop_scope(c);
    return true;
}

bool compile_while_stmt(Compiler *c) {
    next_token(c);

    const Op *loop = new_label(c);
    push_op(c, loop);

    const Op *leave = new_label(c);

    Value *cond = compile_expr_save_stack(c, PREC_LOWEST, NULL, NULL);
    CHECK(cond);

    push_op(c, new_jmpz(c, leave->label_id, cond));

    next_token(c);

    Loop_Ctx old_loop_ctx = c->loop_ctx;
    c->loop_ctx = (Loop_Ctx){
        .in_loop = true,
        .loop_label_id = loop->label_id,
        .leave_label_id = leave->label_id,
    };

    CHECK(compile_stmt(c));

    c->loop_ctx = old_loop_ctx;

    push_op(c, new_jmp(c, loop->label_id));
    push_op(c, leave);

    return true;
}

bool compile_break_stmt(Compiler *c) {
    if (!c->loop_ctx.in_loop) {
        compiler_error(c, &c->cur_token.loc, "'break' statement not in loop");
        return false;
    }
    push_op(c, new_jmp(c, c->loop_ctx.leave_label_id));
    CHECK(expect_peek(c, TOKEN_TYPE_SEMICOLON));
    return true;
}

bool compile_continue_stmt(Compiler *c) {
    if (!c->loop_ctx.in_loop) {
        compiler_error(c, &c->cur_token.loc, "'continue' statement not in loop");
        return false;
    }
    push_op(c, new_jmp(c, c->loop_ctx.loop_label_id));
    CHECK(expect_peek(c, TOKEN_TYPE_SEMICOLON));
    return true;
}

bool compile_expr_stmt(Compiler *c) {
    CHECK(compile_expr_save_stack(c, PREC_LOWEST, NULL, NULL));
    CHECK(expect_peek(c, TOKEN_TYPE_SEMICOLON));
    return true;
}

bool compile_stmt(Compiler *c) {
    switch (c->cur_token.type) {
    case TOKEN_TYPE_VAR:
        return compile_var_stmt(c, /* top_level */ false);
    case TOKEN_TYPE_RETURN:
        return compile_return_stmt(c);
    case TOKEN_TYPE_IF:
        return compile_if_stmt(c);
    case TOKEN_TYPE_LBRACE:
        return compile_block_stmt(c);
    case TOKEN_TYPE_WHILE:
        return compile_while_stmt(c);
    case TOKEN_TYPE_BREAK:
        return compile_break_stmt(c);
    case TOKEN_TYPE_CONTINUE:
        return compile_continue_stmt(c);
    default:
        return compile_expr_stmt(c);
    }
}

bool compile_block(Compiler *c) {
    next_token(c);
    while (!cur_tok_is(c, TOKEN_TYPE_RBRACE)) {
        CHECK(compile_stmt(c));
        // The compiler ends on the last token of the statement.
        // Must be advanced forward by one to start at the next statement.
        next_token(c);
    }

    return true;
}

bool compile_program(Compiler *c) {
    while (!cur_tok_is(c, TOKEN_TYPE_EOF)) {
        bool is_extern = false;

        // qualifiers
        switch (c->cur_token.type) {
        case TOKEN_TYPE_EXTERN: {
            is_extern = true;
            next_token(c);
        } break;
        default: {
        }
        }

        switch (c->cur_token.type) {
        case TOKEN_TYPE_VAR: {
            CHECK(compile_var_stmt(c, /* top_level */ true));
        } break;

        case TOKEN_TYPE_FN: {
            Value *func = new_func(c);
            Type func_type = init_fn_type(c);

            CHECK(expect_peek(c, TOKEN_TYPE_IDENT));

            Token name = c->cur_token;

            CHECK(expect_peek(c, TOKEN_TYPE_LPAREN));
            next_token(c);
            while (!cur_tok_is(c, TOKEN_TYPE_RPAREN)) {
                if (cur_tok_is(c, TOKEN_TYPE_ELLIPSES)) {
                    if (!is_extern) {
                        compiler_error(c, cur_loc(c), "Variadic function parameters not allowed for non extern declarations");
                        return false;
                    }
                    func_type.is_variadic = true;
                    try_peek_tok(c, TOKEN_TYPE_COMMA);
                    CHECK(expect_peek(c, TOKEN_TYPE_RPAREN));
                    break;
                }

                CHECK(expect_cur(c, TOKEN_TYPE_IDENT));
                Token param_token = c->cur_token;

                CHECK(expect_peek(c, TOKEN_TYPE_COLON));

                next_token(c);
                const Type *param_type = parse_type(c);
                if (!param_type) {
                    return false;
                }

                if (!can_append(func_type.params)) {
                    max_param_count_exceeded(c);
                    return NULL;
                }
                append(func_type.params, param_type);

                for (usize i = 0; i < func->params->len; ++i) {
                    const Func_Param *param = func->params->store + i;
                    if (sveq(param->name, param_token.lit)) {
                        compiler_error(c, &param_token.loc, "Redefinition of %.*s", TOKEN_FMT(&param_token));
                        return false;
                    }
                }

                append(func->params, ((Func_Param){.name = param_token.lit}));

                if (try_peek_tok(c, TOKEN_TYPE_COMMA))
                    next_token(c);
                else
                    CHECK(expect_peek(c, TOKEN_TYPE_RPAREN));
            }

            next_token(c);

            func_type.ret = parse_type(c);
            if (!func_type.ret) {
                unknown_type(c);
                return false;
            }

            bool is_declaration = peek_tok_is(c, TOKEN_TYPE_SEMICOLON);

            if (is_extern && !is_declaration) {
                compiler_error(c, &name.loc, "Defining externed function");
                return false;
            }

            Value *existing_func = find_var_near(c, name.lit);
            const Type *interned_func_type = new_type(&c->ty_ctx, &func_type);

            bool already_declared = existing_func != NULL;
            bool already_defined = false;

            if (already_declared) {
                if (is_extern) {
                    compiler_error(c, &name.loc, "Externed function declared previously");
                    return false;
                }

                already_defined = existing_func->ops.store == NULL;
            } else {
                if (is_extern)
                    existing_func = declare_extern_func(c, name.lit, interned_func_type);
                else
                    existing_func = declare_func(c, name.lit, interned_func_type);

                existing_func->params = func->params;
            }

            if (already_defined && !is_declaration) {
                compiler_error(c, &name.loc, "Redefinition of function %.*s", TOKEN_FMT(&name));
                return false;
            }

            if (already_declared) {
                if (!type_cmp(func_type.ret, existing_func->type->ret)) {
                    compiler_error(c, &name.loc, "Conflicting types for %.*s", TOKEN_FMT(&name));
                    return false;
                }

                if (func->params->len != existing_func->params->len) {
                    compiler_error(c, &name.loc, "Conflicting types for %.*s", TOKEN_FMT(&name));
                    return false;
                }

                usize n = func->params->len;
                for (usize i = 0; i < n; ++i) {
                    if (!type_cmp(*at(func_type.params, i), *at(existing_func->type->params, i))) {
                        compiler_error(c, &name.loc, "Conflicting types for %.*s", TOKEN_FMT(&name));
                        return false;
                    }
                }
            }

            if (is_declaration) {
                next_token(c);
                break;
            }

            CHECK(expect_peek(c, TOKEN_TYPE_LBRACE));

            push_scope(c);
            c->func = existing_func;

            for (usize i = 0; i < existing_func->params->len; ++i) {
                Func_Param *param = get_param(existing_func, i);
                const Type *param_type = get_param_type(existing_func, i);
                usize stack_index = alloc_scoped_var(c, param_type);
                param->index = stack_index;
                ZAG_ASSERT(declare_var(c, param->name, stack_index, param_type, /* top_level */ false) != NULL);
            }

            CHECK(compile_block(c));

            c->stack_index = 0;
            pop_scope(c);
        } break;

        default: {
            unexpected_token(c);
            return false;
        }
        }

        // The compiler ends on the last token of the statement.
        // Must be advanced forward by one to start at the next statement.
        next_token(c);
    }
    return true;
}

#ifndef IR_C
#include "ir.c"
#endif

#ifndef X86_64_LINUX_C
#include "x86_64_linux.c"
#endif

static char *program_name;

static char *read_file(const char *file_name, usize *n) {
    FILE *file = fopen(file_name, "r");
    if (file == NULL) {
        return NULL;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    char *buffer = (char *)malloc(file_size + 1);

    usize bytes_read = fread(buffer, 1, file_size, file);
    if ((long)bytes_read != file_size) {
        fclose(file);
        return NULL;
    }

    buffer[file_size] = 0;
    *n = file_size;

    fclose(file);
    return buffer;
}

static char *read_stdin(usize *n) {
    int c;
    usize cap = 1024;
    usize len = 0;
    char *buffer = (char *)malloc(cap);
    if (buffer == NULL) {
        return NULL;
    }

    while ((c = fgetc(stdin)) != EOF) {
        buffer[len++] = c;

        if (len == cap) {
            cap *= 2;

            char *temp = buffer;
            buffer = realloc(buffer, cap);
            if (buffer == NULL) {
                free(temp);
                return NULL;
            }
        }
    }

    buffer[len] = 0;
    *n = len;

    return buffer;
}

#define STDIN_FILE_NAME "stdin"

typedef enum {
    TARGET_LIST,

    TARGET_NATIVE_X86_64_LINUX,

#ifdef QBE_BUILD
    TARGET_QBE_AMD64,
    TARGET_QBE_ARM64,
#endif

    TARGET_COUNT,
} Target;

#if defined(__x86_64__) && defined(QBE_BUILD)
#define DEFAULT_TARGET TARGET_QBE_AMD64
#elif defined(__aarch64__) && defined(QBE_BUILD)
#define DEFAULT_TARGET TARGET_QBE_ARM64
#else
#define DEFAULT_TARGET TARGET_NATIVE_X86_64_LINUX
#endif

const char *target_options[TARGET_COUNT] = {
    [TARGET_LIST] = "list",

    [TARGET_NATIVE_X86_64_LINUX] = "x86_64-linux",

#ifdef QBE_BUILD
    [TARGET_QBE_AMD64] = "qbe-amd64_sysv",
    [TARGET_QBE_ARM64] = "qbe-arm64",
#endif
};

#ifndef QBE_C
#include "qbe.c"
#endif

int main(int argc, char *argv[]) {
    int err = 0;
    program_name = argv[0];

    argp_init(argc, argv, "Zag compiler", /* default_help */ true);

    Target *target = (Target *)argp_flag_enum("t", "target", target_options,
                                              TARGET_COUNT, DEFAULT_TARGET,
                                              "target platform");
    char **output_file = argp_flag_str("o", "output", "OUTPUT_FILE", NULL, "output file path");
    bool *compile_assemble_only = argp_flag_bool("c", NULL, "compile and assemble but do not link");
#ifdef QBE_BUILD
    bool *emit_asm = argp_flag_bool("S", NULL, "compile to assembly, only availble for qbe targets");
#endif

    char **cc_binary_arg = argp_flag_str(NULL, "cc-binary", "CC_BINARY", NULL,
                                         "path to C compiler binary, defaults to `CC` environment variable then to cc");
    bool *lex = argp_flag_bool(NULL, "lex", "print lexer output then exit");
    bool *emit_ir = argp_flag_bool(NULL, "emit-zag-ir", "print zag intermediate representation then exit");

#ifdef QBE_BUILD
    char **qbe_binary_arg = argp_flag_str(NULL, "qbe-binary", "QBE_BINARY", NULL,
                                          "path to qbe binary, defaults to `ZAG_QBE` environment variable then to qbe");
    bool *emit_qbe_il = argp_flag_bool(NULL, "emit-qbe-il", "print qbe intermediate language then exit");

    char **as_binary_arg = argp_flag_str(NULL, "as-binary", "AS_BINARY", NULL,
                                         "path to as binary, defaults to as");
#endif

    char **file = argp_pos_str("file", NULL, true, "input file, use '-' for stdin");

    if (!argp_parse_args()) {
        argp_print_usage(stderr);
        argp_print_error(stderr);
        return 1;
    }

    if (*target == TARGET_LIST) {
        for (usize i = 0; i < ARRAY_SIZE(target_options); ++i) {
            if (i == TARGET_LIST || !target_options[i])
                continue;

            printf("%-15s", target_options[i]);

            if (i == DEFAULT_TARGET)
                printf(" <- Default\n");
            else
                printf("\n");
        }
        return 0;
    }

    if (*file == NULL) {
        argp_print_usage(stderr);
        fprintf(stderr, "No input file\n");
        return 1;
    }

    static char temp[1024];

    char *input;
    usize input_len;
    char *input_file_path;
    char *input_basename;
    if (strcmp(*file, "-") == 0) {
        input_file_path = STDIN_FILE_NAME;
        input_basename = STDIN_FILE_NAME;
        input = read_stdin(&input_len);
    } else {
        input_file_path = *file;
        strcpy(temp, *file);
        input_basename = basename(temp);
        input = read_file(input_file_path, &input_len);
    }

    if (!input) {
        argp_print_usage(stderr);
        fprintf(stderr, "Error: failed to read from %s\n", input_file_path);
        return 1;
    }

    Lexer l = {0};
    lexer_init(&l, input_file_path, input, input_len);

    Compiler c = {0};

    String_Builder obj_filename = {0};
    String_Builder exe_name = {0};

    FILE *obj_file = NULL;

    if (*lex) {
        Token t;
        do {
            t = lexer_next_token(&l);
            printf("%s: %.*s at %u:%u in %s\n", tt_str[t.type], TOKEN_FMT(&t),
                   t.loc.line, t.loc.col, t.loc.input_file_path);
        } while (t.type != TOKEN_TYPE_EOF);
        goto cleanup;
    }

    compiler_init(&c, &l);

    if (!compile_program(&c)) {
        const Location *loc = &c.err_loc;
        fprintf(stderr, "%s:%u:%u: error: %s\n", loc->input_file_path, loc->line, loc->col, c.err_msg);
        err = 1;
        goto cleanup;
    }

    if (*emit_ir) {
        print_ir_program(&c, stdout);
        goto cleanup;
    }

#ifdef QBE_BUILD
    if (*emit_qbe_il) {
        qbe_generate_il(&c, stdout);
        goto cleanup;
    }

    char *qbe_binary = "qbe";
    if (*qbe_binary_arg) {
        qbe_binary = *qbe_binary_arg;
    } else {
        char *qbe_env = getenv("ZAG_QBE");
        if (qbe_env)
            qbe_binary = qbe_env;
    }

    if (*emit_asm)
        return qbe_generate_asm(&c, qbe_binary, *target, stdout);

    char *as_binary = "as";
    if (*as_binary_arg)
        as_binary = *as_binary_arg;
#endif

    if (*output_file) {
        sb_append_cstr(&obj_filename, *output_file);
        if (!*compile_assemble_only) {
            sb_append_cstr(&obj_filename, ".o");
        }
    } else {
        sb_append_cstr(&obj_filename, input_basename);
        if (obj_filename.size > 4 &&
            strncmp(obj_filename.store + obj_filename.size - 4, ".zag", 4) == 0) {
            sb_pop(&obj_filename, 4);
        }
        sb_append_cstr(&obj_filename, ".o");
    }
    sb_append_null(&obj_filename);

    switch (*target) {
    case TARGET_COUNT:
    case TARGET_LIST: UNREACHABLE();

    case TARGET_NATIVE_X86_64_LINUX: {
        obj_file = fopen(obj_filename.store, "w");
        if (!obj_file) {
            fprintf(stderr, "Error opening %s\n", obj_filename.store);
            err = 1;
            goto cleanup;
        }
        x86_64_generate_program(&c, obj_file);

        fclose(obj_file);
        obj_file = NULL;
    } break;

#ifdef QBE_BUILD

    case TARGET_QBE_AMD64:
    case TARGET_QBE_ARM64: {
        if (qbe_generate(&c, qbe_binary, as_binary, *target, obj_filename.store)) {
            err = 1;
            goto cleanup;
        }
    } break;

#endif
    }

    if (*compile_assemble_only)
        goto cleanup;

    char *cc_binary = "cc";
    if (*cc_binary_arg) {
        cc_binary = *cc_binary_arg;
    } else {
        char *cc_env = getenv("CC");
        if (cc_env)
            cc_binary = cc_env;
    }

    if (*output_file) {
        sb_append_cstr(&exe_name, *output_file);
    } else {
        sb_append_cstr(&exe_name, input_basename);
        if (exe_name.size > 4 &&
            strncmp(exe_name.store + exe_name.size - 4, ".zag", 4) == 0) {
            sb_pop(&exe_name, 4);
        }
    }
    sb_append_null(&exe_name);

    char *args[] = {cc_binary, "-o", exe_name.store, obj_filename.store, NULL};

    pid_t pid = fork();
    if (pid == 0) {
        execvp(cc_binary, args);
        perror("Error: failed to start linker");
        exit(1);
    } else if (pid > 0) {
        int status;
        waitpid(pid, &status, 0);
        if (WIFEXITED(status)) {
            err = status;
        } else {
            fprintf(stderr, "Error: linker failed\n");
            err = 1;
        }
    } else {
        perror("Error: linker fork failed");
        err = 1;
    }

cleanup:
    sb_free(&exe_name);
    sb_free(&obj_filename);
    if (obj_file) fclose(obj_file);

    compiler_destroy(&c);
    free(input);

    return err;
}
