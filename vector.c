#define VECTOR_C

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
#define MIN(a, b) ((a) < (b)) ? (a) : (b)

#define CHECK(__expr) \
    if (!(__expr)) return NULL

#define SB_IMPLEMENTATION
#include "sb.h"

#define BUMPALLOC_IMPLEMENTATION
#include "bumpalloc.h"

typedef String_View sv;

#define SV_SPREAD(__sv) (__sv)->store, (__sv)->len
#define SV_FMT(__sv) (int)((__sv)->len), (__sv)->store

#define SHT_IMPLEMENTATION
#include "sht.h"

#define FLAG_IMPLEMENTATION
#include "flag.h"

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

    TOKEN_TYPE_LPAREN,
    TOKEN_TYPE_RPAREN,
    TOKEN_TYPE_LBRACE,
    TOKEN_TYPE_RBRACE,

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
    TOKEN_TYPE_COLON,
    TOKEN_TYPE_SEMICOLON,

    TOKEN_TYPE_ASSIGN,

    // keywords
    TOKEN_TYPE_FN,
    TOKEN_TYPE_RETURN,
    TOKEN_TYPE_VAR,
    TOKEN_TYPE_IF,
    TOKEN_TYPE_ELSE,

    TOKEN_TYPE_COUNT,
} Token_Type;

static const char *tt_str[TOKEN_TYPE_COUNT] = {
    [TOKEN_TYPE_ILLEGAL] = "ILLEGAL",
    [TOKEN_TYPE_EOF] = "EOF",
    [TOKEN_TYPE_INT_LITERAL] = "int literal",
    [TOKEN_TYPE_IDENT] = "identifier",
    [TOKEN_TYPE_LPAREN] = "'('",
    [TOKEN_TYPE_RPAREN] = "')'",
    [TOKEN_TYPE_LBRACE] = "'{'",
    [TOKEN_TYPE_RBRACE] = "'}'",
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
    [TOKEN_TYPE_SEMICOLON] = "';'",
    [TOKEN_TYPE_ASSIGN] = "'='",
    [TOKEN_TYPE_FN] = "fn",
    [TOKEN_TYPE_RETURN] = "return",
    [TOKEN_TYPE_IF] = "if",
    [TOKEN_TYPE_ELSE] = "else",
    [TOKEN_TYPE_VAR] = "var",
};

typedef struct {
    Token_Type type;

    // literal
    sv lit;

    Location loc;
} Token;

#define TOKEN_FMT(t) SV_FMT(&(t)->lit)
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
    else
        return TOKEN_TYPE_IDENT;
}

INLINE void read_comment(Lexer *l) {
    for (; l->ch != '\n' && l->ch != 0 && l->ch != EOF; read_char(l));
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
    case ':': {
        tok.type = TOKEN_TYPE_COLON;
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

#define DEFAULT_INT_LITERAL_TYPE_ID TYPE_I64

typedef enum {
    TYPE_KIND_INT_SIGNED,
    TYPE_KIND_INT_UNSIGNED,

    TYPE_KIND_FLOAT,

    TYPE_KIND_FN,
    TYPE_KIND_PTR,
} Type_Kind;

typedef struct Type Type;

struct Type {
    Type_Id id;

    usize size;
    u8 alignment;

    Type_Kind kind;
    union {
        // function
        struct {
            Dynamic_Array(const Type *) params;
            const Type *ret;
        };

        // ptr
        const Type *internal;

        // builtin type
        const char *name;
    };
};

#define INIT_FN_TYPE() ((Type){.size = 8, .alignment = 8, .kind = TYPE_KIND_FN})
#define INIT_PTR_TYPE() ((Type){.size = 8, .alignment = 8, .kind = TYPE_KIND_PTR})

static const Type *default_int_literal_type;

typedef struct {
    const char *name;
    Type type;
} Builtin_Type;

#define BUILTIN_TYPE(__name, __id, __size, __alignment, __kind) \
    (Builtin_Type) {                                            \
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

    VALUE_TYPE_DEREF,
} Value_Type;

typedef struct {
    Value_Type value_type;
    const Type *type;

    union {
        // int literal
        u64 int_value;

        // variable, deref
        usize stack_index;

        // function
        struct {
            sv name;
            Dynamic_Array(Func_Param) params;
            Dynamic_Array(const Op *) ops;
            usize stack_size;
        };
    };
} Value;

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

    OP_TYPE_COUNT,
} Op_Type;

struct Op {
    Op_Type type;

    // used in almost all Ops to store its result
    usize result;

    union {
        // jmpz
        struct {
            // assign, store, prefix expr, return, ref, deref
            const Value *val;

            // label, jmp
            usize label_id;
        };

        // binary
        struct {
            Token_Type op;
            const Value *lhs;
            const Value *rhs;
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
    Dynamic_Array(Type) types;
    String_Hash_Table named_types;
    usize next_id;
} Types_Ctx;

typedef struct {
    Lexer *l;

    Token cur_token;
    Token peek_token;

    usize stack_index;

    Types_Ctx ty_ctx;

    // Name (string) -> Value
    Dynamic_Array(String_Hash_Table) vars;
    usize scope;

    // current func compiler is working on
    Value *func;

    // type hint for current expr compiler is working on
    const Type *hint;

    Dynamic_Array(const Value *) funcs;

    Bump_Alloc value_alloc;
    Bump_Alloc op_alloc;

    usize next_label_id;

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
INLINE void unexpected_token(Compiler *p) {
    compiler_error(p, &p->cur_token.loc, "Unexpected token %s", tt_str[p->cur_token.type]);
}
INLINE void unknown_type(Compiler *c) {
    compiler_error(c, &c->cur_token.loc, "Unknown type %.*s", SV_SPREAD(&c->cur_token.lit));
}

void compiler_init(Compiler *c, Lexer *l) {
    *c = (Compiler){
        .l = l,
    };
    c->cur_token = lexer_next_token(l);
    c->peek_token = lexer_next_token(l);

    Types_Ctx *ty_ctx = &c->ty_ctx;
    sht_init(&ty_ctx->named_types, sizeof(const Type *), 0);

    usize builtin_types_size = sizeof(builtin_types) / sizeof(*builtin_types);

    for (usize i = 0; i < builtin_types_size; ++i) {
        const Builtin_Type *builtin = builtin_types + i;

        da_append(&ty_ctx->types, builtin->type);

        Type *type = da_last(&ty_ctx->types);
        if (type->id == DEFAULT_INT_LITERAL_TYPE_ID)
            default_int_literal_type = type;
        type->name = builtin->name;

        const Type **t = sht_get(&ty_ctx->named_types, builtin->name, strlen(builtin->name));
        assert(t != NULL);

        *t = type;
    }
    ty_ctx->next_id = BUILTIN_TYPE_ID_COUNT;

    da_append(&c->vars, (String_Hash_Table){0});
    sht_init(&c->vars.store[0], sizeof(Value), 0);
}

INLINE void push_op(Compiler *c, const Op *op) {
    da_append(&c->func->ops, op);
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

INLINE Op *new_binop(Compiler *c, Token_Type op, usize result,
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

INLINE const Type *lookup_named_type(const Types_Ctx *ty_ctx, sv name) {
    return *(const Type **)sht_try_get(&ty_ctx->named_types, SV_SPREAD(&name));
}

INLINE bool type_cmp(const Type *t1, const Type *t2) {
    if (t1->kind != t2->kind ||
        t1->size != t2->size ||
        t1->alignment != t2->alignment)
        return false;

    switch (t1->kind) {
    case TYPE_KIND_FN: {
        if (!type_cmp(t1->ret, t2->ret))
            return false;

        if (t1->params.size != t2->params.size)
            return false;

        for (usize i = 0; i < t1->params.size; ++i) {
            const Type *param1 = *da_at(&t1->params, i);
            const Type *param2 = *da_at(&t2->params, i);

            if (!type_cmp(param1, param2))
                return false;
        }
    } break;
    case TYPE_KIND_PTR:
        return type_cmp(t1->internal, t2->internal);
    default: {
    }
    }

    return true;
}

const Type *lookup_interned_type(const Types_Ctx *ty_ctx, const Type *local_ty) {
    usize n = ty_ctx->types.size;
    for (usize i = 0; i < n; ++i) {
        const Type *type = da_at(&ty_ctx->types, i);
        if (type_cmp(type, local_ty))
            return type;
    }

    return NULL;
}

INLINE const Type *new_type(Types_Ctx *ty_ctx, const Type *local_ty) {
    const Type *existing = lookup_interned_type(ty_ctx, local_ty);
    if (existing) return existing;

    da_append(&ty_ctx->types, *local_ty);
    Type *t = da_last(&ty_ctx->types);
    t->id = ty_ctx->next_id++;
    return t;
}

INLINE const Type *get_ptr_type(Types_Ctx *ty_ctx, const Type *internal) {
    Type local = INIT_PTR_TYPE();
    local.internal = internal;
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

        for (usize i = 0; i + 1 < t->params.size; ++i) {
            const Type *param = *da_at(&t->params, i);
            char *param_type_name = get_type_name(param);
            sb_append_cstr(&type_name, param_type_name);
            sb_append_cstr(&type_name, ", ");
            free(param_type_name);
        }

        if (t->params.size) {
            const Type *param = *da_last(&t->params);
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
        Type fn_type = INIT_FN_TYPE();

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

            da_append(&fn_type.params, param);

            if (peek_tok_is(c, TOKEN_TYPE_COMMA)) {
                next_token(c);
                next_token(c);
            } else if (!expect_peek(c, TOKEN_TYPE_RPAREN)) {
                goto parse_type_fn_cleanup;
            }
        }

        if (!expect_peek(c, TOKEN_TYPE_COLON))
            goto parse_type_fn_cleanup;
        next_token(c);

        const Type *ret = parse_type(c);
        if (!ret)
            goto parse_type_fn_cleanup;

        fn_type.ret = ret;

        const Type *t;
        if (!(t = lookup_interned_type(ty_ctx, &fn_type)))
            t = new_type(ty_ctx, &fn_type);
        else
            da_delete(&fn_type.params);

        return t;

    parse_type_fn_cleanup:
        da_delete(&fn_type.params);

        return NULL;
    } break;

    case TOKEN_TYPE_ASTERISK: {
        Type ptr_type = INIT_PTR_TYPE();

        next_token(c);
        const Type *internal = parse_type(c);
        if (!internal) return NULL;
        ptr_type.internal = internal;

        return new_type(ty_ctx, &ptr_type);
    } break;
    default:
        compiler_error(c, &c->cur_token.loc, "Unable to parse type, expected %s, %s, or %s",
                       tt_str[TOKEN_TYPE_IDENT], tt_str[TOKEN_TYPE_FN], tt_str[TOKEN_TYPE_ASTERISK]);
        return NULL;
    }

    return NULL;
}

INLINE Prec peek_prec(const Compiler *c) {
    return prec_lookup[c->peek_token.type];
}

Value *find_scoped_var(const Compiler *c, usize scope, const sv *name) {
    const String_Hash_Table *scope_vars = c->vars.store + scope;
    return (Value *)sht_try_get(scope_vars, name->store, name->len);
}

INLINE Value *find_var_near(const Compiler *c, const sv *name) {
    return find_scoped_var(c, c->vars.size - 1, name);
}

Value *find_var_far(const Compiler *c, const sv *name) {
    Value *v;
    for (i64 i = (i64)c->vars.size - 1; i >= 0; --i) {
        if ((v = find_scoped_var(c, i, name)) != NULL) {
            return v;
        }
    }
    return NULL;
}

INLINE const Value *declare_var(Compiler *c, const sv *name, usize stack_index, const Type *type) {
    if (find_var_near(c, name) != NULL)
        return NULL;
    Value *var = sht_get(c->vars.store + c->vars.size - 1, SV_SPREAD(name));
    *var = (Value){
        .value_type = VALUE_TYPE_VAR,
        .type = type,
        .stack_index = stack_index,
    };
    return var;
}

INLINE Value *declare_func(Compiler *c, const sv *name, const Type *type) {
    if (find_var_near(c, name) != NULL)
        return NULL;
    Value *func = sht_get(c->vars.store, SV_SPREAD(name));
    *func = (Value){
        .value_type = VALUE_TYPE_FUNC,
        .type = type,
        .name = *name,
    };
    da_append(&c->funcs, func);
    return func;
}

INLINE Func_Param *get_param(const Value *func, usize param_idx) {
    return func->params.store + param_idx;
}

INLINE const Type *get_param_type(const Value *func, usize param_idx) {
    return func->type->params.store[param_idx];
}

INLINE usize alloc_scoped_var(Compiler *c, const Type *type) {
    usize size = type->size;
    c->stack_index += (size - (c->stack_index % size)) % size;
    usize frame = c->stack_index;
    c->stack_index += size;
    c->func->stack_size = MAX(c->stack_index, c->func->stack_size);
    return frame;
}

INLINE bool is_constant(const Value *arg) {
    return arg->value_type == VALUE_TYPE_INT_LITERAL;
}

INLINE bool is_integer_type(const Type *t) {
    return t->kind == TYPE_KIND_INT_SIGNED || t->kind == TYPE_KIND_INT_UNSIGNED;
}

INLINE bool coerce_constant_type(Value *arg, const Type *t) {
    if (!t) return true;
    arg->type = t;
    return true;
}

INLINE void push_scope(Compiler *c) {
    da_append(&c->vars, (String_Hash_Table){0});
    ++c->scope;
    sht_init(c->vars.store + c->scope, sizeof(Value), 0);
}

INLINE void pop_scope(Compiler *c) {
    --c->scope;
    da_pop(&c->vars);
}

typedef bool Compile_Stmt_Fn(Compiler *);

Value *compile_expr(Compiler *c, Prec prec, bool *is_lvalue)
    __attribute__((warn_unused_result));

Value *compile_primary_expr(Compiler *c, bool *is_lvalue) {
    Value *val;
    bool lval;

    Op_Type op_type;
    Token_Type tt;
    switch (tt = c->cur_token.type) {
    case TOKEN_TYPE_INT_LITERAL: {
        const Type *type = c->hint && is_integer_type(c->hint)
                               ? c->hint
                               : default_int_literal_type;

        val = new_int_literal(c, type, atoll(c->cur_token.lit.store));
        lval = false;
    } break;

    case TOKEN_TYPE_IDENT: {
        CHECK(val = find_var_far(c, &c->cur_token.lit));
        lval = true;
    } break;

    case TOKEN_TYPE_LPAREN: {
        next_token(c);
        CHECK(val = compile_expr(c, PREC_LOWEST, &lval));
        CHECK(expect_peek(c, TOKEN_TYPE_RPAREN));
    } break;

    case TOKEN_TYPE_PLUSPLUS: {
        Location loc = c->cur_token.loc;
        next_token(c);

        bool val_is_lval;
        CHECK(val = compile_expr(c, PREC_PREFIX, &val_is_lval));
        if (!val_is_lval) {
            compiler_error(c, &loc, "Cannot increment an rvalue");
            return false;
        }

        // TODO: intern constants mayhaps
        Value *one = new_int_literal(c, val->type, 1);

        push_op(c, new_binop(c, TOKEN_TYPE_PLUS, val->stack_index, val, one));

        lval = false;
    } break;

    case TOKEN_TYPE_MINUSMINUS: {
        Location loc = c->cur_token.loc;
        next_token(c);

        bool val_is_lval;
        CHECK(val = compile_expr(c, PREC_PREFIX, &val_is_lval));
        if (!val_is_lval) {
            compiler_error(c, &loc, "Cannot decrement an rvalue");
            return false;
        }

        Value *one = new_int_literal(c, val->type, 1);

        push_op(c, new_binop(c, TOKEN_TYPE_MINUS, val->stack_index, val, one));

        lval = false;
    } break;

    case TOKEN_TYPE_BAND: {
        Location loc = c->cur_token.loc;

        next_token(c);
        Value *arg;
        bool arg_lval;
        CHECK(arg = compile_expr(c, PREC_PREFIX, &arg_lval));

        if (!arg_lval) {
            compiler_error(c, &loc, "Cannot take the address of an rvalue");
            return false;
        }

        const Type *ptr_type = get_ptr_type(&c->ty_ctx, arg->type);

        usize result = alloc_scoped_var(c, ptr_type);
        push_op(c, new_prefix_op(c, OP_TYPE_REF, result, arg));

        val = new_var(c, ptr_type, result);
        lval = false;
    } break;

    case TOKEN_TYPE_ASTERISK: {
        next_token(c);

        Location loc = c->cur_token.loc;
        Value *arg;
        CHECK(arg = compile_expr(c, PREC_PREFIX, NULL));

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
        CHECK(arg = compile_expr(c, PREC_PREFIX, NULL));
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
                return false;
            }

            usize pre = alloc_scoped_var(c, val->type);
            push_op(c, new_assign_op(c, pre, val));

            Value *one = new_int_literal(c, val->type, 1);

            push_op(c, new_binop(c, TOKEN_TYPE_PLUS, val->stack_index, val, one));

            val = new_var(c, val->type, pre);
            lval = false;
        } break;

        case TOKEN_TYPE_MINUSMINUS: {
            next_token(c);
            if (!lval) {
                compiler_error(c, &c->cur_token.loc, "Cannot decrement rvalue");
                return false;
            }

            Value *one = new_int_literal(c, val->type, 1);

            usize pre = alloc_scoped_var(c, val->type);
            push_op(c, new_assign_op(c, pre, val));

            push_op(c, new_binop(c, TOKEN_TYPE_MINUS, val->stack_index, val, one));

            val = new_var(c, val->type, pre);
            lval = false;
        } break;

        default:
            goto compile_primary_expr_end_loop;
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

    const Type *old_hint = c->hint;
    c->hint = left->type;

    c->hint = old_hint;

    switch (op.type) {
    case TOKEN_TYPE_ASSIGN: {
        if (!left_is_lval) {
            compiler_error(c, &op.loc, "Cannot assign rvalue");
            return NULL;
        }

        const Type *old_hint = c->hint;
        c->hint = left->type;

        // - 1 on precedence to make assign right associative
        CHECK(val = compile_expr(c, PREC_ASSIGN - 1, NULL));

        c->hint = old_hint;

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

            const Type *old_hint = c->hint;
            c->hint = u64_type;

            Value *right;
            CHECK(right = compile_expr(c, prec_lookup[op.type], NULL));

            c->hint = old_hint;

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
                    push_op(c, new_binop(c, op.type, result, left, right));

                    val = new_var(c, left->type, result);
                } else {
                    usize temp = alloc_scoped_var(c, u64_type);

                    Value *m = new_int_literal(c, u64_type, ptr_internal_type_size);

                    push_op(c, new_binop(c, TOKEN_TYPE_ASTERISK, temp, right, m));

                    right = new_var(c, u64_type, temp);

                    usize result = alloc_scoped_var(c, left->type);
                    push_op(c, new_binop(c, op.type, result, left, right));

                    val = new_var(c, left->type, result);
                }
            } else if (op.type == TOKEN_TYPE_MINUS && type_cmp(left->type, right->type)) {
                usize temp = alloc_scoped_var(c, u64_type);

                push_op(c, new_binop(c, TOKEN_TYPE_MINUS, temp, left, right));

                Value *d = new_var(c, u64_type, temp);
                Value *m = new_int_literal(c, u64_type, ptr_internal_type_size);

                usize result = alloc_scoped_var(c, u64_type);

                push_op(c, new_binop(c, TOKEN_TYPE_SLASH, result, d, m));

                val = new_var(c, u64_type, result);
            } else {
                compiler_error(c, &op.loc, "Type Error: invalid operand to pointer arithmetic");
                return NULL;
            }

            return val;
        }
    }
    default: {
        const Type *old_hint = c->hint;
        c->hint = left->type;

        Value *right;
        CHECK(right = compile_expr(c, prec_lookup[op.type], NULL));

        c->hint = old_hint;

        if (!type_cmp(left->type, right->type)) {
            compiler_error(c, &op.loc, "Type Error: invalid binary operation");
            return NULL;
        }

        usize result = alloc_scoped_var(c, left->type);
        push_op(c, new_binop(c, op.type, result, left, right));

        val = new_var(c, left->type, result);
    }
    }

    return val;
}

Value *compile_expr(Compiler *c, Prec prec, bool *is_lvalue) {
    Value *val;
    bool left_is_lval;
    CHECK(val = compile_primary_expr(c, &left_is_lval));

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

Value *compile_expr_save_stack(Compiler *c, Prec prec, bool *is_value) {
    usize saved_stack = c->stack_index;
    Value *res = compile_expr(c, prec, is_value);
    c->stack_index = saved_stack;
    return res;
}

bool compile_stmt(Compiler *c)
    __attribute__((warn_unused_result));

bool compile_block(Compiler *c)
    __attribute__((warn_unused_result));

bool compile_var_stmt(Compiler *c) {
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

        usize stack_index;

        if (peek_tok_is(c, TOKEN_TYPE_ASSIGN)) {
            next_token(c);
            next_token(c);

            c->hint = decl_type;
            Value *arg;
            CHECK(arg = compile_expr_save_stack(c, PREC_LOWEST, NULL));
            c->hint = NULL;

            if (is_constant(arg) && !coerce_constant_type(arg, decl_type)) {
                char *decl_type_name = get_type_name(decl_type);
                compiler_error(c, &c->cur_token.loc, "Type Error: Unable to coerce constant to type '%s'",
                               decl_type_name);
                free(decl_type_name);
                return false;
            }

            if (decl_type && !type_cmp(arg->type, decl_type)) {
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

            stack_index = alloc_scoped_var(c, decl_type);
            push_op(c, new_assign_op(c, stack_index, arg));
        } else {
            if (!decl_type) {
                compiler_error(c, &c->cur_token.loc, "Variable declaration must have type");
                return false;
            }
            stack_index = alloc_scoped_var(c, decl_type);
        }

        if (declare_var(c, &name.lit, stack_index, decl_type) == NULL) {
            compiler_error(c, &c->cur_token.loc, "Redefinition of %.*s", CUR_TOKEN_FMT(c));
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

    c->hint = ret_type;
    CHECK(val = compile_expr_save_stack(c, PREC_LOWEST, NULL));
    c->hint = NULL;

    if (is_constant(val) && !coerce_constant_type(val, ret_type)) {
        char *ret_type_name = get_type_name(ret_type);

        compiler_error(c, &c->cur_token.loc, "Type Error: Unable to coerce constant to type '%s'",
                       ret_type_name);

        free(ret_type_name);
        return false;
    }

    const Type *expr_type = val->type;
    if (!type_cmp(expr_type, ret_type)) {
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
    Value *cond = compile_expr_save_stack(c, PREC_LOWEST, NULL);

    Op *else_label = new_label(c);
    push_op(c, new_jmpz(c, else_label->label_id, cond));

    next_token(c);
    CHECK(compile_stmt(c));

    if (try_peek_tok(c, TOKEN_TYPE_ELSE)) {
        Op *success_label = new_label(c);
        push_op(c, new_jmp(c, success_label->label_id));
        push_op(c, else_label);
        next_token(c);
        CHECK(compile_stmt(c));
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

bool compile_expr_stmt(Compiler *c) {
    CHECK(compile_expr_save_stack(c, PREC_LOWEST, NULL));
    CHECK(expect_peek(c, TOKEN_TYPE_SEMICOLON));
    return true;
}

static Compile_Stmt_Fn *compile_stmt_fns[TOKEN_TYPE_COUNT] = {
    [TOKEN_TYPE_VAR] = compile_var_stmt,
    [TOKEN_TYPE_RETURN] = compile_return_stmt,
    [TOKEN_TYPE_IF] = compile_if_stmt,
    [TOKEN_TYPE_LBRACE] = compile_block_stmt,
};

bool compile_stmt(Compiler *c) {
    Compile_Stmt_Fn *compile_fn = compile_stmt_fns[c->cur_token.type];
    if (compile_fn == NULL) {
        return compile_expr_stmt(c);
    }
    return compile_fn(c);
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
        switch (c->cur_token.type) {
        case TOKEN_TYPE_IDENT: {
            CHECK(compile_var_stmt(c));
        } break;

        case TOKEN_TYPE_FN: {
            Value *func = new_func(c);
            Type func_type = {
                .size = 8,
                .alignment = 8,
                .kind = TYPE_KIND_FN,
            };

            CHECK(expect_peek(c, TOKEN_TYPE_IDENT));

            Token name = c->cur_token;

            CHECK(expect_peek(c, TOKEN_TYPE_LPAREN));

            if (peek_tok_is(c, TOKEN_TYPE_RPAREN)) {
                next_token(c);
            } else {
                do {
                    CHECK(expect_peek(c, TOKEN_TYPE_IDENT));
                    Token param_token = c->cur_token;

                    CHECK(expect_peek(c, TOKEN_TYPE_COLON));

                    next_token(c);
                    const Type *param_type = parse_type(c);
                    if (!param_type) {
                        return false;
                    }

                    da_append(&func_type.params, param_type);

                    for (usize i = 0; i < func->params.size; ++i) {
                        const Func_Param *param = func->params.store + i;
                        if (sveq(param->name, param_token.lit)) {
                            compiler_error(c, &param_token.loc, "Redefinition of %.*s", TOKEN_FMT(&param_token));
                            return false;
                        }
                    }

                    da_append(&func->params, ((Func_Param){.name = param_token.lit}));
                } while (try_peek_tok(c, TOKEN_TYPE_COMMA));

                CHECK(expect_peek(c, TOKEN_TYPE_RPAREN));
            }

            CHECK(expect_peek(c, TOKEN_TYPE_COLON));
            next_token(c);

            func_type.ret = parse_type(c);
            if (!func_type.ret) {
                unknown_type(c);
                return false;
            }

            bool is_declaration = peek_tok_is(c, TOKEN_TYPE_SEMICOLON);

            Value *existing_func = find_var_near(c, &name.lit);
            const Type *interned_func_type = new_type(&c->ty_ctx, &func_type);

            bool already_declared = existing_func != NULL;
            bool already_defined = false;

            if (already_declared) {
                already_defined = existing_func->ops.store == NULL;
            } else {
                existing_func = declare_func(c, &name.lit, interned_func_type);
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

                if (func->params.size != existing_func->params.size) {
                    compiler_error(c, &name.loc, "Conflicting types for %.*s", TOKEN_FMT(&name));
                    return false;
                }

                usize n = func->params.size;
                for (usize i = 0; i < n; ++i) {
                    if (!type_cmp(func_type.params.store[i], existing_func->type->params.store[i])) {
                        compiler_error(c, &name.loc, "Conflicting types for %.*s", TOKEN_FMT(&name));
                        return false;
                    }
                }
            }

            if (is_declaration) {
                next_token(c);
                return true;
            }

            CHECK(expect_peek(c, TOKEN_TYPE_LBRACE));

            push_scope(c);
            c->func = existing_func;

            for (usize i = 0; i < existing_func->params.size; ++i) {
                Func_Param *param = get_param(existing_func, i);
                const Type *param_type = get_param_type(existing_func, i);
                usize stack_index = alloc_scoped_var(c, get_param_type(existing_func, i));
                param->index = stack_index;
                assert(declare_var(c, &param->name, stack_index, param_type) != NULL);
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

char *generate_out_file(const char *v3_file) {
    String_Builder sb = {0};
    sb_append_cstr(&sb, v3_file);
    if (sb.size > 3 &&
        strncmp(sb.store + sb.size - 3, ".v3", 3) == 0) {
        sb_pop(&sb, 3);
    }
    sb_append_cstr(&sb, ".o");
    sb_append_null(&sb);
    return sb.store;
}

static char *program_name;

void usage(FILE *stream) {
    fprintf(stream, "Usage: %s [OPTIONS] [FILE]\n", program_name);
    fprintf(stream, "OPTIONS:\n");
    flag_print_options(stream);
}

char *read_file(const char *file_name, usize *n) {
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

char *read_stdin(usize *n) {
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

int main(int argc, char *argv[]) {
    program_name = argv[0];

    bool *help = flag_bool("help", false, "Print this help then exit.");
    bool *lex = flag_bool("lex", false, "Print lexer output then exit.");
    bool *emit_ir = flag_bool("emit-ir", false, "Emit IR then exit.");
    bool *to_stdout = flag_bool("stdout", false, "Write on standard output.");

    if (!flag_parse(argc, argv)) {
        usage(stderr);
        flag_print_error(stderr);
        exit(1);
    }

    if (*help) {
        usage(stdout);
        exit(0);
    }

    argc = flag_rest_argc();
    argv = flag_rest_argv();

    char *input;
    char *input_file_path;
    usize input_len;
    if (argc == 0 || (strlen(argv[0]) == 1 && argv[0][0] == '-')) {
        input_file_path = STDIN_FILE_NAME;
        input = read_stdin(&input_len);
    } else {
        input_file_path = argv[0];
        input = read_file(input_file_path, &input_len);
    }

    if (!input) {
        usage(stderr);
        fprintf(stderr, "ERROR: failed to read from %s\n", input_file_path);
        return 0;
    }

    Lexer l = {0};
    lexer_init(&l, input_file_path, input, input_len);

    if (*lex) {
        Token t;

        do {
            t = lexer_next_token(&l);
            printf("%s: %.*s at %u:%u in %s\n", tt_str[t.type], TOKEN_FMT(&t),
                   t.loc.line, t.loc.col, t.loc.input_file_path);
        } while (t.type != TOKEN_TYPE_EOF);
        return 0;
    }

    Compiler c = {0};
    compiler_init(&c, &l);

    if (!compile_program(&c)) {
        const Location *loc = &c.err_loc;
        fprintf(stderr, "%s:%u:%u: error: %s\n", loc->input_file_path, loc->line, loc->col, c.err_msg);
        return 1;
    }

    if (*emit_ir) {
        print_ir_program(&c, stdout);
        return 0;
    }

    char *out_file;
    FILE *out;
    if (*to_stdout) {
        out = stdout;
    } else {
        out_file = generate_out_file(c.l->input_file);
        out = fopen(out_file, "w");
        assert(out);
    }

    generate_program(&c, out);

    if (!*to_stdout) {
        printf("Wrote relocatable object file to %s\n", out_file);
        free(out_file);
    }

    fclose(out);
    free(input);

    return 0;
}
