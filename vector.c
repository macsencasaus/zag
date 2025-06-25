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
    if (!(__expr)) return false

#define SB_IMPLEMENTATION
#include "sb.h"

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

    TOKEN_TYPE_COMMA,
    TOKEN_TYPE_COLON,
    TOKEN_TYPE_SEMICOLON,

    TOKEN_TYPE_ASSIGN,

    // keywords
    TOKEN_TYPE_FN,
    TOKEN_TYPE_RETURN,
    TOKEN_TYPE_VAR,

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
    [TOKEN_TYPE_COMMA] = "','",
    [TOKEN_TYPE_COLON] = "':'",
    [TOKEN_TYPE_SEMICOLON] = "';'",
    [TOKEN_TYPE_ASSIGN] = "'='",
    [TOKEN_TYPE_FN] = "fn",
    [TOKEN_TYPE_RETURN] = "return",
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

INLINE bool is_whitespace(char ch) { return ch == ' ' || ch == '\n' || ch == '\t' || ch == '\r'; }
INLINE bool is_digit(char ch) { return ch >= '0' && ch <= '9'; }
INLINE bool valid_ident_char(char ch) { return (ch >= 'A' && ch <= 'Z') ||
                                               (ch >= 'a' && ch <= 'z') ||
                                               ch == '_'; }

INLINE void eat_whitespace(Lexer *l) { for (; is_whitespace(l->ch); read_char(l)); }

Token_Type lookup_keyword(const char *literal, usize n) {
    if (n == 2 && strncmp("fn", literal, 2) == 0) {
        return TOKEN_TYPE_FN;
    } else if (n == 6 && strncmp("return", literal, 6) == 0) {
        return TOKEN_TYPE_RETURN;
    } else if (n == 3 && strncmp("var", literal, 3) == 0) {
        return TOKEN_TYPE_VAR;
    } else {
        return TOKEN_TYPE_IDENT;
    }
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
    case '%': {
        tok.type = TOKEN_TYPE_PERCENT;
    } break;
    case '=': {
        tok.type = TOKEN_TYPE_ASSIGN;
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
            tok.type = lookup_keyword(tok.lit.store, tok.lit.len);
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

#define DEFAULT_INT_LITERAL_TYPE_ID TYPE_I64

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

static const Type *default_int_literal_type;

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

// TODO: add storage type
typedef struct {
    const Type *type;
    usize stack_index;
} Var;

typedef enum {
    VALUE_TYPE_INT_LITERAL,
    VALUE_TYPE_VAR,
} Value_Type;

typedef struct {
    Value_Type value_type;
    const Type *type;

    union {
        // int literal
        i64 int_value;

        // var
        Var var;
    };
} Value;

typedef enum {
    OP_TYPE_STORE,
    OP_TYPE_NEG,
    OP_TYPE_BINOP,
    OP_TYPE_RET,

    OP_TYPE_COUNT,
} Op_Type;

typedef struct {
    Op_Type type;

    // used in almost all Ops to store its result
    usize index;

    union {
        // negate
        Value val;

        // binary
        struct {
            Token_Type op;
            Value lhs;
            Value rhs;
        };
    };
} Op;

#define OP_STORE(__index, __val) \
    ((Op){.type = OP_TYPE_STORE, .index = (__index), .val = (__val)})
#define OP_NEG(__index, __val) \
    ((Op){.type = OP_TYPE_NEG, .index = (__index), .val = (__val)})
#define OP_BINOP(__index, __op, __lhs, __rhs) \
    ((Op){.type = OP_TYPE_BINOP, __index = (__index), .op = (__op), .lhs = (__lhs), .rhs = (__rhs)})
#define OP_RET(__val) \
    ((Op){.type = OP_TYPE_RET, .val = (__val)})

typedef enum {
    PREC_LOWEST,
    PREC_ADD,
    PREC_MULT,
    PREC_PREFIX,
} Prec;

static Prec prec_lookup[TOKEN_TYPE_COUNT] = {
    [TOKEN_TYPE_PLUS] = PREC_ADD,
    [TOKEN_TYPE_MINUS] = PREC_ADD,
    [TOKEN_TYPE_ASTERISK] = PREC_MULT,
    [TOKEN_TYPE_SLASH] = PREC_MULT,
    [TOKEN_TYPE_PERCENT] = PREC_MULT,
};

typedef struct {
    sv name;
    const Type *type;
} Func_Param;

typedef struct {
    sv name;
    const Type *return_type;
    Dynamic_Array(Func_Param) params;
    Dynamic_Array(Op) ops;
    usize stack_size;
} Func;

typedef struct {
    Lexer *l;

    Token cur_token;
    Token peek_token;

    usize stack_index;

    String_Hash_Table types;

    Dynamic_Array(String_Hash_Table) vars;
    usize scope;

    // current func compiler is working on
    Func *func;

    // type hint for current expr compiler is working on
    const Type *hint;

    Dynamic_Array(Func) funcs;

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

    sht_init(&c->types, sizeof(Type), 0);

    usize builtin_type_size = sizeof(builtin_types) / sizeof(Type);
    for (usize i = 0; i < builtin_type_size; ++i) {
        const Type *builtin = builtin_types + i;
        if (builtin->id == DEFAULT_INT_LITERAL_TYPE_ID)
            default_int_literal_type = builtin;
        Type *t = sht_get(&c->types, builtin->name, strlen(builtin->name));
        assert(t != NULL);
        memcpy(t, builtin, sizeof(Type));
    }

    da_append(&c->vars, (String_Hash_Table){0});
    sht_init(&c->vars.store[0], sizeof(Var), 0);
}

INLINE const Type *lookup_type(const Compiler *c, const Token *token) {
    assert(token->type == TOKEN_TYPE_IDENT);
    return sht_try_get(&c->types, SV_SPREAD(&token->lit));
}

INLINE Prec peek_prec(const Compiler *c) {
    return prec_lookup[c->peek_token.type];
}

Var *find_scoped_var(const Compiler *c, usize scope, const sv *name) {
    const String_Hash_Table *scope_vars = c->vars.store + scope;
    return (Var *)sht_try_get(scope_vars, name->store, name->len);
}

INLINE Var *find_var_near(const Compiler *c, const sv *name) {
    return find_scoped_var(c, c->vars.size - 1, name);
}

Var *find_var_far(const Compiler *c, const sv *name) {
    Var *v;
    for (usize i = c->vars.size - 1; i >= 0; --i) {
        if ((v = find_scoped_var(c, i, name)) != NULL) {
            return v;
        }
    }
    return NULL;
}

Func *find_func(const Compiler *c, const sv *func_name) {
    for (usize i = 0; i < c->funcs.size; ++i) {
        Func *func = c->funcs.store + i;
        if (sveq(&func->name, func_name))
            return func;
    }
    return NULL;
}

INLINE const Var *declare_var(Compiler *c, const sv *name, usize stack_index, const Type *type) {
    if (find_var_near(c, name) != NULL)
        return NULL;
    Var *var = sht_get(c->vars.store + c->vars.size - 1, SV_SPREAD(name));
    *var = (Var){.type = type, .stack_index = stack_index};
    return var;
}

INLINE usize alloc_scoped_var(Compiler *c, const Type *type) {
    usize frame = c->stack_index;
    c->stack_index += type->size;
    c->func->stack_size = MAX(c->stack_index, c->func->stack_size);
    return frame;
}

INLINE void pop_scoped_var(Compiler *c, const Type *type) {
    assert(c->stack_index >= type->size);
    c->stack_index -= type->size;
}

INLINE bool is_constant(const Value *arg) {
    return arg->value_type == VALUE_TYPE_INT_LITERAL;
}

INLINE bool is_integer_type(const Type *t) {
    return t->flag == FLAG_INT_SIGNED || t->flag == FLAG_INT_UNSIGNED;
}

INLINE bool coerce_constant_type(Value *arg, const Type *t) {
    if (!t) return true;
    arg->type = t;
    return true;
}

INLINE void push_scope(Compiler *c) {
    da_append(&c->vars, (String_Hash_Table){0});
    ++c->scope;
    sht_init(c->vars.store + c->scope, sizeof(Var), 0);
}

INLINE void pop_scope(Compiler *c) {
    --c->scope;
    da_pop(&c->vars);
}

typedef bool Compile_Stmt_Fn(Compiler *);

bool compile_expr(Compiler *c, Prec prec, Value *val, bool *is_lvalue);

bool compile_primary_expr(Compiler *c, Value *val, bool *is_lvalue) {
    bool lval;
    switch (c->cur_token.type) {
    case TOKEN_TYPE_INT_LITERAL: {
        const Type *type = c->hint && is_integer_type(c->hint)
                               ? c->hint
                               : default_int_literal_type;
        *val = (Value){
            .value_type = VALUE_TYPE_INT_LITERAL,
            .type = type,
            .int_value = atoll(c->cur_token.lit.store),
        };
        lval = false;
    } break;
    case TOKEN_TYPE_IDENT: {
        const Var *var;
        CHECK(var = find_var_far(c, &c->cur_token.lit));
        *val = (Value){
            .value_type = VALUE_TYPE_VAR,
            .type = var->type,
            .var = *var,
        };
        lval = true;
    } break;
    case TOKEN_TYPE_MINUS: {
        next_token(c);

        Value arg;
        compile_expr(c, PREC_PREFIX, &arg, NULL);
        usize result = alloc_scoped_var(c, arg.type);
        da_append(&c->func->ops, OP_NEG(result, arg));

        *val = (Value){
            .value_type = VALUE_TYPE_VAR,
            .type = arg.type,
            .var = (Var){
                .type = arg.type,
                .stack_index = result,
            }};

        lval = false;
    } break;
    default: {
        UNIMPLEMENTED();
    }
    }

    switch (c->peek_token.type) {
    case TOKEN_TYPE_ASSIGN: {
        if (!lval) {
            compiler_error(c, &c->cur_token.loc, "Cannot assign rvalue\n");
            return false;
        }
        next_token(c);
        next_token(c);

        Value arg;
        CHECK(compile_expr(c, PREC_LOWEST, &arg, NULL));

        da_append(&c->func->ops, OP_STORE(val->var.stack_index, arg));
    } break;
    default: {
    }
    }

    if (is_lvalue)
        *is_lvalue = lval;

    return true;
}

bool compile_expr(Compiler *c, Prec prec, Value *val, bool *is_lvalue) {
    CHECK(compile_primary_expr(c, val, is_lvalue));

    while (peek_prec(c) > prec) {
        next_token(c);
        Token op = c->cur_token;
        next_token(c);

        const Type *old_hint = c->hint;
        c->hint = val->type;

        Value rhs;
        compile_expr(c, prec_lookup[op.type], &rhs, NULL);

        c->hint = old_hint;

        // NOTE: temporary
        if (!strict_type_cmp(rhs.type, val->type)) {
            printf("%zu %zu\n", rhs.type->id, val->type->id);
            compiler_error(c, &op.loc, "Type Error: invalid binary operation");
            return false;
        }

        usize temp = alloc_scoped_var(c, val->type);
        da_append(&c->func->ops, OP_BINOP(temp, op.type, *val, rhs));

        *val = (Value){
            .value_type = VALUE_TYPE_VAR,
            .type = val->type,
            .var = (Var){
                .type = val->type,
                .stack_index = temp,
            },
        };

        if (is_lvalue)
            *is_lvalue = false;
    }

    return true;
}

bool compile_expr_save_stack(Compiler *c, Prec prec, Value *val, bool *is_value) {
    usize saved_stack = c->stack_index;
    bool res = compile_expr(c, prec, val, is_value);
    c->stack_index = saved_stack;
    return res;
}

bool compile_var_stmt(Compiler *c) {
    do {
        CHECK(expect_peek(c, TOKEN_TYPE_IDENT));
        Token name = c->cur_token;

        const Type *decl_type = NULL;
        if (try_peek_tok(c, TOKEN_TYPE_COLON)) {
            CHECK(expect_peek(c, TOKEN_TYPE_IDENT));

            decl_type = lookup_type(c, &c->cur_token);
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
            Value arg;
            CHECK(compile_expr_save_stack(c, PREC_LOWEST, &arg, NULL));
            c->hint = NULL;

            if (is_constant(&arg) && !coerce_constant_type(&arg, decl_type)) {
                compiler_error(c, &c->cur_token.loc, "Type Error: Unable to coerce constant to type %s",
                               decl_type->name);
                return false;
            }

            if (decl_type && !strict_type_cmp(arg.type, decl_type)) {
                compiler_error(c, &c->cur_token.loc, "Type Error: assigning expression of type %s to variable of type %s",
                               arg.type->name, decl_type->name);
                return false;
            } else {
                decl_type = arg.type;
            }

            stack_index = alloc_scoped_var(c, decl_type);
            da_append(&c->func->ops, OP_STORE(stack_index, arg));
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
    Value val;
    const Type *ret_type = c->func->return_type;

    c->hint = ret_type;
    CHECK(compile_expr_save_stack(c, PREC_LOWEST, &val, NULL));
    c->hint = NULL;

    if (is_constant(&val) && !coerce_constant_type(&val, ret_type)) {
        compiler_error(c, &c->cur_token.loc, "Type Error: Unable to coerce constant to type %s",
                       ret_type->name);
        return false;
    }

    const Type *expr_type = val.type;
    if (!strict_type_cmp(expr_type, ret_type)) {
        compiler_error(c, &c->cur_token.loc,
                       "Type Error: returning expression of type %s, expected %s",
                       expr_type->name, ret_type->name);
        return false;
    }

    CHECK(expect_peek(c, TOKEN_TYPE_SEMICOLON));

    da_append(&c->func->ops, OP_RET(val));

    return true;
}

bool compile_expr_stmt(Compiler *c) {
    Value _;
    CHECK(compile_expr_save_stack(c, PREC_LOWEST, &_, NULL));
    CHECK(expect_peek(c, TOKEN_TYPE_SEMICOLON));
    return true;
}

static Compile_Stmt_Fn *compile_stmt_fns[TOKEN_TYPE_COUNT] = {
    [TOKEN_TYPE_VAR] = compile_var_stmt,
    [TOKEN_TYPE_RETURN] = compile_return_stmt,
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
            Func func = {0};

            CHECK(expect_peek(c, TOKEN_TYPE_IDENT));

            Token name = c->cur_token;
            func.name = name.lit;

            CHECK(expect_peek(c, TOKEN_TYPE_LPAREN));

            if (peek_tok_is(c, TOKEN_TYPE_RPAREN)) {
                next_token(c);
            } else {
                do {
                    CHECK(expect_peek(c, TOKEN_TYPE_IDENT));
                    Token param_token = c->cur_token;

                    CHECK(expect_peek(c, TOKEN_TYPE_COLON));

                    CHECK(expect_peek(c, TOKEN_TYPE_IDENT));
                    const Type *param_type = lookup_type(c, &c->cur_token);
                    if (!param_type) {
                        unknown_type(c);
                        return false;
                    }

                    for (usize i = 0; i < func.params.size; ++i) {
                        const Func_Param *param = func.params.store + i;
                        if (sveq(&param->name, &param_token.lit)) {
                            compiler_error(c, &param_token.loc, "Redefinition of %.*s", TOKEN_FMT(&param_token));
                            return false;
                        }
                    }

                    da_append(&func.params, ((Func_Param){param_token.lit, param_type}));
                } while (try_peek_tok(c, TOKEN_TYPE_COMMA));

                CHECK(expect_peek(c, TOKEN_TYPE_RPAREN));
            }

            CHECK(expect_peek(c, TOKEN_TYPE_COLON));
            CHECK(expect_peek(c, TOKEN_TYPE_IDENT));
            func.return_type = lookup_type(c, &c->cur_token);
            if (!func.return_type) {
                unknown_type(c);
                return false;
            }

            bool is_declaration = peek_tok_is(c, TOKEN_TYPE_SEMICOLON);

            Func *existing_func = find_func(c, &name.lit);

            bool already_declared = existing_func != NULL;
            bool already_defined = false;

            if (already_declared) {
                already_defined = existing_func->ops.store == NULL;
            } else {
                da_append(&c->funcs, ((Func){.name = name.lit}));
                existing_func = c->funcs.store + c->funcs.size - 1;
            }

            if (already_defined && !is_declaration) {
                compiler_error(c, &name.loc, "Redefinition of function %.*s", TOKEN_FMT(&name));
                return false;
            }

            if (already_declared) {
                if (!strict_type_cmp(func.return_type, existing_func->return_type)) {
                    compiler_error(c, &name.loc, "Conflicting types for %.*s", TOKEN_FMT(&name));
                    return false;
                }

                if (func.params.size != existing_func->params.size) {
                    compiler_error(c, &name.loc, "Conflicting types for %.*s", TOKEN_FMT(&name));
                    return false;
                }

                usize n = func.params.size;
                for (usize i = 0; i < n; ++i) {
                    if (!strict_type_cmp(func.params.store[i].type, existing_func->params.store[i].type)) {
                        compiler_error(c, &name.loc, "Conflicting types for %.*s", TOKEN_FMT(&name));
                        return false;
                    }
                }
            } else {
                *existing_func = func;
            }

            if (is_declaration) {
                next_token(c);
                return true;
            }

            CHECK(expect_peek(c, TOKEN_TYPE_LBRACE));

            push_scope(c);
            c->func = existing_func;

            Dynamic_Array(Func_Param) *func_params = &existing_func->params;

            for (usize i = 0; i < func_params->size; ++i) {
                const Func_Param *param = func_params->store + i;

                usize stack_index = alloc_scoped_var(c, param->type);
                assert(declare_var(c, &param->name, stack_index, param->type) != NULL);
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

    free(input);

    return 0;
}
