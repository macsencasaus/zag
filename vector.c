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

#define SB_IMPLEMENTATION
#include "sb.h"

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

    TOKEN_TYPE_COMMA,
    TOKEN_TYPE_SEMICOLON,

    TOKEN_TYPE_ASSIGN,

    // keywords
    TOKEN_TYPE_FN,
    TOKEN_TYPE_RETURN,

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
    [TOKEN_TYPE_COMMA] = "','",
    [TOKEN_TYPE_SEMICOLON] = "';'",
    [TOKEN_TYPE_ASSIGN] = "'='",
    [TOKEN_TYPE_FN] = "fn",
    [TOKEN_TYPE_RETURN] = "return",
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
    if (n == 2 && strncmp("fn", literal, 2) == 0) {
        return TOKEN_TYPE_FN;
    } else if (n == 6 && strncmp("return", literal, 6) == 0) {
        return TOKEN_TYPE_RETURN;
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
        .literal = l->input + l->pos,
        .length = 1,
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

// TODO: add storage type
typedef struct {
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
            Token op;
            Value lhs;
            Value rhs;
        };
    };
} Op;

#define OP_STORE(__index, __val) \
    ((Op){.type = OP_TYPE_STORE, .index = (__index), .val = (__val)})
#define OP_NEG(__val) \
    ((Op){.type = OP_TYPE_NEG, .val = (__val)})
#define OP_BINOP(__op, __lhs, __rhs) \
    ((Op){.type = OP_TYPE_BINOP, .op = (__op), .lhs = (__lhs), .rhs = (__rhs)})

DYNAMIC_ARRAY_TEMPLATE(Op_Buf, Op);
DYNAMIC_ARRAY_TEMPLATE(Scope_Buf, String_Hash_Table);

typedef struct {
    const char *name;
    usize n;
    const Type *type;
} Func_Param;

DYNAMIC_ARRAY_TEMPLATE(Param_Array, Func_Param);

typedef struct {
    const Type *return_type;
    Param_Array params;
    Op_Buf ops;
} Func;

typedef struct {
    Lexer *l;

    Token cur_token;
    Token peek_token;

    usize stack_index;

    String_Hash_Table types;

    // array of string hash tables
    Scope_Buf vars;
    usize scope;
    Op_Buf *ops;

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

#define TOKEN_FMT(t) (int)(t).length, (t).literal
#define CUR_TOKEN_FMT(c) TOKEN_FMT((c)->cur_token)

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

    sht_init(&c->funcs, sizeof(Func), 0);
}

INLINE const Type *lookup_type(const Compiler *c, const Token *token) {
    assert(token->type == TOKEN_TYPE_IDENT);
    return sht_try_get(&c->types, token->literal, token->length);
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

INLINE const Var *declare_var(Compiler *c, const char *name, usize n, usize stack_index) {
    if (find_var_near(c, name, n) != NULL)
        return NULL;
    Var *var = sht_get(c->vars.store + c->vars.size - 1, name, n);
    *var = (Var){stack_index};
    return var;
}

INLINE usize alloc_scoped_var(Compiler *c, const Type *type) {
    usize frame = c->stack_index;
    c->stack_index += type->size;
    return frame;
}

INLINE bool coerce_constant_type(Value *arg, const Type *t) {
    assert(arg->type == NULL);
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

void push_opcode(Compiler *c, Op_Type op_type, ...) {
    va_list vargs;
    va_start(vargs, op_type);

    switch (op_type) {
    case OP_TYPE_STORE: {
        usize stack_index = va_arg(vargs, usize);
        Value arg = va_arg(vargs, Value);
        da_append(c->ops, OP_STORE(stack_index, arg));
    } break;
    case OP_TYPE_NEG: {
        Value arg = va_arg(vargs, Value);
        da_append(c->ops, OP_NEG(arg));
    } break;
    case OP_TYPE_BINOP: {
        Token op = va_arg(vargs, Token);
        Value lhs = va_arg(vargs, Value);
        Value rhs = va_arg(vargs, Value);
        da_append(c->ops, OP_BINOP(op, lhs, rhs));
    } break;
    default: UNIMPLEMENTED();
    }

    va_end(vargs);
}

bool compile_primary_expr(Compiler *c, Value *val, bool *is_lvalue) {
    switch (c->cur_token.type) {
    case TOKEN_TYPE_INT_LITERAL: {
        *val = (Value){
            .value_type = VALUE_TYPE_INT_LITERAL,
            .int_value = atoll(c->cur_token.literal),
        };
        if (is_lvalue)
            *is_lvalue = false;
    } break;
    case TOKEN_TYPE_IDENT: {
        const Var *var;
        if (!(var = find_var_far(c, c->cur_token.literal, c->cur_token.length)))
            return false;
        *val = (Value){
            .value_type = VALUE_TYPE_VAR,
            .var = *var,
        };
        if (is_lvalue)
            *is_lvalue = true;
    } break;
    case TOKEN_TYPE_MINUS: {
        next_token(c);

        Value arg;
        compile_primary_expr(c, &arg, NULL);
        usize result = alloc_scoped_var(c, arg.type);

        *val = (Value){
            .value_type = VALUE_TYPE_VAR,
            .type = arg.type,
            .var = (Var){
                .stack_index = result,
            }};

        if (is_lvalue)
            *is_lvalue = false;
    } break;
    default: {
        UNIMPLEMENTED();
    }
    }
    return true;
}

bool compile_expr(Compiler *c, Value *val, bool *is_lvalue) {
    return compile_primary_expr(c, val, is_lvalue);
}

bool compile_ident_stmt(Compiler *c) {
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
            if (declare_var(c, token->literal, token->length, stack_index) == NULL) {
                compiler_error(c, &c->cur_token.loc, "Redefinition of %.*s", CUR_TOKEN_FMT(c));
                return false;
            }

            if (peek_tok_is(c, TOKEN_TYPE_ASSIGN)) {
                next_token(c);
                next_token(c);

                Value arg;
                if (!compile_expr(c, &arg, NULL))
                    return false;

                if (!arg.type && !coerce_constant_type(&arg, decl_type)) {
                    compiler_error(c, &c->cur_token.loc, "Type Error: Unable to coerce constant to type %s",
                                   decl_type->name);
                    return false;
                }

                const Type *expr_type = arg.type;
                if (strict_type_cmp(expr_type, decl_type)) {
                    compiler_error(c, &c->cur_token.loc, "Type Error: assigning expression of type %s to variable of type %s",
                                   expr_type->name, decl_type->name);
                    return false;
                }

                push_opcode(c, OP_TYPE_STORE, stack_index, arg);
            }
        } while (next_if_peek_tok_is(c, TOKEN_TYPE_COMMA));

        if (!expect_peek(c, TOKEN_TYPE_SEMICOLON))
            return false;

        return true;
    } break;

    default:
        // TODO: expression statement
        compiler_error(c, &c->cur_token.loc, "Unexpected token %s", tt_str[c->cur_token.type]);
        return false;
    }
}

static Compile_Stmt_Fn *compile_stmt_fns[TOKEN_TYPE_COUNT] = {
    [TOKEN_TYPE_IDENT] = compile_ident_stmt,
};

bool compile_stmt(Compiler *c) {
    Compile_Stmt_Fn *compile_fn = compile_stmt_fns[c->cur_token.type];
    if (compile_fn == NULL) {
        unexpected_token(c);
        return false;
    }
    return compile_fn(c);
}

bool compile_block(Compiler *c) {
    while (!cur_tok_is(c, TOKEN_TYPE_RBRACE)) {
        if (!compile_stmt(c)) {
            return false;
        }

        // The compiler ends on the last token of the statement.
        // Must be advanced forward by one to start at the next statement.
        next_token(c);
    }

    next_token(c);

    return true;
}

bool compile_program(Compiler *c) {
    while (!cur_tok_is(c, TOKEN_TYPE_EOF)) {
        switch (c->cur_token.type) {
        case TOKEN_TYPE_IDENT: {
            if (!compile_ident_stmt(c)) {
                return false;
            }
        } break;

        case TOKEN_TYPE_FN: {
            if (!expect_peek(c, TOKEN_TYPE_IDENT)) {
                return false;
            }

            Func func = {0};

            Token return_type_token = c->cur_token;
            func.return_type = lookup_type(c, &c->cur_token);
            if (func.return_type == NULL) {
                compiler_error(c, &c->cur_token.loc, "Unknown type %.*s", CUR_TOKEN_FMT(c));
                return false;
            }

            if (!expect_peek(c, TOKEN_TYPE_IDENT)) {
                return false;
            }

            Token name = c->cur_token;

            if (!expect_peek(c, TOKEN_TYPE_LPAREN)) {
                return false;
            }

            if (peek_tok_is(c, TOKEN_TYPE_RPAREN)) {
                next_token(c);
            } else {
                do {
                    if (!expect_peek(c, TOKEN_TYPE_IDENT)) {
                        return false;
                    }
                    const Type *param_type = lookup_type(c, &c->cur_token);

                    if (!expect_peek(c, TOKEN_TYPE_IDENT)) {
                        return false;
                    }
                    Token param_token = c->cur_token;

                    for (usize i = 0; i < func.params.size; ++i) {
                        const Func_Param *param = func.params.store + i;
                        if (param->n == param_token.length &&
                            strncmp(param->name, param_token.literal, param->n) == 0) {
                            compiler_error(c, &param_token.loc, "Redefinition of %.*s", TOKEN_FMT(param_token));
                            return false;
                        }
                    }

                    da_append(&func.params, ((Func_Param){param_token.literal, param_token.length, param_type}));
                } while (next_if_peek_tok_is(c, TOKEN_TYPE_COMMA));

                if (!expect_peek(c, TOKEN_TYPE_RPAREN)) {
                    return false;
                }
            }

            bool is_declaration = peek_tok_is(c, TOKEN_TYPE_SEMICOLON);
            bool already_declared = sht_try_get(&c->funcs, name.literal, name.length) != NULL;
            bool already_defined = false;

            Func *existing_func = sht_get(&c->funcs, name.literal, name.length);

            if (already_declared)
                already_defined = existing_func->ops.store == NULL;

            if (already_defined && !is_declaration) {
                compiler_error(c, &name.loc, "Redefinition of function %.*s", TOKEN_FMT(name));
                return false;
            }

            if (already_declared) {
                if (!strict_type_cmp(func.return_type, existing_func->return_type)) {
                    compiler_error(c, &name.loc, "Conflicting types for %.*s", TOKEN_FMT(name));
                    return false;
                }

                if (func.params.size != existing_func->params.size) {
                    compiler_error(c, &name.loc, "Conflicting types for %.*s", TOKEN_FMT(name));
                    return false;
                }

                usize n = func.params.size;
                for (usize i = 0; i < n; ++i) {
                    if (!strict_type_cmp(func.params.store[i].type, existing_func->params.store[i].type)) {
                        compiler_error(c, &name.loc, "Conflicting types for %.*s", TOKEN_FMT(name));
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

            if (!expect_peek(c, TOKEN_TYPE_LBRACE)) {
                return false;
            }

            push_scope(c);

            Param_Array *func_params = &existing_func->params;

            for (usize i = 0; i < func_params->size; ++i) {
                const Func_Param *param = func_params->store + i;

                usize stack_index = alloc_scoped_var(c, param->type);
                assert(declare_var(c, param->name, param->n, stack_index) != NULL);
            }

            Op_Buf *cur_scope_ops = c->ops;
            c->ops = &existing_func->ops;

            if (!compile_block(c)) {
                return false;
            }

            c->ops = cur_scope_ops;

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
        exit(1);
    }

    Lexer l = {0};
    lexer_init(&l, input_file_path, input, input_len);

    if (*lex) {
        Token t;

        do {
            t = lexer_next_token(&l);
            printf("%s: %.*s at %u:%u in %s\n", tt_str[t.type], TOKEN_FMT(t),
                   t.loc.line, t.loc.col, t.loc.input_file_path);
        } while (t.type != TOKEN_TYPE_EOF);
        exit(0);
    }

    Compiler c = {0};
    compiler_init(&c, &l);

    if (!compile_program(&c)) {
        const Location *loc = &c.err_loc;
        fprintf(stderr, "%s:%u:%u: error: %s\n", loc->input_file_path, loc->line, loc->col, c.err_msg);
        return 1;
    }

    free(input);

    return 0;
}
