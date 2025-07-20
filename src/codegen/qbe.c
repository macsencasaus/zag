#ifndef ZAG_C
#define QBE_C
#include "../zag.c"
#endif

typedef usize Var_Id;

typedef struct {
    Var_Id tmp;
    Var_Id stack;

    FILE *il_stream;

    // superfluous labels for jnz inst
    usize label_alloc;
} Qbe_Ctx;

static Qbe_Ctx qbe_global_ctx = {0};

static int qbe_sprintf(const char *format, ...) {
    va_list args;
    va_start(args, format);
    int n = vfprintf(qbe_global_ctx.il_stream, format, args);
    va_end(args);
    return n;
}

usize temp_size = 0;
static char temp[4096];

static char *temp_alloc(usize n) {
    if (sizeof(temp) < temp_size + n)
        return NULL;
    char *res = temp + temp_size;
    temp_size += n;
    return res;
}

static char *temp_sprintf(const char *format, ...) {
    va_list args;
    va_start(args, format);
    int n = vsnprintf(NULL, 0, format, args);
    va_end(args);

    ZAG_ASSERT(n >= 0);
    char *result = temp_alloc(n + 1);
    ZAG_ASSERT(result != NULL && "Extend the size of the temporary allocator");
    va_start(args, format);
    vsnprintf(result, n + 1, format, args);
    va_end(args);

    return result;
}
INLINE void temp_reset(void) { temp_size = 0; }

INLINE usize qbe_label(void) { return qbe_global_ctx.label_alloc++; }

INLINE const char *new_tmp_var(Var_Id *id) {
    if (id) *id = qbe_global_ctx.tmp;
    return temp_sprintf("%%v%zu", qbe_global_ctx.tmp++);
}
INLINE const char *new_stack_var(Var_Id *id) {
    if (id) *id = qbe_global_ctx.stack;
    return temp_sprintf("%%S%zu", qbe_global_ctx.stack++);
}
INLINE const char *tmp_var(Var_Id id) {
    return temp_sprintf("%%v%zu", id);
}
INLINE const char *stack_var(Var_Id id) {
    return temp_sprintf("%%S%zu", id);
}

INLINE const char *qbe_basety(const Type *type) {
    if (type->size <= 4)
        return "w";
    else
        return "l";
}

INLINE const char *qbe_extty(const Type *type) {
    switch (type->size) {
    case 1: return "b";
    case 2: return "h";
    case 4: return "w";
    case 8: return "l";
    default:
        UNREACHABLE();
    }
}

INLINE const char *qbe_sextty(const Type *type) {
    switch (type->size) {
    case 1: return is_signed(type) ? "sb" : "ub";
    case 2: return is_signed(type) ? "sh" : "uh";
    case 4: return is_signed(type) ? "sw" : "uw";
    case 8: return "l";
    default:
        UNREACHABLE();
    }
}

INLINE const char *qbe_abity(const Type *type) {
    switch (type->size) {
    case 1: return is_signed(type) ? "sb" : "ub";
    case 2: return is_signed(type) ? "sh" : "uh";
    case 4: return "w";
    case 8: return "l";
    default:
        UNREACHABLE();
    }
}

INLINE Var_Id qbe_generate_stack_var(usize stack_index) {
    Var_Id stack_var_id;
    qbe_sprintf("\t%s =l add %%S, %zu\n", new_stack_var(&stack_var_id), stack_index);
    return stack_var_id;
}

static Var_Id qbe_load_value_to_tmp(const Value *v) {
    if (v->type->kind == TYPE_KIND_ARRAY)
        UNIMPLEMENTED();

    Var_Id tmp_id = -1;
    switch (v->value_type) {
    case VALUE_TYPE_COUNT: UNREACHABLE();

    case VALUE_TYPE_VAR: {
        Var_Id stack_var_id = qbe_generate_stack_var(v->stack_index);
        qbe_sprintf("\t%s =%s load%s %s\n",
                    new_tmp_var(&tmp_id), qbe_basety(v->type),
                    qbe_sextty(v->type), stack_var(stack_var_id));
    } break;

    case VALUE_TYPE_DEREF: {
        Var_Id tmp2_id;
        Var_Id stack_var_id = qbe_generate_stack_var(v->stack_index);
        qbe_sprintf("\t%s =l loadl %s\n", new_tmp_var(&tmp2_id), stack_var(stack_var_id));
        qbe_sprintf("\t%s =%s load%s %s\n",
                    new_tmp_var(&tmp_id), qbe_basety(v->type),
                    qbe_sextty(v->type), tmp_var(tmp2_id));
    } break;

    case VALUE_TYPE_DATA_OFFSET: {
        Var_Id tmp2_id;
        qbe_sprintf("\t%s =l add $data, %zu\n",
                    new_tmp_var(&tmp2_id), v->offset);
        qbe_sprintf("\t%s =%s load%s %s\n",
                    new_tmp_var(&tmp_id), qbe_basety(v->type),
                    qbe_sextty(v->type), tmp_var(tmp2_id));
    } break;

    case VALUE_TYPE_INIT_LIST:
        UNIMPLEMENTED();

    case VALUE_TYPE_INT_LITERAL:
    case VALUE_TYPE_EXTERN_FUNC:
    case VALUE_TYPE_FUNC:
        UNREACHABLE();
    }

    return tmp_id;
}

static void qbe_store_var_to_stack(Var_Id var, const Type *type, usize stack_index) {
    Var_Id stack_var_id = qbe_generate_stack_var(stack_index);
    qbe_sprintf("\tstore%s %s, %s\n", qbe_extty(type),
                tmp_var(var), stack_var(stack_var_id));
}

// Since we can't assign constants to temporary variables, we either
// return the constant or a temporary variable holding the value
static const char *qbe_value_symbol(const Value *v) {
    switch (v->value_type) {
    case VALUE_TYPE_COUNT: UNREACHABLE();

    case VALUE_TYPE_INT_LITERAL:
        return temp_sprintf("%zu", v->int_value);

    case VALUE_TYPE_EXTERN_FUNC:
    case VALUE_TYPE_FUNC:
        return temp_sprintf("$%.*s", SV_FMT(v->name));

    case VALUE_TYPE_VAR:
    case VALUE_TYPE_DEREF:
    case VALUE_TYPE_DATA_OFFSET:
        return tmp_var(qbe_load_value_to_tmp(v));

    case VALUE_TYPE_INIT_LIST:
        UNIMPLEMENTED();
    }
    UNREACHABLE();
}

static const char *qbe_binop_lookup[BINOP_COUNT] = {
    [BINOP_ADD] = "add",
    [BINOP_SUB] = "sub",
    [BINOP_IMUL] = "mul",
    [BINOP_MUL] = "mul",
    [BINOP_IDIV] = "div",
    [BINOP_DIV] = "udiv",
    [BINOP_IMOD] = "rem",
    [BINOP_MOD] = "urem",

    [BINOP_ULT] = "cult",
    [BINOP_SLT] = "cslt",
    [BINOP_ULE] = "cule",
    [BINOP_SLE] = "csle",
    [BINOP_UGT] = "cugt",
    [BINOP_SGT] = "csgt",
    [BINOP_UGE] = "cuge",
    [BINOP_SGE] = "csge",
    [BINOP_EQ] = "ceq",
    [BINOP_NE] = "cne",

    [BINOP_AND] = "and",
    [BINOP_OR] = "or",
    [BINOP_XOR] = "xor",

    [BINOP_SHL] = "shl",
    [BINOP_ASHR] = "sar",
    [BINOP_LSHR] = "shr",
};

static void qbe_generate_binop(const Op *binop) {
    const Type *type = binop->lhs->type;
    const char *lhs_symbol = qbe_value_symbol(binop->lhs),
               *rhs_symbol = qbe_value_symbol(binop->rhs);

    Var_Id tmp_id;
    if (qbe_binop_lookup[binop->op][0] == 'c') {
        qbe_sprintf("\t%s =%s %s%s %s, %s\n",
                    new_tmp_var(&tmp_id), qbe_basety(type),
                    qbe_binop_lookup[binop->op], qbe_basety(type),
                    lhs_symbol, rhs_symbol);
    } else {
        qbe_sprintf("\t%s =%s %s %s, %s\n",
                    new_tmp_var(&tmp_id), qbe_basety(type),
                    qbe_binop_lookup[binop->op], lhs_symbol, rhs_symbol);
    }

    Var_Id stack_var_id = qbe_generate_stack_var(binop->result);
    qbe_sprintf("\tstore%s %s, %s\n", qbe_extty(type), tmp_var(tmp_id),
                stack_var(stack_var_id));
}

static void qbe_generate_op(const Op *op) {
    const Value *val = op->val;

    switch (op->type) {
    case OP_TYPE_COUNT: UNREACHABLE();

    case OP_TYPE_ASSIGN: {
        Var_Id stack_var_id = qbe_generate_stack_var(op->result);
        const char *val_symbol = qbe_value_symbol(val);
        qbe_sprintf("\tstore%s %s, %s\n", qbe_extty(val->type),
                    val_symbol, stack_var(stack_var_id));
    } break;

    case OP_TYPE_STORE: {
        Var_Id stack_var_id = qbe_generate_stack_var(op->result);
        Var_Id loc_id;
        qbe_sprintf("\t%s =l loadl %s\n", new_tmp_var(&loc_id), stack_var(stack_var_id));
        const char *val_symbol = qbe_value_symbol(val);
        qbe_sprintf("\tstore%s %s, %s\n",
                    qbe_extty(val->type), val_symbol, tmp_var(loc_id));
    } break;

    case OP_TYPE_NEG: {
        const char *val_symbol = qbe_value_symbol(val);
        Var_Id res_id;
        qbe_sprintf("\t%s =%s neg %s\n",
                    new_tmp_var(&res_id), qbe_basety(val->type), val_symbol);
        qbe_store_var_to_stack(res_id, val->type, op->result);
    } break;

    case OP_TYPE_BNOT: {
        const char *val_symbol = qbe_value_symbol(val);
        Var_Id tmp_id, res_id;
        qbe_sprintf("\t%s =%s neg %s\n",
                    new_tmp_var(&tmp_id), qbe_basety(val->type), val_symbol);
        qbe_sprintf("\t%s =%s sub %s, -1\n",
                    new_tmp_var(&res_id), qbe_basety(val->type), tmp_var(tmp_id));
        qbe_store_var_to_stack(res_id, val->type, op->result);
    } break;

    // NOTE: this is stupid
    case OP_TYPE_LNOT: {
        const char *val_symbol = qbe_value_symbol(val);
        usize nz_label = qbe_label(),
              z_label = qbe_label(),
              res_label = qbe_label();
        qbe_sprintf("\tjnz %s, @T%zu, @T%zu\n",
                    val_symbol, nz_label, z_label);
        qbe_sprintf("@T%zu\n", nz_label);
        qbe_sprintf("\tjmp @T%zu\n", res_label);
        qbe_sprintf("@T%zu\n", z_label);
        qbe_sprintf("\tjmp @T%zu\n", res_label);
        qbe_sprintf("@T%zu\n", res_label);
        Var_Id res_id;
        qbe_sprintf("\t%s =%s phi @T%zu 0, @T%zu 1\n",
                    new_tmp_var(&res_id), qbe_basety(val->type),
                    nz_label, z_label);
        qbe_store_var_to_stack(res_id, val->type, op->result);
    } break;

    case OP_TYPE_BINOP: {
        qbe_generate_binop(op);
    } break;

    case OP_TYPE_RET: {
        const char *val_symbol = qbe_value_symbol(val);
        qbe_sprintf("\tret %s\n", val_symbol);
    } break;

    case OP_TYPE_REF: {
        Var_Id stack_var_id = qbe_generate_stack_var(op->result);
        switch (val->value_type) {
        case VALUE_TYPE_COUNT:
        case VALUE_TYPE_INT_LITERAL:
            UNREACHABLE();

        case VALUE_TYPE_VAR: {
            Var_Id ref_val_id = qbe_generate_stack_var(val->stack_index);
            qbe_sprintf("\tstorel %s, %s\n", stack_var(ref_val_id), stack_var(stack_var_id));
        } break;

        case VALUE_TYPE_EXTERN_FUNC:
        case VALUE_TYPE_FUNC: {
            qbe_sprintf("\tstorel $%.*s, %s\n", SV_FMT(val->name), stack_var(stack_var_id));
        } break;

        case VALUE_TYPE_DEREF:
            UNREACHABLE();
        case VALUE_TYPE_INIT_LIST:
            UNREACHABLE();

        case VALUE_TYPE_DATA_OFFSET: {
            Var_Id tmp_id;
            qbe_sprintf("\t%s =l add $data, %zu\n", new_tmp_var(&tmp_id), val->offset);
            qbe_sprintf("\tstorel %s, %s\n", tmp_var(tmp_id), stack_var(stack_var_id));
        } break;
        }
    } break;

    case OP_TYPE_LABEL: {
        qbe_sprintf("@L%zu\n", op->label_id);
    } break;

    case OP_TYPE_JMP: {
        qbe_sprintf("\tjmp @L%zu\n", op->label_id);
    } break;

    case OP_TYPE_JMPZ: {
        const char *cond_symbol = qbe_value_symbol(op->val);
        usize true_label = qbe_label();
        qbe_sprintf("\tjnz %s, @T%zu, @L%zu\n",
                    cond_symbol, true_label, op->label_id);
        qbe_sprintf("@T%zu\n", true_label);
    } break;

    case OP_TYPE_CALL: {
        const Type *func_type = op->func->type;

        Array(const char *, MAX_PARAM_COUNT) param_symbols = {0};
        for (usize i = 0; i < op->params->len; ++i) {
            const Value *param = *at(op->params, i);
            append(&param_symbols, qbe_value_symbol(param));
        }

        const Value *func = op->func;
        const char *func_symbol;

        if (func->value_type == VALUE_TYPE_FUNC ||
            func->value_type == VALUE_TYPE_EXTERN_FUNC) {
            func_symbol = temp_sprintf("$%.*s", SV_FMT(func->name));
        } else {
            Var_Id func_var_id = qbe_load_value_to_tmp(func);
            func_symbol = tmp_var(func_var_id);
        }

        Var_Id tmp_id;
        qbe_sprintf("\t%s =%s call %s(", new_tmp_var(&tmp_id),
                    qbe_abity(func_type->ret), func_symbol);

        for (usize i = 0; i < param_symbols.len; ++i) {
            const Type *param_type = (*da_at(op->params, i))->type;
            qbe_sprintf("%s %s", qbe_abity(param_type), *at(&param_symbols, i));
            if (i < param_symbols.len - 1)
                qbe_sprintf(", ");
        }
        qbe_sprintf(")\n");

        qbe_store_var_to_stack(tmp_id, func_type->ret, op->result);
    } break;
    }

    temp_reset();
}

static void qbe_generate_func(const Value *func) {
    qbe_global_ctx.tmp = 0;
    qbe_global_ctx.stack = 0;

    const Type *func_type = func->type;
    const Type *ret_type = func_type->ret;
    qbe_sprintf("export function %s $%.*s(",
                qbe_basety(ret_type), SV_FMT(func->name));

    usize n = func_type->params->len;
    for (usize i = 0; i < n; ++i) {
        const Type *param_type = get_param_type(func, i);
        qbe_sprintf("%s %s", qbe_basety(param_type), new_tmp_var(NULL));
        if (i < n - 1)
            qbe_sprintf(", ");
    }
    qbe_sprintf(") {\n@start\n");

    qbe_sprintf("\t%%S =l alloc16 %zu\n", func->stack_size);

    temp_reset();

    for (usize i = 0; i < func_type->params->len; ++i) {
        const Func_Param *param = get_param_const(func, i);
        const Type *param_type = get_param_type(func, i);
        qbe_store_var_to_stack(i, param_type, param->index);
    }

    temp_reset();

    for (usize i = 0; i < func->ops.size; ++i) {
        const Op *op = *da_at(&func->ops, i);
        qbe_generate_op(op);
    }

    qbe_sprintf("}\n\n");
}

static void qbe_generate_data(const String_Builder *data) {
    qbe_sprintf("data $data = { b ");
    for (usize i = 0; i < data->size; ++i) {
        char c = *da_at(data, i);
        qbe_sprintf("%u ", c);
    }
    qbe_sprintf("}");
}

static void qbe_generate_funcs(const Compiler *c) {
    for (usize i = 0; i < c->funcs.size; ++i) {
        const Value *func = *da_at(&c->funcs, i);
        qbe_generate_func(func);
    }
}

void qbe_generate_il(const Compiler *c, FILE *il_out) {
    qbe_global_ctx = (Qbe_Ctx){.il_stream = il_out};
    qbe_generate_funcs(c);
    if (c->data.size > 0)
        qbe_generate_data(&c->data);
}

int qbe_generate_asm(const Compiler *c, char *qbe_binary, Target target, FILE *asm_out) {
    int pipefd[2];
    if (pipe(pipefd) == -1) {
        perror("pipe");
        return 1;
    }

    char *target_flag = (char *)target_options[target] + 4;

    char *args[] = {qbe_binary, "-t", target_flag, "-", NULL};

    pid_t pid = fork();
    if (pid == 0) {
        if (close(pipefd[1]) == -1) {
            perror("Error: closing write end of pipe");
            exit(1);
        }

        if (dup2(pipefd[0], STDIN_FILENO) == -1) {
            perror("Error: redirecting input to stdin");
            exit(1);
        }

        if (close(pipefd[0]) == -1) {
            perror("Error: closing write end of pipe (qbe child)");
            exit(1);
        }

        int asm_fd = fileno(asm_out);
        dup2(asm_fd, STDOUT_FILENO);

        execvp(qbe_binary, args);
        perror("Error: Failed to start qbe");
        exit(1);
    } else if (pid > 0) {
        close(pipefd[0]);
        FILE *fp = fdopen(pipefd[1], "w");
        if (!fp) {
            perror("fdopen");
            return 1;
        }

        qbe_generate_il(c, fp);

        if (fclose(fp)) {
            perror("fclose");
            return 1;
        }

        int status;
        waitpid(pid, &status, 0);
        if (WIFEXITED(status)) {
            return WEXITSTATUS(status);
        }
    } else {
        return 1;
    }

    return 0;
}

int qbe_generate(const Compiler *c, char *qbe_binary, char *as_binary, 
                 Target target, char *obj_filename) {
    int pipefd[2];
    if (pipe(pipefd) == -1) {
        perror("pipe");
        return 1;
    }

    char *args[] = {as_binary, "-o", obj_filename, "-", NULL};

    pid_t pid = fork();
    if (pid == 0) {
        if (close(pipefd[1]) == -1) {
            perror("Error: closing write end of pipe");
            exit(1);
        }

        if (dup2(pipefd[0], STDIN_FILENO) == -1) {
            perror("Error: redirecting input to stdin");
            exit(1);
        }

        if (close(pipefd[0]) == -1) {
            perror("Error: closing write end of pipe (qbe child)");
            exit(1);
        }

        execvp(as_binary, args);
        perror("Error: Failed to start qbe");
        exit(1);
    } else if (pid > 0) {
        close(pipefd[0]);
        FILE *fp = fdopen(pipefd[1], "w");
        if (!fp) {
            perror("fdopen");
            return 1;
        }

        int s = qbe_generate_asm(c, qbe_binary, target, fp);
        if (s) return s;

        if (fclose(fp)) {
            perror("fclose");
            return 1;
        }

        int status;
        waitpid(pid, &status, 0);
        if (WIFEXITED(status)) {
            return WEXITSTATUS(status);
        }
    } else {
        return 1;
    }

    return 0;
}
