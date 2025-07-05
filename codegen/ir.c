#ifndef VECTOR_C
#define IR_C
#include "../vector.c"
#endif

char *get_ir_type_name(const Type *type) {
    if (type->kind == TYPE_KIND_PTR || type->kind == TYPE_KIND_FN)
        return "ptr";

    switch (type->size) {
    case 1: return "i8";
    case 2: return "i16";
    case 4: return "i32";
    case 8: return "i64";
    }

    UNIMPLEMENTED();
}

void print_ir_val(const Value *val, FILE *out) {
    fprintf(out, "%s ", get_ir_type_name(val->type));
    switch (val->value_type) {
    case VALUE_TYPE_INT_LITERAL: {
        fprintf(out, "%ld", val->int_value);
    } break;
    case VALUE_TYPE_VAR: {
        fprintf(out, "%%s[%zu]", val->stack_index);
    } break;
    case VALUE_TYPE_FUNC: {
        fprintf(out, "%.*s", SV_FMT(&val->name));
    } break;
    default: UNIMPLEMENTED();
    }
}

static char *binop_inst_lookup[TOKEN_TYPE_COUNT] = {
    [TOKEN_TYPE_PLUS] = "add",
    [TOKEN_TYPE_MINUS] = "sub",
    [TOKEN_TYPE_ASTERISK] = "mul",
    [TOKEN_TYPE_SLASH] = "div",
    [TOKEN_TYPE_PERCENT] = "mod",
    [TOKEN_TYPE_LT] = "icmp lt",
    [TOKEN_TYPE_LTEQ] = "icmp lteq",
    [TOKEN_TYPE_GT] = "icmp gt",
    [TOKEN_TYPE_GTEQ] = "icmp gteq",
    [TOKEN_TYPE_EQ] = "icmp eq",
    [TOKEN_TYPE_NEQ] = "icmp neq",
    [TOKEN_TYPE_BAND] = "and",
    [TOKEN_TYPE_BOR] = "or",
    [TOKEN_TYPE_XOR] = "xor",
    [TOKEN_TYPE_SHL] = "shl",
    [TOKEN_TYPE_SHR] = "shr",
};

void print_ir_op(const Op *op, FILE *out) {
    switch (op->type) {
    case OP_TYPE_STORE: {
        fprintf(out, "    %%s[%zu] = ", op->result);
        print_ir_val(&op->val, out);
        fprintf(out, "\n");
    } break;
    case OP_TYPE_RET: {
        fprintf(out, "    ret ");
        print_ir_val(&op->val, out);
        fprintf(out, "\n");
    } break;
    case OP_TYPE_NEG: {
        fprintf(out, "    %%s[%zu] = neg ", op->result);
        print_ir_val(&op->val, out);
        fprintf(out, "\n");
    } break;
    case OP_TYPE_BNOT: {
        fprintf(out, "    %%s[%zu] = bnot ", op->result);
        print_ir_val(&op->val, out);
        fprintf(out, "\n");
    } break;
    case OP_TYPE_LNOT: {
        fprintf(out, "    %%s[%zu] = lnot ", op->result);
        print_ir_val(&op->val, out);
        fprintf(out, "\n");
    } break;
    case OP_TYPE_BINOP: {
        const char *op_lit = binop_inst_lookup[op->op];
        if (!op_lit) UNIMPLEMENTED();
        fprintf(out, "    %%s[%zu] = %s ", op->result, op_lit);
        print_ir_val(&op->lhs, out);
        fprintf(out, ", ");
        print_ir_val(&op->rhs, out);
        fprintf(out, "\n");
    } break;
    default: UNIMPLEMENTED();
    }
}

void print_ir_func(const Value *func, FILE *out) {
    fprintf(out, "%.*s[%zu](", SV_FMT(&func->name), func->stack_size);
    if (func->params.size > 0) {
        for (usize i = 0; i < func->params.size - 1; ++i) {
            fprintf(out, "%zu,", get_param_type(func, i)->size);
        }
        fprintf(out, "%zu", get_param_type(func, func->params.size - 1)->size);
    }
    fprintf(out, "):\n");

    for (usize i = 0; i < func->ops.size; ++i) {
        const Op *op = func->ops.store + i;
        print_ir_op(op, out);
    }
    fprintf(out, "\n");
}

void print_ir_funcs(const Compiler *c, FILE *out) {
    for (usize i = 0; i < c->funcs.size; ++i) {
        const Value *func = c->funcs.store[i];
        print_ir_func(func, out);
    }
}

void print_description(FILE *out) {
    fprintf(out,
            "; V3 Readable Intermediate Representation\n"
            "; Function header formatted as:\n"
            "; func[<max-stack-size>](<size-of-param>,...)\n"
            "\n");
}

void print_ir_program(const Compiler *c, FILE *out) {
    print_description(out);
    print_ir_funcs(c, out);
}
