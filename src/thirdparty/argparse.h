// argparse.h -- command-line argument parsing
//
//    Inspired by Python argparse module and github.com/tsoding/flag.h
//
// Macros API:
// - ARGP_FLAG_CAP - how many flags you can define
// - ARGP_POS_CAP - how many positional arguments you can define
// - ARGP_PRINT_WIDTH - width at which description is printed in argp_print_usage function
#ifndef ARGPARSE_H
#define ARGPARSE_H

#include <assert.h>
#include <limits.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

typedef enum {
    ARGP_OPT_NONOPT = false,
    ARGP_OPT_OPT = true,
    ARGP_OPT_APPEAR_NONOPT,
} Argp_Opt_Option;

void argp_init(int argc, char **argv, const char *program_desc, bool default_help);

// returns name of flag given its return value
const char *argp_name(void *val);

// only requires that the flag exists to be true
bool *argp_flag_bool(const char *short_name, const char *long_name, const char *desc);

uint64_t *argp_flag_uint(const char *short_name, const char *long_name, const char *meta_var,
                         uint64_t def, const char *desc);

char **argp_flag_str(const char *short_name, const char *long_name, const char *meta_var,
                     char *def, const char *desc);

size_t *argp_flag_enum(const char *short_name, const char *long_name, const char *options[],
                       size_t option_count, size_t def, const char *desc);

uint64_t *argp_pos_uint(const char *name, uint64_t def, Argp_Opt_Option opt, const char *desc);

char **argp_pos_str(const char *name, char *def, Argp_Opt_Option opt, const char *desc);

size_t *argp_pos_enum(const char *name, const char *option[], size_t option_count,
                      size_t def, Argp_Opt_Option opt, const char *desc);

bool argp_parse_args(void);

void argp_print_usage(FILE *stream);
void argp_print_error(FILE *stream);

#endif  // ARGPARSE_H

#ifdef ARGPARSE_IMPLEMENTATION

#include <errno.h>
#include <stdlib.h>
#include <string.h>

typedef enum {
    ARGP_BOOL,
    ARGP_UINT,
    ARGP_STR,
    ARGP_ENUM,
    ARGP_TYPE_COUNT,
} Argp_Type;

typedef enum {
    ARGP_NO_ERROR = 0,
    ARGP_ERROR_UNKNOWN,
    ARGP_ERROR_UNKNOWN_ENUM,
    ARGP_ERROR_NO_VALUE,
    ARGP_ERROR_INVALID_NUMBER,
    ARGP_ERROR_INTEGER_OVERFLOW,
    ARGP_ERROR_COUNT,
} Argp_Error;

typedef union {
    bool as_bool;
    uint64_t as_uint;
    char *as_str;
    size_t as_enum;
} Argp_Value;

typedef struct {
    Argp_Type type;
    const char *short_name;
    const char *long_name;
    const char *meta_var;
    const char *desc;
    Argp_Value val;
    Argp_Value def;

    const char **enum_options;
    size_t option_count;
} Argp_Flag;

typedef struct {
    Argp_Type type;
    const char *name;
    const char *desc;
    Argp_Opt_Option opt;
    Argp_Value val;
    Argp_Value def;

    const char **enum_options;
    size_t option_count;
} Argp_Pos;

#ifndef ARGP_FLAG_CAP
#define ARGP_FLAG_CAP 128
#endif

#ifndef ARGP_POS_CAP
#define ARGP_POS_CAP 128
#endif

#ifndef ARGP_PRINT_WIDTH
#define ARGP_PRINT_WIDTH 24
#endif

typedef struct {
    size_t flag_count;
    Argp_Flag flags[ARGP_FLAG_CAP];

    size_t pos_count;
    Argp_Pos poss[ARGP_POS_CAP];

    const char *program_name;
    const char *program_desc;

    Argp_Flag *help_flag;

    Argp_Error err;
    Argp_Flag *err_flag;
    Argp_Pos *err_pos;

    const char *unknown_option;

    int rest_argc;
    char **rest_argv;
} Argp_Ctx;

static Argp_Ctx argp_global_ctx;

static char *shift_args(void) {
    Argp_Ctx *c = &argp_global_ctx;
    if (c->rest_argc == 0) return NULL;
    char *res = c->rest_argv[0];
    --c->rest_argc;
    ++c->rest_argv;
    return res;
}

static Argp_Flag *argp_new_flag(Argp_Type type, const char *short_name, const char *long_name,
                                const char *meta_var, const char *desc) {
    assert(short_name != NULL || long_name != NULL);
    assert(argp_global_ctx.flag_count < ARGP_FLAG_CAP);
    Argp_Ctx *c = &argp_global_ctx;
    Argp_Flag *flag = c->flags + (c->flag_count++);
    *flag = (Argp_Flag){
        .type = type,
        .short_name = short_name,
        .long_name = long_name,
        .meta_var = meta_var,
        .desc = desc,
    };
    return flag;
}

static Argp_Pos *argp_new_pos(Argp_Type type, const char *name,
                              const char *desc, Argp_Opt_Option opt) {
    assert(argp_global_ctx.pos_count < ARGP_POS_CAP);
    Argp_Pos *pos = argp_global_ctx.poss + (argp_global_ctx.pos_count++);
    *pos = (Argp_Pos){
        .type = type,
        .name = name,
        .desc = desc,
        .opt = opt,
    };
    return pos;
}

void argp_init(int argc, char **argv, const char *program_desc, bool default_help) {
    Argp_Ctx *c = &argp_global_ctx;

    *c = (Argp_Ctx){0};

    c->rest_argc = argc;
    c->rest_argv = argv;

    c->program_name = shift_args();
    c->program_desc = program_desc;

    if (default_help)
        c->help_flag = argp_new_flag(ARGP_BOOL, "h", "help", NULL, "show this help message and exit");
}

bool *argp_flag_bool(const char *short_name, const char *long_name,
                     const char *desc) {
    Argp_Flag *flag = argp_new_flag(ARGP_BOOL, short_name, long_name, NULL, desc);
    return &flag->val.as_bool;
}

uint64_t *argp_flag_uint(const char *short_name, const char *long_name, const char *meta_var,
                         uint64_t def, const char *desc) {
    Argp_Flag *flag = argp_new_flag(ARGP_UINT, short_name, long_name, meta_var, desc);
    flag->val.as_uint = def;
    flag->def.as_uint = def;
    return &flag->val.as_uint;
}

char **argp_flag_str(const char *short_name, const char *long_name, const char *meta_var,
                     char *def, const char *desc) {
    Argp_Flag *flag = argp_new_flag(ARGP_STR, short_name, long_name, meta_var, desc);
    flag->val.as_str = def;
    flag->def.as_str = def;
    return &flag->val.as_str;
}

size_t *argp_flag_enum(const char *short_name, const char *long_name, const char *options[],
                       size_t option_count, size_t def, const char *desc) {
    Argp_Flag *flag = argp_new_flag(ARGP_ENUM, short_name, long_name, NULL, desc);
    flag->val.as_enum = def;
    flag->def.as_enum = def;
    flag->enum_options = options;
    flag->option_count = option_count;
    return &flag->val.as_enum;
}

uint64_t *argp_pos_uint(const char *name, uint64_t def,
                        Argp_Opt_Option opt, const char *desc) {
    Argp_Pos *pos = argp_new_pos(ARGP_UINT, name, desc, opt);
    pos->val.as_uint = def;
    pos->def.as_uint = def;
    return &pos->val.as_uint;
}

char **argp_pos_str(const char *name, char *def, Argp_Opt_Option opt, const char *desc) {
    Argp_Pos *pos = argp_new_pos(ARGP_STR, name, desc, opt);
    pos->val.as_str = def;
    pos->def.as_str = def;
    return &pos->val.as_str;
}

size_t *argp_pos_enum(const char *name, const char *options[], size_t option_count,
                      size_t def, Argp_Opt_Option opt, const char *desc) {
    Argp_Pos *pos = argp_new_pos(ARGP_ENUM, name, desc, opt);
    pos->val.as_enum = def;
    pos->def.as_enum = def;
    pos->enum_options = options;
    pos->option_count = option_count;
    return &pos->val.as_enum;
}

void argp_print_usage(FILE *stream) {
    Argp_Ctx *c = &argp_global_ctx;

    fprintf(stream, "usage: %s", c->program_name);
    if (c->flag_count) {
        fprintf(stream, " [OPTION]...");
    }
    for (size_t i = 0; i < c->pos_count; ++i) {
        const Argp_Pos *pos = c->poss + i;
        if (pos->opt == ARGP_OPT_OPT)
            fprintf(stream, " [%s]", pos->name);
        else
            fprintf(stream, " %s", pos->name);
    }
    fprintf(stream, "\n\n");

    if (c->program_desc) {
        fprintf(stream, "%s\n\n", c->program_desc);
    }

    if (c->pos_count) {
        fprintf(stream, "positional arguments:\n");
        for (size_t i = 0; i < c->pos_count; ++i) {
            int width = 0;

            const Argp_Pos *pos = c->poss + i;
            width += fprintf(stream, "  %s", pos->name);
            if (pos->type == ARGP_ENUM) {
                width += fprintf(stream, " {");
                for (size_t i = 0; i < pos->option_count; ++i) {
                    const char *option = pos->enum_options[i];
                    if (!option) continue;
                    width += fprintf(stream, "%s", option);
                    if (i < pos->option_count - 1)
                        width += fprintf(stream, ",");
                }
                width += fprintf(stream, "}");
            }

            if (width >= ARGP_PRINT_WIDTH) {
                fprintf(stream, "\n");
                width = 0;
            }
            for (int i = 0; i < ARGP_PRINT_WIDTH - width; ++i)
                fprintf(stream, " ");

            fprintf(stream, "%s\n", pos->desc);

            if (!pos->opt) continue;
        }

        fprintf(stream, "\n");
    }

    if (c->flag_count) {
        fprintf(stream, "options:\n");
        for (size_t i = 0; i < c->flag_count; ++i) {
            int width = 0;

            const Argp_Flag *flag = c->flags + i;
            if (flag->short_name && flag->long_name) {
                width += fprintf(stream, "  -%s, --%s", flag->short_name, flag->long_name);
            } else if (flag->short_name) {
                width += fprintf(stream, "  -%s", flag->short_name);
            } else if (flag->long_name) {
                width += fprintf(stream, "  --%s", flag->long_name);
            } else {
                assert(false && "At least one name of a flag must be non null");
            }

            if (flag->meta_var) {
                width += fprintf(stream, " %s", flag->meta_var);
            } else if (flag->type == ARGP_ENUM) {
                width += fprintf(stream, " {");
                for (size_t i = 0; i < flag->option_count; ++i) {
                    const char *option = flag->enum_options[i];
                    if (!option) continue;
                    width += fprintf(stream, "%s", option);
                    if (i < flag->option_count - 1)
                        width += fprintf(stream, ",");
                }
                width += fprintf(stream, "}");
            }

            if (width >= ARGP_PRINT_WIDTH) {
                fprintf(stream, "\n");
                width = 0;
            }
            for (int i = 0; i < ARGP_PRINT_WIDTH - width; ++i)
                fprintf(stream, " ");
            fprintf(stream, "%s\n", flag->desc);
        }
    }
}

void argp_print_error(FILE *stream) {
    Argp_Ctx *c = &argp_global_ctx;
    switch (c->err) {
        case ARGP_NO_ERROR: {
            fprintf(stream, "No errors parsing arguments\n");
            return;
        } break;
        case ARGP_ERROR_UNKNOWN: {
            fprintf(stream, "Error: Unknown option %s\n", c->unknown_option);
            return;
        } break;
        case ARGP_ERROR_UNKNOWN_ENUM: {
            fprintf(stream, "Error: Unknown enum option");
        } break;
        case ARGP_ERROR_NO_VALUE: {
            fprintf(stream, "Error: No value provided");
        } break;
        case ARGP_ERROR_INVALID_NUMBER: {
            fprintf(stream, "Error: Invalid number");
        } break;
        case ARGP_ERROR_INTEGER_OVERFLOW: {
            fprintf(stream, "Error: Integer overflow");
        } break;
        default:
            assert(false && "Unreachable");
    }

    Argp_Type type;
    const char **enum_option;
    size_t option_count;
    if (c->err_flag) {
        const Argp_Flag *flag = c->err_flag;
        if (flag->long_name)
            fprintf(stream, " for flag --%s", flag->long_name);
        else
            fprintf(stream, " for flag -%s", flag->short_name);

        type = c->err_flag->type;
        enum_option = c->err_flag->enum_options;
        option_count = c->err_flag->option_count;
    } else {
        fprintf(stream, " for positional argument %s", c->err_pos->name);

        type = c->err_pos->type;
        enum_option = c->err_pos->enum_options;
        option_count = c->err_pos->option_count;
    }

    if (c->unknown_option)
        fprintf(stream, " got '%s'", c->unknown_option);

    if (type == ARGP_ENUM) {
        fprintf(stream, " expected {");
        for (size_t i = 0; i < option_count; ++i) {
            const char *option = enum_option[i];
            fprintf(stream, "%s", option);

            if (i < option_count - 1)
                fprintf(stream, ",");
        }
        fprintf(stream, "}");
    }

    fprintf(stream, "\n");
}

static Argp_Flag *try_short_name(const char *arg, size_t n) {
    Argp_Ctx *c = &argp_global_ctx;
    if (n < 1) return NULL;
    if (arg[0] != '-') return NULL;
    const char *short_name = arg + 1;

    for (size_t i = 0; i < c->flag_count; ++i) {
        Argp_Flag *flag = c->flags + i;
        if (!flag->short_name) continue;
        if (strcmp(short_name, flag->short_name) == 0)
            return flag;
    }

    return NULL;
}

static Argp_Flag *try_long_name(const char *arg, size_t n) {
    Argp_Ctx *c = &argp_global_ctx;
    if (n < 2) return NULL;
    if (arg[0] != '-' || arg[1] != '-')
        return NULL;
    const char *long_name = arg + 2;

    for (size_t i = 0; i < c->flag_count; ++i) {
        Argp_Flag *flag = c->flags + i;
        if (!flag->long_name) continue;
        if (strcmp(long_name, flag->long_name) == 0)
            return flag;
    }

    return NULL;
}

static bool argp_parse_uint(char *arg, uint64_t *v) {
    Argp_Ctx *c = &argp_global_ctx;

    if (!arg) {
        c->err = ARGP_ERROR_NO_VALUE;
        return false;
    }
    char *endptr;
    uint64_t result = strtoull(arg, &endptr, 10);

    if (*endptr != 0) {
        c->err = ARGP_ERROR_INVALID_NUMBER;
        c->unknown_option = arg;
        return false;
    }
    if (result == ULLONG_MAX && errno == ERANGE) {
        c->err = ARGP_ERROR_INTEGER_OVERFLOW;
        c->unknown_option = arg;
        return false;
    }
    *v = result;
    return true;
}

static bool argp_parse_str(char *arg, char **v) {
    Argp_Ctx *c = &argp_global_ctx;

    if (!arg) {
        c->err = ARGP_ERROR_NO_VALUE;
        return false;
    }
    *v = arg;
    return true;
}

static bool argp_parse_enum(char *arg, size_t *v, const char **enum_options, size_t option_count) {
    Argp_Ctx *c = &argp_global_ctx;

    if (!arg) {
        c->err = ARGP_ERROR_NO_VALUE;
        return false;
    }

    for (size_t i = 0; i < option_count; ++i) {
        const char *option = enum_options[i];
        if (!option) continue;
        if (strcmp(arg, option) == 0) {
            *v = i;
            return true;
        }
    }

    c->err = ARGP_ERROR_UNKNOWN_ENUM;
    c->unknown_option = arg;
    return false;
}

static bool argp_parse_flag(Argp_Flag *flag) {
    Argp_Ctx *c = &argp_global_ctx;
    switch (flag->type) {
        case ARGP_BOOL: {
            flag->val.as_bool = true;
        } break;
        case ARGP_UINT: {
            char *arg = shift_args();
            if (!argp_parse_uint(arg, &flag->val.as_uint)) {
                c->err_flag = flag;
                return false;
            }
        } break;
        case ARGP_STR: {
            char *arg = shift_args();
            if (!argp_parse_str(arg, &flag->val.as_str)) {
                c->err_flag = flag;
                return false;
            }
        } break;
        case ARGP_ENUM: {
            char *arg = shift_args();
            if (!argp_parse_enum(arg, &flag->val.as_enum, flag->enum_options, flag->option_count)) {
                c->err_flag = flag;
                return false;
            }
        } break;
        default:
            assert(false && "Unreachable");
    }
    return true;
}

static bool argp_parse_pos(char *arg, Argp_Pos *pos) {
    Argp_Ctx *c = &argp_global_ctx;
    switch (pos->type) {
        case ARGP_UINT: {
            if (!argp_parse_uint(arg, &pos->val.as_uint)) {
                c->err_pos = pos;
                return false;
            }
        } break;
        case ARGP_STR: {
            if (!argp_parse_str(arg, &pos->val.as_str)) {
                c->err_pos = pos;
                return false;
            }
        } break;
        case ARGP_ENUM: {
            if (!argp_parse_enum(arg, &pos->val.as_enum, pos->enum_options, pos->option_count)) {
                c->err_pos = pos;
                return false;
            }
        } break;
        default:
            assert(false && "Unreachable");
    }
    return true;
}

bool argp_parse_args(void) {
    Argp_Ctx *c = &argp_global_ctx;
    size_t cur_pos = 0;

    char *arg;
    while ((arg = shift_args())) {
        size_t n = strlen(arg);

        Argp_Flag *flag = NULL;
        if (!(flag = try_short_name(arg, n)))
            flag = try_long_name(arg, n);

        if (flag) {
            if (flag == c->help_flag) {
                argp_print_usage(stdout);
                exit(0);
            }
            if (!argp_parse_flag(flag))
                return false;
            continue;
        }

        if (cur_pos == c->pos_count) {
            c->err = ARGP_ERROR_UNKNOWN;
            c->unknown_option = arg;
            return false;
        }

        Argp_Pos *pos = c->poss + (cur_pos++);

        if (!argp_parse_pos(arg, pos))
            return false;

        pos->opt = ARGP_OPT_OPT;
    }

    for (size_t i = 0; i < c->pos_count; ++i) {
        Argp_Pos *pos = c->poss + i;
        if (pos->opt == ARGP_OPT_NONOPT) {
            c->err = ARGP_ERROR_NO_VALUE;
            c->err_pos = pos;
            return false;
        }
    }

    return true;
}

const char *argp_name(void *val) {
    Argp_Ctx *c = &argp_global_ctx;
    for (size_t i = 0; i < ARGP_FLAG_CAP; ++i) {
        const Argp_Flag *flag = c->flags + i;
        if (&flag->val == val) {
            if (flag->long_name)
                return flag->long_name;
            else
                return flag->short_name;
        }
    }
    for (size_t i = 0; i < ARGP_POS_CAP; ++i) {
        const Argp_Pos *pos = c->poss + i;
        if (&pos->val == val)
            return pos->name;
    }
    return NULL;
}

#endif  // ARGPARSE_IMPLEMENTATION

// Copyright 2025 Macsen Casaus <macsencasaus@gmail.com>
//
// Permission is hereby granted, free of charge, to any person obtaining a copy of
// this software and associated documentation files (the "Software"), to deal in
// the Software without restriction, including without limitation the rights to
// use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
// of the Software, and to permit persons to whom the Software is furnished to do
// so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.
