#ifndef ELF_C
#define ELF_C

#ifndef ZAG_C
#define X86_64_LINUX_C
#include "../zag.c"
#endif

#include <elf.h>

enum {
    NULL_SECTION_IDX,
    TEXT_SECTION_IDX,
    RELA_TEXT_SECTION_IDX,
    DATA_SECTION_IDX,
    NOTE_GNU_STACK_SECTION_IDX,
    SYMTAB_SECTION_IDX,
    STRTAB_SECTION_IDX,
    SHSTRTAB_SECTION_IDX,
};

INLINE usize st_append(String_Builder *st, const char *section_name) {
    usize off = st->size;
    usize n = strlen(section_name) + 1;
    sb_append_buf(st, section_name, n);
    return off;
}

INLINE usize st_append_sv(String_Builder *st, sv name) {
    usize off = st->size;
    sb_append_buf(st, name.store, name.len);
    sb_append_null(st);
    return off;
}

typedef struct {
    Elf64_Ehdr header;

    String_Builder text;

    Dynamic_Array(Elf64_Rela) relocations;

    String_View data;

    Dynamic_Array(Elf64_Sym) symbols;

    String_Builder strtab;
    String_Builder shstrtab;

    Dynamic_Array(Elf64_Shdr) section_headers;

    usize data_section_symtab_idx;
} Elf_Builder;

INLINE Elf64_Rela *new_relocation(Elf_Builder *ctx) {
    da_append(&ctx->relocations, (Elf64_Rela){0});
    return da_last(&ctx->relocations);
}

INLINE Elf64_Sym *new_symbol(Elf_Builder *ctx) {
    da_append(&ctx->symbols, (Elf64_Sym){0});
    return da_last(&ctx->symbols);
}

INLINE Elf64_Shdr *new_section_header(Elf_Builder *ctx) {
    da_append(&ctx->section_headers, (Elf64_Shdr){0});
    return da_last(&ctx->section_headers);
}

void Elf_Builder_init(Elf_Builder *ctx, const char *filename) {
    *ctx = (Elf_Builder){0};

    usize null_name_off_strtab = st_append(&ctx->strtab, "");
    ZAG_ASSERT(null_name_off_strtab == 0);
    da_append(&ctx->symbols, (Elf64_Sym){0});

    usize filename_off = st_append(&ctx->strtab, filename);
    Elf64_Sym *file_symbol = new_symbol(ctx);
    *file_symbol = (Elf64_Sym){
        .st_name = filename_off,
        .st_info = ELF64_ST_INFO(STB_LOCAL, STT_FILE),
        .st_other = STV_DEFAULT,
        .st_shndx = SHN_ABS,
        .st_value = 0,
        .st_size = 0,
    };

    Elf64_Sym *data_section_symbol = new_symbol(ctx);
    *data_section_symbol = (Elf64_Sym){
        .st_name = 0,
        .st_info = ELF64_ST_INFO(STB_LOCAL, STT_SECTION),
        .st_other = STV_DEFAULT,
        .st_shndx = DATA_SECTION_IDX,
        .st_value = 0,
        .st_size = 0,
    };

    ctx->data_section_symtab_idx = ctx->symbols.size - 1;
}

void Elf_Builder_delete(Elf_Builder *ctx) {
    da_delete(&ctx->text);
    da_delete(&ctx->symbols);
    da_delete(&ctx->strtab);
    da_delete(&ctx->shstrtab);
    da_delete(&ctx->section_headers);
}

void Elf_Builder_compile(Elf_Builder *ctx) {
    usize offset = sizeof(Elf64_Ehdr);

    // sections

    // null
    usize null_name_off = st_append(&ctx->shstrtab, "");
    ZAG_ASSERT(null_name_off == 0);
    assert(NULL_SECTION_IDX == ctx->section_headers.size);
    da_append(&ctx->section_headers, (Elf64_Shdr){0});

    // text
    usize text_name_off = st_append(&ctx->shstrtab, ".text");
    ZAG_ASSERT(TEXT_SECTION_IDX == ctx->section_headers.size);
    Elf64_Shdr *text_section_header = new_section_header(ctx);
    *text_section_header = (Elf64_Shdr){
        .sh_name = text_name_off,
        .sh_type = SHT_PROGBITS,
        .sh_flags = SHF_ALLOC | SHF_EXECINSTR,
        .sh_addr = 0,
        .sh_offset = offset,
        .sh_size = ctx->text.size,
        .sh_link = 0,
        .sh_info = 0,
        .sh_addralign = 1,
        .sh_entsize = 0,
    };

    offset += text_section_header->sh_size;

    // rela.text
    usize rela_text_name_off = st_append(&ctx->shstrtab, ".rela.text");
    ZAG_ASSERT(RELA_TEXT_SECTION_IDX == ctx->section_headers.size);
    Elf64_Shdr *rela_text_section_header = new_section_header(ctx);
    *rela_text_section_header = (Elf64_Shdr){
        .sh_name = rela_text_name_off,
        .sh_type = SHT_RELA,
        .sh_flags = SHF_INFO_LINK,
        .sh_addr = 0,
        .sh_offset = offset,
        .sh_size = ctx->relocations.size * sizeof(Elf64_Rela),
        .sh_link = SYMTAB_SECTION_IDX,
        .sh_info = TEXT_SECTION_IDX,
        .sh_addralign = _Alignof(Elf64_Rela),
        .sh_entsize = sizeof(Elf64_Rela),
    };

    offset += rela_text_section_header->sh_size;

    // data
    usize data_text_name_off = st_append(&ctx->shstrtab, ".data");
    ZAG_ASSERT(DATA_SECTION_IDX == ctx->section_headers.size);
    Elf64_Shdr *data_section_header = new_section_header(ctx);
    *data_section_header = (Elf64_Shdr) {
        .sh_name = data_text_name_off,
        .sh_type = SHT_PROGBITS,
        .sh_flags = SHF_WRITE | SHF_ALLOC,
        .sh_addr = 0,
        .sh_offset = offset,
        .sh_size = ctx->data.len,
        .sh_link = 0,
        .sh_info = 0,
        .sh_addralign = 1,
        .sh_entsize = 0,
    };

    offset += data_section_header->sh_size;

    // note.gnu-stack
    usize note_gnu_stack_name_off = st_append(&ctx->shstrtab, ".note.GNU-stack");
    ZAG_ASSERT(NOTE_GNU_STACK_SECTION_IDX == ctx->section_headers.size);
    Elf64_Shdr *note_gnu_stack_section_header = new_section_header(ctx);
    *note_gnu_stack_section_header = (Elf64_Shdr){
        .sh_name = note_gnu_stack_name_off,
        .sh_type = SHT_PROGBITS,
        .sh_offset = offset,
        .sh_addralign = 1,
    };

    // symtab
    usize symtab_name_off = st_append(&ctx->shstrtab, ".symtab");
    ZAG_ASSERT(SYMTAB_SECTION_IDX == ctx->section_headers.size);
    Elf64_Shdr *symtab_section_header = new_section_header(ctx);
    *symtab_section_header = (Elf64_Shdr){
        .sh_name = symtab_name_off,
        .sh_type = SHT_SYMTAB,
        .sh_flags = 0,
        .sh_addr = 0,
        .sh_offset = offset,
        .sh_size = ctx->symbols.size * sizeof(Elf64_Sym),
        .sh_link = STRTAB_SECTION_IDX,
        .sh_info = 3,  // first non-local symbol
        .sh_addralign = 8,
        .sh_entsize = sizeof(Elf64_Sym),
    };

    offset += symtab_section_header->sh_size;

    // strtab
    ZAG_ASSERT(STRTAB_SECTION_IDX == ctx->section_headers.size);
    usize strtab_name_off = st_append(&ctx->shstrtab, ".strtab");
    Elf64_Shdr *strtab_section_header = new_section_header(ctx);
    *strtab_section_header = (Elf64_Shdr){
        .sh_name = strtab_name_off,
        .sh_type = SHT_STRTAB,
        .sh_flags = 0,
        .sh_addr = 0,
        .sh_offset = offset,
        .sh_size = ctx->strtab.size,
        .sh_link = 0,
        .sh_info = 0,
        .sh_addralign = 1,
        .sh_entsize = 0,
    };

    offset += strtab_section_header->sh_size;

    // shstrtab
    usize shstrtab_name_off = st_append(&ctx->shstrtab, ".shstrtab");
    ZAG_ASSERT(SHSTRTAB_SECTION_IDX == ctx->section_headers.size);
    Elf64_Shdr *shstrtab_section_header = new_section_header(ctx);
    *shstrtab_section_header = (Elf64_Shdr){
        .sh_name = shstrtab_name_off,
        .sh_type = SHT_STRTAB,
        .sh_flags = 0,
        .sh_addr = 0,
        .sh_offset = offset,
        .sh_size = ctx->shstrtab.size,
        .sh_link = 0,
        .sh_info = 0,
        .sh_addralign = 1,
        .sh_entsize = 0,
    };

    offset += shstrtab_section_header->sh_size;

    // header
    ctx->header = (Elf64_Ehdr){
        .e_ident = {
            [EI_MAG0] = ELFMAG0,
            [EI_MAG1] = ELFMAG1,
            [EI_MAG2] = ELFMAG2,
            [EI_MAG3] = ELFMAG3,
            [EI_CLASS] = ELFCLASS64,
            [EI_DATA] = ELFDATA2LSB,
            [EI_VERSION] = EV_CURRENT,
            [EI_OSABI] = ELFOSABI_NONE,
            [EI_ABIVERSION] = 0,
        },
        .e_type = ET_REL,
        .e_machine = EM_X86_64,
        .e_version = EV_CURRENT,
        .e_entry = 0,
        .e_phoff = 0,
        .e_shoff = offset,
        .e_flags = 0,
        .e_ehsize = sizeof(Elf64_Ehdr),
        .e_phentsize = 0,
        .e_phnum = 0,
        .e_shentsize = sizeof(Elf64_Shdr),
        .e_shnum = ctx->section_headers.size,
        .e_shstrndx = SHSTRTAB_SECTION_IDX,
    };
}

void Elf_write_o_file(const Elf_Builder *ctx, FILE *out) {
    String_Builder o = {0};

    sb_append_buf(&o, (char *)&ctx->header, sizeof(Elf64_Ehdr));
    sb_append_buf(&o, ctx->text.store, ctx->text.size);
    sb_append_buf(&o, (char *)ctx->relocations.store, ctx->relocations.size * sizeof(Elf64_Rela));
    sb_append_buf(&o, ctx->data.store, ctx->data.len);
    sb_append_buf(&o, (char *)ctx->symbols.store, ctx->symbols.size * sizeof(Elf64_Sym));
    sb_append_buf(&o, ctx->strtab.store, ctx->strtab.size);
    sb_append_buf(&o, ctx->shstrtab.store, ctx->shstrtab.size);
    sb_append_buf(&o, (char *)ctx->section_headers.store, ctx->section_headers.size * sizeof(Elf64_Shdr));

    ZAG_ASSERT(fwrite(o.store, 1, o.size, out) == o.size);

    sb_free(&o);
}

void Elf_add_data(Elf_Builder *ctx, const char *v, usize n) {
    ctx->data = (sv){.store = v, .len = n};
}

usize Elf_new_func(Elf_Builder *ctx, sv name,
                   const char *code, usize n) {
    usize name_tab_off = st_append_sv(&ctx->strtab, name);

    usize text_off = ctx->text.size;
    da_append_buf(&ctx->text, code, n);

    *new_symbol(ctx) = (Elf64_Sym){
        .st_name = name_tab_off,
        .st_info = ELF64_ST_INFO(STB_GLOBAL, STT_FUNC),
        .st_other = STV_DEFAULT,
        .st_shndx = TEXT_SECTION_IDX,
        .st_value = text_off,
        .st_size = n,
    };

    return ctx->symbols.size - 1;
}

void Elf_add_relocation(Elf_Builder *ctx, u64 code_offset, usize symbol) {
    *new_relocation(ctx) = (Elf64_Rela){
        .r_offset = code_offset,
        .r_info = ELF64_R_INFO(symbol, R_X86_64_PLT32),
        .r_addend = -0x4,
    };
}

void Elf_add_data_reloc(Elf_Builder *ctx, u64 code_offset, u64 data_offset) {
    *new_relocation(ctx) = (Elf64_Rela){
        .r_offset = code_offset,
        .r_info = ELF64_R_INFO(ctx->data_section_symtab_idx, R_X86_64_PC32),
        .r_addend = -0x4 + data_offset,
    };
}

usize Elf_add_external_func(Elf_Builder *ctx, sv name) {
    usize name_tab_off = st_append_sv(&ctx->strtab, name);

    *new_symbol(ctx) = (Elf64_Sym){
        .st_name = name_tab_off,
        .st_info = ELF64_ST_INFO(STB_GLOBAL, STT_NOTYPE),
        .st_other = STV_DEFAULT,
        .st_shndx = SHN_UNDEF,
    };

    return ctx->symbols.size - 1;
}

#endif  // ELF_C
