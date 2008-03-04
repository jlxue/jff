#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <sys/user.h>
#include <fcntl.h>
#include <unistd.h>
#include <elf.h>

struct strtable {
    int index;      /* section index */
    int num;
    char** strings;
};

struct strtables {
    int num;
    struct strtable* tables;
};

char* nameofELFCLASS(unsigned char param);
char* nameofELFDATA(unsigned char param);
char* nameofELFOSABI_(unsigned char param);
char* nameofET_(uint16_t param);
char* nameofEM_(uint16_t param);
char* nameofPT_(uint32_t param);
char* nameofSHT_(uint32_t param);


void dump_SHT_PROGBITS_section(void* data, size_t length, int i);
void dump_SHT_SYMTAB_section(void* data, size_t length, int i);
void dump_SHT_STRTAB_section(void* data, size_t length, int i);
void dump_SHT_RELA_section(void* data, size_t length, int i);
void dump_SHT_HASH_section(void* data, size_t length, int i);
void dump_SHT_DYNAMIC_section(void* data, size_t length, int i);
void dump_SHT_NOTE_section(void* data, size_t length, int i);
void dump_SHT_NOBITS_section(void* data, size_t length, int i);
void dump_SHT_REL_section(void* data, size_t length, int i);
void dump_SHT_SHLIB_section(void* data, size_t length, int i);
void dump_SHT_DYNSYM_section(void* data, size_t length, int i);
void dump_SHT_INIT_ARRAY_section(void* data, size_t length, int i);
void dump_SHT_FINI_ARRAY_section(void* data, size_t length, int i);
void dump_SHT_PREINIT_ARRAY_section(void* data, size_t length, int i);
void dump_SHT_GROUP_section(void* data, size_t length, int i);
void dump_SHT_SYMTAB_SHNDX_section(void* data, size_t length, int i);
void dump_SHT_NUM_section(void* data, size_t length, int i);
void dump_SHT_GNU_LIBLIST_section(void* data, size_t length, int i);
void dump_SHT_CHECKSUM_section(void* data, size_t length, int i);
void dump_SHT_SUNW_move_section(void* data, size_t length, int i);
void dump_SHT_SUNW_COMDAT_section(void* data, size_t length, int i);
void dump_SHT_SUNW_syminfo_section(void* data, size_t length, int i);
void dump_SHT_GNU_verdef_section(void* data, size_t length, int i);
void dump_SHT_GNU_verneed_section(void* data, size_t length, int i);
void dump_SHT_GNU_versym_section(void* data, size_t length, int i);


#define DEFAULT_NAME_COUNT      512
char** parse_strtable(char* data, size_t length, int* n) {
    char* p = data;
    char* end = data + length;
    char** strings = (char**)malloc(sizeof(char*) * DEFAULT_NAME_COUNT);
    int capacity = DEFAULT_NAME_COUNT;
    int i;
    
    assert(strings != NULL && *p == '\0');

    strings[0] = p;
    i = 0;
    --end;      /* so we don't have to check (p + 1) < end */
    while (p < end) {
        if (*p == '\0') {
            ++i;
            if (i == capacity) {
                char** new_strings;
                capacity = capacity + 128;
                new_strings = (char**)malloc(sizeof(char*) * capacity);
                assert(new_strings != NULL);
                memcpy(new_strings, strings, sizeof(char*) * i);
                free(strings);
                strings = new_strings;
            }
            strings[i] = p + 1;
            //printf("strings[%d]=%s\n", i, strings[i]);
        }
        ++p;
    }

    *n = i + 1;
    return strings;
}

void parse_strtables(void* data, size_t length, struct strtables* strtabs) {
    Elf32_Ehdr* ehdr = (Elf32_Ehdr*)data;
    Elf32_Shdr* shdrs = (Elf32_Shdr*)((char*)data + ehdr->e_shoff);
    int shnum = ehdr->e_shnum;
    int i, n;

    assert(strtabs != NULL);

    n = 0;
    for (i = 0; i < shnum; ++i) {
        if (shdrs[i].sh_type == SHT_STRTAB)
            ++n;
    }

    strtabs->num = n;
    strtabs->tables = (struct strtable*)malloc(n * sizeof(struct strtable));
    assert(strtabs->tables != NULL);
    memset(strtabs->tables, 0, n * sizeof(struct strtable));

    n = 0;
    for (i = 0; i < shnum; ++i) {
        if (shdrs[i].sh_type == SHT_STRTAB) {
            strtabs->tables[n].index = i;
            strtabs->tables[n].strings = parse_strtable((char*)data + shdrs[i].sh_offset,
                    shdrs[i].sh_size, &strtabs->tables[n].num);
            ++n;
        }
    }
}

char* get_str(void* data, int n, int i) {
    Elf32_Ehdr* ehdr = (Elf32_Ehdr*)data;
    Elf32_Shdr* shdrs = (Elf32_Shdr*)((char*)data + ehdr->e_shoff);
    char* str = NULL;

    assert(n < ehdr->e_shnum && i < shdrs[n].sh_size);
    str = (char*)data + shdrs[n].sh_offset + i;

    return str;
}


void dump_ehdr(Elf32_Ehdr* ehdr) {
    int i;
    unsigned char b;

    puts("Elf32_Ehdr:");

    fputs("e_ident:", stdout);
    for (i = 0; i < EI_NIDENT; ++i)
        printf(" %x", ehdr->e_ident[i]);
    puts("");

    printf("\te_ident[EI_MAG0=%d]: %x\n", EI_MAG0, ehdr->e_ident[EI_MAG0]);
    printf("\te_ident[EI_MAG1=%d]: %c\n", EI_MAG1, ehdr->e_ident[EI_MAG1]);
    printf("\te_ident[EI_MAG2=%d]: %c\n", EI_MAG2, ehdr->e_ident[EI_MAG2]);
    printf("\te_ident[EI_MAG3=%d]: %c\n", EI_MAG3, ehdr->e_ident[EI_MAG3]);
    assert(memcmp(ehdr->e_ident, ELFMAG, SELFMAG) == 0);

    b = ehdr->e_ident[EI_CLASS];
    printf("\te_ident[EI_CLASS=%d]: %s\n", EI_CLASS, nameofELFCLASS(b));
    assert(b == ELFCLASS32);

    b = ehdr->e_ident[EI_DATA];
    printf("\te_ident[EI_DATA=%d]: %s\n", EI_DATA, nameofELFDATA(b));

    b = ehdr->e_ident[EI_VERSION];
    printf("\te_ident[EI_VERSION=%d]: %s\n", EI_VERSION,
            b == EV_CURRENT ? "EV_CURRENT" :
            b == EV_NONE ? "EV_NONE" : "invalid");

    b = ehdr->e_ident[EI_OSABI];
    printf("\te_ident[EI_OSABI=%d]: %s\n", EI_OSABI, nameofELFOSABI_(b));

    printf("\te_ident[EI_ABIVERSION=%d]: %d\n", EI_ABIVERSION,
            ehdr->e_ident[EI_ABIVERSION]);

    printf("\te_ident[EI_PAD=%d]: %d\n", EI_PAD,
            ehdr->e_ident[EI_PAD]);


    printf("e_type: %s\n", nameofET_(ehdr->e_type));

    printf("e_machine: %s\n", nameofEM_(ehdr->e_machine));

    printf("e_version: %s\n",
            ehdr->e_version == EV_CURRENT? "EV_CURRENT" :
            ehdr->e_version == EV_NONE ? "EV_NONE" : "invalid");

    printf("e_entry: %10x\n", ehdr->e_entry);
    printf("e_phoff: %d\n", ehdr->e_phoff);
    printf("e_shoff: %d\n", ehdr->e_shoff);
    printf("e_flags: %d\n", ehdr->e_flags);
    printf("e_ehsize: %d\n", ehdr->e_ehsize);
    assert(sizeof(Elf32_Ehdr) == ehdr->e_ehsize);

    printf("e_phentsize: %d\n", ehdr->e_phentsize);
    printf("e_phnum: %d\n", ehdr->e_phnum);
    printf("e_shentsize: %d\n", ehdr->e_shentsize);
    printf("e_shnum: %d\n", ehdr->e_shnum);
    printf("e_shstrndx: %d\n", ehdr->e_shstrndx);
} 

void dump_phdrs(void* data, size_t length) {
    Elf32_Ehdr* ehdr = (Elf32_Ehdr*)data;
    Elf32_Phdr* phdrs = (Elf32_Phdr*)((char*)data + ehdr->e_phoff);
    int phnum = ehdr->e_phnum;
    int phentsize = ehdr->e_phentsize;
    int i;
    uint32_t type;
    char flags[4];

    assert(sizeof(Elf32_Phdr) == ehdr->e_phentsize);

    puts("Elf32_Phdr:");
    flags[3] = '\0';

    puts("[Nr] p_type              p_offset  p_vaddr  p_paddr  p_filesz  p_memsz  p_flags  p_align");
    for (i = 0; i < phnum; ++i) {
        type = phdrs[i].p_type;
        flags[0] = flags[1] = flags[2] = ' ';
        if ((phdrs[i].p_flags & PF_R) == PF_R)
            flags[0] = 'R';
        if ((phdrs[i].p_flags & PF_W) == PF_W)
            flags[1] = 'W';
        if ((phdrs[i].p_flags & PF_X) == PF_X)
            flags[2] = 'X';

        printf("[%2d] %-12s     %8u %10u %10u %8u %8u %6s %6u\n",
                i,
                nameofPT_(type),
                phdrs[i].p_offset,
                phdrs[i].p_vaddr,
                phdrs[i].p_paddr,
                phdrs[i].p_filesz,
                phdrs[i].p_memsz,
                flags,
                phdrs[i].p_align);
        assert(phdrs[i].p_offset + phdrs[i].p_filesz <= length);
        assert(phdrs[i].p_offset == phdrs[i].p_vaddr % PAGE_SIZE);
        assert(phdrs[i].p_filesz <= phdrs[i].p_memsz);
    }
}


void dump_shdrs(void* data, size_t length) {
    Elf32_Ehdr* ehdr = (Elf32_Ehdr*)data;
    Elf32_Shdr* shdrs = (Elf32_Shdr*)((char*)data + ehdr->e_shoff);
    int shnum = ehdr->e_shnum;
    int shentsize = ehdr->e_shentsize;
    int i;
    uint32_t type;
    char flags[4];

    assert(sizeof(Elf32_Shdr) == ehdr->e_shentsize);
    puts("Elf32_Shdr:");
    flags[3] = '\0';

    puts("[Nr]  name            type             flags       addr     offset size link info addralign entsize");
    for (i = 0; i < shnum; ++i) {
        type = shdrs[i].sh_type;
        flags[0] = flags[1] = flags[2] = ' ';
        if ((shdrs[i].sh_flags & SHF_WRITE) == SHF_WRITE)
            flags[0] = 'W';
        if ((shdrs[i].sh_flags & SHF_EXECINSTR) == SHF_EXECINSTR)
            flags[1] = 'E';
        if ((shdrs[i].sh_flags & SHF_ALLOC) == SHF_ALLOC)
            flags[2] = 'A';

        printf("[%2d] %-16s %-16s [%3s] %10u %10u %4d %2d %2d %2d %2d\n",
                i,
                get_str(data, ehdr->e_shstrndx, shdrs[i].sh_name),
                nameofSHT_(type),
                flags,
                shdrs[i].sh_addr,
                shdrs[i].sh_offset,
                shdrs[i].sh_size,
                shdrs[i].sh_link,
                shdrs[i].sh_info,
                shdrs[i].sh_addralign,
                shdrs[i].sh_entsize);
    }
}

void dump_sections(void* data, size_t length) {
    Elf32_Ehdr* ehdr = (Elf32_Ehdr*)data;
    Elf32_Shdr* shdrs = (Elf32_Shdr*)((char*)data + ehdr->e_shoff);
    int shnum = ehdr->e_shnum;
    int shentsize = ehdr->e_shentsize;
    int i;

    assert(sizeof(Elf32_Shdr) == ehdr->e_shentsize);

    for (i = 0; i < shnum; ++i) {
        if (shdrs[i].sh_type == SHT_NULL) {
            printf("[%2d] <SHT_NULL>\n\n", i);
            continue;
        }
        printf("[%2d] <%s>\n\n", i, get_str(data, ehdr->e_shstrndx, shdrs[i].sh_name));
        switch (shdrs[i].sh_type) {
            case SHT_PROGBITS:
                dump_SHT_PROGBITS_section(data, length, i);
                break;
            case SHT_SYMTAB:
                dump_SHT_SYMTAB_section(data, length, i);
                break;
            case SHT_STRTAB:
                dump_SHT_STRTAB_section(data, length, i);
                break;
            case SHT_RELA:
                dump_SHT_RELA_section(data, length, i);
                break;
            case SHT_HASH:
                dump_SHT_HASH_section(data, length, i);
                break;
            case SHT_DYNAMIC:
                dump_SHT_DYNAMIC_section(data, length, i);
                break;
            case SHT_NOTE:
                dump_SHT_NOTE_section(data, length, i);
                break;
            case SHT_NOBITS:
                dump_SHT_NOBITS_section(data, length, i);
                break;
            case SHT_REL:
                dump_SHT_REL_section(data, length, i);
                break;
            case SHT_SHLIB:
                dump_SHT_SHLIB_section(data, length, i);
                break;
            case SHT_DYNSYM:
                dump_SHT_DYNSYM_section(data, length, i);
                break;
            case SHT_INIT_ARRAY:
                dump_SHT_INIT_ARRAY_section(data, length, i);
                break;
            case SHT_FINI_ARRAY:
                dump_SHT_FINI_ARRAY_section(data, length, i);
                break;
            case SHT_PREINIT_ARRAY:
                dump_SHT_PREINIT_ARRAY_section(data, length, i);
                break;
            case SHT_GROUP:
                dump_SHT_GROUP_section(data, length, i);
                break;
            case SHT_SYMTAB_SHNDX:
                dump_SHT_SYMTAB_SHNDX_section(data, length, i);
                break;
            case SHT_NUM:
                dump_SHT_NUM_section(data, length, i);
                break;
            case SHT_GNU_LIBLIST:
                dump_SHT_GNU_LIBLIST_section(data, length, i);
                break;
            case SHT_CHECKSUM:
                dump_SHT_CHECKSUM_section(data, length, i);
                break;
            case SHT_SUNW_move:
                dump_SHT_SUNW_move_section(data, length, i);
                break;
            case SHT_SUNW_COMDAT:
                dump_SHT_SUNW_COMDAT_section(data, length, i);
                break;
            case SHT_SUNW_syminfo:
                dump_SHT_SUNW_syminfo_section(data, length, i);
                break;
            case SHT_GNU_verdef:
                dump_SHT_GNU_verdef_section(data, length, i);
                break;
            case SHT_GNU_verneed:
                dump_SHT_GNU_verneed_section(data, length, i);
                break;
            case SHT_GNU_versym:
                dump_SHT_GNU_versym_section(data, length, i);
                break;
            default:
                printf("[%2d] <--other-->\n\n", i);
                break;
        } /* end of switch */
    } /* end of for */
}

void dump_elf(void* data, size_t length) {
    Elf32_Ehdr* ehdr = (Elf32_Ehdr*)data;
    struct strtables strtabs;

    assert(length > sizeof(Elf32_Ehdr));

    dump_ehdr(ehdr);
    puts("");

    memset(&strtabs, 0, sizeof(strtabs));
    parse_strtables(data, length, &strtabs);

    if (ehdr->e_phoff > 0) {
        assert(ehdr->e_phoff + ehdr->e_phentsize * ehdr->e_phnum < length);
        dump_phdrs(data, length);
        puts("");
    } else
        puts("No program header\n");

    if (ehdr->e_shoff > 0) {
        assert(ehdr->e_shoff + ehdr->e_shentsize * ehdr->e_shnum < length);
        dump_shdrs(data, length);
        puts("");
    } else
        puts("No section header\n");

    dump_sections(data, length);
}


int main(int argc, char** argv) {
    int fd;
    size_t length;
    void* elf_data;
    struct stat st;
    
    if (argc < 2) {
        printf("Usage: %s elf_file\n", argv[0]);
        return EXIT_FAILURE;
    }

    fd = open(argv[1], O_RDONLY);
    if (-1 == fd) {
        perror("open()");
        return EXIT_FAILURE;
    }

    if (-1 == fstat(fd, &st)) {
        perror("fstat()");
        return EXIT_FAILURE;
    }
    length = st.st_size;

    elf_data = mmap(0, length, PROT_READ, MAP_PRIVATE, fd, 0);

    dump_elf(elf_data, length);

    munmap(elf_data, length);
    close(fd);

    return EXIT_SUCCESS;
}

