/* autogen.c - C89 AutoGenerator of Makefiles and other wonders!

    gcc autogen.c -o autogen
    ./autogen */
#include <ctype.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Autogenerate makefiles and such from the given input */
static const char* files[] = { "as386.c", "ast.c", "constevl.c", "diag.c",
    "lexer.c", "main.c", "mf370.c", "optzer.c", "parexpr.c", "parser.c",
    "partyp.c", "ssa.c", "util.c" };
static const char* project_name = "occ";
static const char* gcc_opt = "-DTARGET_AS386=1 -std=c99 -O3 -I.";
static const char* pdpclib_path = "../pdos/pdpclib";

/* Makefile for windows */
static void makefile_win(FILE* f)
{
    fprintf(f, "AS=aswin\n");
    fprintf(f, "CC=gccwin\n");
    fprintf(f, "LD=ldwin\n");
    fprintf(f, "CFLAGS=%s -fno-common -D__WIN32__ -D__NOBIVA__ -I%s\n", gcc_opt,
        pdpclib_path);

    fprintf(f, "all: clean %s.exe\n", project_name);
    fprintf(f, "%s.exe: $(OBJS)\n", project_name);
    fprintf(f, "  $(LD) -s -o %s.exe %s/w32start.o $(OBJS) %s/msvcrt.a\n",
        project_name, pdpclib_path, pdpclib_path);
    fprintf(f, ".c.o:\n");
    fprintf(f, "  $(CC) $(CFLAGS) $<\n");
    fprintf(f, "  $(AS) -o $@ $*.s\n");
    fprintf(f, "  rm -f $*.s\n");
    fprintf(f, "clean:\n");
    fprintf(f, "  rm -f *.o %s.exe\n", project_name);
}

static const char* subst_extension(
    const char* str, const char* s1, const char* s2)
{
    static char tmpbuf[80];
    char* s;
    strcpy(tmpbuf, str);
    if ((s = strstr(tmpbuf, s1)) != NULL)
        memcpy(s, s2, strlen(s2 + 1));
    return tmpbuf;
}

static void makefile_lnx(FILE* f)
{
    size_t i;
    fprintf(f, "CC=gcc\n");
    fprintf(f, "CFLAGS=-std=c99 -DANSI_COLOUR=1 -DTARGET_AS386=1\n");
    fprintf(f, "all: build\n");
    fprintf(f, "run: build\n");
    fprintf(f, "\t./%s\n", project_name);
    fprintf(f, "build: %s\n", project_name);
    fprintf(f, "clean:\n");
    fprintf(f, "\t$(RM) *.o %s\n", project_name);
    fprintf(f, ".PHONY: build all clean\n");
    /* Dependency list */
    fprintf(f, "%s: ", project_name);
    for (i = 0; i < (sizeof(files) / sizeof(files[0])); i++) {
        const char* file = subst_extension(files[i], ".c", ".o");
        fprintf(f, "%s ", file);
    }
    fprintf(f, "\n");
    fprintf(f, "\t$(CC) $(CFLAGS) $^ -o $@\n");
    fprintf(f, "%%.o: %%.c %%.h\n");
    fprintf(f, "\t$(CC) $(CFLAGS) -c $< -o $@\n");
    fprintf(f, "%%.o: %%.c\n");
    fprintf(f, "\t$(CC) $(CFLAGS) -c $< -o $@\n");
}

static const char* mvs_name(const char* s)
{
    static char tmpbuf[80];
    size_t i, j = 0;
    for (i = 0; i < strlen(s); i++)
        if (isalnum(s[i]))
            tmpbuf[j++] = toupper(s[i]);
    tmpbuf[j] = '\0';
    return tmpbuf;
}

static void mvs_jcl(FILE* f)
{
    size_t i;
    fprintf(f,
        "//PMALIAS JOB CLASS=C,REGION=0K\n"
        "//IDCAMS   EXEC PGM=IDCAMS\n"
        "//SYSPRINT DD  SYSOUT=*\n"
        "//SYSIN    DD  *\n");

    fprintf(f,
        "  DEFINE ALIAS (NAME(%s) RELATE(SYS1.UCAT.TSO)) -\n"
        "		 CATALOG(SYS1.VMASTCAT/SECRET)\n"
        "  SET MAXCC=0\n",
        mvs_name(project_name));

    fprintf(f,
        "//PMDELET JOB CLASS=C,REGION=0K\n"
        "//CREATE   PROC PMPREF='%s'\n"
        "//DELETE   EXEC PGM=IEFBR14\n"
        "//DD1      DD DSN=&PMPREF..SOURCE,DISP=(MOD,DELETE),\n"
        "//       UNIT=SYSALLDA,SPACE=(TRK,(0))\n"
        "//DD2      DD DSN=&PMPREF..INCLUDE,DISP=(MOD,DELETE),\n"
        "//       UNIT=SYSALLDA,SPACE=(TRK,(0))\n"
        "//DD4     DD DSN=&PMPREF..LINKLIB,DISP=(MOD,DELETE),\n"
        "//       UNIT=SYSALLDA,SPACE=(TRK,(0))\n"
        "//DD5      DD DSN=&PMPREF..JCL,DISP=(MOD,DELETE),\n"
        "//       UNIT=SYSALLDA,SPACE=(TRK,(0))\n"
        "//ALLOC    EXEC PGM=IEFBR14\n"
        "//DD1      DD DSN=&PMPREF..SOURCE,DISP=(,CATLG),\n"
        "// DCB=(RECFM=VB,LRECL=255,BLKSIZE=6144),\n"
        "// SPACE=(6144,(99,99,44)),UNIT=SYSALLDA\n"
        "//DD2      DD DSN=&PMPREF..INCLUDE,DISP=(,CATLG),\n"
        "// DCB=(RECFM=VB,LRECL=255,BLKSIZE=6144),\n"
        "// SPACE=(6144,(19,19,44)),UNIT=SYSALLDA\n"
        "//DD4      DD DSN=&PMPREF..LINKLIB,DISP=(,CATLG),\n"
        "// DCB=(RECFM=U,LRECL=0,BLKSIZE=6144),\n"
        "// SPACE=(6144,(46,46,44),,,ROUND),UNIT=SYSALLDA\n"
        "//DD5      DD DSN=&PMPREF..JCL,DISP=(,CATLG),\n"
        "// DCB=(RECFM=FB,LRECL=80,BLKSIZE=6080),\n"
        "// SPACE=(6080,(16,16,44)),UNIT=SYSALLDA\n"
        "//         PEND\n"
        "//S1 EXEC CREATE\n",
        mvs_name(project_name));

    fprintf(f,
        "//PMTRANSF JOB CLASS=C,REGION=0K\n"
        "//TRANSFER PROC PMPREF='%s'\n"
        "//DELETE   EXEC PGM=IEFBR14\n"
        "//DD1      DD DSN=&PMPREF..ALLZIPS,DISP=(MOD,DELETE),\n"
        "//       UNIT=SYSALLDA,SPACE=(TRK,(0))\n"
        "//COPY     EXEC PGM=IEBGENER\n"
        "//SYSUT1   DD DSN=HERC02.IN,DISP=OLD,\n"
        "//         UNIT=TAPE,VOL=SER=PCTOMF,LABEL=(1,NL),\n"
        "//         DCB=(RECFM=U,LRECL=0,BLKSIZE=6144)\n"
        "//SYSUT2   DD DSN=&PMPREF..ALLZIPS,DISP=(,CATLG),\n"
        "//         SPACE=(6144,(200,200),RLSE),UNIT=SYSALLDA,\n"
        "//         DCB=(RECFM=U,LRECL=0,BLKSIZE=6144)\n"
        "//SYSIN    DD DUMMY\n"
        "//SYSPRINT DD SYSOUT=*\n"
        "//         PEND\n",
        mvs_name(project_name));

    fprintf(f,
        "//S1 EXEC TRANSFER\n"
        "//\n");

    fprintf(f,
        "//PMUNZIP  JOB CLASS=C,REGION=0K\n"
        "//CREATE   EXEC PGM=IEFBR14\n"
        "//DD1      DD DSN=&&ZIPS,DISP=(,PASS),\n"
        "// DCB=(RECFM=U,LRECL=0,BLKSIZE=6144),\n"
        "// SPACE=(6144,(200,200,44)),UNIT=SYSALLDA\n"
        "//UNZIP1   PROC MINPREF='MINIZIP',PMPREF='%s'\n"
        "//MINI     EXEC PGM=MINIUNZ,PARM='dd:input dd:output'\n"
        "//STEPLIB  DD DSN=&MINPREF..LINKLIB,DISP=SHR\n"
        "//INPUT    DD DSN=&PMPREF..ALLZIPS,DISP=SHR\n"
        "//OUTPUT   DD DSN=&&ZIPS,DISP=(OLD,PASS)\n"
        "//SYSIN    DD DUMMY\n"
        "//SYSPRINT DD SYSOUT=*\n"
        "//SYSTERM  DD SYSOUT=*\n"
        "//         PEND\n",
        mvs_name(project_name));

    fprintf(f,
        "//UNZIP2   PROC IN=,OUT=,MINPREF='MINIZIP',PMPREF='%s'\n"
        "//MINI     EXEC PGM=MINIUNZ,PARM='-a dd:input dd:output'\n"
        "//STEPLIB  DD DSN=&MINPREF..LINKLIB,DISP=SHR\n"
        "//INPUT    DD DSN=&&ZIPS(&IN),DISP=(OLD,PASS)\n"
        "//OUTPUT   DD DSN=&PMPREF..&OUT,DISP=SHR\n"
        "//SYSIN    DD DUMMY\n"
        "//SYSPRINT DD SYSOUT=*\n"
        "//SYSTERM  DD SYSOUT=*\n"
        "//         PEND\n",
        mvs_name(project_name));

    fprintf(f,
        "//S1 EXEC UNZIP1\n"
        "//S2 EXEC UNZIP2,IN='PMSRC',OUT='SOURCE'\n"
        "//S3 EXEC UNZIP2,IN='PMINC',OUT='INCLUDE'\n"
        "//S4 EXEC UNZIP2,IN='PMJCL',OUT='JCL'\n");

    fprintf(f,
        "//PMCMPLE JOB CLASS=C,REGION=0K\n"
        "//PMCMP   PROC PMPREF='%s',MEMBER='',GCCPREF='GCC',\n"
        "// PDPPREF='PDPCLIB',\n", mvs_name(project_name));
    fprintf(f,
        "// COS1='-S %s',\n"
        "// COS2='-o dd:out -'\n",
        gcc_opt);
    fprintf(f,
        "//COMPL    EXEC PGM=GCC,\n"
        "// PARAM='&COS1 &COS2'\n"
        "//STEPLIB  DD DSN=&GCCPREF..LINKLIB,DISP=SHR\n"
        "//SYSIN    DD DSN=&PMPREF..SOURCE(&MEMBER),DISP=SHR\n"
        "//INCLUDE  DD DSN=&PMPREF..INCLUDE,DISP=SHR,DCB=BLKSIZE=32720\n"
        "//         DD DSN=&PDPPREF..INCLUDE,DISP=SHR\n"
        "//SYSINCL  DD DSN=&PMPREF..INCLUDE,DISP=SHR,DCB=BLKSIZE=32720\n"
        "//         DD DSN=&PDPPREF..INCLUDE,DISP=SHR\n"
        "//OUT      DD DSN=&&TEMP1,DISP=(,PASS),UNIT=SYSALLDA,\n"
        "//            DCB=(LRECL=80,BLKSIZE=6080,RECFM=FB),\n"
        "//            SPACE=(6080,(500,500))\n"
        "//SYSPRINT DD SYSOUT=*\n"
        "//SYSTERM  DD SYSOUT=*\n"
        "//ASM      EXEC PGM=ASMA90,\n"
        "//            PARM='DECK,LIST',\n"
        "//            COND=(4,LT,COMP)\n"
        "//SYSLIB   DD DSN=SYS1.MACLIB,DISP=SHR,DCB=BLKSIZE=32720\n"
        "//         DD DSN=&PDPPREF..MACLIB,DISP=SHR\n"
        "//SYSUT1   DD UNIT=SYSALLDA,SPACE=(CYL,(2,1))\n"
        "//SYSUT2   DD UNIT=SYSALLDA,SPACE=(CYL,(2,1))\n"
        "//SYSUT3   DD UNIT=SYSALLDA,SPACE=(CYL,(2,1))\n"
        "//SYSPRINT DD SYSOUT=*\n"
        "//SYSLIN   DD DUMMY\n"
        "//SYSGO    DD DUMMY\n"
        "//SYSPUNCH DD DSN=&&OBJSET,UNIT=SYSALLDA,SPACE=(80,(40000,0)),\n"
        "//            DISP=(MOD,PASS)\n"
        "//SYSIN    DD DSN=&&TEMP1,DISP=(OLD,DELETE)\n"
        "//         PEND\n");

    fprintf(f,
        "//LINK     PROC PMPREF='%s',PDPPREF='PDPCLIB'\n"
        "//LKED     EXEC PGM=IEWL,\n"
        "// PARM='MAP,LIST,SIZE=(999424,65536),AMODE=31,RMODE=ANY'\n"
        "//SYSUT1   DD UNIT=SYSDA,SPACE=(CYL,(30,10))\n"
        "//SYSPRINT DD SYSOUT=*\n"
        "//SYSLIN   DD DSN=&&OBJSET,DISP=(OLD,DELETE)\n"
        "//SYSLIB   DD DSN=&PDPPREF..NCALIB,DISP=SHR,DCB=BLKSIZE=32760\n"
        "//SYSLMOD  DD DSN=&PMPREF..LINKLIB(%s),DISP=SHR\n"
        "//         PEND\n",
        mvs_name(project_name), mvs_name(project_name));

    fprintf(f,
        "//LINK     PROC PMPREF='%s',PDPPREF='PDPCLIB'\n"
        "//LKED     EXEC PGM=IEWL,\n"
        "// PARM='MAP,LIST,SIZE=(999424,65536),AMODE=31,RMODE=ANY'\n"
        "//SYSUT1   DD UNIT=SYSDA,SPACE=(CYL,(30,10))\n"
        "//SYSPRINT DD SYSOUT=*\n"
        "//SYSLIN   DD DSN=&&OBJSET,DISP=(OLD,DELETE)\n"
        "//SYSLIB   DD DSN=&PDPPREF..NCALIB,DISP=SHR,DCB=BLKSIZE=32760\n"
        "//SYSLMOD  DD DSN=&PMPREF..LINKLIB(%s),DISP=SHR\n"
        "//         PEND\n",
        mvs_name(project_name), mvs_name(project_name));

    for (i = 0; i < (sizeof(files) / sizeof(files[0])); i++) {
        const char* s = subst_extension(files[i], ".c", "");
        fprintf(f, "//%-8s EXEC PMCMP,MEMBER=%s\n", mvs_name(s), mvs_name(s));
    }

    fprintf(f, "//DOLINK   EXEC LINK\n");
}

int main(int argc, char** argv)
{
    FILE* f = NULL;
    if (argc != 2)
        goto usage;

    if (!strcmp(argv[1], "unix")) {
        f = fopen("Makefile", "wt");
        if (f == NULL) {
            fprintf(stderr, "Unable to create Makefile!\n");
            return -1;
        }
        makefile_lnx(f);
    } else if (!strcmp(argv[1], "win")) {
        f = fopen("Makefile", "wt");
        if (f == NULL) {
            fprintf(stderr, "Unable to create Makefile!\n");
            return -1;
        }
        makefile_win(f);
    } else if (!strcmp(argv[1], "mvs")) {
        f = fopen("occ.jcl", "wt");
        if (f == NULL) {
            fprintf(stderr, "Unable to create JCL!\n");
            return -1;
        }
        mvs_jcl(f);
    } else {
    usage:
        fprintf(stderr, "Usage: %s [unix|win|mvs]\n", argv[0]);
        return -1;
    }

    if (f != NULL)
        fclose(f);
    return 0;
}
