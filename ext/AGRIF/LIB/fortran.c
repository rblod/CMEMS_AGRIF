/* A Bison parser, made by GNU Bison 3.0.4.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "3.0.4"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1


/* Substitute the variable and function names.  */
#define yyparse         fortran_parse
#define yylex           fortran_lex
#define yyerror         fortran_error
#define yydebug         fortran_debug
#define yynerrs         fortran_nerrs

#define yylval          fortran_lval
#define yychar          fortran_char

/* Copy the first part of user declarations.  */
#line 36 "fortran.y" /* yacc.c:339  */

#define YYMAXDEPTH 1000
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "decl.h"

extern int line_num_input;

char c_selectorname[LONG_M];
char ligne[LONG_M];
char truename[LONG_VNAME];
char identcopy[LONG_VNAME];
int c_selectorgiven=0;
listvar *curlistvar;
int in_select_case_stmt=0;
typedim c_selectordim;
listcouple *coupletmp;
int removeline=0;
int token_since_endofstmt = 0;
int increment_nbtokens = 1;
int in_complex_literal = 0;
int close_or_connect = 0;
int in_io_control_spec = 0;
int intent_spec = 0;
long int my_position;
long int my_position_before;
int suborfun = 0;
int indeclaration = 0;
int endoffile = 0;
int in_inquire = 0;
int in_char_selector = 0;
int in_kind_selector =0;
int char_length_toreset = 0;

typedim my_dim;

listvar *test;

char linebuf1[1024];
char linebuf2[1024];

int fortran_error(const char *s)
{
  if (endoffile == 1) 
  {
  endoffile = 0;
  return 0;
  }
    printf("%s line %d, file %s culprit = |%s|\n", s, line_num_input, cur_filename, strcat(linebuf1, linebuf2));
    exit(1);
}


#line 129 "fortran.tab.c" /* yacc.c:339  */

# ifndef YY_NULLPTR
#  if defined __cplusplus && 201103L <= __cplusplus
#   define YY_NULLPTR nullptr
#  else
#   define YY_NULLPTR 0
#  endif
# endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif


/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 1
#endif
#if YYDEBUG
extern int fortran_debug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    TOK_EQV = 258,
    TOK_NEQV = 259,
    TOK_OR = 260,
    TOK_XOR = 261,
    TOK_AND = 262,
    TOK_NOT = 263,
    TOK_LT = 264,
    TOK_GT = 265,
    TOK_LE = 266,
    TOK_GE = 267,
    TOK_EQ = 268,
    TOK_NE = 269,
    TOK_DSLASH = 270,
    TOK_SLASH = 271,
    TOK_DASTER = 272,
    TOK_SEMICOLON = 273,
    TOK_PARAMETER = 274,
    TOK_RESULT = 275,
    TOK_ONLY = 276,
    TOK_INCLUDE = 277,
    TOK_SUBROUTINE = 278,
    TOK_PROGRAM = 279,
    TOK_FUNCTION = 280,
    TOK_LABEL_FORMAT = 281,
    TOK_LABEL_CONTINUE = 282,
    TOK_LABEL_END_DO = 283,
    TOK_MAX = 284,
    TOK_TANH = 285,
    TOK_COMMENT = 286,
    TOK_WHERE = 287,
    TOK_ELSEWHEREPAR = 288,
    TOK_ELSEWHERE = 289,
    TOK_ENDWHERE = 290,
    TOK_MAXVAL = 291,
    TOK_TRIM = 292,
    TOK_NULL_PTR = 293,
    TOK_SUM = 294,
    TOK_SQRT = 295,
    TOK_CASE = 296,
    TOK_SELECTCASE = 297,
    TOK_FILE = 298,
    TOK_REC = 299,
    TOK_NAME_EQ = 300,
    TOK_IOLENGTH = 301,
    TOK_ACCESS = 302,
    TOK_ACTION = 303,
    TOK_FORM = 304,
    TOK_RECL = 305,
    TOK_STATUS = 306,
    TOK_UNIT = 307,
    TOK_OPENED = 308,
    TOK_FMT = 309,
    TOK_NML = 310,
    TOK_END = 311,
    TOK_EOR = 312,
    TOK_EOF = 313,
    TOK_ERR = 314,
    TOK_POSITION = 315,
    TOK_IOSTAT = 316,
    TOK_IOMSG = 317,
    TOK_EXIST = 318,
    TOK_MIN = 319,
    TOK_FLOAT = 320,
    TOK_EXP = 321,
    TOK_LEN = 322,
    TOK_COS = 323,
    TOK_COSH = 324,
    TOK_ACOS = 325,
    TOK_NINT = 326,
    TOK_CYCLE = 327,
    TOK_SIN = 328,
    TOK_SINH = 329,
    TOK_ASIN = 330,
    TOK_EQUIVALENCE = 331,
    TOK_BACKSPACE = 332,
    TOK_LOG = 333,
    TOK_TAN = 334,
    TOK_ATAN = 335,
    TOK_RECURSIVE = 336,
    TOK_ABS = 337,
    TOK_MOD = 338,
    TOK_SIGN = 339,
    TOK_MINLOC = 340,
    TOK_MAXLOC = 341,
    TOK_EXIT = 342,
    TOK_KIND = 343,
    TOK_MOLD = 344,
    TOK_SOURCE = 345,
    TOK_ERRMSG = 346,
    TOK_MINVAL = 347,
    TOK_PUBLIC = 348,
    TOK_PRIVATE = 349,
    TOK_ALLOCATABLE = 350,
    TOK_RETURN = 351,
    TOK_THEN = 352,
    TOK_ELSEIF = 353,
    TOK_ELSE = 354,
    TOK_ENDIF = 355,
    TOK_PRINT = 356,
    TOK_PLAINGOTO = 357,
    TOK_LOGICALIF = 358,
    TOK_LOGICALIF_PAR = 359,
    TOK_PLAINDO = 360,
    TOK_CONTAINS = 361,
    TOK_ENDDO = 362,
    TOK_MODULE = 363,
    TOK_ENDMODULE = 364,
    TOK_WHILE = 365,
    TOK_CONCURRENT = 366,
    TOK_ALLOCATE = 367,
    TOK_OPEN = 368,
    TOK_CLOSE = 369,
    TOK_INQUIRE = 370,
    TOK_WRITE_PAR = 371,
    TOK_WRITE = 372,
    TOK_FLUSH = 373,
    TOK_READ_PAR = 374,
    TOK_READ = 375,
    TOK_REWIND = 376,
    TOK_DEALLOCATE = 377,
    TOK_NULLIFY = 378,
    TOK_DIMENSION = 379,
    TOK_ENDSELECT = 380,
    TOK_EXTERNAL = 381,
    TOK_INTENT = 382,
    TOK_INTRINSIC = 383,
    TOK_NAMELIST = 384,
    TOK_DEFAULT = 385,
    TOK_OPTIONAL = 386,
    TOK_POINTER = 387,
    TOK_CONTINUE = 388,
    TOK_SAVE = 389,
    TOK_TARGET = 390,
    TOK_IMPLICIT = 391,
    TOK_NONE = 392,
    TOK_CALL = 393,
    TOK_STAT = 394,
    TOK_POINT_TO = 395,
    TOK_COMMON = 396,
    TOK_GLOBAL = 397,
    TOK_LEFTAB = 398,
    TOK_RIGHTAB = 399,
    TOK_PAUSE = 400,
    TOK_PROCEDURE = 401,
    TOK_STOP = 402,
    TOK_FOURDOTS = 403,
    TOK_HEXA = 404,
    TOK_ASSIGNTYPE = 405,
    TOK_OUT = 406,
    TOK_INOUT = 407,
    TOK_IN = 408,
    TOK_USE = 409,
    TOK_EQUALEQUAL = 410,
    TOK_SLASHEQUAL = 411,
    TOK_INFEQUAL = 412,
    TOK_SUPEQUAL = 413,
    TOK_TRUE = 414,
    TOK_FALSE = 415,
    TOK_LABEL = 416,
    TOK_LABEL_DJVIEW = 417,
    TOK_PLAINDO_LABEL_DJVIEW = 418,
    TOK_PLAINDO_LABEL = 419,
    TOK_TYPE = 420,
    TOK_TYPEPAR = 421,
    TOK_ENDTYPE = 422,
    TOK_COMMACOMPLEX = 423,
    TOK_REAL = 424,
    TOK_INTEGER = 425,
    TOK_LOGICAL = 426,
    TOK_DOUBLEPRECISION = 427,
    TOK_ENDSUBROUTINE = 428,
    TOK_ENDFUNCTION = 429,
    TOK_ENDPROGRAM = 430,
    TOK_ENDUNIT = 431,
    TOK_CHARACTER = 432,
    TOK_CHAR_CONSTANT = 433,
    TOK_CHAR_CUT = 434,
    TOK_DATA = 435,
    TOK_CHAR_MESSAGE = 436,
    TOK_CSTREAL = 437,
    TOK_COMPLEX = 438,
    TOK_DOUBLECOMPLEX = 439,
    TOK_NAME = 440,
    TOK_CSTINT = 441
  };
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED

union YYSTYPE
{
#line 91 "fortran.y" /* yacc.c:355  */

    char        na[LONG_M];
    listdim     *d;
    listvar     *l;
    listcouple  *lc;
    listname    *lnn;
    typedim     dim1;
    variable    *v;

#line 363 "fortran.tab.c" /* yacc.c:355  */
};

typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE fortran_lval;

int fortran_parse (void);



/* Copy the second part of user declarations.  */

#line 380 "fortran.tab.c" /* yacc.c:358  */

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif

#ifndef YY_ATTRIBUTE
# if (defined __GNUC__                                               \
      && (2 < __GNUC__ || (__GNUC__ == 2 && 96 <= __GNUC_MINOR__)))  \
     || defined __SUNPRO_C && 0x5110 <= __SUNPRO_C
#  define YY_ATTRIBUTE(Spec) __attribute__(Spec)
# else
#  define YY_ATTRIBUTE(Spec) /* empty */
# endif
#endif

#ifndef YY_ATTRIBUTE_PURE
# define YY_ATTRIBUTE_PURE   YY_ATTRIBUTE ((__pure__))
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# define YY_ATTRIBUTE_UNUSED YY_ATTRIBUTE ((__unused__))
#endif

#if !defined _Noreturn \
     && (!defined __STDC_VERSION__ || __STDC_VERSION__ < 201112)
# if defined _MSC_VER && 1200 <= _MSC_VER
#  define _Noreturn __declspec (noreturn)
# else
#  define _Noreturn YY_ATTRIBUTE ((__noreturn__))
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
#endif

#if defined __GNUC__ && 407 <= __GNUC__ * 100 + __GNUC_MINOR__
/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN \
    _Pragma ("GCC diagnostic push") \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")\
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# define YY_IGNORE_MAYBE_UNINITIALIZED_END \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif


#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYSIZE_T yynewbytes;                                            \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / sizeof (*yyptr);                          \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, (Count) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYSIZE_T yyi;                         \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  2
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   4539

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  203
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  524
/* YYNRULES -- Number of rules.  */
#define YYNRULES  1070
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  1734

/* YYTRANSLATE[YYX] -- Symbol number corresponding to YYX as returned
   by yylex, with out-of-bounds checking.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   441

#define YYTRANSLATE(YYX)                                                \
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, without out-of-bounds checking.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     197,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,   199,     2,     2,
     193,   194,    21,    19,     3,    20,     2,   198,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     4,     2,
     195,     5,   196,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,   201,     2,   202,     2,   200,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    53,    54,    55,    56,    57,    58,    59,    60,
      61,    62,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    84,    85,    86,    87,    88,    89,    90,
      91,    92,    93,    94,    95,    96,    97,    98,    99,   100,
     101,   102,   103,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,   127,   128,   129,   130,
     131,   132,   133,   134,   135,   136,   137,   138,   139,   140,
     141,   142,   143,   144,   145,   146,   147,   148,   149,   150,
     151,   152,   153,   154,   155,   156,   157,   158,   159,   160,
     161,   162,   163,   164,   165,   166,   167,   168,   169,   170,
     171,   172,   173,   174,   175,   176,   177,   178,   179,   180,
     181,   182,   183,   184,   185,   186,   187,   188,   189,   190,
     191,   192
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   514,   514,   515,   517,   518,   519,   521,   523,   524,
     525,   526,   529,   530,   531,   533,   534,   542,   560,   564,
     565,   566,   570,   571,   584,   852,   853,  1104,  1105,  1106,
    1107,  1108,  1110,  1111,  1115,  1116,  1117,  1118,  1119,  1120,
    1121,  1122,  1123,  1124,  1125,  1126,  1127,  1128,  1129,  1130,
    1131,  1132,  1133,  1134,  1135,  1137,  1138,  1139,  1140,  1143,
    1144,  1147,  1148,  1149,  1153,  1164,  1165,  1166,  1166,  1167,
    1167,  1169,  1170,  1170,  1179,  1191,  1192,  1195,  1196,  1199,
    1200,  1203,  1204,  1205,  1206,  1207,  1208,  1209,  1211,  1258,
    1259,  1260,  1261,  1262,  1263,  1264,  1266,  1269,  1270,  1271,
    1272,  1274,  1275,  1285,  1286,  1338,  1341,  1342,  1367,  1368,
    1372,  1373,  1386,  1387,  1388,  1389,  1390,  1391,  1392,  1393,
    1394,  1395,  1396,  1397,  1398,  1401,  1402,  1406,  1409,  1410,
    1414,  1415,  1419,  1420,  1423,  1424,  1428,  1432,  1433,  1436,
    1437,  1441,  1442,  1446,  1447,  1448,  1449,  1450,  1451,  1452,
    1453,  1454,  1459,  1460,  1461,  1462,  1463,  1471,  1472,  1473,
    1474,  1475,  1476,  1477,  1478,  1479,  1480,  1481,  1482,  1483,
    1505,  1506,  1507,  1508,  1509,  1510,  1511,  1512,  1513,  1514,
    1515,  1516,  1520,  1523,  1528,  1529,  1533,  1534,  1535,  1536,
    1538,  1542,  1561,  1562,  1566,  1567,  1571,  1572,  1576,  1580,
    1581,  1582,  1593,  1593,  1595,  1596,  1601,  1601,  1603,  1603,
    1605,  1605,  1607,  1607,  1609,  1609,  1611,  1611,  1616,  1617,
    1623,  1625,  1627,  1634,  1635,  1640,  1641,  1646,  1647,  1663,
    1664,  1669,  1670,  1677,  1683,  1684,  1685,  1689,  1690,  1691,
    1694,  1695,  1700,  1701,  1706,  1707,  1708,  1709,  1710,  1714,
    1716,  1718,  1719,  1723,  1725,  1730,  1731,  1732,  1736,  1737,
    1741,  1741,  1746,  1747,  1750,  1751,  1754,  1755,  1758,  1759,
    1763,  1766,  1767,  1770,  1774,  1775,  1778,  1779,  1783,  1784,
    1788,  1792,  1795,  1796,  1797,  1800,  1801,  1805,  1806,  1807,
    1807,  1808,  1811,  1812,  1816,  1839,  1840,  1844,  1845,  1848,
    1849,  1853,  1854,  1855,  1859,  1864,  1866,  1869,  1870,  1874,
    1875,  1879,  1880,  1883,  1884,  1888,  1889,  1893,  1894,  1895,
    1899,  1901,  1916,  1920,  1924,  1928,  1929,  1934,  1935,  1939,
    1944,  1946,  1951,  1955,  1956,  1955,  2023,  2024,  2027,  2028,
    2032,  2033,  2037,  2038,  2040,  2040,  2042,  2044,  2044,  2046,
    2047,  2049,  2051,  2053,  2055,  2060,  2062,  2067,  2101,  2104,
    2107,  2108,  2112,  2118,  2124,  2133,  2137,  2139,  2144,  2145,
    2145,  2150,  2152,  2154,  2156,  2158,  2162,  2168,  2177,  2179,
    2184,  2189,  2193,  2199,  2208,  2210,  2215,  2221,  2230,  2235,
    2258,  2259,  2278,  2279,  2283,  2284,  2288,  2292,  2294,  2296,
    2302,  2301,  2320,  2321,  2325,  2327,  2332,  2333,  2338,  2337,
    2352,  2353,  2356,  2357,  2361,  2371,  2373,  2379,  2381,  2386,
    2387,  2391,  2397,  2404,  2406,  2411,  2412,  2416,  2420,  2425,
    2427,  2429,  2431,  2432,  2433,  2434,  2435,  2439,  2440,  2456,
    2457,  2458,  2459,  2460,  2461,  2462,  2468,  2476,  2481,  2483,
    2481,  2528,  2528,  2537,  2537,  2550,  2551,  2550,  2570,  2572,
    2577,  2594,  2595,  2594,  2602,  2603,  2606,  2607,  2610,  2611,
    2615,  2617,  2618,  2622,  2626,  2630,  2632,  2631,  2643,  2644,
    2648,  2651,  2652,  2656,  2657,  2661,  2664,  2665,  2667,  2668,
    2672,  2676,  2679,  2680,  2684,  2684,  2687,  2688,  2692,  2693,
    2694,  2699,  2700,  2699,  2709,  2710,  2718,  2724,  2732,  2733,
    2736,  2738,  2737,  2747,  2749,  2757,  2763,  2763,  2772,  2773,
    2774,  2775,  2784,  2787,  2800,  2803,  2807,  2811,  2814,  2818,
    2821,  2824,  2828,  2829,  2831,  2846,  2851,  2856,  2857,  2862,
    2864,  2864,  2876,  2880,  2885,  2890,  2892,  2899,  2900,  2902,
    2924,  2926,  2928,  2930,  2932,  2934,  2936,  2937,  2939,  2941,
    2945,  2947,  2949,  2951,  2953,  2956,  2970,  2974,  2975,  2974,
    2983,  2984,  2988,  2989,  2993,  2994,  2998,  3002,  3006,  3007,
    3011,  3015,  3016,  3019,  3020,  3024,  3025,  3029,  3032,  3033,
    3037,  3041,  3045,  3046,  3045,  3051,  3052,  3055,  3056,  3060,
    3061,  3065,  3066,  3075,  3085,  3086,  3087,  3088,  3093,  3098,
    3099,  3103,  3104,  3111,  3112,  3114,  3116,  3117,  3122,  3126,
    3128,  3132,  3134,  3139,  3140,  3145,  3148,  3149,  3154,  3155,
    3156,  3157,  3158,  3159,  3160,  3161,  3162,  3164,  3165,  3167,
    3172,  3173,  3179,  3180,  3186,  3187,  3192,  3193,  3198,  3202,
    3206,  3210,  3211,  3215,  3218,  3222,  3226,  3230,  3231,  3234,
    3238,  3245,  3249,  3253,  3256,  3260,  3266,  3267,  3279,  3280,
    3281,  3289,  3290,  3294,  3295,  3299,  3300,  3304,  3308,  3312,
    3315,  3324,  3328,  3329,  3330,  3334,  3338,  3341,  3342,  3345,
    3346,  3349,  3350,  3354,  3358,  3359,  3360,  3364,  3368,  3372,
    3373,  3377,  3378,  3383,  3384,  3388,  3392,  3395,  3396,  3401,
    3402,  3406,  3411,  3412,  3423,  3424,  3425,  3426,  3429,  3430,
    3431,  3432,  3436,  3437,  3438,  3439,  3444,  3445,  3446,  3447,
    3451,  3455,  3464,  3465,  3469,  3470,  3481,  3482,  3488,  3498,
    3503,  3504,  3505,  3506,  3507,  3508,  3509,  3510,  3511,  3512,
    3513,  3514,  3515,  3516,  3517,  3518,  3519,  3529,  3530,  3533,
    3534,  3545,  3550,  3553,  3554,  3558,  3562,  3565,  3566,  3567,
    3570,  3573,  3574,  3575,  3578,  3582,  3583,  3584,  3588,  3589,
    3593,  3594,  3598,  3599,  3603,  3607,  3610,  3611,  3612,  3615,
    3619,  3619,  3620,  3620,  3624,  3625,  3629,  3629,  3630,  3630,
    3635,  3635,  3636,  3640,  3641,  3646,  3647,  3648,  3649,  3653,
    3657,  3658,  3662,  3666,  3670,  3674,  3675,  3679,  3680,  3684,
    3685,  3686,  3690,  3694,  3698,  3698,  3698,  3701,  3702,  3706,
    3707,  3708,  3709,  3710,  3711,  3712,  3713,  3714,  3715,  3716,
    3720,  3724,  3728,  3728,  3732,  3733,  3737,  3738,  3739,  3740,
    3741,  3746,  3745,  3751,  3750,  3755,  3756,  3761,  3760,  3766,
    3765,  3773,  3774,  3776,  3777,  3780,  3784,  3785,  3786,  3787,
    3788,  3789,  3790,  3791,  3792,  3793,  3794,  3798,  3799,  3800,
    3803,  3804,  3807,  3808,  3812,  3813,  3817,  3818,  3822,  3825,
    3826,  3836,  3840,  3841,  3845,  3846,  3850,  3851,  3855,  3856,
    3857,  3858,  3859,  3863,  3864,  3868,  3869,  3873,  3874,  3875,
    3876,  3877,  3883,  3882,  3886,  3885,  3890,  3894,  3895,  3899,
    3900,  3901,  3902,  3903,  3904,  3905,  3906,  3907,  3908,  3909,
    3913,  3917,  3917,  3920,  3921,  3926,  3925,  3946,  3945,  3970,
    3971,  3974,  3975,  3978,  3981,  3982,  3985,  3986,  3989,  3990,
    3993,  3994,  3998,  4003,  4002,  4041,  4040,  4092,  4093,  4094,
    4098,  4099,  4104,  4107,  4108,  4111,  4112,  4117,  4116,  4130,
    4131,  4130,  4142,  4143,  4145,  4146,  4149,  4153,  4156,  4162,
    4166,  4175,  4185,  4187,  4196,  4204,  4212,  4220,  4224,  4228,
    4229,  4232,  4233,  4236,  4240,  4244,  4245,  4248,  4252,  4253,
    4253,  4260,  4259,  4273,  4272,  4285,  4286,  4285,  4300,  4300,
    4324,  4325,  4326,  4330,  4331,  4336,  4344,  4355,  4356,  4366,
    4369,  4370,  4374,  4375,  4379,  4381,  4383,  4388,  4393,  4394,
    4392,  4418,  4443,  4448,  4449,  4453,  4470,  4469,  4474,  4475,
    4479,  4484,  4483,  4498,  4515,  4520,  4564,  4565,  4569,  4570,
    4570,  4575,  4576,  4581,  4593,  4607,  4609,  4614,  4615,  4620,
    4619,  4655,  4656,  4763,  4764,  4765,  4766,  4767,  4784,  4877,
    4878
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 0
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "','", "':'", "'='", "TOK_EQV",
  "TOK_NEQV", "TOK_OR", "TOK_XOR", "TOK_AND", "TOK_NOT", "TOK_LT",
  "TOK_GT", "TOK_LE", "TOK_GE", "TOK_EQ", "TOK_NE", "TOK_DSLASH", "'+'",
  "'-'", "'*'", "TOK_SLASH", "TOK_DASTER", "TOK_SEMICOLON",
  "TOK_PARAMETER", "TOK_RESULT", "TOK_ONLY", "TOK_INCLUDE",
  "TOK_SUBROUTINE", "TOK_PROGRAM", "TOK_FUNCTION", "TOK_LABEL_FORMAT",
  "TOK_LABEL_CONTINUE", "TOK_LABEL_END_DO", "TOK_MAX", "TOK_TANH",
  "TOK_COMMENT", "TOK_WHERE", "TOK_ELSEWHEREPAR", "TOK_ELSEWHERE",
  "TOK_ENDWHERE", "TOK_MAXVAL", "TOK_TRIM", "TOK_NULL_PTR", "TOK_SUM",
  "TOK_SQRT", "TOK_CASE", "TOK_SELECTCASE", "TOK_FILE", "TOK_REC",
  "TOK_NAME_EQ", "TOK_IOLENGTH", "TOK_ACCESS", "TOK_ACTION", "TOK_FORM",
  "TOK_RECL", "TOK_STATUS", "TOK_UNIT", "TOK_OPENED", "TOK_FMT", "TOK_NML",
  "TOK_END", "TOK_EOR", "TOK_EOF", "TOK_ERR", "TOK_POSITION", "TOK_IOSTAT",
  "TOK_IOMSG", "TOK_EXIST", "TOK_MIN", "TOK_FLOAT", "TOK_EXP", "TOK_LEN",
  "TOK_COS", "TOK_COSH", "TOK_ACOS", "TOK_NINT", "TOK_CYCLE", "TOK_SIN",
  "TOK_SINH", "TOK_ASIN", "TOK_EQUIVALENCE", "TOK_BACKSPACE", "TOK_LOG",
  "TOK_TAN", "TOK_ATAN", "TOK_RECURSIVE", "TOK_ABS", "TOK_MOD", "TOK_SIGN",
  "TOK_MINLOC", "TOK_MAXLOC", "TOK_EXIT", "TOK_KIND", "TOK_MOLD",
  "TOK_SOURCE", "TOK_ERRMSG", "TOK_MINVAL", "TOK_PUBLIC", "TOK_PRIVATE",
  "TOK_ALLOCATABLE", "TOK_RETURN", "TOK_THEN", "TOK_ELSEIF", "TOK_ELSE",
  "TOK_ENDIF", "TOK_PRINT", "TOK_PLAINGOTO", "TOK_LOGICALIF",
  "TOK_LOGICALIF_PAR", "TOK_PLAINDO", "TOK_CONTAINS", "TOK_ENDDO",
  "TOK_MODULE", "TOK_ENDMODULE", "TOK_WHILE", "TOK_CONCURRENT",
  "TOK_ALLOCATE", "TOK_OPEN", "TOK_CLOSE", "TOK_INQUIRE", "TOK_WRITE_PAR",
  "TOK_WRITE", "TOK_FLUSH", "TOK_READ_PAR", "TOK_READ", "TOK_REWIND",
  "TOK_DEALLOCATE", "TOK_NULLIFY", "TOK_DIMENSION", "TOK_ENDSELECT",
  "TOK_EXTERNAL", "TOK_INTENT", "TOK_INTRINSIC", "TOK_NAMELIST",
  "TOK_DEFAULT", "TOK_OPTIONAL", "TOK_POINTER", "TOK_CONTINUE", "TOK_SAVE",
  "TOK_TARGET", "TOK_IMPLICIT", "TOK_NONE", "TOK_CALL", "TOK_STAT",
  "TOK_POINT_TO", "TOK_COMMON", "TOK_GLOBAL", "TOK_LEFTAB", "TOK_RIGHTAB",
  "TOK_PAUSE", "TOK_PROCEDURE", "TOK_STOP", "TOK_FOURDOTS", "TOK_HEXA",
  "TOK_ASSIGNTYPE", "TOK_OUT", "TOK_INOUT", "TOK_IN", "TOK_USE",
  "TOK_EQUALEQUAL", "TOK_SLASHEQUAL", "TOK_INFEQUAL", "TOK_SUPEQUAL",
  "TOK_TRUE", "TOK_FALSE", "TOK_LABEL", "TOK_LABEL_DJVIEW",
  "TOK_PLAINDO_LABEL_DJVIEW", "TOK_PLAINDO_LABEL", "TOK_TYPE",
  "TOK_TYPEPAR", "TOK_ENDTYPE", "TOK_COMMACOMPLEX", "TOK_REAL",
  "TOK_INTEGER", "TOK_LOGICAL", "TOK_DOUBLEPRECISION", "TOK_ENDSUBROUTINE",
  "TOK_ENDFUNCTION", "TOK_ENDPROGRAM", "TOK_ENDUNIT", "TOK_CHARACTER",
  "TOK_CHAR_CONSTANT", "TOK_CHAR_CUT", "TOK_DATA", "TOK_CHAR_MESSAGE",
  "TOK_CSTREAL", "TOK_COMPLEX", "TOK_DOUBLECOMPLEX", "TOK_NAME",
  "TOK_CSTINT", "'('", "')'", "'<'", "'>'", "'\\n'", "'/'", "'%'", "'_'",
  "'['", "']'", "$accept", "input", "line", "line-break",
  "suite_line_list", "suite_line", "fin_line", "program-unit",
  "external-subprogram", "filename", "opt_comma", "uexpr", "signe",
  "operation", "after_slash", "after_equal", "lhs", "beforefunctionuse",
  "array_ele_substring_func_ref", "$@4", "$@5", "begin_array", "$@6",
  "structure_component", "funarglist", "funargs", "funarg", "triplet",
  "ident", "simple_const", "string_constant", "opt_substring", "opt_expr",
  "specification-part", "opt-use-stmt-list",
  "opt-declaration-construct-list", "declaration-construct-list",
  "declaration-construct", "opt-execution-part", "execution-part",
  "opt-execution-part-construct-list", "execution-part-construct-list",
  "execution-part-construct", "opt-internal-subprogram-part",
  "internal-subprogram-part", "opt-internal-subprogram",
  "internal-subprogram-list", "internal-subprogram",
  "other-specification-stmt", "executable-construct", "action-stmt",
  "keyword", "scalar-constant", "constant", "literal-constant",
  "named-constant", "opt-label", "label", "opt-label-djview",
  "label-djview", "type-param-value", "declaration-type-spec", "$@7",
  "intrinsic-type-spec", "$@8", "$@9", "$@10", "$@11", "$@12", "$@13",
  "opt-kind-selector", "kind-selector", "signed-int-literal-constant",
  "int-literal-constant", "kind-param", "signed-real-literal-constant",
  "real-literal-constant", "complex-literal-constant", "real-part",
  "imag-part", "opt-char_length-star", "opt-char-selector",
  "char-selector", "length-selector", "char-length",
  "char-literal-constant", "logical-literal-constant", "derived-type-def",
  "$@14", "derived-type-stmt", "opt-type-attr-spec-list-comma-fourdots",
  "opt-type-attr-spec-list-comma", "type-attr-spec-list", "type-attr-spec",
  "type-param-name-list", "type-param-name", "end-type-stmt",
  "opt-component-part", "component-part", "component-def-stmt",
  "data-component-def-stmt", "opt-component-attr-spec-list-comma-2points",
  "component-attr-spec-list", "component-attr-spec", "$@15",
  "component-decl-list", "component-decl", "opt-component-array-spec",
  "component-array-spec", "opt-component-initialization",
  "component-initialization", "initial-data-target", "derived-type-spec",
  "type-param-spec-list", "type-param-spec", "structure-constructor",
  "component-spec-list", "component-spec", "component-data-source",
  "array-constructor", "ac-spec", "lbracket", "rbracket", "ac-value-list",
  "ac-value", "ac-implied-do", "ac-implied-do-control", "ac-do-variable",
  "type-declaration-stmt", "$@16", "$@17", "opt-attr-spec-construct",
  "opt-attr-spec-comma-list", "attr-spec-comma-list", "attr-spec", "$@18",
  "$@19", "entity-decl-list", "entity-decl", "object-name",
  "object-name-noident", "opt-initialization", "initialization",
  "null-init", "access-spec", "opt-array-spec-par", "$@20", "array-spec",
  "explicit-shape-spec-list", "explicit-shape-spec", "lower-bound",
  "upper-bound", "assumed-shape-spec-list", "assumed-shape-spec",
  "deferred-shape-spec-list", "deferred-shape-spec", "assumed-size-spec",
  "opt-explicit-shape-spec-list-comma", "opt-lower-bound-2points",
  "implied-shape-spec-list", "implied-shape-spec", "intent-spec",
  "access-stmt", "$@21", "opt-access-id-list", "access-id-list",
  "access-id", "data-stmt", "$@22", "opt-data-stmt-set-nlist",
  "data-stmt-set-nlist", "data-stmt-set", "data-stmt-object-list",
  "data-stmt-value-list", "data-stmt-object", "data-implied-do",
  "data-i-do-object-list", "data-i-do-object", "data-i-do-variable",
  "data-stmt-value", "opt-data-stmt-star", "data-stmt-constant",
  "scalar-constant-subobject", "constant-subobject", "dimension-stmt",
  "$@23", "$@24", "array-name-spec-list", "$@25", "$@26", "parameter-stmt",
  "$@27", "$@28", "named-constant-def-list", "named-constant-def",
  "save-stmt", "$@29", "$@30", "opt-TOK_FOURDOTS", "opt-saved-entity-list",
  "saved-entity-list", "saved-entity", "proc-pointer-name",
  "get_my_position", "implicit-stmt", "$@31", "implicit-spec-list",
  "implicit-spec", "letter-spec-list", "letter-spec", "namelist-stmt",
  "opt-namelist-other", "namelist-group-object-list",
  "namelist-group-object", "equivalence-stmt", "equivalence-set-list",
  "equivalence-set", "$@32", "equivalence-object-list",
  "equivalence-object", "common-stmt", "$@33", "$@34",
  "opt-common-block-name", "common-block-name", "opt-comma",
  "opt-common-block-list", "$@35", "common-block-object-list",
  "common-block-object", "$@36", "designator", "scalar-variable",
  "variable", "variable-name", "scalar-logical-variable",
  "logical-variable", "char-variable", "scalar-default-char-variable",
  "default-char-variable", "scalar-int-variable", "int-variable",
  "substring", "substring-range", "data-ref", "opt-part-ref", "part-ref",
  "$@37", "scalar-structure-component", "structure-component",
  "array-element", "array-section", "section-subscript-list",
  "section-subscript", "section_subscript_ambiguous", "vector-subscript",
  "allocate-stmt", "$@38", "$@39", "opt-alloc-opt-list-comma",
  "alloc-opt-list", "alloc-opt", "stat-variable", "errmsg-variable",
  "allocation-list", "allocation", "allocate-object",
  "opt-allocate-shape-spec-list-par", "allocate-shape-spec-list",
  "allocate-shape-spec", "opt-lower-bound-expr", "lower-bound-expr",
  "upper-bound-expr", "deallocate-stmt", "$@40", "$@41",
  "allocate-object-list", "opt-dealloc-opt-list-comma", "dealloc-opt-list",
  "dealloc-opt", "primary", "level-1-expr", "mult-operand", "add-operand",
  "level-2-expr", "power-op", "mult-op", "add-op", "level-3-expr",
  "concat-op", "level-4-expr", "rel-op", "and-operand", "or-operand",
  "equiv-operand", "level-5-expr", "not-op", "and-op", "or-op", "equiv-op",
  "expr", "scalar-default-char-expr", "default-char-expr", "int-expr",
  "opt-scalar-int-expr", "scalar-int-expr", "specification-expr",
  "constant-expr", "scalar-default-char-constant-expr",
  "default-char-constant-expr", "scalar-int-constant-expr",
  "int-constant-expr", "assignment-stmt", "pointer-assignment-stmt",
  "opt-bounds-spec-list-par", "bounds-spec-list", "bounds-remapping-list",
  "bounds-spec", "bounds-remapping", "data-target",
  "procedure-component-name", "proc-component-ref", "proc-target",
  "where-stmt", "where-construct", "opt-where-body-construct",
  "opt-masked-elsewhere-construct", "opt-elsewhere-construct",
  "where-construct-stmt", "where-body-construct", "where-assignment-stmt",
  "mask-expr", "masked-elsewhere-stmt", "elsewhere-stmt", "end-where-stmt",
  "forall-header", "block", "opt-execution-part-construct", "do-construct",
  "block-do-construct", "label-do-stmt", "label-do-stmt-djview",
  "nonlabel-do-stmt", "loop-control", "do-variable", "do-block", "end-do",
  "end-do-stmt", "nonblock-do-construct", "action-term-do-construct",
  "do-term-action-stmt", "do-term-action-stmt-special",
  "outer-shared-do-construct", "label-do-stmt-djview-do-block-list",
  "inner-shared-do-construct", "do-term-shared-stmt",
  "opt-do-construct-name", "cycle-stmt", "if-construct",
  "opt-else-if-stmt-block", "else-if-stmt-block", "opt-else-stmt-block",
  "else-stmt-block", "if-then-stmt", "else-if-stmt", "else-stmt",
  "end-if-stmt", "if-stmt", "case-construct", "opt_case-stmt-block",
  "case-stmt-block", "select-case-stmt", "$@42", "$@43", "case-stmt",
  "end-select-stmt", "$@44", "$@45", "case-selector", "$@46",
  "case-value-range-list", "case-value-range", "case-value", "exit-stmt",
  "goto-stmt", "arithmetic-if-stmt", "continue-stmt", "stop-stmt",
  "stop-code", "io-unit", "file-unit-number", "internal-file-variable",
  "open-stmt", "$@47", "$@48", "connect-spec-list", "connect-spec",
  "file-name-expr", "iomsg-variable", "close-stmt", "$@49",
  "close-spec-list", "close-spec", "read-stmt", "$@50", "$@51",
  "write-stmt", "$@52", "$@53", "print-stmt", "io-control-spec-list",
  "namelist-group-name", "io-control-spec", "format", "input-item-list",
  "input-item", "output-item-list", "output-item", "io-implied-do",
  "io-implied-do-object-list", "io-implied-do-object",
  "io-implied-do-control", "rewind-stmt", "position-spec-list",
  "position-spec", "flush-stmt", "flush-spec-list", "flush-spec",
  "inquire-stmt", "$@54", "$@55", "set_in_inquire", "inquire-spec-list",
  "inquire-spec", "format-stmt", "module", "$@56",
  "opt-module-subprogram-part", "module-stmt", "$@57", "end-module-stmt",
  "$@58", "opt-tok-module", "opt-ident", "module-subprogram-part",
  "opt-module-subprogram-list", "module-subprogram-list",
  "module-subprogram", "use-stmt-list", "save_olduse", "use-stmt", "$@59",
  "$@60", "opt-module-nature-2points", "opt-only-list", "main-program",
  "opt-specification-part", "program-stmt", "$@61", "end-program-stmt",
  "$@62", "$@63", "opt-tok-program", "opt-tok-name", "module-nature",
  "opt-rename-list", "rename-list", "rename", "only-list", "only",
  "only-use-name", "generic-spec", "external-stmt", "external-name-list",
  "external-name", "intrinsic-stmt", "intrinsic-procedure-name-list",
  "intrinsic-procedure-name", "function-reference", "$@64", "call-stmt",
  "$@65", "$@66", "$@67", "$@68", "before-call-stmt", "$@69",
  "procedure-designator", "actual-arg-spec-list", "actual-arg-spec",
  "actual-arg", "opt-prefix", "prefix", "prefix-spec",
  "function-subprogram", "function-stmt", "$@70", "$@71", "function-name",
  "dummy-arg-name", "opt-suffix", "suffix", "end-function-stmt", "$@72",
  "opt-tok-function", "subroutine-subprogram", "subroutine-stmt", "$@73",
  "subroutine-name", "end-subroutine-stmt", "close_subroutine",
  "opt-tok-subroutine", "opt-dummy-arg-list-par", "$@74",
  "opt-dummy-arg-list", "dummy-arg-list", "dummy-arg", "return-stmt",
  "contains-stmt", "$@75", "opt_name", "after_rewind",
  "declare_after_percent", "pointer_name_list", YY_NULLPTR
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[NUM] -- (External) token number corresponding to the
   (internal) symbol number NUM (which must be that of a token).  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,    44,    58,    61,   258,   259,   260,   261,
     262,   263,   264,   265,   266,   267,   268,   269,   270,    43,
      45,    42,   271,   272,   273,   274,   275,   276,   277,   278,
     279,   280,   281,   282,   283,   284,   285,   286,   287,   288,
     289,   290,   291,   292,   293,   294,   295,   296,   297,   298,
     299,   300,   301,   302,   303,   304,   305,   306,   307,   308,
     309,   310,   311,   312,   313,   314,   315,   316,   317,   318,
     319,   320,   321,   322,   323,   324,   325,   326,   327,   328,
     329,   330,   331,   332,   333,   334,   335,   336,   337,   338,
     339,   340,   341,   342,   343,   344,   345,   346,   347,   348,
     349,   350,   351,   352,   353,   354,   355,   356,   357,   358,
     359,   360,   361,   362,   363,   364,   365,   366,   367,   368,
     369,   370,   371,   372,   373,   374,   375,   376,   377,   378,
     379,   380,   381,   382,   383,   384,   385,   386,   387,   388,
     389,   390,   391,   392,   393,   394,   395,   396,   397,   398,
     399,   400,   401,   402,   403,   404,   405,   406,   407,   408,
     409,   410,   411,   412,   413,   414,   415,   416,   417,   418,
     419,   420,   421,   422,   423,   424,   425,   426,   427,   428,
     429,   430,   431,   432,   433,   434,   435,   436,   437,   438,
     439,   440,   441,    40,    41,    60,    62,    10,    47,    37,
      95,    91,    93
};
# endif

#define YYPACT_NINF -1435

#define yypact_value_is_default(Yystate) \
  (!!((Yystate) == (-1435)))

#define YYTABLE_NINF -1022

#define yytable_value_is_error(Yytable_value) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
   -1435,  1099, -1435, -1435, -1435,    46,   134, -1435, -1435, -1435,
     164,  1017, -1435, -1435,   130,   230, -1435, -1435, -1435, -1435,
     585, -1435,   200, -1435,   200,   488,   798, -1435, -1435,   200,
   -1435,   200, -1435, -1435, -1435, -1435, -1435, -1435, -1435, -1435,
   -1435, -1435, -1435,    77,   242,   305, -1435, -1435, -1435,   748,
   -1435, -1435,  3966,   377,   200, -1435,   450,  4246,   232,   353,
   -1435, -1435,  4246,  4246, -1435,   133,   133,    79,    79,    79,
      79,    93,    79,  1456, -1435, -1435, -1435, -1435, -1435, -1435,
     133,   379, -1435, -1435,    94,    78,   429,   576, -1435, -1435,
      94,   106, -1435, -1435,   919, -1435,   607, -1435,   444, -1435,
    3966, -1435, -1435,   511,   705,   473, -1435, -1435, -1435,   523,
     356, -1435, -1435, -1435,   573, -1435, -1435,   568,   569, -1435,
   -1435, -1435, -1435,   -42,   724, -1435,   521, -1435, -1435, -1435,
   -1435, -1435, -1435, -1435, -1435, -1435, -1435, -1435, -1435, -1435,
   -1435, -1435, -1435, -1435, -1435, -1435, -1435, -1435, -1435, -1435,
     622, -1435, -1435, -1435,   483,   557,   578,  1918,   337,    66,
     -35,   593,   596, -1435,  3686,  3710,   609,   613,  3445,   818,
     719, -1435,  4145, -1435,   995, -1435, -1435, -1435, -1435, -1435,
   -1435, -1435, -1435, -1435, -1435, -1435, -1435,   794, -1435, -1435,
   -1435, -1435, -1435, -1435, -1435, -1435, -1435, -1435, -1435, -1435,
   -1435, -1435,   643, -1435, -1435,   666, -1435,   668,   719,   719,
     130,   130,   654,  3492, -1435, -1435, -1435, -1435, -1435,   549,
     894, -1435, -1435, -1435, -1435, -1435, -1435, -1435, -1435, -1435,
   -1435,  3734, -1435, -1435, -1435,   663,   669,  3764, -1435,   214,
     874, -1435, -1435, -1435,   713, -1435, -1435,   473, -1435,    83,
   -1435, -1435,  3734, -1435, -1435,   870, -1435,   733,    88,  1349,
     704, -1435, -1435,   885,   896,   760,  2264, -1435, -1435, -1435,
   -1435,   716,   739,   130, -1435,   113, -1435, -1435,   130,   434,
     133,   706, -1435,   114, -1435, -1435,   744,   753,   604,   130,
     133,   695,   762,   448,   599,   122,   646, -1435, -1435, -1435,
   -1435,   369, -1435, -1435,  3445,  3021,  3764,   133,   915,   934,
    3764,   710,   107, -1435,   766,   429,   429,   433,  3812,  3764,
     801,  3764,  3764,   767, -1435,  4067,   537,   791,   857,   226,
   -1435, -1435, -1435,   709, -1435, -1435, -1435,  3764,  3764,   294,
     444, -1435, -1435,   133,   133,   130,   133, -1435, -1435, -1435,
   -1435, -1435,   782,  3204, -1435,   133,  3352,   133, -1435,   787,
     130, -1435, -1435, -1435, -1435, -1435, -1435, -1435,   133,   393,
     133, -1435, -1435, -1435,  4168, -1435, -1435, -1435,  3764,   796,
    2939,  2939,  3021, -1435,   653,   229,   407, -1435, -1435,   788,
     133, -1435, -1435, -1435, -1435, -1435, -1435,   988,   806,  1456,
   -1435, -1435,   996,  1000,   100,  3734,   859,  1004, -1435, -1435,
   -1435,   726,   726,   506,   836, -1435,   837,   839,  1349,   821,
    1456,  1456, -1435,   817, -1435,  1349, -1435, -1435,  1349, -1435,
   -1435,  1349,   846,   733, -1435, -1435, -1435, -1435, -1435, -1435,
   -1435, -1435, -1435, -1435, -1435, -1435, -1435,  2264,  2264, -1435,
    3764, -1435,  3764, -1435, -1435,  3764, -1435,   833,   842,   905,
     379,   130,   841, -1435, -1435,  1025,   130,   114,   706,   130,
   -1435,   124, -1435,  1013, -1435,   847,   848, -1435,   130,  1034,
   -1435, -1435,   133, -1435,   851, -1435,  1043, -1435, -1435, -1435,
   -1435, -1435, -1435, -1435,   125,   919,   919,  1106,  3764,    94,
      94,  1036,   130,   133, -1435,   112, -1435, -1435, -1435,   131,
     854,   130,   940,  3764,   858,  1047, -1435,   260,   880,   750,
   -1435, -1435,   891,   875,   911,  1064,   133, -1435,  1070, -1435,
   -1435,   884,   237, -1435,   887,   142, -1435, -1435,    96,   878,
   -1435, -1435, -1435, -1435,   133,  1079, -1435,   108,   109, -1435,
   -1435,   905,   133,   890,   787, -1435, -1435,    94,  1084,   980,
    1222, -1435, -1435, -1435, -1435,   386, -1435,   374, -1435,   914,
     822, -1435, -1435,   974, -1435,   923,   133,   937, -1435, -1435,
   -1435,   922,   930,   130,   130,   130,   787,  2868,  2533,  3764,
      66,   905,   905,   821, -1435,   110, -1435,   130,  3764,    66,
     905,   905, -1435,   119, -1435,   130,   787, -1435,   121,   130,
     939,   406, -1435,   951, -1435,   941, -1435, -1435,  1132,  3503,
    3021,   947,    66,    66,    66,   905, -1435, -1435, -1435, -1435,
   -1435, -1435,   126, -1435, -1435, -1435,   129,   150,    48,   905,
   -1435, -1435, -1435,  1104, -1435, -1435, -1435, -1435,   403,   954,
   -1435, -1435, -1435, -1435,  3764,   130,   222,   133,   222,   964,
   -1435,   966, -1435,  3764, -1435,   955,  1456,  3764,  3764, -1435,
    1148,   821, -1435,  3734, -1435, -1435, -1435, -1435,   233,   979,
   -1435, -1435, -1435, -1435, -1435, -1435, -1435, -1435,   733,    88,
    1136, -1435,   885,   896, -1435,  3764,  1151,   137, -1435,   257,
    1154, -1435, -1435,   968, -1435, -1435,  3764, -1435,  3764,   130,
   -1435,   744,   130,   787,  1140,   973,  1165, -1435,   695,   130,
     978,   599,   133,   919, -1435, -1435,   983, -1435,  1157, -1435,
   -1435,   225, -1435,   986, -1435, -1435,   679, -1435,  1157, -1435,
    1159,   501, -1435,   993,   130,   133,   130,   133,  1570,    66,
    3764,   219,   139, -1435, -1435,   217, -1435,   130,  3843,   130,
    1069,  3764,   133, -1435,  3764,   558, -1435,   787,   410, -1435,
   -1435, -1435, -1435,   989, -1435,   998, -1435, -1435, -1435, -1435,
   -1435, -1435, -1435, -1435,  1185, -1435,  1005, -1435,   891,   130,
     766,  1011,  1204, -1435, -1435, -1435,  1206, -1435, -1435, -1435,
     133,  1019,   523,   130,  1021,   130,  3764,  3764, -1435,  3764,
    1072, -1435,   133,   130, -1435, -1435,   130,   133,  1046,   447,
    1026,  3896,  1029,  1203,   905, -1435, -1435, -1435, -1435, -1435,
   -1435, -1435, -1435, -1435, -1435, -1435, -1435, -1435, -1435, -1435,
   -1435, -1435,   791, -1435, -1435,  1086,  3764,   472, -1435,   728,
   -1435, -1435, -1435, -1435,  1080,  1223,   130,  1128,   449, -1435,
   -1435, -1435, -1435,  1242, -1435,  1053,  3764,  3764,  3764,  3764,
    3764,  1243,  3764,    66,  3764,   905, -1435,   140, -1435,  3764,
    1244,   905,   905,   905,   905,  3764,   905,    66,   905,   905,
   -1435,   149, -1435, -1435, -1435, -1435, -1435, -1435, -1435, -1435,
   -1435, -1435,  3204,   133, -1435, -1435, -1435, -1435,  3352,   133,
   -1435,  1250,   787, -1435,  3764, -1435,   797, -1435, -1435, -1435,
    1217,  4269,  3392,  3764, -1435, -1435, -1435, -1435, -1435, -1435,
   -1435, -1435, -1435,  2939,  3843,  1048,  1048,   130, -1435, -1435,
    3764,   888, -1435,  1607,   133,   130, -1435,   133,   133,   214,
    1249, -1435, -1435,   162, -1435, -1435, -1435, -1435, -1435,  1061,
    1253, -1435,   130,  1063,  1227,  1233,  1071, -1435,   165,   172,
    1073,  3734, -1435, -1435, -1435, -1435, -1435,  1074,   176,  3764,
     842, -1435,   905,  3764,  1075,  1260, -1435, -1435,  1261, -1435,
   -1435, -1435, -1435,   848,   642, -1435, -1435,   184, -1435,   223,
   -1435,  1265, -1435,   130, -1435,  1456,   486, -1435, -1435,  3576,
    1106, -1435, -1435, -1435, -1435,  1163,   130,   130,  3764,  1268,
   -1435, -1435,  3624,  1036, -1435,  1679,  3764, -1435,  3843, -1435,
     167, -1435, -1435,   133,  1090,   130, -1435, -1435,  1085, -1435,
     275, -1435, -1435,  1087,   170, -1435,   133,   130, -1435, -1435,
     875,   133, -1435,  1252, -1435, -1435, -1435,  1094,   133,   133,
     237,   130,  1266,   187, -1435, -1435, -1435, -1435, -1435, -1435,
    1283, -1435,  1285, -1435,   905,   130,   130,    94,   133,   130,
    3764,  2816,  2752,  3174, -1435,   787,  3764,  4347, -1435,   791,
    1098,   133,   130,   518, -1435, -1435, -1435, -1435,   191, -1435,
   -1435,  1108,   130, -1435,   133,   241,  1101,  3764, -1435, -1435,
   -1435, -1435, -1435, -1435, -1435, -1435,  3764, -1435, -1435, -1435,
   -1435,  2868, -1435, -1435,   905,  1109, -1435, -1435, -1435, -1435,
   -1435, -1435, -1435, -1435, -1435, -1435,  3102, -1435, -1435,   130,
   -1435,   130,   255,  1112, -1435,  1113, -1435, -1435,  1111, -1435,
    1045,   224,  1303,  3764,    66,   905, -1435,   195, -1435, -1435,
   -1435,   133,  1306,  3843, -1435,   133,  1308, -1435, -1435,   177,
    1118,   540,   555, -1435, -1435,   653,  3764, -1435,   196, -1435,
    1311,   130,   133,   130,   130,  3764,  3764, -1435, -1435,   222,
    1290, -1435,  1108, -1435,  1108, -1435,  1224, -1435,  1246, -1435,
   -1435,   223,  1126,  1316, -1435, -1435, -1435, -1435, -1435, -1435,
     133,   197, -1435,  1130, -1435,  3764,   787,   178,  1782, -1435,
     133,   604,   978,   133,  3764,   233,   455, -1435, -1435, -1435,
   -1435, -1435, -1435, -1435, -1435, -1435, -1435, -1435, -1435,  1321,
     205, -1435, -1435,  1133, -1435, -1435, -1435, -1435,   133, -1435,
    3764,    66, -1435, -1435,  3764,  1323, -1435,   821, -1435,  1328,
   -1435,  3843,   130,   130,  1230, -1435,   558, -1435,  1870,  1252,
     787,   130,   130,  1782,   749, -1435,   130,  1782,   549,   251,
    1782,  1143,   130,   130, -1435,  1161,  1019, -1435, -1435,  3764,
     133,   130,   133,   130,  1160,  3764, -1435, -1435, -1435, -1435,
   -1435,   -68,   405,   746,   789,   877,   513,   539,  1162,  3764,
    1149, -1435,   905,  1164,   482,  1167,  1012,  1804,   208,  1168,
   -1435,  1258,   130,   133,   130,  1359,  1207,  1361, -1435,   133,
   -1435, -1435,   130,   905,  1360,  1366, -1435, -1435, -1435,   209,
   -1435,  3764,  1367, -1435, -1435,   133, -1435,  3843, -1435,   133,
     905,  1365, -1435,  1369, -1435, -1435, -1435, -1435, -1435,  3764,
      66,  3764, -1435, -1435, -1435,  3392,   133,   130,   133,   130,
    1048,   133,   130,   560,   133,   130,   133,   130,   653, -1435,
    1607, -1435,  3764,   130,   444, -1435, -1435,   133, -1435,  1181,
   -1435, -1435, -1435, -1435,  1370,  1372, -1435,  3764,   130,   905,
   -1435, -1435,  1375, -1435,   130,  1357, -1435,  1187,  1381, -1435,
    1382, -1435,  1384, -1435,  1387, -1435, -1435,  3764,  1364,  1388,
   -1435, -1435,  1389,   130, -1435, -1435,   130,  1391, -1435, -1435,
    3812,  3812, -1435,   130, -1435, -1435, -1435,  3764,  3843, -1435,
     133,  1870, -1435, -1435,  1198,  1392,  1394,  1387,   252, -1435,
    1202, -1435, -1435, -1435,  1208,  1209, -1435,  3764,   623, -1435,
   -1435,  1214, -1435, -1435, -1435,   130,   130,   771, -1435, -1435,
   -1435, -1435, -1435, -1435,   968, -1435,   263, -1435, -1435,  1218,
   -1435, -1435,  1733,  3764,  3764,  3764,  3764,  3764,  3764,  3764,
    3764,  3764,  3764,  3764,  3764,  3764,  3764,  2233,  3764,  2376,
    2487, -1435, -1435,  4347,   567,   130,  1210,  1221,  1226,   130,
     133, -1435, -1435,   905,   120,   133,  3764, -1435, -1435, -1435,
     130,  1306,   130, -1435,   905,   371,   133,   133,   133,  1225,
    1418, -1435, -1435,   130,   130, -1435,   130,   133,   130,   130,
     130, -1435, -1435,   130,  1231,   133, -1435,   133,  3764,  1456,
    1421, -1435,  3764,  1235, -1435,  3764,  3654,  1903,  1424,  1425,
    1409, -1435, -1435,  3764,   848,  3764, -1435, -1435, -1435,  1426,
   -1435,  1238,   130,  1241, -1435,  3764,  3764,  3764,   623, -1435,
   -1435, -1435, -1435, -1435, -1435, -1435, -1435, -1435, -1435,  1782,
      22, -1435, -1435, -1435,  3764, -1435, -1435, -1435, -1435, -1435,
   -1435, -1435, -1435, -1435, -1435, -1435, -1435, -1435, -1435, -1435,
   -1435,  3764,  3764, -1435, -1435, -1435,  3764, -1435,  3764, -1435,
     133,   130,  1207, -1435, -1435,  1435, -1435, -1435, -1435, -1435,
   -1435,   130, -1435, -1435, -1435,   130, -1435,   133, -1435, -1435,
     130,   130,   130,  4347,    66,   130,  1248,   130,   133,   130,
    1251,  1255,  3764, -1435,  1422, -1435, -1435, -1435, -1435,  1436,
   -1435, -1435, -1435, -1435, -1435,  1165,   212,  3764, -1435, -1435,
   -1435, -1435, -1435,  1257,  1149,  1259,  2014,  1270,  1271,  1272,
   -1435, -1435, -1435, -1435, -1435,   130,   133,  1210,   130,   133,
   -1435,   130, -1435, -1435,  1440,   787, -1435,  3764, -1435,  1443,
   -1435, -1435,  2155,  1451, -1435, -1435,  1455, -1435,   905, -1435,
     130, -1435,   130,  3764, -1435,  1274,  3764,  3764,  1458,  2014,
    3764, -1435, -1435, -1435,  1452, -1435,  3764, -1435,  1465,  3764,
   -1435,  3764, -1435, -1435
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_uint16 yydefact[] =
{
       2,     0,     1,     6,     8,     0,     0,    17,     9,  1026,
    1025,     0,    18,     3,     4,     5,    12,    15,    20,  1024,
       0,    21,   106,    19,   106,     0,   202,  1022,    22,   106,
      23,   106,    24,    18,   967,   935,   208,   206,   216,   210,
     214,   212,    88,   305,     0,     0,     7,    11,    18,   202,
     203,   964,   108,     0,   107,   950,   192,   192,     0,     0,
    1025,  1023,   192,   192,    16,     0,     0,   218,   218,   218,
     218,   242,   218,     0,   204,   205,    10,    13,    14,   455,
       0,     0,   366,   367,    25,     0,   464,     0,   501,   194,
      25,   264,   255,   257,     0,   256,    88,   195,   539,   105,
     109,   110,   116,     0,   193,     0,   112,   260,   117,   202,
     402,   143,   145,   146,     0,   113,   151,     0,     0,   115,
     150,   147,   144,   523,     0,   521,   532,   537,   520,   518,
     519,   118,   119,   120,   709,   707,   707,   710,   736,   737,
     121,   707,   122,   124,   114,   148,   149,   123,   952,   951,
       0,   193,   931,   934,   202,     0,     0,   103,     0,     0,
       0,     0,     0,   916,     0,     0,     0,     0,     0,    88,
     134,   126,   192,   152,     0,   157,   163,   158,   173,   179,
     156,   687,   153,   162,   155,   170,   154,   786,   165,   164,
     181,   161,   178,   172,   160,   175,   180,   174,   177,   166,
     171,   159,  1001,   176,  1043,  1048,  1031,     0,   134,   134,
     968,   936,     0,     0,   209,   219,   207,   217,   211,     0,
       0,   215,   243,   244,   213,   201,   648,   621,   622,   200,
    1011,     0,   258,   259,  1012,   231,   225,     0,   323,   539,
       0,   604,   309,   616,   186,   187,   189,   190,   188,     0,
     307,   605,     0,   603,   608,   609,   611,   613,   623,     0,
     626,   640,   642,   644,   646,   653,     0,   656,   659,   199,
     606,     0,     0,   930,   494,     0,   492,    26,   723,     0,
       0,     0,   993,     0,   991,   465,     0,     0,   504,   715,
       0,     0,     0,     0,     0,   508,     0,   415,   420,   523,
     419,     0,   540,   111,     0,     0,     0,     0,    88,     0,
     657,   202,   336,   400,     0,   464,   464,   202,     0,     0,
       0,     0,   657,   536,   731,   192,   196,   196,   767,   957,
    1059,   474,   943,   202,   946,   948,   949,     0,     0,    88,
     539,   167,   104,     0,     0,   810,     0,  1062,  1061,   169,
     567,   824,     0,     0,   822,     0,     0,     0,   592,     0,
     815,   655,   663,   665,   817,   662,   818,   664,     0,     0,
       0,   969,   135,   127,   192,   130,   132,   133,     0,     0,
       0,     0,     0,  1008,   689,     0,     0,   787,   707,  1005,
       0,  1049,  1041,  1028,   474,   474,   222,     0,     0,     0,
     254,   251,     0,     0,     0,     0,     0,   322,   325,   328,
     327,     0,     0,   539,   616,   235,   187,     0,     0,     0,
       0,     0,   306,     0,   618,     0,   619,   620,     0,   617,
     223,     0,   186,   614,   630,   632,   631,   633,   628,   629,
     625,   634,   635,   637,   639,   636,   638,     0,     0,   649,
       0,   650,     0,   651,   652,     0,   641,   999,     0,     0,
       0,   491,     0,   705,   730,     0,   725,     0,     0,   989,
     997,     0,   995,     0,   506,     0,     0,   505,   717,   267,
     268,   270,     0,   265,     0,   427,     0,   423,   543,   426,
     542,   425,   509,   408,   508,     0,     0,     0,     0,    25,
      25,   547,  1057,     0,   879,   225,   878,   655,   877,     0,
       0,   814,     0,     0,     0,     0,   658,   282,     0,   202,
     278,   280,     0,     0,     0,   339,     0,   406,   403,   404,
     407,     0,   466,   476,     0,     0,   478,    88,   603,     0,
     522,   682,   683,   684,     0,     0,   590,     0,     0,   673,
     675,     0,     0,     0,     0,   708,   198,    25,     0,     0,
     192,   707,   712,   732,   738,     0,   758,   192,   713,     0,
     771,   768,   707,     0,   958,     0,     0,     0,   932,   947,
     698,     0,     0,   765,   811,   812,     0,     0,     0,     0,
       0,     0,     0,   656,   907,     0,   905,   903,     0,     0,
       0,     0,   898,     0,   896,   894,     0,  1069,     0,   816,
       0,   202,   962,     0,   131,     0,   842,   820,     0,     0,
       0,     0,     0,     0,     0,     0,    88,   527,   823,   866,
     819,   821,     0,   869,   863,   868,     0,     0,     0,     0,
     697,   695,   696,   691,   688,   694,   802,   800,     0,   796,
     788,   785,   789,  1003,     0,  1002,  1051,     0,  1051,     0,
    1027,     0,  1040,     0,   220,     0,     0,     0,     0,   249,
       0,   327,   320,     0,   228,   227,   232,   226,     0,   187,
     607,   310,   308,   324,   321,   186,   610,   612,   615,   624,
     627,   643,   645,   647,   998,     0,     0,     0,   458,   524,
       0,   498,   500,   532,   499,   493,     0,   729,     0,   990,
     992,     0,   994,     0,     0,   515,   510,   513,     0,   262,
       0,     0,     0,     0,   412,   416,   539,   432,   223,   433,
     229,   437,   435,     0,   436,   434,     0,   417,   437,   446,
     304,     0,   365,     0,   722,     0,   714,     0,   551,     0,
       0,   539,     0,   548,   556,   565,   566,  1058,     0,   861,
       0,     0,     0,   534,   657,     0,   283,     0,     0,   261,
     279,   351,   343,     0,   346,     0,   349,   350,   352,   353,
     354,   340,   342,   359,   334,   355,   368,   337,     0,   401,
       0,     0,   449,   358,   470,   462,   467,   468,   471,   472,
       0,     0,   202,   475,     0,   670,   677,     0,   672,     0,
       0,   679,     0,   666,   533,   538,   719,     0,     0,     0,
       0,     0,     0,     0,   193,   740,   744,   741,   755,   739,
     749,   746,   733,   751,   743,   753,   756,   752,   754,   745,
     750,   742,   759,   707,   757,     0,     0,     0,   769,     0,
     772,   707,   770,   976,     0,   977,  1060,   939,     0,   792,
     581,   543,   582,   570,   578,   583,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   829,     0,   827,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     919,     0,   917,   908,   911,   531,   909,   530,   529,   841,
     528,   910,     0,     0,   899,   902,   901,   900,     0,     0,
     595,   597,     0,   168,     0,   136,   202,   139,   141,   142,
     972,   192,     0,     0,   820,   867,   871,   865,   870,   872,
     873,   874,   875,     0,   857,   851,     0,   855,  1010,  1009,
       0,     0,   687,     0,     0,   794,   798,     0,     0,   539,
       0,  1018,  1017,     0,  1013,  1015,  1056,  1032,  1055,     0,
    1052,  1053,  1042,     0,  1038,  1046,     0,   253,     0,     0,
       0,     0,   326,   191,   239,   237,   238,     0,     0,     0,
       0,   456,     0,   657,     0,     0,   996,   524,   486,   488,
     490,   507,   516,     0,   502,   269,   273,     0,   271,   539,
     424,     0,   428,   409,   413,   540,     0,   430,   431,     0,
       0,   414,   429,   224,   230,     0,   724,   716,     0,   552,
     559,   555,     0,     0,   541,   560,     0,   550,     0,   886,
       0,   884,   887,     0,     0,   667,   535,   288,     0,   291,
       0,   285,   287,   295,     0,   292,     0,   274,   344,   347,
       0,     0,   369,   240,   341,   405,   451,     0,     0,     0,
       0,   477,   483,     0,   481,   479,   680,   681,   678,   591,
       0,   674,     0,   676,     0,   668,   721,    25,     0,   734,
       0,  1067,  1065,     0,   747,     0,     0,   192,   761,   760,
       0,     0,   780,     0,   773,   766,   774,   959,     0,   953,
     940,   941,   693,   685,     0,     0,     0,     0,   580,   840,
     654,   834,   831,   832,   835,   838,     0,   830,   833,   837,
     836,     0,   825,   921,     0,     0,   922,   923,   929,   920,
     526,   928,   525,   924,   926,   925,     0,   912,   906,   904,
     897,   895,     0,     0,  1070,     0,   140,   973,   974,   784,
       0,   193,     0,     0,     0,     0,   846,     0,   844,   876,
     864,     0,   859,     0,   882,     0,   853,   880,   883,     0,
       0,     0,     0,   687,   686,   690,     0,   809,     0,   803,
     805,   795,     0,   797,  1004,     0,     0,  1006,  1050,     0,
    1033,  1039,   941,  1047,   941,   221,     0,   250,     0,   247,
     246,   539,     0,     0,   332,   233,  1000,   661,   460,   459,
       0,     0,   496,     0,   728,     0,     0,   508,   390,   514,
       0,     0,     0,     0,     0,     0,   191,   439,   183,   184,
     185,   441,   442,   444,   445,   443,   438,   440,   311,     0,
       0,   313,   315,   679,   317,   318,   319,   418,     0,   553,
       0,     0,   557,   549,     0,   561,   564,   886,   891,     0,
     889,     0,   862,   777,     0,   289,     0,   284,     0,   240,
       0,   281,   275,   390,     0,   356,   335,   390,     0,   360,
     390,     0,   450,   463,   469,     0,     0,   480,   677,     0,
       0,   718,     0,   735,     0,     0,    32,    33,    91,    71,
      94,   258,   259,   255,   257,   256,   231,   225,     0,     0,
      27,    63,    65,    62,   539,    28,   101,   656,     0,     0,
     762,     0,   781,     0,   782,     0,     0,   978,   979,     0,
     942,   937,   793,     0,     0,   571,   572,   579,   568,     0,
     585,     0,     0,   839,   828,     0,   927,     0,   918,     0,
       0,     0,   596,   598,   599,   593,   790,   975,   970,     0,
       0,     0,   847,   849,   848,     0,     0,   858,     0,   852,
       0,     0,   856,     0,     0,   701,     0,   703,   692,   807,
       0,   801,   806,   799,   539,  1016,  1014,     0,  1054,     0,
    1029,  1034,  1045,  1045,     0,     0,   329,     0,   457,     0,
     495,   533,   726,   489,   485,     0,   384,     0,   371,   376,
       0,   379,   372,   382,   373,   386,   374,   392,     0,   375,
     394,   660,   381,   503,   511,   272,   263,     0,   236,   234,
       0,     0,   312,   775,   554,   558,   562,     0,     0,   885,
       0,     0,   286,   388,     0,   297,     0,   298,   299,   293,
       0,   398,   399,   397,     0,     0,   241,     0,     0,   357,
     361,     0,   453,   484,   482,   669,   720,     0,    31,  1064,
    1066,    30,  1068,    66,   532,    67,    72,  1063,    95,    98,
      96,   102,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    55,     0,     0,
       0,    29,   748,   192,     0,   783,   960,     0,     0,   954,
       0,   577,   574,     0,     0,     0,     0,   584,   587,   589,
     826,   914,   913,   601,     0,     0,     0,     0,     0,     0,
       0,   850,   845,   843,   860,   881,   854,     0,   699,   702,
     704,   804,   808,  1007,     0,     0,  1036,     0,     0,     0,
       0,   497,     0,     0,   517,   391,   385,     0,     0,     0,
       0,   380,   396,   392,     0,     0,   316,   314,   563,     0,
     890,     0,   776,     0,   296,     0,     0,     0,     0,   294,
     300,   345,   348,   370,   362,   364,   363,   304,   452,   390,
       0,    64,    64,    64,     0,    54,    60,    39,    49,    51,
      50,    52,    45,    40,    47,    46,    38,    48,    34,    35,
      36,     0,     0,    53,    56,    37,     0,    42,     0,    41,
       0,   778,   987,   955,   986,   961,   982,   985,   984,   981,
     980,   938,   576,   575,   573,   569,   586,     0,   602,   600,
     594,   791,   971,   192,     0,   700,     0,  1030,     0,  1044,
       0,     0,     0,   727,     0,   377,   378,   381,   384,     0,
     383,   387,   393,   389,   395,   512,     0,     0,   888,   290,
     301,   303,   302,     0,    74,    61,    75,     0,     0,     0,
      59,    57,    58,    44,    43,   779,     0,     0,   915,     0,
    1035,  1037,   245,   248,   330,     0,   385,     0,   421,     0,
     454,    72,    87,    76,    77,    80,    79,    68,     0,    73,
     956,   983,   813,     0,   487,     0,     0,     0,    85,     0,
      86,    70,   331,   422,   892,    84,     0,    78,    81,     0,
      83,     0,   893,    82
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
   -1435, -1435, -1435,  1093, -1435,  1423,   428, -1435, -1435, -1435,
   -1435, -1435, -1435, -1435, -1435, -1435,  -116, -1435, -1435, -1435,
   -1435, -1435, -1435, -1435,  -650, -1435,  -248, -1435,   -11, -1435,
   -1435, -1435, -1435, -1435, -1435, -1435, -1435,  1378,   889, -1435,
   -1435, -1435,   -85,   756, -1435, -1435, -1435,   565, -1435,   -76,
    -894,  -632, -1435, -1435,   476,   478,   -45,    63, -1435,   659,
    -215,   -67, -1435,  1466, -1435, -1435, -1435, -1435, -1435, -1435,
     712, -1435,  -223,  -180,  1076,  -453,  -209, -1435, -1435, -1435,
     216, -1435, -1435, -1435,   211,   -33, -1435, -1435, -1435, -1435,
   -1435, -1435, -1435,   772, -1435,   271, -1435, -1435, -1435,   975,
   -1435, -1435, -1435,   231, -1435, -1435,   234, -1435,    54, -1435,
   -1435,  -966,  1485, -1435,  1078,   494, -1435,    70,    73, -1435,
    1254, -1435, -1435,  1100,  -599, -1435, -1435, -1435, -1435, -1435,
   -1435, -1435, -1435, -1435,   720, -1435, -1435, -1435,   457, -1435,
   -1435, -1435, -1435,  -963,  -258, -1435, -1435, -1185, -1148, -1434,
   -1170, -1219, -1435,   -48, -1105,   -47, -1435, -1435,    99, -1435,
     -53, -1435, -1435, -1435, -1435, -1435,   727, -1435, -1435, -1435,
   -1435,  -418, -1435, -1435,  1027,  -249, -1435,   800, -1435,   512,
    -362, -1435,   522, -1435, -1435, -1435, -1435, -1435, -1435, -1435,
   -1435, -1435, -1435, -1435,   547, -1435, -1435, -1435,   -16, -1435,
   -1435,   469, -1435,     1, -1435, -1435, -1435,   730, -1435,   249,
   -1435, -1435,  -152,   328, -1435, -1435,  1088, -1435, -1435,  -918,
   -1435, -1435, -1435, -1435,  -267,  -469, -1435, -1435,   -18,   554,
   -1435,  1353, -1435,  1911,  -451,   660, -1435, -1435,  -798, -1435,
    -535, -1435,  -458,  -285,  -290, -1435,   997, -1435, -1435,  -262,
    -288, -1435, -1435,   530, -1435, -1435,   994, -1435, -1435, -1435,
   -1435,    41,    32,   207, -1435,   453,  -557, -1435, -1435,    43,
   -1435,  -261,   220,  1002, -1435, -1435, -1435, -1435, -1435,    38,
   -1435, -1435,   272,   -39,  1120, -1435, -1435,  -189,  1117, -1435,
    1302, -1435,  1119,  1121,  1115, -1435, -1435, -1435, -1435, -1435,
    1996,  -787,  -145,  -166,   807,   -37,  -831, -1298, -1435, -1435,
    -210, -1435,   -34,   456, -1435, -1435, -1435,   765,   768,  -501,
     774, -1435,  1264,  -375,  -372,  -866, -1435, -1435, -1435, -1435,
    -820,  -805, -1435, -1435, -1435, -1435,  -107, -1435,   471, -1435,
   -1435,  1018, -1435,   -80,  -695,  -119,  1267, -1435, -1435, -1435,
   -1435, -1435, -1435, -1435,  1020, -1435, -1435, -1435,   493, -1435,
    -495, -1435, -1435, -1435, -1435, -1435, -1435,  1016, -1435, -1435,
    1200, -1435, -1435, -1435, -1435, -1435, -1435, -1435, -1435, -1435,
   -1435,   204, -1085, -1435,  1028, -1435,     2, -1435, -1435,   972,
    -149, -1435,  1035, -1435, -1435, -1435,   466,   718,   999,  1039,
   -1435, -1435,   236,  1044, -1435, -1435,  1049, -1435, -1435,     5,
    1232,   982,   673,  -232,   671,   238,  -864,  -960,  -876, -1435,
     174, -1435,  1054, -1435,   708,  1057, -1435,   721,  1060, -1435,
   -1435, -1435, -1435,   489,   508, -1435, -1435, -1435, -1435, -1435,
   -1435, -1435, -1435,  -376, -1435, -1435, -1435,  1291, -1435, -1435,
    1575, -1435, -1435, -1435, -1435, -1435,   780, -1435, -1435, -1435,
   -1435, -1435, -1435, -1435, -1435, -1435, -1435, -1026, -1435,   -54,
   -1435, -1400, -1435,  1354,  1166, -1435, -1435,   925,  -477, -1435,
    1082, -1435, -1435, -1435, -1435, -1435, -1435,  1001,   943,   460,
     467, -1435, -1435,  1625,  -141, -1435, -1435, -1435, -1435, -1435,
   -1435, -1435, -1435, -1435, -1435,  -123, -1435, -1435, -1435, -1435,
     261, -1435, -1435, -1435,  1003, -1435,   464,   535, -1435, -1435,
   -1435, -1435, -1435,   570
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,    13,    14,    15,    16,    46,    17,    18,    33,
     279,  1308,  1309,  1501,  1613,  1595,  1310,  1676,  1311,  1591,
    1592,  1312,  1593,  1313,  1677,  1703,  1704,  1705,   340,  1315,
    1316,  1480,   341,    51,    52,    99,   100,   101,   170,   171,
     373,   374,   375,   371,   372,   915,   916,   917,   102,   172,
     173,   240,  1227,  1228,   241,   974,   174,   104,   559,  1087,
     242,    19,    20,    44,    68,    67,    70,    72,    71,    69,
     214,   215,   243,   244,   676,   415,   245,   246,   417,   977,
    1279,   221,   222,   223,   401,   247,   248,   106,   311,   107,
     292,   293,   479,   480,   997,   998,   769,   518,   519,   520,
     521,   767,  1040,  1041,  1441,  1044,  1045,  1269,  1444,  1579,
    1580,   732,   733,   249,   250,   734,  1240,  1241,  1242,   251,
     406,   252,   684,   407,   408,   409,  1202,  1203,   108,   109,
    1051,   523,   524,   525,   781,  1273,  1274,   784,   785,   794,
     786,  1459,  1460,   735,   110,  1053,  1277,  1407,  1408,  1409,
    1410,  1411,  1412,  1413,  1414,  1415,  1416,  1417,  1418,  1419,
    1420,  1454,   111,   526,   313,   528,   529,   112,   722,   493,
     494,   295,   296,   736,   297,   298,   486,   487,  1001,   737,
    1007,  1236,   738,   739,   113,   114,  1058,   792,  1280,  1589,
     115,   272,  1210,   697,   698,   116,   117,  1059,   286,   795,
     796,   797,   798,    53,   119,   800,   535,   536,  1063,  1064,
     120,  1217,   988,   989,   121,   275,   276,   459,  1211,   700,
     122,   288,  1220,   476,   799,   495,   994,  1564,   716,   717,
    1218,   253,   539,   124,   860,  1131,  1132,   628,   899,   900,
    1632,   897,   125,   514,   126,   323,   127,   501,   489,   128,
     129,   130,   752,   753,  1027,   754,   175,   586,  1515,  1106,
    1335,  1336,  1633,  1512,   863,   864,   865,  1108,  1339,  1340,
    1341,  1342,  1068,   176,   606,  1526,   911,  1143,  1353,  1354,
     254,   255,   256,   257,   258,   425,   428,   259,   260,   447,
     261,   448,   262,   263,   264,   265,   266,   450,   452,   455,
     267,  1109,  1110,   268,   515,   354,  1422,  1208,   364,   365,
     366,   367,   177,   178,   320,   547,   548,   549,   550,  1245,
     542,   543,  1246,   179,   180,   384,   643,   941,   181,   644,
     645,   581,   942,  1173,  1174,   707,   324,   325,   182,   134,
     135,   561,   136,   280,   465,   326,   562,   563,   137,   138,
     564,   829,   139,   565,   566,  1088,   343,   183,   184,   570,
     571,   849,   850,   141,   572,   851,  1095,   185,   186,   386,
     387,   187,  1527,  1104,   388,   651,   947,  1182,   648,   943,
    1178,  1179,  1180,   188,   189,   190,   191,   192,   368,   629,
     630,   631,   193,   587,  1345,   877,   878,  1111,   901,   194,
     922,  1157,  1158,   195,  1165,  1371,   196,  1161,  1368,   197,
     632,   633,   634,   635,  1166,  1167,  1030,  1031,  1032,  1259,
    1260,  1571,   198,   603,   604,   199,   595,   596,   200,  1349,
    1637,   352,   891,   892,   377,    21,   331,   152,    22,    66,
     578,  1510,  1101,  1331,   153,   332,   333,   334,    54,   329,
      55,  1329,  1686,   575,  1623,    23,    56,    24,    65,   612,
     613,  1528,  1148,  1358,   854,  1099,  1327,  1624,  1625,  1626,
    1627,   530,   145,   283,   284,   146,   471,   472,   270,   695,
     201,   390,   948,   654,  1387,   202,   638,   271,   953,   954,
     955,    25,    26,    27,    28,    29,   658,  1545,   207,   958,
    1390,  1391,   660,  1648,  1192,    30,    31,   657,   205,   662,
    1546,  1194,   392,   656,   959,   960,   961,   203,   154,   576,
     349,  1084,  1590,   608
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      43,   702,   363,   398,   488,   404,   491,   103,   701,   641,
     290,   150,   642,   335,   414,   355,   357,   327,   131,   105,
     742,   477,   950,   362,   105,   723,  1002,  1149,   416,   105,
     105,   336,   490,   481,   328,   429,   269,   553,  1103,   400,
    1233,    98,   312,  1235,   729,   485,    98,   363,   418,   910,
     812,    98,    98,   118,   142,   103,   896,   143,   545,  1168,
    1168,   105,   239,   950,  1212,   906,   131,   105,  1258,   431,
    1162,   105,  1328,   509,   972,   848,  1175,   724,   430,   432,
    1112,  1113,  1114,    98,  1126,  1127,   421,  1119,  1450,    98,
     932,  1379,  1455,    98,   314,  1461,   376,   277,  1446,  -523,
     212,   118,   142,   668,   318,   143,  1628,   227,   228,   291,
     522,   807,   809,   902,   219,  -195,   460,   468,     4,   151,
    1445,  1655,   908,   -89,   912,   492,   -89,   711,   492,   933,
     414,  1299,   933,   414,   758,  1170,  -195,     4,     4,   105,
     980,  1655,  1023,  1121,   416,   802,  -410,   416,     4,  -411,
     637,   319,  1136,   936,    47,     4,   347,     4,     8,  1584,
     508,    98,   348,  1447,   418,  1186,     4,   418,  1196,   703,
    1261,   704,   230,  1270,     4,  1198,  -195,     8,     8,  1186,
    1370,   492,   414,   269,   665,   828,  -410,  1222,     8,  -411,
    1286,     4,   335,  1320,     4,     8,   416,     8,  1365,  1380,
    1399,     4,     4,  1258,   594,   681,     8,   602,  1431,   679,
     336,   912,  1516,    42,     8,  1697,   418,  1333,  1325,  -182,
     433,  1025,   346,   234,  1022,   976,   413,  1360,  -730,   573,
      32,     8,   281,    89,     8,   508,   508,   508,   432,    42,
     555,     8,     8,   956,   517,   685,  1006,  1559,   685,   376,
     534,   432,   227,   228,    49,   474,  1457,  1577,    97,   475,
    -266,  -338,   990,   765,   782,  1334,  -540,   503,   464,   282,
      73,  1446,   213,   516,   727,  1294,  1204,   422,  1266,  1670,
     236,   652,   546,    98,  -540,   516,   220,  1628,   730,   614,
    -523,    12,   105,  1445,   669,  -523,   861,  1542,   376,   531,
     532,  1439,   808,   810,   903,  1004,  -195,  1378,   741,  -195,
      12,    12,   412,   909,    98,   913,   861,   728,  -764,  -410,
     934,    12,  -411,   935,   862,    34,  1346,    48,    12,  1343,
      12,   981,   577,  1024,  1122,   756,  1447,  1656,  1333,    12,
    1120,   105,   413,  1137,   862,   413,  1125,    12,   607,  1128,
     640,   105,  1350,  1134,  1094,    35,  1187,  1656,  -764,  1197,
    -474,     4,   269,    98,    12,   646,  1199,    12,   506,  1008,
    1206,  1026,   972,    98,    12,    12,  1012,  1239,  1223,   433,
     574,  1287,  1326,   269,   269,  1446,  1334,  1659,   926,  1366,
    1381,  1400,   688,  1559,   413,   659,   661,  1458,  1578,  1432,
    1351,     8,  1502,  1517,  1673,  1446,  1698,   302,    92,    93,
     239,    95,   302,   957,   766,    42,   302,  -540,   310,   745,
     747,   235,   647,   204,    42,   236,   105,     4,    42,  1267,
      47,   488,    42,   491,     4, -1020,    74, -1020,   876,   890,
     893,   610,   842,   506,   506,   506,    42,   639,   699,   904,
     302,   968,   517,   966,   385,   975,  -539,   969,   970,   490,
     481,    64,   105,   105,   731,   852,   429,     8,  1350,   730,
     918,     4,   485,     4,     8,   508,    76,   817,  1258,   498,
     499,  1551,  1630,  1521,    98,    98,   726,  -197,   919,   741,
     751,  -764,  1585,     9,  1168,  1586,     4,   363,   430,    75,
     431,   363,   363,   498,   499,   227,   228,  1042,   132,   430,
     285,     8, -1020,     8, -1020,   823,  1351,    58,   105,    59,
      60,   793,   845,   133,   702,  1221,   827,   105,   344,   742,
     782,   701,  1014,   742,    12,  1511,     8,   148,   649,   500,
      98,    89,     4,    98,   206,   140,  1070,  -464,  1072,    98,
     462,   463,  1511,  1232,   950,   557,   132,   729,   105,   105,
     144,  1013,   832,   500,     4,   837,    97,   105,   105,   832,
       9,   133,   274,  1290,  1531,   699,   533,   558,    11,     4,
      98,    98,     8,   285,     4,  1352,  1561,   147,  -137,    98,
      98,     4,   105,   140,   944,   699,   -90,    60,   287,   -90,
      12,  1046,   483,    48,     8,    11,   105,    12,   144,  1149,
     230,   301,  1671,   304,    98,  1672,    89,    89,   305,     8,
    1364,   306,   474,   824,     8,    42,   475,   938,    98,   269,
     151,     8,  -933,    92,    93,   147,    95,   302,  1078,   484,
      42,    97,    97,   949,    12,   492,    12,  1103,  1005,   496,
     307,   232,   233,   894,  -539,    11,  -236,    82,    83,  1037,
    -508,   234,   905,  1091,  -508,  -944,   310,   973,   497,    12,
      92,    93,   357,    95,   235,  1476,  1477,    42,   236,  1225,
    -236,   -61,  1010,   506,   949,   929,   930,   931,  1038,   235,
     105,   155,   703,   236,   704,    11,  1039,   686,  1213,   302,
     687,  1011,   987,   315,   -93,   556,   557,   -93,   316,  1323,
     999,   317,    98,   411,   322,    12,   434,   435,   436,   437,
     438,   439,   440,  1117,  1089,  1657,  1561,   516,   558,   321,
     -92,  1374,  1561,  1469,   330,   534,  1129,    12, -1020,   412,
   -1020,   236,   399,  1569,  1096,  1657,  1376,   230,  1405,  1149,
     337,  1537,    12,   594,   426,   427,  1043,    12,  1620,   602,
      36,    37,    38,    39,    12,   990,   453,   454,    40,  1069,
     546,   338,   546,  1156,    41,   918,     5, -1020,     6, -1020,
     216,   217,   218,  1231,   224,     7,   350,   727,   508,   351,
      42,   105,   294,   919,    82,    83,     9,   416,   234,  1239,
     641,   730,   358,   642,    57,   414,   359,    92,    93,    62,
      95,    63,  1020,    98,    42,   861,  1392,   741,  1393,   416,
      89,   741,   369,    60,   640,   105, -1020, -1021, -1020, -1021,
     728,   370,   569,  1115,  1093,     9,   389,    92,    93,   418,
      95,   385,   105,   862,    42,    97,   396,    98,   105,   105,
     105,   105,   861,   105,  1473,   105,   105,   756,  1481,   391,
     414,   393,    10,   411,    98,   441,   442,   443,   444,   412,
      98,    98,    98,    98,   416,    98,  1150,    98,    98,   420,
     862,    11,    11,  -276,     9,     9,  1159,  -223,   105,    92,
      93,  -945,    95,   424,   418,   449,   308,   282,   225,   445,
     446,  1144,   105,   105,   451,   226,  1451,  1452,  1453,   457,
      98,    60,    60,   227,   228,   229,   771,   674,   675,   512,
      11,   639,    11,  -277,    98,    98,   569,   847,  1171,  1172,
     -97,   -97,   458,   -97,   602,   470,  1118,   -97,    89,   513,
     -97,   702,  1678,  1679,   473,    77,   516,   551,   701,   105,
    1133,   208,   209,   482,  1424,    92,    93,   527,    95,   556,
    1201,   569,    42,    97,   394,   395,   554,   402,   269,    11,
      11,   699,   876,  -100,  -100,   588,  -100,   731,    42,  -138,
    -100,   742,   653,  -100,  1151,   560,   567,   890,   403,   616,
      82,    83,   772,   663,   239,  1226,   506,  1292,   239,   726,
     664,   666,  1429,   641,  1362,   667,   642,   673,  1042,   672,
    -234,  -229,   751,   678,  1427,   680,   730,   413,   230,   683,
    -224,   773,  1474,   774,   775,   776,  1474,   694,   777,   778,
     708,   779,   780,   696,   706,   713,   741,   718,   714,   715,
     748,   105,  1150,   231,   720,   430,   721,   226,   760,   793,
     761,   764,   763,   768,   105,   227,   228,   749,   363,   232,
     233,   -99,   -99,    98,   -99,   787,   783,   788,   -99,   234,
     546,   -99,  1314,   790,   607,   791,    98,   804,    92,    93,
     801,    95,   235,   806,   814,    42,   236,   237,   818,    92,
      93,   105,    95,   819,   699,   238,    42,   304,   400,     2,
       3,   742,   305,    92,    93,   378,    95,   846,   853,   703,
      42,   704,   294,    98,   855,   379,   858,   380,   105,   857,
     381,   382,   105,     4,   859,   227,   228,     5, -1020,     6,
   -1020,   699,   914,   920,   307,   921,     7,   923,   927,   383,
      98,   640,   105,   940,    98,   946,   964,   304,   965,   967,
     824,   971,   305,  -230,   440,  1359,   979,   982,   210,   211,
     230,   983,   991,     8,    98,   379,   992,   380,   993,   996,
     381,   382,  1033,   273,  1384,   949,  1005,   278,  1006,  1009,
    -447,  1421,  1048,   289,   307,   231,     9,  1015,  1050,   383,
     750,  1049,    36,    37,    38,    39,  1479,    93,  1052,    95,
      40,   232,   233,    42,  1056,   987,    41,  1057,    42,  1060,
    1062,   234,  1066,    10,  1428,  1077,  1156,  1363,  1074,  1080,
      92,    93,  1085,    95,   235,   307,  1098,    42,   236,   237,
     230,  1421,    92,    93,  1097,    95,  1421,   238,   639,    42,
    1421,  1163,  1100,  1421,   990,  1105,  1107,  1147,  1116,  1124,
    1721,   345,  1069,  1142,  1185,  1188,  1189,  1190,  1191,  1043,
     820,   360,  1193,  1215,  1216,  1195,  1248,  1200,  1205,  1214,
    1224,    11,  1250,  1278,  -202,  -202,  -202,  -202,  1265,   105,
    1268,   234,  -202,   105,  1264,  1281,  1285,  1288,  -202,  1289,
      92,    93,  1321,    95,   235,  1338,    12,    42,   236,  1330,
     105,    98,  1357,  1347,  1069,    98,  1355,  1356,  1361,  1261,
     305,  1370,  1373,  1086,  1435,  1382,  1389,   105,  1394,  1395,
    1396,  1397,    98,   379,  1401,   380,  1430,  1437,   381,   382,
     159,  1438,  -522,  1440,  1651,  -197,  1462,   105,  1650,    98,
     161,   162,   307,   163,   640,   105,   164,   383,  1472,   821,
     166,   822,  1463,  1507,  1467,  1666,  1470,  1475,  1478,    98,
    1550,  1504,  1503,  1506,  1508,  1513,   105,    98,   461,  1514,
    1524,  1519,  1525,   466,  1544,  1548,   469,  1549,  1552,  1553,
    1421,  1554,   363,   478,  1555,  1562,  1556,  1557,   699,    89,
    1558,  1563,  1574,  -380,  1565,  1575,  1581,   502,  1576,   363,
     511,  1622,  1582,  1583,  1421,   123,    92,    93,  1588,    95,
     123,  -255,  1629,    42,    97,   123,   123,  1326,  1474,  1643,
     239,  1644,  1646,  1530,  1652,   105,  1654,  1201,  1443,  1662,
    1663,  1667,  1668,   640,   105,  1669,   583,   584,  1687,   585,
    1696,   639,  1690,  1713,  1695,  1692,  1716,   299,   597,  1693,
     605,  1700,  1701,   123,  1719,  1729,    98,   299,  1150,  1720,
     225,   609,  1726,   611,  1707,  1708,  1709,   226,  1723,  1731,
     105,  1727,    78,   230,  1674,   227,   228,   229,   303,   546,
     105,  1146,  1229,   655,  1230,  1448,    50,  1715,   677,  1456,
     995,   105,    98,  1425,   770,  1573,    45,  1442,   231,   682,
    1234,  1567,    98,  1566,  1449,   670,   423,  1275,  1054,  1660,
    1664,  1661,   269,    98,   232,   233,  1560,  1055,  1421,  1421,
    1421,  1000,  1247,   725,   234,   123,  1421,  1209,  1237,  1284,
     639,   363,  1065,    92,    93,  1464,    95,   235,  1421,  1421,
      42,   236,   237,  1714,  1403,   105,  1665,  1219,   705,  1135,
     238,   815,  1421,  1253,   825,  1634,  1638,  1523,  1337,  1636,
     709,  1518,   826,  1639,   712,   690,   824,   689,   456,   691,
     693,  1036,  1071,   692,  1018,   719,   830,  1073,  1067,  1675,
     230,   226,   544,   843,  1541,   844,   650,  1344,   831,   227,
     228,   925,   744,   746,   568,   833,   757,  1123,  1150,   834,
     907,  1532,   759,   928,   835,   231,  1160,  1169,  1535,   836,
     105,  1176,  1570,   636,   838,  1694,  1140,   839,   226,   789,
     840,   232,   233,  1138,   579,  1348,   227,   228,   803,   149,
    1699,   234,    98,  1711,   710,   467,   986,   805,   978,   939,
      92,    93,   841,    95,   235,   813,  1386,    42,   236,   237,
     816,    61,  1385,  1388,  1547,  1318,     0,   238,     0,     0,
       0,   963,     0,     0,     0,     0,     0,     0,     0,   856,
       0,   538,     0,     0,     0,   105,  1722,     0,   123,  1724,
       0,     0,     0,  1254,   987,     0,     0,     0,     0,     0,
     226,     0,  1732,     0,   230,     0,     0,    98,   227,   228,
       0,     0,     0,     0,     0,     0,  1151,  1689,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   231,
       0,     0,     0,     0,     0,     0,     0,   123,     0,     0,
     937,   230,     0,   538,   538,   232,   233,   299,  1594,     0,
       0,   945,     0,     0,   226,   234,     0,     0,     0,     0,
     962,     0,   227,   228,    92,    93,   231,    95,   235,     0,
       0,    42,   236,   237,     0,     0,     0,     0,     0,     0,
       0,   238,   232,   233,     0,     0,     0,     0,     0,     0,
       0,     0,   234,     0,     0,     0,  1406,     0,     0,     0,
       0,    92,    93,   226,    95,   235,     0,     0,    42,   236,
     237,   227,   228,   230,     0,     0,     0,     0,   238,  1482,
    1483,  1484,  1485,  1486,  1487,  1003,  1488,  1489,  1490,  1491,
    1492,  1493,     0,  1494,  1495,  1496,  1497,  1498,   231,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1016,     0,
    1017,     0,     0,     0,   232,   233,     0,     0,   299,   299,
     740,     0,     0,     0,   234,  1035,     0,   230,     0,     0,
       0,  1047,     0,    92,    93,     0,    95,   235,     0,     0,
      42,   236,   237,     0,  1443,     0,     0,     0,     0,     0,
     238,   226,   231,     0,     0,     0,     0,     0,     0,   227,
     228,     0,     0,  1061,     0,     0,     0,     0,   232,   233,
       0,     0,     0,     0,   299,  1075,   230,  1658,   234,     0,
    1076,     0,  1079,   299,   226,     0,     0,    92,    93,     0,
      95,   235,   227,   228,    42,   236,   237,     0,     0,   226,
       0,   231,     0,     0,   238,     0,     0,   227,   228,     0,
    1092,     0,  -763,     0,   299,   299,     0,   232,   233,     0,
       0,  1102,     0,   299,   299,     0,     0,   234,     0,     0,
       0,     0,     0,     0,     0,     0,    92,    93,     0,    95,
     235,     0,   538,    42,   236,   237,     0,     0,   299,     0,
       0,     0,  -763,   238,     0,     0,     0,     0,     0,     0,
       0,     0,   299,     0,   230,     0,  1139,     0,   680,  1499,
    1500,     0,  1141,     0,     0,   300,     0,   538,     0,     0,
       0,     0,     0,     0,     0,   309,     0,     0,  1702,   231,
       0,     0,     0,     0,     0,   226,     0,   230,     0,     0,
       0,     0,     0,   227,   228,   232,   233,  1181,     0,     0,
    1183,  1184,   230,     0,     0,   234,     0,     0,   538,     0,
       0,     0,   231,     0,    92,    93,     0,    95,   235,     0,
       0,    42,   236,   237,     0,     0,     0,   231,   232,   233,
       0,   238,     0,     0,     0,     0,   299,     0,   234,     0,
       0,     0,     0,   232,   233,     0,     0,    92,    93,     0,
      95,   235,     0,   234,    42,   236,   237,     0,     0,     0,
       0,     0,    92,    93,   238,    95,   235,     0,     0,   339,
     236,   237,     0,     0,     0,  -763,     0,     0,     0,   238,
       0,     0,     0,  1262,     0,     0,  1263,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1271,   230,  1272,
       0,     0,     0,     0,  1276,     0,     0,     0,     0,     0,
       0,  1282,  1283,   342,     0,     0,     0,     0,     0,  1717,
       0,     0,     0,   231,   361,     0,   226,     0,     0,     0,
    1291,  1293,     0,     0,   227,   228,     0,   299,     0,   232,
     233,     0,     0,     0,  1322,     0,  1324,     0,     0,   234,
       0,     0,     0,     0,     0,     0,     0,  1332,    92,    93,
       0,    95,   235,     0,     0,    42,   236,   237,     0,     0,
       0,   299,     0,     0,     0,   238,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   410,   299,   540,
       0,     0,     0,   419,   299,   299,   299,   299,  1611,   299,
       0,   299,   299,     0,   226,     0,     0,     0,   410,     0,
       0,     0,   227,   228,  1367,  1612,     0,     0,  1369,     0,
       0,     0,  1372,     0,  1375,  1377,     0,     0,     0,     0,
       0,     0,     0,     0,   123,  1383,     0,     0,     0,   230,
       0,     0,     0,   227,   228,     0,   538,     0,   299,   299,
       0,   627,   627,     0,     0,     0,     0,     0,     0,     0,
       0,   507,   510,  1398,   231,     0,     0,     0,     0,     0,
    1404,     0,     0,  1423,   541,     0,  1426,   552,     0,     0,
     232,   233,     0,     0,     0,     0,     0,     0,     0,     0,
     234,     0,     0,   580,   582,     0,     0,     0,     0,    92,
      93,  1433,    95,   235,     0,     0,    42,   236,   237,   593,
       0,     0,   593,     0,     0,     0,   238,   230,     0,   740,
       0,     0,   538,   740,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   615,     0,   361,   361,   507,     0,
       0,  1616,   231,  1465,     0,  1466,     0,   226,   230,     0,
       0,     0,     0,     0,     0,   227,   228,     0,   232,   233,
       0,   671,     0,     0,     0,     0,   300,   300,   234,     0,
       0,     0,     0,   231,     0,     0,  1505,    92,    93,     0,
      95,   235,  1509,     0,    42,   236,   237,   299,     0,   232,
     233,     0,     0,     0,   238,     0,     0,     0,  1520,   234,
     123,     0,  1522,     0,     0,     0,     0,     0,    92,    93,
       0,    95,   235,     0,     0,    42,   236,   237,     0,  1533,
       0,  1534,   811,     0,  1536,   238,  1538,  1539,     0,  1540,
       0,     0,     0,     0,     0,     0,     0,   299,     0,     0,
    1543,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1618,     0,   743,     0,     0,   755,   226,     0,
     230,     0,   895,   898,   299,     0,   227,   228,   299,   762,
       0,   895,   898,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   231,     0,     0,   299,     0,
     627,     0,     0,  1572,     0,     0,   895,     0,   538,   538,
       0,   232,   233,     0,   226,     0,     0,     0,     0,     0,
     309,   234,   227,   228,     0,     0,     0,     0,     0,     0,
      92,    93,     0,    95,   235,   951,     0,    42,   236,   237,
       0,     0,     0,     0,     0,     0,     0,   238,     0,     0,
       0,     0,   879,     0,   880,   881,   882,   883,     0,   884,
       0,   885,   886,     0,     0,     0,     0,  1621,   887,     0,
     888,     0,   889,  1631,     0,     0,   951,     0,  1635,     0,
       0,   230,     0,     0,     0,     0,   507,     0,     0,  1640,
    1641,  1642,     0,     0,     0,     0,     0,     0,     0,     0,
    1645,     0,     0,     0,   300,     0,   231,     0,  1647,     0,
    1649,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     952,     0,   232,   233,     0,     0,     0,   230,     0,     0,
       0,     0,   234,     0,     0,     0,     0,     0,     0,   410,
       0,    92,    93,     0,    95,   235,     0,     0,    42,   236,
     237,     0,   231,     0,     0,     0,   299,     0,   238,     0,
       0,   952,     0,     0,     0,     0,     0,     0,   232,   233,
       0,     0,   984,   299,   985,     0,     0,     0,   234,     0,
       0,     0,     0,  1685,     0,     0,     0,    92,    93,     0,
      95,   235,     0,   299,    42,   236,   237,     0,     0,     0,
    1688,   299,     0,     0,   238,   309,     0,     0,     0,     0,
       0,  1691,     0,     0,  1019,     0,  1021,     0,     0,     0,
       0,     0,     0,     0,  1029,     0,     0,  1034,  -225,  -225,
    -225,     0,  -225,     0,  -225,  -225,  -225,  -225,  -225,  -225,
    -225,  -225,  -225,  -225,  -225,  -225,  -225,     0,     0,  1710,
       0,     0,  1712,   538,   538,     0,   895,     0,     0,     0,
       0,     0,   895,   898,   898,   895,     0,  1130,     0,   895,
    1130,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1587,     0,     0,     0,     0,  -225,     0,     0,     0,
     299,     0,   -88,   -88,   -88,     0,   -88,     0,   -88,   -88,
     -88,   -88,   -88,   -88,   -88,   -88,   -88,   -88,   -88,   -88,
     -88,     0,  1090,     0,   627,     0,  1164,  1164,     0,     0,
       0,     0,     0,     0,     0,     0,   123,     0,     0,     0,
       0,     0,   507,   507,   507,   507,   299,     0,     0,     0,
     507,     0,     0,     0,     0,   507,     0,   299,     0,   226,
     -88,     0,     0,     0,     0,     0,     0,   227,   228,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1145,     0,     0,  -225,  -225,  -225,  -225,   866,     0,     0,
    1243,   867,   868,   869,   870,   871,   872,     0,     0,   361,
    1029,  1587,     0,   873,   874,   875,   580,     0,     0,  1177,
       0,     0,     0,     0,  -225,     0,     0,  -225,  -225,  -225,
     226,     0,   412,     0,     0,     0,     0,     0,   227,   228,
     617,     0,     0,     0,     0,     0,     0,   410,     0,     0,
       0,     0,     0,     0,     0,  1207,     0,   -88,   -88,   -88,
     -88,     0,     0,     0,     0,   811,     0,     0,     0,   618,
       0,     0,   230,     0,     0,     0,   123,   619,     0,   620,
     621,   622,   623,     0,   624,  1244,   625,     0,   -88,   -88,
       0,   -88,   -88,   -88,  1249,   -88,     0,   231,  1252,   755,
       0,  1255,  1256,     0,  1257,     0,     0,     0,     0,     0,
       0,     0,   226,   232,   233,   898,     0,     0,     0,     0,
     227,   228,   504,   234,     0,     0,     0,     0,     0,     0,
       0,     0,    92,    93,     0,    95,   235,     0,     0,    42,
     236,   237,   309,   230,     0,     0,   895,     0,     0,   238,
       0,     0,     0,     0,     0,     0,   580,     0,     0,  1317,
       0,     0,  1319,     0,     0,     0,     0,     0,   231,     0,
       0,     0,     0,     0,     0,     0,   951,   951,     0,     0,
       0,     0,     0,     0,   232,   233,    89,     0,     0,     0,
       0,     0,   507,   226,   234,     0,     0,     0,     0,     0,
       0,   227,   228,    92,    93,     0,    95,   235,     0,     0,
     626,   505,   237,     0,     0,     0,     0,     0,     0,     0,
     238,     0,     0,     0,     0,   230,     0,     0,     0,     0,
       0,   879,     0,   880,     0,   882,   883,     0,   884,  1029,
     885,   886,     0,     0,     0,     0,     0,   887,     0,   888,
     231,   889,  1177,     0,     0,     0,     0,     0,     0,     0,
       0,   952,   952,     0,     0,  1295,   232,   233,    89,     0,
       0,     0,     0,  1296,  1297,     0,   234,     0,     0,     0,
       0,     0,     0,     0,     0,    92,    93,     0,    95,   235,
       0,  1402,    42,   505,   237,   226,     0,     0,  1298,     0,
       0,     0,   238,   227,   228,     0,   230,     0,     0,     0,
       0,     0,   598,     0,     0,     0,     0,     0,     0,   599,
       0,   600,   601,     0,   898,     0,  1434,     0,     0,     0,
    1436,   231,     0,     0,     0,     0,     0,  1029,     0,     0,
       0,   898,   589,     0,     0,     0,     0,   232,   233,   590,
       0,   591,   592,     0,     0,     0,     0,   234,     0,     0,
       0,  1164,     0,  1299,     0,     0,    92,    93,     0,    95,
     235,  1468,     0,    42,   236,   237,     0,     0,   230,     0,
       0,     0,     0,   238,     0,  1471,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   231,     0,     0,     0,     0,   230,  1300,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1301,
    1302,  1243,  1243,  1029,     0,     0,     0,     0,     0,   234,
       0,     0,     0,   231,     0,  1529,     0,   507,  1303,  1304,
       0,  1305,  1306,   226,     0,    42,  1307,   237,     0,   232,
     233,   227,   228,     0,     0,   238,  1177,     0,  1177,   234,
       0,     0,     0,     0,     0,     0,     0,     0,    92,    93,
       0,    95,   235,     0,     0,    42,   236,   237,     0,     0,
       0,     0,     0,   226,     0,   238,     0,     0,     0,     0,
     598,   227,   228,     0,     0,     0,     0,   599,     0,   600,
     601,     0,     0,     0,   895,     0,  1244,  1244,     0,     0,
       0,     0,     0,  1568,  1029,   895,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1152,
    1153,     0,     0,  1207,     0,     0,   226,  1154,     0,  1155,
       0,     0,     0,     0,   227,   228,     0,     0,     0,     4,
       0,     0,     0,     0,     0,     0,   230,     0,  1596,  1597,
    1598,  1599,  1600,  1601,  1602,  1603,  1604,  1605,  1606,  1607,
    1608,  1609,  1610,  1614,  1615,  1617,  1619,     0,     0,     0,
       0,   231,     0,   226,     0,     0,     0,     0,     0,     8,
       0,   227,   228,     0,   226,     0,   230,   232,   233,     0,
       0,     0,   227,   228,   924,     0,     0,   234,     0,     0,
       0,     0,     0,     0,     0,     0,    92,    93,     0,    95,
     235,   231,     0,    42,   236,   237,     0,     0,  1653,     0,
       0,     0,     0,   238,     0,     0,     0,   232,   233,     0,
       0,     0,     0,     0,     0,     0,     0,   234,     0,   230,
       0,     0,     0,  1207,     0,     0,    92,    93,     0,    95,
     235,     0,     0,    42,   236,   237,   397,   226,     0,     0,
    1680,     0,     0,   238,   231,   227,   228,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1681,  1682,     0,
     232,   233,  1683,     0,  1684,     0,   230,     0,     0,     0,
     234,     0,     0,     0,     0,     0,     0,   230,     0,    92,
      93,     0,    95,   235,     0,   226,    42,   236,   237,     0,
       0,   231,    12,   227,   228,  1251,   238,     0,     0,     0,
       0,     0,   231,     0,     0,     0,     0,   232,   233,     0,
       0,     0,     0,     0,     0,   226,     0,   234,   232,   233,
       0,     0,  1706,   227,   228,  -393,    92,    93,   234,    95,
     235,     0,     0,    42,   236,   237,     0,    92,    93,     0,
      95,   235,     0,   238,    42,   236,   237,   226,  1718,     0,
     230,     0,     0,     0,   238,   227,   228,     0,     0,     0,
       0,     0,     0,  1725,     0,  1706,  1728,     0,     0,     0,
       0,   226,  1730,     0,     0,   231,     0,  1733,     0,   227,
     228,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   232,   233,     0,     0,   226,     0,     0,   230,     0,
       0,   234,     0,   227,   228,     0,     0,     0,     0,     0,
      92,    93,     0,    95,   235,     0,     0,   537,   236,   237,
    1238,     0,     0,   231,     0,   226,     0,   238,   230,     0,
       0,     0,     0,   227,   228,     0,     0,     0,     0,   232,
     233,     0,     0,     0,     0,     0,     0,     0,     0,   234,
       0,     0,     0,   231,     0,     0,     0,     0,    92,    93,
     230,    95,   235,     0,     0,    42,   236,   237,     0,   232,
     233,     0,     0,   226,     0,   238,     0,     0,     0,   234,
       0,   227,   228,     0,   230,   231,     0,     0,    92,    93,
       0,    95,   235,     0,     0,    42,   236,   237,     0,     0,
       0,   232,   233,     0,   226,   238,     0,     0,   230,   231,
       0,   234,   227,   228,     0,     0,     0,     0,     0,     0,
      92,    93,     0,    95,   235,   232,   233,    42,   236,   353,
       0,     0,     0,   231,     0,   234,     0,   238,   230,     0,
       0,     0,     0,     0,    92,    93,     0,    95,   235,   232,
     233,    42,   236,   356,     0,     0,     0,   226,     0,   234,
       0,   238,     0,   231,     0,   227,   228,     0,    92,    93,
       0,    95,   235,     0,     0,    42,   236,   405,     0,   232,
     233,     0,     0,     0,     0,   238,   230,     0,     0,   234,
       0,     0,     0,     0,     0,     0,     0,     0,    92,    93,
       0,    95,   235,     0,     0,    42,   236,   237,     0,     0,
       0,   231,     0,     0,     0,   238,     0,   230,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   232,   233,     0,
       0,     0,     0,     0,     0,     0,     0,   234,     0,     0,
       0,    79,   231,     0,     0,     0,    92,    93,    80,    95,
     235,     0,     0,   537,   236,   237,     0,     0,   232,   233,
       0,     0,     0,   238,     0,     0,     0,     0,   234,     0,
     230,     0,     0,     0,     0,     0,     0,    92,    93,     0,
      95,   235,     0,     0,    42,   236,  1028,     0,     0,     0,
       0,     0,     0,     0,   238,   231,     0,     0,    81,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   232,   233,     0,     0,    82,    83,     0,     0,     0,
       0,   234,     0,     0,     0,     0,     0,    84,     0,     0,
      92,    93,     0,    95,   235,     0,     0,  1081,  1082,  1083,
       0,     0,     0,     0,     0,     0,  -448,   238,    85,    80,
      86,    87,     0,     0,     0,   155,  -461,     0,  -474,     0,
       0,     0,     0,    88,  -706,   156,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    89,     0,     0,    90,    91,  -333,     0,
       0,  -333,  -333,  -333,  -333,   157,     0,     0,     0,  -333,
      92,    93,    94,    95,     0,  -333,     0,    96,    97,     0,
     158,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -706,  -706,  -706,     0,   159,     0,    80,    84,     0,
    -706,     0,   160,   155,     0,   161,   162,     0,   163,     0,
       0,   164,     0,   156,   165,   166,   167,     0,  -706,     0,
      80,     0,     0,     0,     0,     0,   155,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   156,     0,     0,     0,
     168,     0,     0,   157,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    89,  -706,  -706,    90,   158,     0,
       0,     0,     0,     0,     0,     0,   157,     0,     0,     0,
       0,    92,    93,   159,    95,     0,    84,  -128,   169,    97,
     160,   158,     0,   161,   162,     0,   163,     0,     0,   164,
       0,     0,   165,   166,   167,     0,   159,     0,     0,    84,
    -129,     0,     0,   160,   155,     0,   161,   162,     0,   163,
       0,     0,   164,     0,   156,   165,   166,   167,   168,     0,
       0,     0,     0,     0,     0,     0,     0,   820,     0,     0,
       0,     0,    89,     0,     0,    90,     0,     0,     0,     0,
       0,   168,     0,     0,   157,     0,     0,  -128,     0,    92,
      93,     0,    95,     0,     0,    89,   169,    97,    90,   158,
       0,     0,     0,     0,     0,     0,     0,   157,     0,     0,
    -129,     0,    92,    93,   159,    95,     0,    84,  -125,   169,
      97,   160,   158,     0,   161,   162,     0,   163,     0,     0,
     164,     0,  1033,   165,   166,   167,     0,   159,     0,     0,
       0,     0,     0,     0,   160,   820,     0,   161,   162,     0,
     163,     0,     0,   164,     0,     0,   165,   166,   167,   168,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    89,     0,     0,    90,     0,     0,     0,
       0,     0,   168,     0,     0,   157,     0,     0,  -125,     0,
      92,    93,     0,    95,     0,     0,    89,   169,    97,     0,
     158,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    92,    93,   159,    95,     0,     0,     0,
      42,    97,   160,     0,     0,   161,   162,     0,   163,     0,
       0,   164,     0,     0,   165,   166,   167,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     168,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    89,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    92,    93,     0,    95,     0,     0,     0,    42,    97
};

static const yytype_int16 yycheck[] =
{
      11,   459,   168,   213,   294,   220,   294,    52,   459,   384,
      90,    56,   384,   154,   237,   164,   165,   136,    52,    52,
     497,   288,   654,   168,    57,   494,   721,   921,   237,    62,
      63,   154,   294,   291,   141,   258,    73,   322,   858,   219,
    1006,    52,   109,  1006,   497,   294,    57,   213,   237,   606,
     551,    62,    63,    52,    52,   100,   591,    52,   319,   935,
     936,    94,    73,   695,   982,   600,   100,   100,  1028,   258,
     934,   104,  1098,   305,   673,   570,   942,   495,   258,   259,
     867,   868,   869,    94,   882,   883,     3,   874,  1273,   100,
     625,  1176,  1277,   104,   110,  1280,   172,     3,  1268,     3,
      21,   100,   100,     3,   146,   100,  1506,    19,    20,     3,
       3,     3,     3,     3,    21,     3,     3,     3,    24,    56,
    1268,  1555,     3,   191,     3,     3,   194,     3,     3,     3,
     353,   109,     3,   356,     3,   940,    24,    24,    24,   172,
       3,  1575,     3,     3,   353,     3,    24,   356,    24,    24,
     382,   193,     3,     3,    24,    24,   191,    24,    64,  1457,
     305,   172,   197,  1268,   353,     3,    24,   356,     3,   459,
       3,   459,   124,     3,    24,     3,    64,    64,    64,     3,
       3,     3,   405,   220,   399,   560,    64,     3,    64,    64,
       3,    24,   333,  1087,    24,    64,   405,    64,     3,     3,
       3,    24,    24,  1163,   353,   420,    64,   356,     3,   418,
     333,     3,     3,   191,    64,     3,   405,    97,    27,     5,
     259,     4,   159,   175,     5,   678,   237,     3,     5,     3,
     184,    64,   154,   167,    64,   380,   381,   382,   418,   191,
     325,    64,    64,    21,   311,   425,    21,  1417,   428,   325,
     317,   431,    19,    20,    24,    18,     5,     5,   192,    22,
     154,   154,   713,     3,   522,   145,     3,   304,   279,   191,
     193,  1441,   193,   310,   497,  1080,   971,   194,     3,  1577,
     192,   388,   319,   294,    21,   322,   193,  1687,   497,   374,
     194,   197,   325,  1441,   194,   199,   586,  1382,   374,   315,
     316,  1261,   194,   194,   194,   723,   194,  1173,   497,   197,
     197,   197,   200,   194,   325,   194,   606,   497,    24,   197,
     194,   197,   197,   194,   586,   191,  1124,   197,   197,  1116,
     197,   194,   331,   194,   194,   501,  1441,  1556,    97,   197,
     875,   374,   353,   194,   606,   356,   881,   197,   359,   884,
     384,   384,    97,   888,   849,   191,   194,  1576,    64,   194,
     160,    24,   399,   374,   197,   136,   194,   197,   305,   731,
     194,   154,   971,   384,   197,   197,   738,  1009,   194,   418,
     154,   194,   191,   420,   421,  1555,   145,  1557,   620,   194,
     194,   194,   431,  1563,   405,   394,   395,   146,   146,   194,
     145,    64,   194,   194,  1589,  1575,   194,   193,   184,   185,
     421,   187,   193,   191,   154,   191,   193,   154,   193,   499,
     500,   188,   193,   191,   191,   192,   459,    24,   191,   154,
      24,   721,   191,   721,    24,    29,   194,    31,   587,   588,
     589,    48,   561,   380,   381,   382,   191,   384,   459,   598,
     193,   666,   519,   663,    47,   678,   199,   667,   668,   721,
     718,    33,   495,   496,   497,   572,   689,    64,    97,   678,
     611,    24,   721,    24,    64,   620,    48,   557,  1438,   110,
     111,  1399,  1508,  1347,   495,   496,   497,   113,   611,   678,
     501,   197,  1458,    87,  1370,  1458,    24,   663,   678,   194,
     689,   667,   668,   110,   111,    19,    20,   765,    52,   689,
     154,    64,    29,    64,    31,   560,   145,    29,   551,    31,
     114,   532,   567,    52,   982,   994,   560,   560,   191,  1006,
     788,   982,   741,  1010,   197,  1333,    64,   160,   131,   170,
     551,   167,    24,   554,   191,    52,   807,   191,   809,   560,
     116,   117,  1350,  1006,  1186,   169,   100,  1010,   591,   592,
      52,   741,   560,   170,    24,   560,   192,   600,   601,   567,
      87,   100,   193,  1074,  1361,   586,   143,   191,   172,    24,
     591,   592,    64,   154,    24,  1142,  1417,    52,   182,   600,
     601,    24,   625,   100,   191,   606,   191,   114,    22,   194,
     197,   191,   154,   197,    64,   172,   639,   197,   100,  1503,
     124,     4,  1578,   102,   625,  1578,   167,   167,   107,    64,
    1155,   110,    18,   560,    64,   191,    22,   638,   639,   666,
     567,    64,   182,   184,   185,   100,   187,   193,   191,   191,
     191,   192,   192,   654,   197,     3,   197,  1467,   193,     3,
     139,   165,   166,   590,   199,   172,   174,    99,   100,   101,
      18,   175,   599,   191,    22,   182,   193,   678,    22,   197,
     184,   185,   821,   187,   188,   193,   194,   191,   192,   193,
     174,   199,     3,   620,   695,   622,   623,   624,   130,   188,
     723,    38,   982,   192,   982,   172,   138,   425,   983,   193,
     428,    22,   713,   130,   191,   168,   169,   194,   140,   191,
     721,   142,   723,   200,   193,   197,    12,    13,    14,    15,
      16,    17,    18,   872,   843,  1556,  1557,   764,   191,     5,
     191,   191,  1563,   194,   112,   802,   885,   197,    29,   200,
      31,   192,   193,  1438,   851,  1576,   191,   124,  1217,  1643,
     193,   191,   197,   902,    21,    22,   767,   197,   191,   908,
     175,   176,   177,   178,   197,  1216,     6,     7,   183,   806,
     807,   193,   809,   922,   189,   916,    28,    29,    30,    31,
      68,    69,    70,  1006,    72,    37,   193,  1010,   933,   193,
     191,   824,   193,   916,    99,   100,    87,  1006,   175,  1431,
    1175,  1010,   193,  1175,    24,  1028,   193,   184,   185,    29,
     187,    31,   749,   824,   191,  1105,  1192,  1006,  1194,  1028,
     167,  1010,     4,   114,   858,   858,    29,    29,    31,    31,
    1010,   112,   104,   870,   106,    87,   193,   184,   185,  1028,
     187,    47,   875,  1105,   191,   192,   192,   858,   881,   882,
     883,   884,  1142,   886,  1312,   888,   889,  1023,  1316,   193,
    1083,   193,   114,   200,   875,   161,   162,   163,   164,   200,
     881,   882,   883,   884,  1083,   886,   921,   888,   889,     5,
    1142,   172,   172,   173,    87,    87,   923,   174,   921,   184,
     185,   182,   187,    23,  1083,    10,   191,   191,     4,   195,
     196,   912,   935,   936,     8,    11,   157,   158,   159,   193,
     921,   114,   114,    19,    20,    21,    25,   191,   192,     4,
     172,   858,   172,   173,   935,   936,   104,   105,    40,    41,
     184,   185,   193,   187,  1083,   191,   873,   191,   167,     5,
     194,  1399,  1592,  1593,   191,   197,   983,   146,  1399,   982,
     887,    62,    63,   191,  1221,   184,   185,   191,   187,   168,
     971,   104,   191,   192,   208,   209,   199,    73,  1005,   172,
     172,   982,  1121,   184,   185,   193,   187,  1010,   191,   182,
     191,  1458,   194,   194,   921,   326,   327,  1136,    94,   193,
      99,   100,   101,     5,  1005,  1006,   933,  1077,  1009,  1010,
     194,     5,  1225,  1378,  1153,     5,  1378,     3,  1266,   150,
     174,   174,  1023,   174,  1224,   194,  1225,  1028,   124,   202,
     174,   130,  1312,   132,   133,   134,  1316,   194,   137,   138,
       5,   140,   141,   191,   193,    22,  1225,     3,   191,   191,
       4,  1074,  1087,   149,   193,  1225,     3,    11,   194,  1060,
     110,     4,   194,   173,  1087,    19,    20,    21,  1224,   165,
     166,   184,   185,  1074,   187,   154,   191,     3,   191,   175,
    1107,   194,  1083,     3,  1085,   191,  1087,   199,   184,   185,
     193,   187,   188,     4,   194,   191,   192,   193,     4,   184,
     185,  1124,   187,   113,  1105,   201,   191,   102,  1278,     0,
       1,  1578,   107,   184,   185,   110,   187,   193,   134,  1399,
     191,  1399,   193,  1124,   191,   120,   194,   122,  1151,   182,
     125,   126,  1155,    24,   194,    19,    20,    28,    29,    30,
      31,  1142,   193,   182,   139,   194,    37,     5,   191,   144,
    1151,  1175,  1175,    39,  1155,   191,   182,   102,   182,   194,
    1087,     3,   107,   174,    18,   110,     5,     3,    65,    66,
     124,   193,    22,    64,  1175,   120,   193,   122,     3,   191,
     125,   126,   103,    80,  1185,  1186,   193,    84,    21,   193,
      21,  1218,   193,    90,   139,   149,    87,   194,     3,   144,
     154,   193,   175,   176,   177,   178,   184,   185,   193,   187,
     183,   165,   166,   191,   193,  1216,   189,     3,   191,     3,
     191,   175,   191,   114,  1225,   169,  1365,  1154,   146,   193,
     184,   185,   193,   187,   188,   139,     3,   191,   192,   193,
     124,  1268,   184,   185,   154,   187,  1273,   201,  1175,   191,
    1277,   193,   114,  1280,  1695,     3,   193,    30,     5,     5,
    1708,   158,  1289,     3,     5,   194,     3,   194,    31,  1270,
      38,   168,    29,     3,     3,   194,   103,   194,   194,   194,
       5,   172,     4,    21,   175,   176,   177,   178,   193,  1312,
     193,   175,   183,  1316,   194,   191,    20,     4,   189,     4,
     184,   185,   194,   187,   188,   194,   197,   191,   192,   191,
    1333,  1312,   191,   194,  1341,  1316,   194,   194,     5,     3,
     107,     3,   194,   110,  1251,     4,    26,  1350,    94,    73,
     194,     5,  1333,   120,   194,   122,     5,     4,   125,   126,
     108,     3,   199,   103,  1549,   113,   193,  1370,  1548,  1350,
     118,   119,   139,   121,  1378,  1378,   124,   144,   199,   127,
     128,   129,   191,   146,   194,  1565,   194,   193,   191,  1370,
    1397,   103,   194,     4,     3,     5,  1399,  1378,   275,     3,
       5,     4,     3,   280,   193,     5,   283,     5,     3,    22,
    1417,   194,  1548,   290,     3,    21,     4,     3,  1399,   167,
       3,     3,   194,     4,     3,     3,   194,   304,     4,  1565,
     307,   191,   194,   194,  1441,    52,   184,   185,   194,   187,
      57,   193,   191,   191,   192,    62,    63,   191,  1708,   194,
    1431,     3,   191,  1360,     3,  1458,   191,  1438,     4,     4,
      21,     5,   194,  1467,  1467,   194,   343,   344,     3,   346,
       4,  1378,   194,     3,    22,   194,     3,    94,   355,   194,
     357,   194,   193,   100,     3,     3,  1467,   104,  1503,     4,
       4,   368,     4,   370,   194,   194,   194,    11,   194,     4,
    1503,  1719,    49,   124,  1590,    19,    20,    21,   100,  1516,
    1513,   916,  1006,   390,  1006,  1269,    20,  1697,   412,  1278,
     718,  1524,  1503,  1222,   519,  1441,    11,  1266,   149,   421,
    1006,  1431,  1513,  1430,  1270,   405,   252,  1050,   788,  1557,
    1563,  1558,  1549,  1524,   165,   166,  1417,   790,  1555,  1556,
    1557,   721,  1010,   496,   175,   172,  1563,   980,  1006,  1060,
    1467,  1697,   802,   184,   185,  1286,   187,   188,  1575,  1576,
     191,   192,   193,  1695,  1216,  1578,  1564,   993,   460,   889,
     201,   554,  1589,  1023,   560,  1514,  1524,  1350,  1105,  1516,
     467,  1341,   560,  1525,   471,   448,  1503,   447,   266,   450,
     455,   764,   807,   452,     4,   482,   560,   809,   804,  1590,
     124,    11,   318,   565,  1380,   565,   386,  1121,   560,    19,
      20,   619,   499,   500,   327,   560,   503,   879,  1643,   560,
     601,  1365,   509,   621,   560,   149,   933,   936,  1370,   560,
    1643,     4,  1438,   381,   560,  1652,   908,   560,    11,   526,
     560,   165,   166,   902,   333,  1136,    19,    20,   535,    54,
    1667,   175,  1643,  1687,   468,   281,   711,   544,   695,   638,
     184,   185,   560,   187,   188,   552,  1186,   191,   192,   193,
     557,    26,  1185,  1189,  1393,  1085,    -1,   201,    -1,    -1,
      -1,   658,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   576,
      -1,   318,    -1,    -1,    -1,  1708,  1713,    -1,   325,  1716,
      -1,    -1,    -1,     4,  1695,    -1,    -1,    -1,    -1,    -1,
      11,    -1,  1729,    -1,   124,    -1,    -1,  1708,    19,    20,
      -1,    -1,    -1,    -1,    -1,    -1,  1643,  1644,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   374,    -1,    -1,
     637,   124,    -1,   380,   381,   165,   166,   384,     5,    -1,
      -1,   648,    -1,    -1,    11,   175,    -1,    -1,    -1,    -1,
     657,    -1,    19,    20,   184,   185,   149,   187,   188,    -1,
      -1,   191,   192,   193,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   201,   165,   166,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   175,    -1,    -1,    -1,     4,    -1,    -1,    -1,
      -1,   184,   185,    11,   187,   188,    -1,    -1,   191,   192,
     193,    19,    20,   124,    -1,    -1,    -1,    -1,   201,     5,
       6,     7,     8,     9,    10,   722,    12,    13,    14,    15,
      16,    17,    -1,    19,    20,    21,    22,    23,   149,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   745,    -1,
     747,    -1,    -1,    -1,   165,   166,    -1,    -1,   495,   496,
     497,    -1,    -1,    -1,   175,   762,    -1,   124,    -1,    -1,
      -1,   768,    -1,   184,   185,    -1,   187,   188,    -1,    -1,
     191,   192,   193,    -1,     4,    -1,    -1,    -1,    -1,    -1,
     201,    11,   149,    -1,    -1,    -1,    -1,    -1,    -1,    19,
      20,    -1,    -1,   800,    -1,    -1,    -1,    -1,   165,   166,
      -1,    -1,    -1,    -1,   551,   812,   124,     4,   175,    -1,
     817,    -1,   819,   560,    11,    -1,    -1,   184,   185,    -1,
     187,   188,    19,    20,   191,   192,   193,    -1,    -1,    11,
      -1,   149,    -1,    -1,   201,    -1,    -1,    19,    20,    -1,
     847,    -1,    24,    -1,   591,   592,    -1,   165,   166,    -1,
      -1,   858,    -1,   600,   601,    -1,    -1,   175,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   184,   185,    -1,   187,
     188,    -1,   619,   191,   192,   193,    -1,    -1,   625,    -1,
      -1,    -1,    64,   201,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   639,    -1,   124,    -1,   903,    -1,   194,   195,
     196,    -1,   909,    -1,    -1,    94,    -1,   654,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   104,    -1,    -1,     4,   149,
      -1,    -1,    -1,    -1,    -1,    11,    -1,   124,    -1,    -1,
      -1,    -1,    -1,    19,    20,   165,   166,   944,    -1,    -1,
     947,   948,   124,    -1,    -1,   175,    -1,    -1,   695,    -1,
      -1,    -1,   149,    -1,   184,   185,    -1,   187,   188,    -1,
      -1,   191,   192,   193,    -1,    -1,    -1,   149,   165,   166,
      -1,   201,    -1,    -1,    -1,    -1,   723,    -1,   175,    -1,
      -1,    -1,    -1,   165,   166,    -1,    -1,   184,   185,    -1,
     187,   188,    -1,   175,   191,   192,   193,    -1,    -1,    -1,
      -1,    -1,   184,   185,   201,   187,   188,    -1,    -1,   191,
     192,   193,    -1,    -1,    -1,   197,    -1,    -1,    -1,   201,
      -1,    -1,    -1,  1030,    -1,    -1,  1033,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1044,   124,  1046,
      -1,    -1,    -1,    -1,  1051,    -1,    -1,    -1,    -1,    -1,
      -1,  1058,  1059,   157,    -1,    -1,    -1,    -1,    -1,     4,
      -1,    -1,    -1,   149,   168,    -1,    11,    -1,    -1,    -1,
    1077,  1078,    -1,    -1,    19,    20,    -1,   824,    -1,   165,
     166,    -1,    -1,    -1,  1091,    -1,  1093,    -1,    -1,   175,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1104,   184,   185,
      -1,   187,   188,    -1,    -1,   191,   192,   193,    -1,    -1,
      -1,   858,    -1,    -1,    -1,   201,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   231,   875,   318,
      -1,    -1,    -1,   237,   881,   882,   883,   884,     5,   886,
      -1,   888,   889,    -1,    11,    -1,    -1,    -1,   252,    -1,
      -1,    -1,    19,    20,  1161,    22,    -1,    -1,  1165,    -1,
      -1,    -1,  1169,    -1,  1171,  1172,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   921,  1182,    -1,    -1,    -1,   124,
      -1,    -1,    -1,    19,    20,    -1,   933,    -1,   935,   936,
      -1,   380,   381,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   305,   306,  1210,   149,    -1,    -1,    -1,    -1,    -1,
    1217,    -1,    -1,  1220,   318,    -1,  1223,   321,    -1,    -1,
     165,   166,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     175,    -1,    -1,   337,   338,    -1,    -1,    -1,    -1,   184,
     185,  1248,   187,   188,    -1,    -1,   191,   192,   193,   353,
      -1,    -1,   356,    -1,    -1,    -1,   201,   124,    -1,  1006,
      -1,    -1,  1009,  1010,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   378,    -1,   380,   381,   382,    -1,
      -1,     5,   149,  1290,    -1,  1292,    -1,    11,   124,    -1,
      -1,    -1,    -1,    -1,    -1,    19,    20,    -1,   165,   166,
      -1,   405,    -1,    -1,    -1,    -1,   495,   496,   175,    -1,
      -1,    -1,    -1,   149,    -1,    -1,  1323,   184,   185,    -1,
     187,   188,  1329,    -1,   191,   192,   193,  1074,    -1,   165,
     166,    -1,    -1,    -1,   201,    -1,    -1,    -1,  1345,   175,
    1087,    -1,  1349,    -1,    -1,    -1,    -1,    -1,   184,   185,
      -1,   187,   188,    -1,    -1,   191,   192,   193,    -1,  1366,
      -1,  1368,   551,    -1,  1371,   201,  1373,  1374,    -1,  1376,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1124,    -1,    -1,
    1387,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,     5,    -1,   498,    -1,    -1,   501,    11,    -1,
     124,    -1,   591,   592,  1151,    -1,    19,    20,  1155,   513,
      -1,   600,   601,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   149,    -1,    -1,  1175,    -1,
     619,    -1,    -1,  1440,    -1,    -1,   625,    -1,  1185,  1186,
      -1,   165,   166,    -1,    11,    -1,    -1,    -1,    -1,    -1,
     639,   175,    19,    20,    -1,    -1,    -1,    -1,    -1,    -1,
     184,   185,    -1,   187,   188,   654,    -1,   191,   192,   193,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   201,    -1,    -1,
      -1,    -1,    49,    -1,    51,    52,    53,    54,    -1,    56,
      -1,    58,    59,    -1,    -1,    -1,    -1,  1504,    65,    -1,
      67,    -1,    69,  1510,    -1,    -1,   695,    -1,  1515,    -1,
      -1,   124,    -1,    -1,    -1,    -1,   620,    -1,    -1,  1526,
    1527,  1528,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1537,    -1,    -1,    -1,   723,    -1,   149,    -1,  1545,    -1,
    1547,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     654,    -1,   165,   166,    -1,    -1,    -1,   124,    -1,    -1,
      -1,    -1,   175,    -1,    -1,    -1,    -1,    -1,    -1,   673,
      -1,   184,   185,    -1,   187,   188,    -1,    -1,   191,   192,
     193,    -1,   149,    -1,    -1,    -1,  1333,    -1,   201,    -1,
      -1,   695,    -1,    -1,    -1,    -1,    -1,    -1,   165,   166,
      -1,    -1,   706,  1350,   708,    -1,    -1,    -1,   175,    -1,
      -1,    -1,    -1,  1620,    -1,    -1,    -1,   184,   185,    -1,
     187,   188,    -1,  1370,   191,   192,   193,    -1,    -1,    -1,
    1637,  1378,    -1,    -1,   201,   824,    -1,    -1,    -1,    -1,
      -1,  1648,    -1,    -1,   748,    -1,   750,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   758,    -1,    -1,   761,     6,     7,
       8,    -1,    10,    -1,    12,    13,    14,    15,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    -1,    -1,  1686,
      -1,    -1,  1689,  1430,  1431,    -1,   875,    -1,    -1,    -1,
      -1,    -1,   881,   882,   883,   884,    -1,   886,    -1,   888,
     889,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1458,    -1,    -1,    -1,    -1,    64,    -1,    -1,    -1,
    1467,    -1,     6,     7,     8,    -1,    10,    -1,    12,    13,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    -1,   846,    -1,   933,    -1,   935,   936,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1503,    -1,    -1,    -1,
      -1,    -1,   866,   867,   868,   869,  1513,    -1,    -1,    -1,
     874,    -1,    -1,    -1,    -1,   879,    -1,  1524,    -1,    11,
      64,    -1,    -1,    -1,    -1,    -1,    -1,    19,    20,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     914,    -1,    -1,   161,   162,   163,   164,    49,    -1,    -1,
    1009,    53,    54,    55,    56,    57,    58,    -1,    -1,   933,
     934,  1578,    -1,    65,    66,    67,   940,    -1,    -1,   943,
      -1,    -1,    -1,    -1,   192,    -1,    -1,   195,   196,   197,
      11,    -1,   200,    -1,    -1,    -1,    -1,    -1,    19,    20,
      21,    -1,    -1,    -1,    -1,    -1,    -1,   971,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   979,    -1,   161,   162,   163,
     164,    -1,    -1,    -1,    -1,  1074,    -1,    -1,    -1,    50,
      -1,    -1,   124,    -1,    -1,    -1,  1643,    58,    -1,    60,
      61,    62,    63,    -1,    65,  1009,    67,    -1,   192,   193,
      -1,   195,   196,   197,  1018,   199,    -1,   149,  1022,  1023,
      -1,  1025,  1026,    -1,  1028,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    11,   165,   166,  1124,    -1,    -1,    -1,    -1,
      19,    20,    21,   175,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   184,   185,    -1,   187,   188,    -1,    -1,   191,
     192,   193,  1151,   124,    -1,    -1,  1155,    -1,    -1,   201,
      -1,    -1,    -1,    -1,    -1,    -1,  1080,    -1,    -1,  1083,
      -1,    -1,  1086,    -1,    -1,    -1,    -1,    -1,   149,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1185,  1186,    -1,    -1,
      -1,    -1,    -1,    -1,   165,   166,   167,    -1,    -1,    -1,
      -1,    -1,  1116,    11,   175,    -1,    -1,    -1,    -1,    -1,
      -1,    19,    20,   184,   185,    -1,   187,   188,    -1,    -1,
     191,   192,   193,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     201,    -1,    -1,    -1,    -1,   124,    -1,    -1,    -1,    -1,
      -1,    49,    -1,    51,    -1,    53,    54,    -1,    56,  1163,
      58,    59,    -1,    -1,    -1,    -1,    -1,    65,    -1,    67,
     149,    69,  1176,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1185,  1186,    -1,    -1,    11,   165,   166,   167,    -1,
      -1,    -1,    -1,    19,    20,    -1,   175,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   184,   185,    -1,   187,   188,
      -1,  1215,   191,   192,   193,    11,    -1,    -1,    44,    -1,
      -1,    -1,   201,    19,    20,    -1,   124,    -1,    -1,    -1,
      -1,    -1,    58,    -1,    -1,    -1,    -1,    -1,    -1,    65,
      -1,    67,    68,    -1,  1333,    -1,  1250,    -1,    -1,    -1,
    1254,   149,    -1,    -1,    -1,    -1,    -1,  1261,    -1,    -1,
      -1,  1350,    58,    -1,    -1,    -1,    -1,   165,   166,    65,
      -1,    67,    68,    -1,    -1,    -1,    -1,   175,    -1,    -1,
      -1,  1370,    -1,   109,    -1,    -1,   184,   185,    -1,   187,
     188,  1295,    -1,   191,   192,   193,    -1,    -1,   124,    -1,
      -1,    -1,    -1,   201,    -1,  1309,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   149,    -1,    -1,    -1,    -1,   124,   155,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   165,
     166,  1430,  1431,  1347,    -1,    -1,    -1,    -1,    -1,   175,
      -1,    -1,    -1,   149,    -1,  1359,    -1,  1361,   184,   185,
      -1,   187,   188,    11,    -1,   191,   192,   193,    -1,   165,
     166,    19,    20,    -1,    -1,   201,  1380,    -1,  1382,   175,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   184,   185,
      -1,   187,   188,    -1,    -1,   191,   192,   193,    -1,    -1,
      -1,    -1,    -1,    11,    -1,   201,    -1,    -1,    -1,    -1,
      58,    19,    20,    -1,    -1,    -1,    -1,    65,    -1,    67,
      68,    -1,    -1,    -1,  1513,    -1,  1430,  1431,    -1,    -1,
      -1,    -1,    -1,  1437,  1438,  1524,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    57,
      58,    -1,    -1,  1457,    -1,    -1,    11,    65,    -1,    67,
      -1,    -1,    -1,    -1,    19,    20,    -1,    -1,    -1,    24,
      -1,    -1,    -1,    -1,    -1,    -1,   124,    -1,  1482,  1483,
    1484,  1485,  1486,  1487,  1488,  1489,  1490,  1491,  1492,  1493,
    1494,  1495,  1496,  1497,  1498,  1499,  1500,    -1,    -1,    -1,
      -1,   149,    -1,    11,    -1,    -1,    -1,    -1,    -1,    64,
      -1,    19,    20,    -1,    11,    -1,   124,   165,   166,    -1,
      -1,    -1,    19,    20,    21,    -1,    -1,   175,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   184,   185,    -1,   187,
     188,   149,    -1,   191,   192,   193,    -1,    -1,  1552,    -1,
      -1,    -1,    -1,   201,    -1,    -1,    -1,   165,   166,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   175,    -1,   124,
      -1,    -1,    -1,  1577,    -1,    -1,   184,   185,    -1,   187,
     188,    -1,    -1,   191,   192,   193,    94,    11,    -1,    -1,
    1594,    -1,    -1,   201,   149,    19,    20,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1611,  1612,    -1,
     165,   166,  1616,    -1,  1618,    -1,   124,    -1,    -1,    -1,
     175,    -1,    -1,    -1,    -1,    -1,    -1,   124,    -1,   184,
     185,    -1,   187,   188,    -1,    11,   191,   192,   193,    -1,
      -1,   149,   197,    19,    20,    21,   201,    -1,    -1,    -1,
      -1,    -1,   149,    -1,    -1,    -1,    -1,   165,   166,    -1,
      -1,    -1,    -1,    -1,    -1,    11,    -1,   175,   165,   166,
      -1,    -1,  1676,    19,    20,    21,   184,   185,   175,   187,
     188,    -1,    -1,   191,   192,   193,    -1,   184,   185,    -1,
     187,   188,    -1,   201,   191,   192,   193,    11,  1702,    -1,
     124,    -1,    -1,    -1,   201,    19,    20,    -1,    -1,    -1,
      -1,    -1,    -1,  1717,    -1,  1719,  1720,    -1,    -1,    -1,
      -1,    11,  1726,    -1,    -1,   149,    -1,  1731,    -1,    19,
      20,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   165,   166,    -1,    -1,    11,    -1,    -1,   124,    -1,
      -1,   175,    -1,    19,    20,    -1,    -1,    -1,    -1,    -1,
     184,   185,    -1,   187,   188,    -1,    -1,   191,   192,   193,
     194,    -1,    -1,   149,    -1,    11,    -1,   201,   124,    -1,
      -1,    -1,    -1,    19,    20,    -1,    -1,    -1,    -1,   165,
     166,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   175,
      -1,    -1,    -1,   149,    -1,    -1,    -1,    -1,   184,   185,
     124,   187,   188,    -1,    -1,   191,   192,   193,    -1,   165,
     166,    -1,    -1,    11,    -1,   201,    -1,    -1,    -1,   175,
      -1,    19,    20,    -1,   124,   149,    -1,    -1,   184,   185,
      -1,   187,   188,    -1,    -1,   191,   192,   193,    -1,    -1,
      -1,   165,   166,    -1,    11,   201,    -1,    -1,   124,   149,
      -1,   175,    19,    20,    -1,    -1,    -1,    -1,    -1,    -1,
     184,   185,    -1,   187,   188,   165,   166,   191,   192,   193,
      -1,    -1,    -1,   149,    -1,   175,    -1,   201,   124,    -1,
      -1,    -1,    -1,    -1,   184,   185,    -1,   187,   188,   165,
     166,   191,   192,   193,    -1,    -1,    -1,    11,    -1,   175,
      -1,   201,    -1,   149,    -1,    19,    20,    -1,   184,   185,
      -1,   187,   188,    -1,    -1,   191,   192,   193,    -1,   165,
     166,    -1,    -1,    -1,    -1,   201,   124,    -1,    -1,   175,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   184,   185,
      -1,   187,   188,    -1,    -1,   191,   192,   193,    -1,    -1,
      -1,   149,    -1,    -1,    -1,   201,    -1,   124,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   165,   166,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   175,    -1,    -1,
      -1,    25,   149,    -1,    -1,    -1,   184,   185,    32,   187,
     188,    -1,    -1,   191,   192,   193,    -1,    -1,   165,   166,
      -1,    -1,    -1,   201,    -1,    -1,    -1,    -1,   175,    -1,
     124,    -1,    -1,    -1,    -1,    -1,    -1,   184,   185,    -1,
     187,   188,    -1,    -1,   191,   192,   193,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   201,   149,    -1,    -1,    82,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   165,   166,    -1,    -1,    99,   100,    -1,    -1,    -1,
      -1,   175,    -1,    -1,    -1,    -1,    -1,   111,    -1,    -1,
     184,   185,    -1,   187,   188,    -1,    -1,   191,   192,   193,
      -1,    -1,    -1,    -1,    -1,    -1,   130,   201,   132,    32,
     134,   135,    -1,    -1,    -1,    38,   140,    -1,   142,    -1,
      -1,    -1,    -1,   147,    47,    48,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   167,    -1,    -1,   170,   171,   172,    -1,
      -1,   175,   176,   177,   178,    78,    -1,    -1,    -1,   183,
     184,   185,   186,   187,    -1,   189,    -1,   191,   192,    -1,
      93,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   104,   105,   106,    -1,   108,    -1,    32,   111,    -1,
     113,    -1,   115,    38,    -1,   118,   119,    -1,   121,    -1,
      -1,   124,    -1,    48,   127,   128,   129,    -1,   131,    -1,
      32,    -1,    -1,    -1,    -1,    -1,    38,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    48,    -1,    -1,    -1,
     153,    -1,    -1,    78,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   167,   168,   169,   170,    93,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    78,    -1,    -1,    -1,
      -1,   184,   185,   108,   187,    -1,   111,   112,   191,   192,
     115,    93,    -1,   118,   119,    -1,   121,    -1,    -1,   124,
      -1,    -1,   127,   128,   129,    -1,   108,    -1,    -1,   111,
     112,    -1,    -1,   115,    38,    -1,   118,   119,    -1,   121,
      -1,    -1,   124,    -1,    48,   127,   128,   129,   153,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    38,    -1,    -1,
      -1,    -1,   167,    -1,    -1,   170,    -1,    -1,    -1,    -1,
      -1,   153,    -1,    -1,    78,    -1,    -1,   182,    -1,   184,
     185,    -1,   187,    -1,    -1,   167,   191,   192,   170,    93,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    78,    -1,    -1,
     182,    -1,   184,   185,   108,   187,    -1,   111,   112,   191,
     192,   115,    93,    -1,   118,   119,    -1,   121,    -1,    -1,
     124,    -1,   103,   127,   128,   129,    -1,   108,    -1,    -1,
      -1,    -1,    -1,    -1,   115,    38,    -1,   118,   119,    -1,
     121,    -1,    -1,   124,    -1,    -1,   127,   128,   129,   153,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   167,    -1,    -1,   170,    -1,    -1,    -1,
      -1,    -1,   153,    -1,    -1,    78,    -1,    -1,   182,    -1,
     184,   185,    -1,   187,    -1,    -1,   167,   191,   192,    -1,
      93,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   184,   185,   108,   187,    -1,    -1,    -1,
     191,   192,   115,    -1,    -1,   118,   119,    -1,   121,    -1,
      -1,   124,    -1,    -1,   127,   128,   129,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     153,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   167,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   184,   185,    -1,   187,    -1,    -1,    -1,   191,   192
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_uint16 yystos[] =
{
       0,   204,     0,     1,    24,    28,    30,    37,    64,    87,
     114,   172,   197,   205,   206,   207,   208,   210,   211,   264,
     265,   638,   641,   658,   660,   694,   695,   696,   697,   698,
     708,   709,   184,   212,   191,   191,   175,   176,   177,   178,
     183,   189,   191,   231,   266,   315,   209,    24,   197,    24,
     266,   236,   237,   406,   651,   653,   659,   659,    29,    31,
     114,   696,   659,   659,   209,   661,   642,   268,   267,   272,
     269,   271,   270,   193,   194,   194,   209,   197,   208,    25,
      32,    82,    99,   100,   111,   132,   134,   135,   147,   167,
     170,   171,   184,   185,   186,   187,   191,   192,   231,   238,
     239,   240,   251,   259,   260,   288,   290,   292,   331,   332,
     347,   365,   370,   387,   388,   393,   398,   399,   406,   407,
     413,   417,   423,   434,   436,   445,   447,   449,   452,   453,
     454,   515,   516,   541,   542,   543,   545,   551,   552,   555,
     561,   566,   589,   612,   637,   675,   678,   720,   160,   653,
     259,   260,   640,   647,   721,    38,    48,    78,    93,   108,
     115,   118,   119,   121,   124,   127,   128,   129,   153,   191,
     241,   242,   252,   253,   259,   459,   476,   515,   516,   526,
     527,   531,   541,   560,   561,   570,   571,   574,   586,   587,
     588,   589,   590,   595,   602,   606,   609,   612,   625,   628,
     631,   683,   688,   720,   191,   711,   191,   701,   241,   241,
     206,   206,    21,   193,   273,   274,   273,   273,   273,    21,
     193,   284,   285,   286,   273,     4,    11,    19,    20,    21,
     124,   149,   165,   166,   175,   188,   192,   193,   201,   231,
     254,   257,   263,   275,   276,   279,   280,   288,   289,   316,
     317,   322,   324,   434,   483,   484,   485,   486,   487,   490,
     491,   493,   495,   496,   497,   498,   499,   503,   506,   508,
     681,   690,   394,   206,   193,   418,   419,     3,   206,   213,
     546,   154,   191,   676,   677,   154,   401,    22,   424,   206,
     546,     3,   293,   294,   193,   374,   375,   377,   378,   434,
     436,     4,   193,   240,   102,   107,   110,   139,   191,   436,
     193,   291,   264,   367,   401,   130,   140,   142,   146,   193,
     517,     5,   193,   448,   539,   540,   548,   548,   539,   652,
     112,   639,   648,   649,   650,   697,   708,   193,   193,   191,
     231,   235,   503,   559,   191,   206,   260,   191,   197,   723,
     193,   193,   634,   193,   508,   593,   193,   593,   193,   193,
     206,   503,   505,   506,   511,   512,   513,   514,   591,     4,
     112,   246,   247,   243,   244,   245,   252,   637,   110,   120,
     122,   125,   126,   144,   528,    47,   572,   573,   577,   193,
     684,   193,   715,   193,   246,   246,   192,    94,   513,   193,
     276,   287,    73,    94,   263,   193,   323,   326,   327,   328,
     503,   200,   200,   231,   275,   278,   279,   281,   490,   503,
       5,     3,   194,   323,    23,   488,    21,    22,   489,   275,
     276,   490,   276,   486,    12,    13,    14,    15,    16,    17,
      18,   161,   162,   163,   164,   195,   196,   492,   494,    10,
     500,     8,   501,     6,     7,   502,   493,   193,   193,   420,
       3,   206,   116,   117,   231,   547,   206,   676,     3,   206,
     191,   679,   680,   191,    18,    22,   426,   427,   206,   295,
     296,   347,   191,   154,   191,   378,   379,   380,   447,   451,
     452,   453,     3,   372,   373,   428,     3,    22,   110,   111,
     170,   450,   206,   508,    21,   192,   260,   503,   505,   616,
     503,   206,     4,     5,   446,   507,   508,   264,   300,   301,
     302,   303,     3,   334,   335,   336,   366,   191,   368,   369,
     674,   401,   401,   143,   264,   409,   410,   191,   434,   435,
     436,   503,   523,   524,   525,   474,   508,   518,   519,   520,
     521,   146,   503,   446,   199,   245,   168,   169,   191,   261,
     262,   544,   549,   550,   553,   556,   557,   262,   549,   104,
     562,   563,   567,     3,   154,   656,   722,   406,   643,   650,
     503,   534,   503,   206,   206,   206,   460,   596,   193,    58,
      65,    67,    68,   503,   593,   629,   630,   206,    58,    65,
      67,    68,   593,   626,   627,   206,   477,   231,   726,   206,
      48,   206,   662,   663,   245,   503,   193,    21,    50,    58,
      60,    61,    62,    63,    65,    67,   191,   436,   440,   592,
     593,   594,   613,   614,   615,   616,   613,   616,   689,   260,
     515,   526,   527,   529,   532,   533,   136,   193,   581,   131,
     573,   578,   539,   194,   686,   206,   716,   710,   699,   406,
     705,   406,   712,     5,   194,   263,     5,     5,     3,   194,
     326,   503,   150,     3,   191,   192,   277,   277,   174,   279,
     194,   263,   317,   202,   325,   276,   485,   485,   486,   487,
     491,   495,   496,   497,   194,   682,   191,   396,   397,   231,
     422,   437,   445,   447,   453,   419,   193,   538,     5,   206,
     677,     3,   206,    22,   191,   191,   431,   432,     3,   206,
     193,     3,   371,   428,   374,   377,   231,   275,   276,   278,
     279,   288,   314,   315,   318,   346,   376,   382,   385,   386,
     434,   490,   681,   503,   206,   546,   206,   546,     4,    21,
     154,   231,   455,   456,   458,   503,   506,   206,     3,   206,
     194,   110,   503,   194,     4,     3,   154,   304,   173,   299,
     302,    25,   101,   130,   132,   133,   134,   137,   138,   140,
     141,   337,   347,   191,   340,   341,   343,   154,     3,   206,
       3,   191,   390,   231,   342,   402,   403,   404,   405,   427,
     408,   193,     3,   206,   199,   206,     4,     3,   194,     3,
     194,   436,   522,   206,   194,   449,   206,   546,     4,   113,
      38,   127,   129,   259,   260,   459,   476,   515,   526,   554,
     570,   587,   589,   595,   602,   606,   609,   612,   625,   628,
     631,   683,   548,   544,   557,   259,   193,   105,   563,   564,
     565,   568,   539,   134,   667,   191,   206,   182,   194,   194,
     437,   447,   452,   467,   468,   469,    49,    53,    54,    55,
      56,    57,    58,    65,    66,    67,   593,   598,   599,    49,
      51,    52,    53,    54,    56,    58,    59,    65,    67,    69,
     593,   635,   636,   593,   260,   436,   443,   444,   436,   441,
     442,   601,     3,   194,   593,   260,   443,   601,     3,   194,
     469,   479,     3,   194,   193,   248,   249,   250,   697,   708,
     182,   194,   603,     5,    21,   592,   616,   191,   614,   260,
     260,   260,   443,     3,   194,   194,     3,   206,   231,   690,
      39,   530,   535,   582,   191,   206,   191,   579,   685,   231,
     254,   436,   503,   691,   692,   693,    21,   191,   702,   717,
     718,   719,   206,   717,   182,   182,   513,   194,   263,   513,
     513,     3,   327,   231,   258,   275,   278,   282,   691,     5,
       3,   194,     3,   193,   503,   503,   680,   231,   415,   416,
     437,    22,   193,     3,   429,   296,   191,   297,   298,   231,
     380,   381,   547,   206,   374,   193,    21,   383,   383,   193,
       3,    22,   383,   276,   279,   194,   206,   206,     4,   503,
     260,   503,     5,     3,   194,     4,   154,   457,   193,   503,
     619,   620,   621,   103,   503,   206,   507,   101,   130,   138,
     305,   306,   347,   231,   308,   309,   191,   206,   193,   193,
       3,   333,   193,   348,   337,   369,   193,     3,   389,   400,
       3,   206,   191,   411,   412,   410,   191,   523,   475,   508,
     474,   520,   474,   521,   146,   206,   206,   169,   191,   206,
     193,   191,   192,   193,   724,   193,   110,   262,   558,   548,
     503,   191,   206,   106,   563,   569,   539,   154,     3,   668,
     114,   645,   206,   533,   576,     3,   462,   193,   470,   504,
     505,   600,   504,   504,   504,   508,     5,   593,   260,   504,
     443,     3,   194,   600,     5,   443,   441,   441,   443,   593,
     436,   438,   439,   260,   443,   438,     3,   194,   630,   206,
     627,   206,     3,   480,   231,   503,   250,    30,   665,   253,
     259,   260,    57,    58,    65,    67,   593,   604,   605,   508,
     615,   610,   619,   193,   436,   607,   617,   618,   621,   617,
     534,    40,    41,   536,   537,   528,     4,   503,   583,   584,
     585,   206,   580,   206,   206,     5,     3,   194,   194,     3,
     194,    31,   707,    29,   714,   194,     3,   194,     3,   194,
     194,   231,   329,   330,   547,   194,   194,   503,   510,   397,
     395,   421,   422,   446,   194,     3,     3,   414,   433,   432,
     425,   428,     3,   194,     5,   193,   231,   255,   256,   257,
     258,   275,   278,   314,   318,   346,   384,   385,   194,   254,
     319,   320,   321,   436,   503,   522,   525,   382,   103,   503,
       4,    21,   503,   456,     4,   503,   503,   503,   620,   622,
     623,     3,   206,   206,   194,   193,     3,   154,   193,   310,
       3,   206,   206,   338,   339,   341,   206,   349,    21,   283,
     391,   191,   206,   206,   404,    20,     3,   194,     4,     4,
     522,   206,   546,   206,   534,    11,    19,    20,    44,   109,
     155,   165,   166,   184,   185,   187,   188,   192,   214,   215,
     219,   221,   224,   226,   231,   232,   233,   503,   726,   503,
     253,   194,   206,   191,   206,    27,   191,   669,   670,   654,
     191,   646,   206,    97,   145,   463,   464,   468,   194,   471,
     472,   473,   474,   504,   599,   597,   441,   194,   636,   632,
      97,   145,   469,   481,   482,   194,   194,   191,   666,   110,
       3,     5,   593,   260,   443,     3,   194,   206,   611,   206,
       3,   608,   206,   194,   191,   206,   191,   206,   528,   585,
       3,   194,     4,   206,   231,   693,   692,   687,   719,    26,
     703,   704,   646,   646,    94,    73,   194,     5,   206,     3,
     194,   194,   503,   416,   206,   428,     4,   350,   351,   352,
     353,   354,   355,   356,   357,   358,   359,   360,   361,   362,
     363,   508,   509,   206,   427,   298,   206,   513,   231,   275,
       5,     3,   194,   206,   503,   260,   503,     4,     3,   620,
     103,   307,   306,     4,   311,   351,   353,   357,   283,   309,
     350,   157,   158,   159,   364,   350,   287,     5,   146,   344,
     345,   350,   193,   191,   412,   206,   206,   194,   503,   194,
     194,   503,   199,   445,   447,   193,   193,   194,   191,   184,
     234,   445,     5,     6,     7,     8,     9,    10,    12,    13,
      14,    15,    16,    17,    19,    20,    21,    22,    23,   195,
     196,   216,   194,   194,   103,   206,     4,   146,     3,   206,
     644,   441,   466,     5,     3,   461,     3,   194,   475,     4,
     206,   619,   206,   466,     5,     3,   478,   575,   664,   503,
     260,   504,   605,   206,   206,   618,   206,   191,   206,   206,
     206,   584,   585,   206,   193,   700,   713,   713,     5,     5,
     508,   422,     3,    22,   194,     3,     4,     3,     3,   353,
     361,   509,    21,     3,   430,     3,   321,   320,   503,   547,
     623,   624,   206,   311,   194,     3,     4,     5,   146,   312,
     313,   194,   194,   194,   510,   314,   346,   434,   194,   392,
     725,   222,   223,   225,     5,   218,   503,   503,   503,   503,
     503,   503,   503,   503,   503,   503,   503,   503,   503,   503,
     503,     5,    22,   217,   503,   503,     5,   503,     5,   503,
     191,   206,   191,   657,   670,   671,   672,   673,   674,   191,
     670,   206,   443,   465,   464,   206,   472,   633,   465,   482,
     206,   206,   206,   194,     3,   206,   191,   206,   706,   206,
     513,   263,     3,   503,   191,   352,   354,   509,     4,   353,
     356,   358,     4,    21,   363,   431,   513,     5,   194,   194,
     510,   314,   346,   350,   219,   231,   220,   227,   227,   227,
     503,   503,   503,   503,   503,   206,   655,     3,   206,   260,
     194,   206,   194,   194,   508,    22,     4,     3,   194,   508,
     194,   193,     4,   228,   229,   230,   503,   194,   194,   194,
     206,   672,   206,     3,   415,   513,     3,     4,   503,     3,
       4,   445,   508,   194,   508,   503,     4,   229,   503,     3,
     503,     4,   508,   503
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint16 yyr1[] =
{
       0,   203,   204,   204,   205,   205,   205,   206,   206,   206,
     206,   206,   207,   207,   207,   208,   208,   208,   209,   210,
     210,   210,   211,   211,   212,   213,   213,   214,   214,   214,
     214,   214,   215,   215,   216,   216,   216,   216,   216,   216,
     216,   216,   216,   216,   216,   216,   216,   216,   216,   216,
     216,   216,   216,   216,   216,   217,   217,   217,   217,   218,
     218,   219,   219,   219,   220,   221,   221,   222,   221,   223,
     221,   224,   225,   224,   226,   227,   227,   228,   228,   229,
     229,   230,   230,   230,   230,   230,   230,   230,   231,   232,
     232,   232,   232,   232,   232,   232,   232,   233,   233,   233,
     233,   234,   234,   235,   235,   236,   237,   237,   238,   238,
     239,   239,   240,   240,   240,   240,   240,   240,   240,   240,
     240,   240,   240,   240,   240,   241,   241,   242,   243,   243,
     244,   244,   245,   245,   246,   246,   247,   248,   248,   249,
     249,   250,   250,   251,   251,   251,   251,   251,   251,   251,
     251,   251,   252,   252,   252,   252,   252,   253,   253,   253,
     253,   253,   253,   253,   253,   253,   253,   253,   253,   253,
     253,   253,   253,   253,   253,   253,   253,   253,   253,   253,
     253,   253,   254,   255,   256,   256,   257,   257,   257,   257,
     257,   258,   259,   259,   260,   260,   261,   261,   262,   263,
     263,   263,   265,   264,   264,   264,   267,   266,   268,   266,
     269,   266,   270,   266,   271,   266,   272,   266,   273,   273,
     274,   274,   274,   275,   275,   276,   276,   277,   277,   278,
     278,   279,   279,   280,   281,   281,   281,   282,   282,   282,
     283,   283,   284,   284,   285,   285,   285,   285,   285,   286,
     286,   286,   286,   287,   287,   288,   288,   288,   289,   289,
     291,   290,   292,   292,   293,   293,   294,   294,   295,   295,
     296,   297,   297,   298,   299,   299,   300,   300,   301,   301,
     302,   303,   304,   304,   304,   305,   305,   306,   306,   307,
     306,   306,   308,   308,   309,   310,   310,   311,   311,   312,
     312,   313,   313,   313,   314,   315,   315,   316,   316,   317,
     317,   318,   318,   319,   319,   320,   320,   321,   321,   321,
     322,   322,   323,   324,   325,   326,   326,   327,   327,   328,
     329,   329,   330,   332,   333,   331,   334,   334,   335,   335,
     336,   336,   337,   337,   338,   337,   337,   339,   337,   337,
     337,   337,   337,   337,   337,   340,   340,   341,   342,   343,
     344,   344,   345,   345,   345,   346,   347,   347,   348,   349,
     348,   350,   350,   350,   350,   350,   351,   351,   352,   352,
     353,   354,   355,   355,   356,   356,   357,   357,   358,   359,
     360,   360,   361,   361,   362,   362,   363,   364,   364,   364,
     366,   365,   367,   367,   368,   368,   369,   369,   371,   370,
     372,   372,   373,   373,   374,   375,   375,   376,   376,   377,
     377,   378,   378,   379,   379,   380,   380,   380,   381,   382,
     382,   382,   382,   382,   382,   382,   382,   383,   383,   384,
     384,   384,   384,   384,   384,   384,   385,   386,   388,   389,
     387,   391,   390,   392,   390,   394,   395,   393,   396,   396,
     397,   399,   400,   398,   401,   401,   402,   402,   403,   403,
     404,   404,   404,   405,   406,   407,   408,   407,   409,   409,
     410,   411,   411,   412,   412,   413,   414,   414,   415,   415,
     416,   417,   418,   418,   420,   419,   421,   421,   422,   422,
     422,   424,   425,   423,   426,   426,   427,   427,   428,   428,
     429,   430,   429,   431,   431,   432,   433,   432,   434,   434,
     434,   434,   435,   436,   437,   438,   439,   440,   441,   442,
     443,   444,   445,   445,   445,   446,   447,   448,   448,   449,
     450,   449,   451,   452,   453,   454,   454,   455,   455,   455,
     456,   456,   456,   456,   456,   456,   456,   456,   456,   456,
     457,   457,   457,   457,   457,   457,   458,   460,   461,   459,
     462,   462,   463,   463,   464,   464,   465,   466,   467,   467,
     468,   469,   469,   470,   470,   471,   471,   472,   473,   473,
     474,   475,   477,   478,   476,   479,   479,   480,   480,   481,
     481,   482,   482,   483,   483,   483,   483,   483,   484,   485,
     485,   486,   486,   487,   487,   487,   487,   487,   488,   489,
     489,   490,   490,   491,   491,   492,   493,   493,   494,   494,
     494,   494,   494,   494,   494,   494,   494,   494,   494,   494,
     495,   495,   496,   496,   497,   497,   498,   498,   499,   500,
     501,   502,   502,   503,   504,   505,   506,   507,   507,   508,
     509,   510,   511,   512,   513,   514,   515,   515,   516,   516,
     516,   517,   517,   518,   518,   519,   519,   520,   521,   522,
     523,   524,   525,   525,   525,   526,   527,   528,   528,   529,
     529,   530,   530,   531,   532,   532,   532,   533,   534,   535,
     535,   536,   536,   537,   537,   538,   539,   540,   540,   541,
     541,   541,   542,   542,   543,   543,   543,   543,   544,   544,
     544,   544,   545,   545,   545,   545,   546,   546,   546,   546,
     547,   548,   549,   549,   550,   550,   551,   551,   552,   553,
     554,   554,   554,   554,   554,   554,   554,   554,   554,   554,
     554,   554,   554,   554,   554,   554,   554,   555,   555,   556,
     556,   557,   558,   559,   559,   560,   561,   562,   562,   562,
     563,   564,   564,   564,   565,   566,   566,   566,   567,   567,
     568,   568,   569,   569,   570,   571,   572,   572,   572,   573,
     575,   574,   576,   574,   577,   577,   579,   578,   580,   578,
     582,   581,   581,   583,   583,   584,   584,   584,   584,   585,
     586,   586,   587,   588,   589,   590,   590,   591,   591,   592,
     592,   592,   593,   594,   596,   597,   595,   598,   598,   599,
     599,   599,   599,   599,   599,   599,   599,   599,   599,   599,
     600,   601,   603,   602,   604,   604,   605,   605,   605,   605,
     605,   607,   606,   608,   606,   606,   606,   610,   609,   611,
     609,   612,   612,   613,   613,   614,   615,   615,   615,   615,
     615,   615,   615,   615,   615,   615,   615,   616,   616,   616,
     617,   617,   618,   618,   619,   619,   620,   620,   621,   622,
     622,   623,   624,   624,   625,   625,   626,   626,   627,   627,
     627,   627,   627,   628,   628,   629,   629,   630,   630,   630,
     630,   630,   632,   631,   633,   631,   634,   635,   635,   636,
     636,   636,   636,   636,   636,   636,   636,   636,   636,   636,
     637,   639,   638,   640,   640,   642,   641,   644,   643,   645,
     645,   646,   646,   647,   648,   648,   649,   649,   650,   650,
     651,   651,   652,   654,   653,   655,   653,   656,   656,   656,
     657,   657,   658,   659,   659,   241,   241,   661,   660,   663,
     664,   662,   665,   665,   666,   666,   667,   668,   668,   669,
     669,   670,   671,   671,   672,   672,   672,   673,   674,   675,
     675,   676,   676,   677,   678,   679,   679,   680,   681,   682,
     681,   684,   683,   685,   683,   686,   687,   683,   689,   688,
     690,   690,   690,   691,   691,   692,   692,   693,   693,   693,
     694,   694,   695,   695,   696,   696,   696,   697,   699,   700,
     698,   701,   702,   703,   703,   704,   706,   705,   707,   707,
     708,   710,   709,   711,   712,   713,   714,   714,   715,   716,
     715,   717,   717,   718,   718,   719,   719,   720,   720,   722,
     721,   723,   723,   724,   724,   724,   724,   724,   725,   726,
     726
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     0,     2,     1,     1,     1,     2,     1,     1,
       3,     2,     1,     3,     3,     1,     3,     1,     0,     1,
       1,     1,     1,     1,     1,     0,     1,     1,     1,     2,
       2,     2,     1,     1,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     3,     3,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     0,     1,     2,     2,     2,
       1,     1,     1,     1,     0,     1,     2,     0,     5,     0,
       6,     1,     0,     5,     4,     1,     2,     1,     3,     1,
       1,     3,     5,     4,     3,     2,     2,     1,     1,     1,
       1,     1,     1,     1,     1,     2,     2,     1,     2,     1,
       1,     0,     1,     0,     1,     2,     0,     1,     0,     1,
       1,     2,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     0,     1,     2,     0,     1,
       1,     2,     1,     1,     0,     1,     3,     0,     1,     1,
       2,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     2,     4,     2,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     0,     1,     1,     1,     0,     1,     1,     1,
       1,     1,     0,     2,     3,     3,     0,     3,     0,     3,
       0,     3,     0,     3,     0,     3,     0,     3,     0,     1,
       3,     5,     2,     1,     2,     1,     3,     1,     1,     1,
       2,     1,     3,     5,     1,     1,     1,     1,     1,     1,
       0,     2,     0,     1,     1,     9,     5,     5,     9,     3,
       5,     2,     3,     3,     1,     1,     1,     1,     1,     1,
       0,     4,     4,     7,     0,     2,     0,     2,     1,     3,
       1,     1,     3,     1,     2,     3,     0,     1,     1,     2,
       1,     4,     0,     1,     3,     1,     3,     1,     1,     0,
       5,     1,     1,     3,     4,     0,     3,     1,     1,     0,
       1,     2,     2,     2,     1,     1,     4,     1,     3,     1,
       3,     3,     4,     1,     3,     1,     3,     1,     1,     1,
       3,     3,     1,     1,     1,     1,     3,     1,     1,     5,
       5,     7,     1,     0,     0,     6,     0,     2,     0,     1,
       2,     3,     1,     1,     0,     5,     1,     0,     5,     1,
       1,     1,     1,     1,     1,     1,     3,     4,     1,     1,
       0,     1,     2,     2,     2,     1,     1,     1,     0,     0,
       4,     1,     1,     1,     1,     1,     1,     3,     3,     1,
       1,     1,     1,     3,     1,     2,     1,     3,     1,     3,
       0,     2,     0,     2,     1,     3,     2,     1,     1,     1,
       0,     4,     0,     2,     1,     3,     1,     1,     0,     5,
       0,     1,     2,     3,     4,     1,     3,     1,     3,     1,
       1,     9,    11,     1,     3,     1,     1,     1,     1,     2,
       2,     2,     1,     1,     1,     1,     1,     0,     2,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     0,     0,
       6,     0,     5,     0,     7,     0,     0,     7,     1,     3,
       3,     0,     0,     6,     0,     1,     0,     1,     1,     3,
       1,     1,     1,     1,     0,     4,     0,     5,     1,     3,
       4,     1,     3,     1,     3,     7,     0,     6,     1,     3,
       1,     3,     1,     3,     0,     6,     1,     3,     1,     1,
       1,     0,     0,     7,     0,     1,     1,     3,     0,     1,
       0,     0,     5,     1,     3,     1,     0,     5,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     4,     4,     3,     2,     0,     3,     1,
       0,     5,     1,     1,     1,     1,     4,     0,     1,     3,
       2,     1,     2,     3,     4,     2,     1,     3,     4,     2,
       1,     2,     3,     4,     2,     0,     1,     0,     0,     8,
       0,     2,     1,     3,     2,     3,     1,     1,     1,     3,
       2,     1,     1,     0,     3,     1,     3,     2,     0,     2,
       1,     1,     0,     0,     8,     1,     3,     0,     2,     1,
       3,     2,     3,     1,     1,     1,     1,     3,     1,     1,
       3,     1,     3,     1,     2,     3,     1,     2,     1,     1,
       1,     1,     1,     1,     3,     1,     1,     3,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     2,     1,     3,     1,     3,     1,     3,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     0,     1,     1,
       1,     1,     1,     1,     1,     1,     4,     5,     5,     7,
       4,     0,     3,     1,     3,     1,     3,     2,     3,     1,
       1,     3,     1,     1,     1,     5,     5,     0,     2,     0,
       3,     0,     3,     5,     1,     1,     1,     1,     1,     4,
       5,     2,     3,     2,     3,     0,     1,     0,     2,     1,
       1,     1,     3,     3,     4,     2,     5,     3,     4,     2,
       5,     3,     4,     2,     5,     3,     6,     8,     5,     3,
       1,     1,     1,     2,     3,     4,     1,     1,     3,     2,
       1,     1,     1,     1,     1,     1,     1,     2,     4,     1,
       1,     1,     1,     1,     1,     1,     1,     4,     3,     2,
       3,     3,     2,     0,     1,     3,     5,     0,     1,     2,
       2,     0,     1,     2,     2,     7,     8,     6,     6,     7,
       2,     3,     2,     3,     5,     3,     0,     1,     2,     2,
       0,     8,     0,     6,     3,     4,     0,     3,     0,     4,
       0,     4,     1,     1,     3,     1,     2,     2,     3,     1,
       2,     3,     3,    10,     3,     2,     3,     1,     1,     1,
       1,     1,     1,     1,     0,     0,     7,     1,     3,     1,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     3,
       1,     1,     0,     7,     1,     3,     1,     2,     2,     2,
       3,     0,     6,     0,     7,     4,     6,     0,     6,     0,
       7,     4,     6,     1,     3,     1,     1,     2,     1,     1,
       2,     2,     2,     2,     2,     2,     3,     1,     1,     1,
       1,     3,     1,     1,     1,     3,     1,     1,     5,     1,
       3,     1,     5,     7,     3,     5,     1,     3,     1,     2,
       2,     2,     2,     3,     5,     1,     3,     1,     2,     2,
       2,     2,     0,     7,     0,     9,     0,     1,     3,     1,
       2,     2,     2,     2,     2,     2,     2,     3,     2,     2,
       2,     0,     5,     0,     1,     0,     4,     0,     6,     0,
       1,     0,     1,     2,     0,     1,     1,     2,     1,     1,
       1,     2,     0,     0,     8,     0,    11,     0,     1,     3,
       0,     1,     5,     0,     1,     0,     1,     0,     4,     0,
       0,     6,     0,     1,     0,     1,     1,     0,     2,     1,
       3,     3,     1,     3,     1,     1,     1,     1,     1,     3,
       4,     1,     3,     1,     4,     1,     3,     1,     3,     0,
       5,     0,     3,     0,     5,     0,     0,     7,     0,     4,
       1,     1,     1,     1,     3,     1,     3,     1,     1,     1,
       0,     1,     1,     2,     1,     1,     1,     5,     0,     0,
      10,     1,     1,     0,     1,     4,     0,     7,     0,     1,
       5,     0,     6,     1,     6,     0,     0,     1,     0,     0,
       4,     0,     1,     1,     3,     1,     1,     3,     4,     0,
       4,     1,     1,     3,     3,     1,     3,     1,     0,     1,
       3
};


#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)
#define YYEMPTY         (-2)
#define YYEOF           0

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                  \
do                                                              \
  if (yychar == YYEMPTY)                                        \
    {                                                           \
      yychar = (Token);                                         \
      yylval = (Value);                                         \
      YYPOPSTACK (yylen);                                       \
      yystate = *yyssp;                                         \
      goto yybackup;                                            \
    }                                                           \
  else                                                          \
    {                                                           \
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;                                                  \
    }                                                           \
while (0)

/* Error token number */
#define YYTERROR        1
#define YYERRCODE       256



/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)

/* This macro is provided for backward compatibility. */
#ifndef YY_LOCATION_PRINT
# define YY_LOCATION_PRINT(File, Loc) ((void) 0)
#endif


# define YY_SYMBOL_PRINT(Title, Type, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Type, Value); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*----------------------------------------.
| Print this symbol's value on YYOUTPUT.  |
`----------------------------------------*/

static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
{
  FILE *yyo = yyoutput;
  YYUSE (yyo);
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# endif
  YYUSE (yytype);
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
{
  YYFPRINTF (yyoutput, "%s %s (",
             yytype < YYNTOKENS ? "token" : "nterm", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yytype_int16 *yybottom, yytype_int16 *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yytype_int16 *yyssp, YYSTYPE *yyvsp, int yyrule)
{
  unsigned long int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       yystos[yyssp[yyi + 1 - yynrhs]],
                       &(yyvsp[(yyi + 1) - (yynrhs)])
                                              );
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, Rule); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif


#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
static YYSIZE_T
yystrlen (const char *yystr)
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
yystpcpy (char *yydest, const char *yysrc)
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
        switch (*++yyp)
          {
          case '\'':
          case ',':
            goto do_not_strip_quotes;

          case '\\':
            if (*++yyp != '\\')
              goto do_not_strip_quotes;
            /* Fall through.  */
          default:
            if (yyres)
              yyres[yyn] = *yyp;
            yyn++;
            break;

          case '"':
            if (yyres)
              yyres[yyn] = '\0';
            return yyn;
          }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return 1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return 2 if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYSIZE_T *yymsg_alloc, char **yymsg,
                yytype_int16 *yyssp, int yytoken)
{
  YYSIZE_T yysize0 = yytnamerr (YY_NULLPTR, yytname[yytoken]);
  YYSIZE_T yysize = yysize0;
  enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULLPTR;
  /* Arguments of yyformat. */
  char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
  /* Number of reported tokens (one for the "unexpected", one per
     "expected"). */
  int yycount = 0;

  /* There are many possibilities here to consider:
     - If this state is a consistent state with a default action, then
       the only way this function was invoked is if the default action
       is an error action.  In that case, don't check for expected
       tokens because there are none.
     - The only way there can be no lookahead present (in yychar) is if
       this state is a consistent state with a default action.  Thus,
       detecting the absence of a lookahead is sufficient to determine
       that there is no unexpected or expected token to report.  In that
       case, just report a simple "syntax error".
     - Don't assume there isn't a lookahead just because this state is a
       consistent state with a default action.  There might have been a
       previous inconsistent state, consistent state with a non-default
       action, or user semantic action that manipulated yychar.
     - Of course, the expected token list depends on states to have
       correct lookahead information, and it depends on the parser not
       to perform extra reductions after fetching a lookahead from the
       scanner and before detecting a syntax error.  Thus, state merging
       (from LALR or IELR) and default reductions corrupt the expected
       token list.  However, the list is correct for canonical LR with
       one exception: it will still contain any token that will not be
       accepted due to an error action in a later state.
  */
  if (yytoken != YYEMPTY)
    {
      int yyn = yypact[*yyssp];
      yyarg[yycount++] = yytname[yytoken];
      if (!yypact_value_is_default (yyn))
        {
          /* Start YYX at -YYN if negative to avoid negative indexes in
             YYCHECK.  In other words, skip the first -YYN actions for
             this state because they are default actions.  */
          int yyxbegin = yyn < 0 ? -yyn : 0;
          /* Stay within bounds of both yycheck and yytname.  */
          int yychecklim = YYLAST - yyn + 1;
          int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
          int yyx;

          for (yyx = yyxbegin; yyx < yyxend; ++yyx)
            if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR
                && !yytable_value_is_error (yytable[yyx + yyn]))
              {
                if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
                  {
                    yycount = 1;
                    yysize = yysize0;
                    break;
                  }
                yyarg[yycount++] = yytname[yyx];
                {
                  YYSIZE_T yysize1 = yysize + yytnamerr (YY_NULLPTR, yytname[yyx]);
                  if (! (yysize <= yysize1
                         && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
                    return 2;
                  yysize = yysize1;
                }
              }
        }
    }

  switch (yycount)
    {
# define YYCASE_(N, S)                      \
      case N:                               \
        yyformat = S;                       \
      break
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
# undef YYCASE_
    }

  {
    YYSIZE_T yysize1 = yysize + yystrlen (yyformat);
    if (! (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
      return 2;
    yysize = yysize1;
  }

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return 1;
    }

  /* Avoid sprintf, as that infringes on the user's name space.
     Don't have undefined behavior even if the translation
     produced a string with the wrong number of "%s"s.  */
  {
    char *yyp = *yymsg;
    int yyi = 0;
    while ((*yyp = *yyformat) != '\0')
      if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount)
        {
          yyp += yytnamerr (yyp, yyarg[yyi++]);
          yyformat += 2;
        }
      else
        {
          yyp++;
          yyformat++;
        }
  }
  return 0;
}
#endif /* YYERROR_VERBOSE */

/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
{
  YYUSE (yyvaluep);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yytype);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}




/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Number of syntax errors so far.  */
int yynerrs;


/*----------.
| yyparse.  |
`----------*/

int
yyparse (void)
{
    int yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       'yyss': related to states.
       'yyvs': related to semantic values.

       Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yytype_int16 yyssa[YYINITDEPTH];
    yytype_int16 *yyss;
    yytype_int16 *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    YYSIZE_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken = 0;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yyssp = yyss = yyssa;
  yyvsp = yyvs = yyvsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */
  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        YYSTYPE *yyvs1 = yyvs;
        yytype_int16 *yyss1 = yyss;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * sizeof (*yyssp),
                    &yyvs1, yysize * sizeof (*yyvsp),
                    &yystacksize);

        yyss = yyss1;
        yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yytype_int16 *yyss1 = yyss;
        union yyalloc *yyptr =
          (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
        if (! yyptr)
          goto yyexhaustedlab;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
                  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = yylex ();
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token.  */
  yychar = YYEMPTY;

  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 6:
#line 519 "fortran.y" /* yacc.c:1646  */
    {yyerrok;yyclearin;}
#line 3520 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 7:
#line 522 "fortran.y" /* yacc.c:1646  */
    {token_since_endofstmt = 0; increment_nbtokens = 0;}
#line 3526 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 16:
#line 535 "fortran.y" /* yacc.c:1646  */
    {
            if (inmoduledeclare == 0 )
            {
                pos_end = setposcur();
                RemoveWordSET_0(fortran_out,pos_curinclude,pos_end-pos_curinclude);
            }
        }
#line 3538 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 18:
#line 560 "fortran.y" /* yacc.c:1646  */
    { pos_cur = setposcur(); }
#line 3544 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 24:
#line 584 "fortran.y" /* yacc.c:1646  */
    { Add_Include_1((yyvsp[0].na)); }
#line 3550 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 27:
#line 1104 "fortran.y" /* yacc.c:1646  */
    { strcpy((yyval.na),(yyvsp[0].na)); }
#line 3556 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 28:
#line 1105 "fortran.y" /* yacc.c:1646  */
    { strcpy((yyval.na),(yyvsp[0].na)); }
#line 3562 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 29:
#line 1106 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na),"%s%s",(yyvsp[-1].na),(yyvsp[0].na)); }
#line 3568 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 30:
#line 1107 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na),"%s%s",(yyvsp[-1].na),(yyvsp[0].na)); }
#line 3574 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 31:
#line 1108 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na),"%s%s",(yyvsp[-1].na),(yyvsp[0].na)); }
#line 3580 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 32:
#line 1110 "fortran.y" /* yacc.c:1646  */
    { strcpy((yyval.na),"+"); }
#line 3586 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 33:
#line 1111 "fortran.y" /* yacc.c:1646  */
    { strcpy((yyval.na),"-"); }
#line 3592 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 34:
#line 1115 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na),"+%s",(yyvsp[0].na)); }
#line 3598 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 35:
#line 1116 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na),"-%s",(yyvsp[0].na)); }
#line 3604 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 36:
#line 1117 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na),"*%s",(yyvsp[0].na)); }
#line 3610 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 37:
#line 1118 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na),"%s%s",(yyvsp[-1].na),(yyvsp[0].na)); }
#line 3616 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 38:
#line 1119 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na),"%s%s",(yyvsp[-1].na),(yyvsp[0].na)); }
#line 3622 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 39:
#line 1120 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na),"%s%s",(yyvsp[-1].na),(yyvsp[0].na)); }
#line 3628 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 40:
#line 1121 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na),"%s%s",(yyvsp[-1].na),(yyvsp[0].na)); }
#line 3634 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 41:
#line 1122 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na)," > %s",(yyvsp[0].na)); }
#line 3640 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 42:
#line 1123 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na)," < %s",(yyvsp[0].na)); }
#line 3646 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 43:
#line 1124 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na)," >= %s",(yyvsp[0].na)); }
#line 3652 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 44:
#line 1125 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na)," <= %s",(yyvsp[0].na)); }
#line 3658 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 45:
#line 1126 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na),"%s%s",(yyvsp[-1].na),(yyvsp[0].na)); }
#line 3664 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 46:
#line 1127 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na),"%s%s",(yyvsp[-1].na),(yyvsp[0].na)); }
#line 3670 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 47:
#line 1128 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na),"%s%s",(yyvsp[-1].na),(yyvsp[0].na)); }
#line 3676 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 48:
#line 1129 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na),"%s%s",(yyvsp[-1].na),(yyvsp[0].na)); }
#line 3682 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 49:
#line 1130 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na),"%s%s",(yyvsp[-1].na),(yyvsp[0].na)); }
#line 3688 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 50:
#line 1131 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na),"%s%s",(yyvsp[-1].na),(yyvsp[0].na)); }
#line 3694 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 51:
#line 1132 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na),"%s%s",(yyvsp[-1].na),(yyvsp[0].na)); }
#line 3700 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 52:
#line 1133 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na),"%s%s",(yyvsp[-1].na),(yyvsp[0].na)); }
#line 3706 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 53:
#line 1134 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na),"%s",(yyvsp[0].na)); }
#line 3712 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 54:
#line 1135 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na),"%s",(yyvsp[0].na)); }
#line 3718 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 55:
#line 1137 "fortran.y" /* yacc.c:1646  */
    { strcpy((yyval.na),""); }
#line 3724 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 56:
#line 1138 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na),"/%s",(yyvsp[0].na)); }
#line 3730 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 57:
#line 1139 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na),"/= %s",(yyvsp[0].na));}
#line 3736 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 58:
#line 1140 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na),"//%s",(yyvsp[0].na)); }
#line 3742 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 59:
#line 1143 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na),"==%s",(yyvsp[0].na)); }
#line 3748 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 60:
#line 1144 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na),"= %s",(yyvsp[0].na)); }
#line 3754 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 61:
#line 1147 "fortran.y" /* yacc.c:1646  */
    { strcpy((yyval.na),(yyvsp[0].na)); }
#line 3760 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 62:
#line 1148 "fortran.y" /* yacc.c:1646  */
    { strcpy((yyval.na),(yyvsp[0].na)); }
#line 3766 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 63:
#line 1149 "fortran.y" /* yacc.c:1646  */
    { strcpy((yyval.na),(yyvsp[0].na)); }
#line 3772 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 64:
#line 1153 "fortran.y" /* yacc.c:1646  */
    {
            agrif_parentcall = 0;
            if ( !strcasecmp(identcopy, "Agrif_Parent") )   agrif_parentcall = 1;
            if ( Agrif_in_Tok_NAME(identcopy) )
            {
                inagrifcallargument = 1;
                Add_SubroutineWhereAgrifUsed_1(subroutinename, curmodulename);
            }
        }
#line 3786 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 65:
#line 1164 "fortran.y" /* yacc.c:1646  */
    { strcpy((yyval.na),(yyvsp[0].na)); if ( incalldeclare == 0 ) inagrifcallargument = 0;   }
#line 3792 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 66:
#line 1165 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na)," %s %s ",(yyvsp[-1].na),(yyvsp[0].na)); }
#line 3798 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 67:
#line 1166 "fortran.y" /* yacc.c:1646  */
    {in_complex_literal=0;}
#line 3804 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 68:
#line 1166 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na)," %s ( %s )",(yyvsp[-4].na),(yyvsp[-1].na)); }
#line 3810 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 69:
#line 1167 "fortran.y" /* yacc.c:1646  */
    {in_complex_literal=0;}
#line 3816 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 70:
#line 1167 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na)," %s ( %s ) %s ",(yyvsp[-5].na),(yyvsp[-2].na),(yyvsp[0].na)); }
#line 3822 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 72:
#line 1170 "fortran.y" /* yacc.c:1646  */
    {in_complex_literal=0;}
#line 3828 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 73:
#line 1171 "fortran.y" /* yacc.c:1646  */
    {
            if ( inside_type_declare ) break;
            sprintf((yyval.na)," %s ( %s )",(yyvsp[-4].na),(yyvsp[-1].na));
            ModifyTheAgrifFunction_0((yyvsp[-1].na));
            agrif_parentcall = 0;
        }
#line 3839 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 74:
#line 1180 "fortran.y" /* yacc.c:1646  */
    {
            sprintf((yyval.na)," %s %% %s ",(yyvsp[-3].na),(yyvsp[0].na));
            if ( incalldeclare == 0 ) inagrifcallargument = 0;
        }
#line 3848 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 75:
#line 1191 "fortran.y" /* yacc.c:1646  */
    { strcpy((yyval.na)," "); }
#line 3854 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 76:
#line 1192 "fortran.y" /* yacc.c:1646  */
    { strcpy((yyval.na),(yyvsp[0].na)); }
#line 3860 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 77:
#line 1195 "fortran.y" /* yacc.c:1646  */
    {  strcpy((yyval.na),(yyvsp[0].na)); }
#line 3866 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 78:
#line 1196 "fortran.y" /* yacc.c:1646  */
    {  sprintf((yyval.na),"%s,%s",(yyvsp[-2].na),(yyvsp[0].na)); }
#line 3872 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 79:
#line 1199 "fortran.y" /* yacc.c:1646  */
    {strcpy((yyval.na),(yyvsp[0].na));}
#line 3878 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 80:
#line 1200 "fortran.y" /* yacc.c:1646  */
    {strcpy((yyval.na),(yyvsp[0].na));}
#line 3884 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 81:
#line 1203 "fortran.y" /* yacc.c:1646  */
    {  sprintf((yyval.na),"%s :%s",(yyvsp[-2].na),(yyvsp[0].na));}
#line 3890 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 82:
#line 1204 "fortran.y" /* yacc.c:1646  */
    {  sprintf((yyval.na),"%s :%s :%s",(yyvsp[-4].na),(yyvsp[-2].na),(yyvsp[0].na));}
#line 3896 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 83:
#line 1205 "fortran.y" /* yacc.c:1646  */
    {  sprintf((yyval.na),":%s :%s",(yyvsp[-2].na),(yyvsp[0].na));}
#line 3902 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 84:
#line 1206 "fortran.y" /* yacc.c:1646  */
    {  sprintf((yyval.na),": : %s",(yyvsp[0].na));}
#line 3908 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 85:
#line 1207 "fortran.y" /* yacc.c:1646  */
    {  sprintf((yyval.na),":%s",(yyvsp[0].na));}
#line 3914 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 86:
#line 1208 "fortran.y" /* yacc.c:1646  */
    {  sprintf((yyval.na),"%s :",(yyvsp[-1].na));}
#line 3920 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 87:
#line 1209 "fortran.y" /* yacc.c:1646  */
    {  sprintf((yyval.na),":");}
#line 3926 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 88:
#line 1212 "fortran.y" /* yacc.c:1646  */
    {
       //  if (indeclaration == 1) break;
            if ( afterpercent == 0 )
            {
                if ( Agrif_in_Tok_NAME((yyvsp[0].na)) ) Add_SubroutineWhereAgrifUsed_1(subroutinename, curmodulename);
                if ( !strcasecmp((yyvsp[0].na),"Agrif_Parent") )   agrif_parentcall = 1;
                if ( VariableIsFunction((yyvsp[0].na)) )
                {
                    if ( inagrifcallargument == 1 )
                    {
                        if ( !strcasecmp((yyvsp[0].na),identcopy) )
                        {
                            strcpy(sameagrifname,identcopy);
                            sameagrifargument = 1;
                        }
                    }
                    strcpy(identcopy,(yyvsp[0].na));
                    pointedvar = 0;

                    if (variscoupled_0((yyvsp[0].na))) strcpy(truename, getcoupledname_0((yyvsp[0].na)));
                    else                    strcpy(truename, (yyvsp[0].na));

                    if ( VarIsNonGridDepend(truename) == 0 && (! Variableshouldberemoved(truename)) )
                    {
                        if ( inagrifcallargument == 1 || varispointer_0(truename) == 1 )
                        {
                            if ( (IsinListe(List_UsedInSubroutine_Var,(yyvsp[0].na)) == 1) || (inagrifcallargument == 1) )
                            {
                                if (varistyped_0(truename) == 0)    ModifyTheVariableName_0(truename,strlen((yyvsp[0].na)));
                            }
                        }
                        if ( inagrifcallargument != 1 || sameagrifargument ==1 )
                        {
                            Add_UsedInSubroutine_Var_1(truename);
                        }
                    }
                    NotifyAgrifFunction_0(truename);
                }
            }
            else
            {
                afterpercent = 0;
            }
        }
#line 3975 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 89:
#line 1258 "fortran.y" /* yacc.c:1646  */
    { strcpy((yyval.na),".TRUE.");}
#line 3981 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 90:
#line 1259 "fortran.y" /* yacc.c:1646  */
    { strcpy((yyval.na),".FALSE.");}
#line 3987 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 91:
#line 1260 "fortran.y" /* yacc.c:1646  */
    { strcpy((yyval.na),"NULL()"); }
#line 3993 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 92:
#line 1261 "fortran.y" /* yacc.c:1646  */
    { strcpy((yyval.na),(yyvsp[0].na)); }
#line 3999 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 93:
#line 1262 "fortran.y" /* yacc.c:1646  */
    { strcpy((yyval.na),(yyvsp[0].na)); }
#line 4005 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 94:
#line 1263 "fortran.y" /* yacc.c:1646  */
    { strcpy((yyval.na),(yyvsp[0].na)); }
#line 4011 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 95:
#line 1265 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na),"%s%s",(yyvsp[-1].na),(yyvsp[0].na)); }
#line 4017 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 97:
#line 1269 "fortran.y" /* yacc.c:1646  */
    { strcpy((yyval.na),(yyvsp[0].na));}
#line 4023 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 99:
#line 1271 "fortran.y" /* yacc.c:1646  */
    { strcpy((yyval.na),(yyvsp[0].na));}
#line 4029 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 100:
#line 1272 "fortran.y" /* yacc.c:1646  */
    { strcpy((yyval.na),(yyvsp[0].na));}
#line 4035 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 101:
#line 1274 "fortran.y" /* yacc.c:1646  */
    { strcpy((yyval.na)," ");}
#line 4041 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 102:
#line 1275 "fortran.y" /* yacc.c:1646  */
    { strcpy((yyval.na),(yyvsp[0].na));}
#line 4047 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 103:
#line 1285 "fortran.y" /* yacc.c:1646  */
    { strcpy((yyval.na)," ");}
#line 4053 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 104:
#line 1286 "fortran.y" /* yacc.c:1646  */
    { strcpy((yyval.na),(yyvsp[0].na));}
#line 4059 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 169:
#line 1484 "fortran.y" /* yacc.c:1646  */
    {
            /* if we never meet the contains keyword               */
            if ( firstpass == 0 )
            {
                RemoveWordCUR_0(fortran_out, strlen((yyvsp[0].na))+11);    // Remove word "end module"
                if ( inmoduledeclare && ! aftercontainsdeclare )
                {
                    Write_Closing_Module(1);
                }
                fprintf(fortran_out,"\n      end module %s\n", curmodulename);
                if ( module_declar && insubroutinedeclare == 0 )
                {
                    fclose(module_declar);
                }
            }
            inmoduledeclare = 0 ;
            inmodulemeet = 0 ;
            aftercontainsdeclare = 1;
            strcpy(curmodulename, "");
            GlobalDeclaration = 0 ;
        }
#line 4085 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 189:
#line 1537 "fortran.y" /* yacc.c:1646  */
    {in_complex_literal=0;}
#line 4091 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 192:
#line 1561 "fortran.y" /* yacc.c:1646  */
    {strcpy((yyval.na),"");}
#line 4097 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 196:
#line 1571 "fortran.y" /* yacc.c:1646  */
    {strcpy((yyval.na),"");}
#line 4103 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 197:
#line 1573 "fortran.y" /* yacc.c:1646  */
    {strcpy((yyval.na),(yyvsp[0].na));}
#line 4109 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 202:
#line 1593 "fortran.y" /* yacc.c:1646  */
    {pos_cur_decl=my_position_before;}
#line 4115 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 203:
#line 1594 "fortran.y" /* yacc.c:1646  */
    {strcpy((yyval.na),(yyvsp[0].na));}
#line 4121 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 205:
#line 1597 "fortran.y" /* yacc.c:1646  */
    {strcpy(DeclType,"type"); GlobalDeclarationType = 1;  }
#line 4127 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 206:
#line 1601 "fortran.y" /* yacc.c:1646  */
    {in_kind_selector = 1;}
#line 4133 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 207:
#line 1602 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),"%s%s",(yyvsp[-2].na),(yyvsp[0].na));strcpy(DeclType,(yyvsp[-2].na)); in_kind_selector =0;}
#line 4139 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 208:
#line 1603 "fortran.y" /* yacc.c:1646  */
    {in_kind_selector = 1;}
#line 4145 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 209:
#line 1604 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),"%s%s",(yyvsp[-2].na),(yyvsp[0].na));strcpy(DeclType,(yyvsp[-2].na));in_kind_selector =0;}
#line 4151 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 210:
#line 1605 "fortran.y" /* yacc.c:1646  */
    {in_kind_selector = 1;}
#line 4157 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 211:
#line 1606 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),"%s%s",(yyvsp[-2].na),(yyvsp[0].na));strcpy(DeclType,"real"); strcpy(NamePrecision,"8");in_kind_selector =0;}
#line 4163 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 212:
#line 1607 "fortran.y" /* yacc.c:1646  */
    {in_kind_selector = 1;}
#line 4169 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 213:
#line 1608 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),"%s%s",(yyvsp[-2].na),(yyvsp[0].na));strcpy(DeclType,(yyvsp[-2].na));in_kind_selector =0;}
#line 4175 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 214:
#line 1609 "fortran.y" /* yacc.c:1646  */
    {in_char_selector = 1;}
#line 4181 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 215:
#line 1610 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),"%s%s",(yyvsp[-2].na),(yyvsp[0].na));strcpy(DeclType,(yyvsp[-2].na));in_char_selector = 0;}
#line 4187 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 216:
#line 1611 "fortran.y" /* yacc.c:1646  */
    {in_kind_selector = 1;}
#line 4193 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 217:
#line 1612 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),"%s%s",(yyvsp[-2].na),(yyvsp[0].na));strcpy(DeclType,(yyvsp[-2].na));in_kind_selector =0;}
#line 4199 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 218:
#line 1616 "fortran.y" /* yacc.c:1646  */
    {strcpy((yyval.na),"");strcpy(NamePrecision,"");}
#line 4205 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 219:
#line 1618 "fortran.y" /* yacc.c:1646  */
    {strcpy((yyval.na),(yyvsp[0].na));}
#line 4211 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 220:
#line 1624 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),"(%s)",(yyvsp[-1].na)); strcpy(NamePrecision,(yyvsp[-1].na));}
#line 4217 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 221:
#line 1626 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),"(KIND=%s)",(yyvsp[-1].na)); strcpy(NamePrecision,(yyvsp[-1].na));}
#line 4223 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 222:
#line 1628 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),"*%s",(yyvsp[0].na));strcpy(NamePrecision,(yyvsp[0].na));}
#line 4229 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 224:
#line 1636 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),"%s%s",(yyvsp[-1].na),(yyvsp[0].na));}
#line 4235 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 226:
#line 1642 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),"%s_%s",(yyvsp[-2].na),(yyvsp[0].na));}
#line 4241 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 230:
#line 1665 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),"%s%s",(yyvsp[-1].na),(yyvsp[0].na));}
#line 4247 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 232:
#line 1671 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),"%s_%s",(yyvsp[-2].na),(yyvsp[0].na));}
#line 4253 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 233:
#line 1678 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),"(%s,%s)",(yyvsp[-3].na),(yyvsp[-1].na));}
#line 4259 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 241:
#line 1696 "fortran.y" /* yacc.c:1646  */
    {char_length_toreset = 1;}
#line 4265 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 242:
#line 1700 "fortran.y" /* yacc.c:1646  */
    {strcpy((yyval.na),"");}
#line 4271 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 243:
#line 1702 "fortran.y" /* yacc.c:1646  */
    {strcpy((yyval.na),"");}
#line 4277 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 249:
#line 1715 "fortran.y" /* yacc.c:1646  */
    {strcpy(CharacterSize,(yyvsp[-1].na));}
#line 4283 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 250:
#line 1717 "fortran.y" /* yacc.c:1646  */
    {strcpy(CharacterSize,(yyvsp[-1].na));}
#line 4289 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 253:
#line 1724 "fortran.y" /* yacc.c:1646  */
    {c_star=1; strcpy(CharacterSize,(yyvsp[-1].na));}
#line 4295 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 254:
#line 1726 "fortran.y" /* yacc.c:1646  */
    {c_selectorgiven = 1; strcpy(c_selectorname,(yyvsp[0].na));}
#line 4301 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 260:
#line 1741 "fortran.y" /* yacc.c:1646  */
    { inside_type_declare = 1;}
#line 4307 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 261:
#line 1742 "fortran.y" /* yacc.c:1646  */
    { inside_type_declare = 0;}
#line 4313 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 289:
#line 1807 "fortran.y" /* yacc.c:1646  */
    {in_complex_literal=0;}
#line 4319 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 294:
#line 1817 "fortran.y" /* yacc.c:1646  */
    {
            PublicDeclare = 0;
            PrivateDeclare = 0;
            ExternalDeclare = 0;
            strcpy(NamePrecision,"");
            c_star = 0;
            InitialValueGiven = 0 ;
            strcpy(IntentSpec,"");
            VariableIsParameter =  0 ;
            Allocatabledeclare = 0 ;
            Targetdeclare = 0 ;
            SaveDeclare = 0;
            pointerdeclare = 0;
            optionaldeclare = 0 ;
            dimsgiven=0;
            c_selectorgiven=0;
            strcpy(nameinttypename,"");
            strcpy(c_selectorname,"");
            GlobalDeclarationType = 0;
         }
#line 4344 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 304:
#line 1860 "fortran.y" /* yacc.c:1646  */
    {strcpy(my_dim.last,"");}
#line 4350 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 305:
#line 1865 "fortran.y" /* yacc.c:1646  */
    {strcpy(NamePrecision,(yyvsp[0].na));}
#line 4356 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 320:
#line 1900 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na),"(/%s/)",(yyvsp[-1].na));}
#line 4362 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 321:
#line 1902 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na),"[%s]",(yyvsp[-1].na)); }
#line 4368 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 326:
#line 1930 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),"%s,%s",(yyvsp[-2].na),(yyvsp[0].na));}
#line 4374 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 329:
#line 1940 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),"(%s,%s)",(yyvsp[-3].na),(yyvsp[-1].na));}
#line 4380 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 330:
#line 1945 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),"%s=%s,%s",(yyvsp[-4].na),(yyvsp[-2].na),(yyvsp[0].na));}
#line 4386 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 331:
#line 1947 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),"%s=%s,%s,%s",(yyvsp[-6].na),(yyvsp[-4].na),(yyvsp[-2].na),(yyvsp[0].na));}
#line 4392 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 333:
#line 1955 "fortran.y" /* yacc.c:1646  */
    {indeclaration=1;}
#line 4398 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 334:
#line 1956 "fortran.y" /* yacc.c:1646  */
    {
            /* if the variable is a parameter we can suppose that is*/
            /*    value is the same on each grid. It is not useless */
            /*    to create a copy of it on each grid               */
            if ( ! inside_type_declare )
            {
                pos_end = setposcur();
                //printf("POS = %d %d\n",pos_cur_decl,pos_end);
                RemoveWordSET_0(fortran_out,pos_cur_decl,pos_end-pos_cur_decl);
                ReWriteDeclarationAndAddTosubroutine_01((yyvsp[0].l));
                pos_cur_decl = setposcur();
                if ( firstpass == 0 && GlobalDeclaration == 0
                                    && insubroutinedeclare == 0 )
                {
                    fprintf(fortran_out,"\n#include \"Module_Declar_%s.h\"\n", curmodulename);
                    sprintf(ligne, "Module_Declar_%s.h", curmodulename);
                    module_declar = open_for_write(ligne);
                    GlobalDeclaration = 1 ;
                    pos_cur_decl = setposcur();
                }

                if ( firstpass )
                {
                    Add_Globliste_1((yyvsp[0].l));
                    if ( insubroutinedeclare )
                    {
                        if ( pointerdeclare ) Add_Pointer_Var_From_List_1((yyvsp[0].l));
                        Add_Parameter_Var_1((yyvsp[0].l));
                    }
                    else
                        Add_GlobalParameter_Var_1((yyvsp[0].l));

                    /* If there's a SAVE declaration in module's subroutines we should    */
                    /*    remove it from the subroutines declaration and add it in the    */
                    /*    global declarations                                             */
                                        
                    if ( aftercontainsdeclare && SaveDeclare )
                    {
                        if ( inmodulemeet ) Add_SubroutineDeclarationSave_Var_1((yyvsp[0].l));
                        else                Add_Save_Var_dcl_1((yyvsp[0].l));
                    }
                }
            }
            indeclaration = 0;
            PublicDeclare = 0;
            PrivateDeclare = 0;
            ExternalDeclare = 0;
            strcpy(NamePrecision,"");
            c_star = 0;
            InitialValueGiven = 0 ;
            strcpy(IntentSpec,"");
            VariableIsParameter =  0 ;
            Allocatabledeclare = 0 ;
            Targetdeclare = 0 ;
            SaveDeclare = 0;
            pointerdeclare = 0;
            optionaldeclare = 0 ;
            dimsgiven=0;
            c_selectorgiven=0;
            strcpy(nameinttypename,"");
            strcpy(c_selectorname,"");
            strcpy(DeclType,"");
            GlobalDeclarationType = 0;
        }
#line 4467 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 343:
#line 2039 "fortran.y" /* yacc.c:1646  */
    { Allocatabledeclare = 1; }
#line 4473 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 344:
#line 2040 "fortran.y" /* yacc.c:1646  */
    {in_complex_literal=0;}
#line 4479 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 345:
#line 2041 "fortran.y" /* yacc.c:1646  */
    { dimsgiven = 1; curdim = (yyvsp[-1].d); }
#line 4485 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 346:
#line 2043 "fortran.y" /* yacc.c:1646  */
    { ExternalDeclare = 1; }
#line 4491 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 347:
#line 2044 "fortran.y" /* yacc.c:1646  */
    {in_complex_literal=0;}
#line 4497 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 348:
#line 2045 "fortran.y" /* yacc.c:1646  */
    { strcpy(IntentSpec,(yyvsp[-1].na)); }
#line 4503 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 350:
#line 2048 "fortran.y" /* yacc.c:1646  */
    { optionaldeclare = 1 ; }
#line 4509 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 351:
#line 2050 "fortran.y" /* yacc.c:1646  */
    {VariableIsParameter = 1; }
#line 4515 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 352:
#line 2052 "fortran.y" /* yacc.c:1646  */
    { pointerdeclare = 1 ; }
#line 4521 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 353:
#line 2054 "fortran.y" /* yacc.c:1646  */
    { SaveDeclare = 1 ; }
#line 4527 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 354:
#line 2056 "fortran.y" /* yacc.c:1646  */
    { Targetdeclare = 1; }
#line 4533 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 355:
#line 2061 "fortran.y" /* yacc.c:1646  */
    {(yyval.l)=insertvar(NULL,(yyvsp[0].v));}
#line 4539 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 356:
#line 2063 "fortran.y" /* yacc.c:1646  */
    {(yyval.l)=insertvar((yyvsp[-2].l),(yyvsp[0].v));}
#line 4545 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 357:
#line 2068 "fortran.y" /* yacc.c:1646  */
    {
            if ( ! inside_type_declare )
            {
                if (dimsgiven == 1) curvar = createvar((yyvsp[-3].na),curdim);
                else                curvar = createvar((yyvsp[-3].na),(yyvsp[-2].d));
                CreateAndFillin_Curvar(DeclType, curvar);
                strcpy(curvar->v_typevar,DeclType);
                curvar->v_catvar = get_cat_var(curvar);
                
                if (!strcasecmp(DeclType,"character"))
                {
                    if (c_selectorgiven == 1)
                    {
                        Save_Length(c_selectorname,1);
                        strcpy(curvar->v_dimchar,c_selectorname);
                    }
                }
            }
            strcpy(vallengspec,"");
            if (char_length_toreset == 1)
            {
            c_selectorgiven = 0;
            c_star = 0;
            strcpy(c_selectorname,"");
            strcpy(CharacterSize,"");
            char_length_toreset = 0;
            }
            (yyval.v)=curvar;
        }
#line 4579 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 360:
#line 2107 "fortran.y" /* yacc.c:1646  */
    {InitialValueGiven = 0; }
#line 4585 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 362:
#line 2113 "fortran.y" /* yacc.c:1646  */
    {
            if ( inside_type_declare ) break;
            strcpy(InitValue,(yyvsp[0].na));
            InitialValueGiven = 1;
        }
#line 4595 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 363:
#line 2119 "fortran.y" /* yacc.c:1646  */
    {
            if ( inside_type_declare ) break;
            strcpy(InitValue,(yyvsp[0].na));
            InitialValueGiven = 2;
        }
#line 4605 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 364:
#line 2125 "fortran.y" /* yacc.c:1646  */
    {
            if ( inside_type_declare ) break;
            strcpy(InitValue,(yyvsp[0].na));
            InitialValueGiven = 2;
        }
#line 4615 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 366:
#line 2138 "fortran.y" /* yacc.c:1646  */
    {PublicDeclare = 1;  }
#line 4621 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 367:
#line 2140 "fortran.y" /* yacc.c:1646  */
    {PrivateDeclare = 1;  }
#line 4627 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 368:
#line 2144 "fortran.y" /* yacc.c:1646  */
    {(yyval.d)=NULL;}
#line 4633 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 369:
#line 2145 "fortran.y" /* yacc.c:1646  */
    {in_complex_literal=0;}
#line 4639 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 370:
#line 2146 "fortran.y" /* yacc.c:1646  */
    {(yyval.d)=(yyvsp[-1].d);}
#line 4645 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 371:
#line 2151 "fortran.y" /* yacc.c:1646  */
    {(yyval.d)=(yyvsp[0].d);}
#line 4651 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 372:
#line 2153 "fortran.y" /* yacc.c:1646  */
    {(yyval.d)=(yyvsp[0].d);}
#line 4657 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 373:
#line 2155 "fortran.y" /* yacc.c:1646  */
    {(yyval.d)=(yyvsp[0].d);}
#line 4663 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 374:
#line 2157 "fortran.y" /* yacc.c:1646  */
    {(yyval.d)=(yyvsp[0].d);}
#line 4669 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 375:
#line 2159 "fortran.y" /* yacc.c:1646  */
    {(yyval.d)=(yyvsp[0].d);}
#line 4675 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 376:
#line 2163 "fortran.y" /* yacc.c:1646  */
    {
            (yyval.d) = (listdim*) NULL;
            if ( inside_type_declare ) break;
            if ( created_dimensionlist == 1 || agrif_parentcall == 1 )  (yyval.d)=insertdim(NULL,(yyvsp[0].dim1));
        }
#line 4685 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 377:
#line 2169 "fortran.y" /* yacc.c:1646  */
    {
            (yyval.d) = (listdim*) NULL;
            if ( inside_type_declare ) break;
            if ( (!inside_type_declare) && created_dimensionlist == 1 ) (yyval.d)=insertdim((yyvsp[-2].d),(yyvsp[0].dim1));
        }
#line 4695 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 378:
#line 2178 "fortran.y" /* yacc.c:1646  */
    {strcpy((yyval.dim1).first,(yyvsp[-2].na));  Save_Length((yyvsp[-2].na),2); strcpy((yyval.dim1).last,(yyvsp[0].na)); Save_Length((yyvsp[0].na),1); }
#line 4701 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 379:
#line 2180 "fortran.y" /* yacc.c:1646  */
    {strcpy((yyval.dim1).first,"1"); strcpy((yyval.dim1).last,(yyvsp[0].na)); Save_Length((yyvsp[0].na),1);}
#line 4707 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 380:
#line 2185 "fortran.y" /* yacc.c:1646  */
    {strcpy((yyval.na),(yyvsp[0].na));}
#line 4713 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 382:
#line 2194 "fortran.y" /* yacc.c:1646  */
    {
            (yyval.d) = (listdim*) NULL;
            if ( inside_type_declare ) break;
            if ( created_dimensionlist == 1 || agrif_parentcall == 1 )  (yyval.d)=insertdim(NULL,(yyvsp[0].dim1));
        }
#line 4723 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 383:
#line 2200 "fortran.y" /* yacc.c:1646  */
    {
            (yyval.d) = (listdim*) NULL;
            if ( inside_type_declare ) break;
            if ( (!inside_type_declare) && created_dimensionlist == 1 ) (yyval.d)=insertdim((yyvsp[-2].d),(yyvsp[0].dim1));
        }
#line 4733 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 384:
#line 2209 "fortran.y" /* yacc.c:1646  */
    { strcpy((yyval.dim1).first,"");  strcpy((yyval.dim1).last,"");  }
#line 4739 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 385:
#line 2211 "fortran.y" /* yacc.c:1646  */
    { strcpy((yyval.dim1).first,(yyvsp[-1].na));  Save_Length((yyvsp[-1].na),2); strcpy((yyval.dim1).last,""); }
#line 4745 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 386:
#line 2216 "fortran.y" /* yacc.c:1646  */
    {
            (yyval.d) = (listdim*) NULL;
            if ( inside_type_declare ) break;
            if ( created_dimensionlist == 1 || agrif_parentcall == 1 )  (yyval.d)=insertdim(NULL,(yyvsp[0].dim1));
        }
#line 4755 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 387:
#line 2222 "fortran.y" /* yacc.c:1646  */
    {
            (yyval.d) = (listdim*) NULL;
            if ( inside_type_declare ) break;
            if ( (!inside_type_declare) && created_dimensionlist == 1 ) (yyval.d)=insertdim((yyvsp[-2].d),(yyvsp[0].dim1));
        }
#line 4765 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 388:
#line 2231 "fortran.y" /* yacc.c:1646  */
    { strcpy((yyval.dim1).first,"");  strcpy((yyval.dim1).last,"");  }
#line 4771 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 389:
#line 2236 "fortran.y" /* yacc.c:1646  */
    {
            (yyval.d) = (listdim*) NULL;
            if ( inside_type_declare ) break;
            if ( created_dimensionlist == 1 || agrif_parentcall == 1 ) 
            {
            if (!strcasecmp((yyvsp[-1].na),""))
            {
            strcpy(my_dim.first,"1");
            }
            else
            {
            strcpy(my_dim.first,(yyvsp[-1].na));
            }
            strcpy(my_dim.last,"*");
            (yyval.d)=insertdim((yyvsp[-2].d),my_dim);
            strcpy(my_dim.first,"");
            strcpy(my_dim.last,"");
            }
        }
#line 4795 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 390:
#line 2258 "fortran.y" /* yacc.c:1646  */
    {(yyval.d) = (listdim *) NULL;}
#line 4801 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 391:
#line 2260 "fortran.y" /* yacc.c:1646  */
    {(yyval.d) = (yyvsp[-1].d);}
#line 4807 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 392:
#line 2278 "fortran.y" /* yacc.c:1646  */
    {strcpy((yyval.na),"");}
#line 4813 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 393:
#line 2280 "fortran.y" /* yacc.c:1646  */
    {strcpy((yyval.na),(yyvsp[-1].na));}
#line 4819 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 397:
#line 2293 "fortran.y" /* yacc.c:1646  */
    { strcpy((yyval.na),(yyvsp[0].na)); }
#line 4825 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 398:
#line 2295 "fortran.y" /* yacc.c:1646  */
    { strcpy((yyval.na),(yyvsp[0].na)); }
#line 4831 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 399:
#line 2297 "fortran.y" /* yacc.c:1646  */
    { strcpy((yyval.na),(yyvsp[0].na)); }
#line 4837 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 400:
#line 2302 "fortran.y" /* yacc.c:1646  */
    {
            if ((firstpass == 0) && (PublicDeclare == 1))
            {
                if ((yyvsp[0].lnn))
                {
                    removeglobfromlist(&((yyvsp[0].lnn)));
                    pos_end = setposcur();
                    RemoveWordSET_0(fortran_out,pos_cur,pos_end-pos_cur);
                    writelistpublic((yyvsp[0].lnn));
                }
            }
     PublicDeclare = 0;
     PrivateDeclare = 0;
     }
#line 4856 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 402:
#line 2320 "fortran.y" /* yacc.c:1646  */
    {(yyval.lnn)=(listname *)NULL;}
#line 4862 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 403:
#line 2322 "fortran.y" /* yacc.c:1646  */
    {(yyval.lnn)=(yyvsp[0].lnn);}
#line 4868 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 404:
#line 2326 "fortran.y" /* yacc.c:1646  */
    {(yyval.lnn)=Insertname(NULL,(yyvsp[0].na),0);}
#line 4874 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 405:
#line 2328 "fortran.y" /* yacc.c:1646  */
    {(yyval.lnn)=Insertname((yyvsp[-2].lnn),(yyvsp[0].na),0);}
#line 4880 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 408:
#line 2338 "fortran.y" /* yacc.c:1646  */
    {
            /* we should remove the data declaration                */
            pos_end = setposcur();
            RemoveWordSET_0(fortran_out,pos_curdata,pos_end-pos_curdata);
            if ( aftercontainsdeclare == 1  && firstpass == 0 )
            {
                ReWriteDataStatement_0(fortran_out);
                pos_end = setposcur();
            }
            Init_List_Data_Var();
        }
#line 4896 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 414:
#line 2362 "fortran.y" /* yacc.c:1646  */
    {
            if (firstpass == 1)  
            {
            Add_Data_Var_Names_01(&List_Data_Var,(yyvsp[-3].l),(yyvsp[-1].lnn));
            }
            else                 Add_Data_Var_Names_01(&List_Data_Var_Cur,(yyvsp[-3].l),(yyvsp[-1].lnn));
        }
#line 4908 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 415:
#line 2372 "fortran.y" /* yacc.c:1646  */
    { (yyval.l)=insertvar(NULL,(yyvsp[0].v)); }
#line 4914 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 416:
#line 2374 "fortran.y" /* yacc.c:1646  */
    {
     (yyval.l) = insertvar((yyvsp[-2].l),(yyvsp[0].v));
     }
#line 4922 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 417:
#line 2380 "fortran.y" /* yacc.c:1646  */
    {(yyval.lnn)=Insertname(NULL,(yyvsp[0].na),0);}
#line 4928 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 418:
#line 2382 "fortran.y" /* yacc.c:1646  */
    {(yyval.lnn) = Insertname((yyvsp[-2].lnn),(yyvsp[0].na),1);   }
#line 4934 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 421:
#line 2392 "fortran.y" /* yacc.c:1646  */
    {printf("DOVARIABLE = %s %s %s\n",(yyvsp[-5].na),(yyvsp[-3].na),(yyvsp[-1].na));
     printf("AUTRE = %s %s\n",(yyvsp[-7].l)->var->v_nomvar,(yyvsp[-7].l)->var->v_initialvalue_array);
     Insertdoloop((yyvsp[-7].l)->var,(yyvsp[-5].na),(yyvsp[-3].na),(yyvsp[-1].na),"");
     (yyval.v)=(yyvsp[-7].l)->var;
     }
#line 4944 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 422:
#line 2398 "fortran.y" /* yacc.c:1646  */
    {
     Insertdoloop((yyvsp[-9].l)->var,(yyvsp[-7].na),(yyvsp[-5].na),(yyvsp[-3].na),(yyvsp[-1].na));
     (yyval.v)=(yyvsp[-9].l)->var;
     }
#line 4953 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 423:
#line 2405 "fortran.y" /* yacc.c:1646  */
    {(yyval.l)=insertvar(NULL,(yyvsp[0].v));}
#line 4959 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 424:
#line 2407 "fortran.y" /* yacc.c:1646  */
    {(yyval.l) = insertvar((yyvsp[-2].l),(yyvsp[0].v));}
#line 4965 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 426:
#line 2413 "fortran.y" /* yacc.c:1646  */
    {(yyval.v)->v_initialvalue_array=Insertname((yyval.v)->v_initialvalue_array,my_dim.last,0);
     strcpy(my_dim.last,"");
     }
#line 4973 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 429:
#line 2426 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),"%s%s",(yyvsp[-1].na),(yyvsp[0].na));}
#line 4979 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 430:
#line 2428 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),"%s%s",(yyvsp[-1].na),(yyvsp[0].na));}
#line 4985 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 431:
#line 2430 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),"%s%s",(yyvsp[-1].na),(yyvsp[0].na));}
#line 4991 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 437:
#line 2439 "fortran.y" /* yacc.c:1646  */
    {strcpy((yyval.na),"");}
#line 4997 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 438:
#line 2441 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),"*%s",(yyvsp[0].na));}
#line 5003 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 447:
#line 2477 "fortran.y" /* yacc.c:1646  */
    {strcpy(my_dim.last,"");}
#line 5009 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 448:
#line 2481 "fortran.y" /* yacc.c:1646  */
    {positioninblock = 0; pos_curdimension = my_position_before;}
#line 5015 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 449:
#line 2483 "fortran.y" /* yacc.c:1646  */
    {
            /* if the variable is a parameter we can suppose that is   */
            /*    value is the same on each grid. It is not useless to */
            /*    create a copy of it on each grid                     */
            if ( ! inside_type_declare )
            {
                if ( firstpass )
                {
                    Add_Globliste_1((yyvsp[0].l));
                    /* if variableparamlists has been declared in a subroutine   */
                    if ( insubroutinedeclare )     Add_Dimension_Var_1((yyvsp[0].l));
                    
                    /* Add it to the List_SubroutineDeclaration_Var list if not present */
                    /* NB: if not done, a variable declared with DIMENSION but with no type given */
                    /* will not be declared by the conv */
                    ReWriteDeclarationAndAddTosubroutine_01((yyvsp[0].l));
                }
                else
                {
                    pos_end = setposcur();
                    RemoveWordSET_0(fortran_out,pos_curdimension,pos_end-pos_curdimension);
                    ReWriteDeclarationAndAddTosubroutine_01((yyvsp[0].l));
                }
            }
            PublicDeclare = 0;
            PrivateDeclare = 0;
            ExternalDeclare = 0;
            strcpy(NamePrecision,"");
            c_star = 0;
            InitialValueGiven = 0 ;
            strcpy(IntentSpec,"");
            VariableIsParameter =  0 ;
            Allocatabledeclare = 0 ;
            Targetdeclare = 0 ;
            SaveDeclare = 0;
            pointerdeclare = 0;
            optionaldeclare = 0 ;
            dimsgiven=0;
            c_selectorgiven=0;
            strcpy(nameinttypename,"");
            strcpy(c_selectorname,"");
        }
#line 5062 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 451:
#line 2528 "fortran.y" /* yacc.c:1646  */
    {in_complex_literal = 0;}
#line 5068 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 452:
#line 2529 "fortran.y" /* yacc.c:1646  */
    {
        if ( inside_type_declare ) break;
        curvar = createvar((yyvsp[-4].na),(yyvsp[-1].d));
        CreateAndFillin_Curvar("", curvar);
        curlistvar=insertvar(NULL, curvar);
        (yyval.l) = settype("",curlistvar);
        strcpy(vallengspec,"");
     }
#line 5081 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 453:
#line 2537 "fortran.y" /* yacc.c:1646  */
    {in_complex_literal = 0;}
#line 5087 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 454:
#line 2538 "fortran.y" /* yacc.c:1646  */
    {
        if ( inside_type_declare ) break;
        curvar = createvar((yyvsp[-4].na),(yyvsp[-1].d));
        CreateAndFillin_Curvar("", curvar);
        curlistvar = insertvar((yyvsp[-6].l), curvar);
        (yyval.l) = curlistvar;
        strcpy(vallengspec,"");
        }
#line 5100 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 455:
#line 2550 "fortran.y" /* yacc.c:1646  */
    { VariableIsParameter = 1; pos_curparameter = setposcur()-9; }
#line 5106 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 456:
#line 2551 "fortran.y" /* yacc.c:1646  */
    {
            if ( ! inside_type_declare )
            {
                if ( firstpass )
                {
                    if ( insubroutinedeclare )  Add_Parameter_Var_1((yyvsp[-1].l));
                    else                        Add_GlobalParameter_Var_1((yyvsp[-1].l));
                }
                else
                {
                    pos_end = setposcur();
                    RemoveWordSET_0(fortran_out, pos_curparameter, pos_end-pos_curparameter);
                }
            }
            VariableIsParameter =  0 ;
        }
#line 5127 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 458:
#line 2571 "fortran.y" /* yacc.c:1646  */
    {(yyval.l)=insertvar(NULL,(yyvsp[0].v));}
#line 5133 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 459:
#line 2573 "fortran.y" /* yacc.c:1646  */
    {(yyval.l)=insertvar((yyvsp[-2].l),(yyvsp[0].v));}
#line 5139 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 460:
#line 2578 "fortran.y" /* yacc.c:1646  */
    {
            if ( inside_type_declare ) break;
            curvar=(variable *) calloc(1,sizeof(variable));
            Init_Variable(curvar);
            curvar->v_VariableIsParameter = 1;
            strcpy(curvar->v_nomvar,(yyvsp[-2].na));
            strcpy(curvar->v_subroutinename,subroutinename);
            strcpy(curvar->v_modulename,curmodulename);
            curvar->v_initialvalue=Insertname(curvar->v_initialvalue,(yyvsp[0].na),0);
            strcpy(curvar->v_commoninfile,cur_filename);
            Save_Length((yyvsp[0].na),14);
            (yyval.v) = curvar;
        }
#line 5157 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 461:
#line 2594 "fortran.y" /* yacc.c:1646  */
    {pos_cursave = my_position_before;}
#line 5163 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 462:
#line 2595 "fortran.y" /* yacc.c:1646  */
    {
     pos_end = setposcur();
     RemoveWordSET_0(fortran_out,pos_cursave,pos_end-pos_cursave);
     }
#line 5172 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 470:
#line 2616 "fortran.y" /* yacc.c:1646  */
    {if ( ! inside_type_declare ) Add_Save_Var_1((yyvsp[0].na),(listdim*) NULL); }
#line 5178 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 474:
#line 2626 "fortran.y" /* yacc.c:1646  */
    {my_position = my_position_before;}
#line 5184 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 476:
#line 2632 "fortran.y" /* yacc.c:1646  */
    {
            if ( insubroutinedeclare == 1 )
            {
                Add_ImplicitNoneSubroutine_1();
                pos_end = setposcur();
                RemoveWordSET_0(fortran_out,my_position,pos_end-my_position);
            }
        }
#line 5197 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 494:
#line 2684 "fortran.y" /* yacc.c:1646  */
    {in_complex_literal=0;}
#line 5203 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 501:
#line 2699 "fortran.y" /* yacc.c:1646  */
    { positioninblock = 0; pos_curcommon = my_position_before; indeclaration=1;}
#line 5209 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 502:
#line 2700 "fortran.y" /* yacc.c:1646  */
    {
            indeclaration = 0;
            if ( inside_type_declare ) break;
            pos_end = setposcur();
            RemoveWordSET_0(fortran_out,pos_curcommon,pos_end-pos_curcommon);
     }
#line 5220 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 505:
#line 2711 "fortran.y" /* yacc.c:1646  */
    {
     if ( inside_type_declare ) break;
     sprintf(charusemodule,"%s",(yyvsp[0].na));
     Add_NameOfCommon_1((yyvsp[0].na),subroutinename);
     }
#line 5230 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 506:
#line 2719 "fortran.y" /* yacc.c:1646  */
    {
            strcpy((yyval.na),"");
            positioninblock=0;
            strcpy(commonblockname,"");
        }
#line 5240 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 507:
#line 2725 "fortran.y" /* yacc.c:1646  */
    {
            strcpy((yyval.na),(yyvsp[-1].na));
            positioninblock=0;
            strcpy(commonblockname,(yyvsp[-1].na));
        }
#line 5250 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 511:
#line 2738 "fortran.y" /* yacc.c:1646  */
    {
     if ( inside_type_declare ) break;
     sprintf(charusemodule,"%s",(yyvsp[0].na));
     Add_NameOfCommon_1((yyvsp[0].na),subroutinename);
     }
#line 5260 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 513:
#line 2748 "fortran.y" /* yacc.c:1646  */
    {if ( ! inside_type_declare ) Add_Common_var_1(); }
#line 5266 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 514:
#line 2750 "fortran.y" /* yacc.c:1646  */
    {if ( ! inside_type_declare ) Add_Common_var_1(); }
#line 5272 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 515:
#line 2758 "fortran.y" /* yacc.c:1646  */
    {
            positioninblock = positioninblock + 1 ;
            strcpy(commonvar,(yyvsp[0].na));
            commondim = (listdim*) NULL;
        }
#line 5282 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 516:
#line 2763 "fortran.y" /* yacc.c:1646  */
    {in_complex_literal=0;}
#line 5288 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 517:
#line 2764 "fortran.y" /* yacc.c:1646  */
    {
            positioninblock = positioninblock + 1 ;
            strcpy(commonvar,(yyvsp[-4].na));
            commondim = (yyvsp[-1].d);
        }
#line 5298 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 521:
#line 2776 "fortran.y" /* yacc.c:1646  */
    {(yyval.v)=createvar((yyvsp[0].na),NULL);}
#line 5304 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 523:
#line 2788 "fortran.y" /* yacc.c:1646  */
    {if (strcmp(my_dim.last,""))
       {
       (yyval.v)->v_initialvalue_array=Insertname(NULL,my_dim.last,0);
       }
       strcpy(my_dim.last,"");
       }
#line 5315 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 533:
#line 2830 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),"%s(%s)",(yyvsp[-3].na),(yyvsp[-1].na));}
#line 5321 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 534:
#line 2832 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),"%s(%s)",(yyvsp[-3].na),(yyvsp[-1].na));}
#line 5327 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 535:
#line 2847 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),"%s:%s",(yyvsp[-2].na),(yyvsp[0].na));}
#line 5333 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 536:
#line 2852 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),"%s%s",(yyvsp[-1].v)->v_nomvar,(yyvsp[0].na));}
#line 5339 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 537:
#line 2856 "fortran.y" /* yacc.c:1646  */
    {strcpy((yyval.na),"");}
#line 5345 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 538:
#line 2858 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),"%s%%%s",(yyvsp[-2].na),(yyvsp[0].v)->v_nomvar);}
#line 5351 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 539:
#line 2863 "fortran.y" /* yacc.c:1646  */
    {(yyval.v)=createvar((yyvsp[0].na),NULL);}
#line 5357 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 540:
#line 2864 "fortran.y" /* yacc.c:1646  */
    {in_complex_literal=0;}
#line 5363 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 541:
#line 2865 "fortran.y" /* yacc.c:1646  */
    {sprintf(ligne,"%s(%s)",(yyvsp[-4].na),(yyvsp[-1].na));(yyval.v)=createvar((yyvsp[-4].na),NULL);strcpy(my_dim.last,(yyvsp[-1].na));}
#line 5369 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 543:
#line 2881 "fortran.y" /* yacc.c:1646  */
    {strcpy(my_dim.last,"");}
#line 5375 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 544:
#line 2886 "fortran.y" /* yacc.c:1646  */
    {strcpy(my_dim.last,"");}
#line 5381 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 545:
#line 2891 "fortran.y" /* yacc.c:1646  */
    {strcpy(my_dim.last,"");}
#line 5387 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 546:
#line 2893 "fortran.y" /* yacc.c:1646  */
    {strcpy(my_dim.last,"");}
#line 5393 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 547:
#line 2899 "fortran.y" /* yacc.c:1646  */
    {strcpy((yyval.na),"");}
#line 5399 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 548:
#line 2901 "fortran.y" /* yacc.c:1646  */
    {strcpy((yyval.na),(yyvsp[0].na));}
#line 5405 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 549:
#line 2903 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),"%s,%s",(yyvsp[-2].na),(yyvsp[0].na));}
#line 5411 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 550:
#line 2925 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),"%s%s",(yyvsp[-1].na),(yyvsp[0].na));}
#line 5417 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 551:
#line 2927 "fortran.y" /* yacc.c:1646  */
    {strcpy((yyval.na),":");}
#line 5423 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 552:
#line 2929 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),":%s",(yyvsp[0].na));}
#line 5429 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 553:
#line 2931 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),": :%s",(yyvsp[0].na));}
#line 5435 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 554:
#line 2933 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),":%s :%s",(yyvsp[-2].na),(yyvsp[0].na));}
#line 5441 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 555:
#line 2935 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),"::%s",(yyvsp[0].na));}
#line 5447 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 557:
#line 2938 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),"%s=%s",(yyvsp[-2].na),(yyvsp[0].na));}
#line 5453 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 558:
#line 2940 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),"%s=*%s",(yyvsp[-3].na),(yyvsp[0].na));}
#line 5459 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 559:
#line 2942 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),"*%s",(yyvsp[0].na));}
#line 5465 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 560:
#line 2946 "fortran.y" /* yacc.c:1646  */
    {strcpy((yyval.na),":");}
#line 5471 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 561:
#line 2948 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),":%s",(yyvsp[0].na));}
#line 5477 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 562:
#line 2950 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),": :%s",(yyvsp[0].na));}
#line 5483 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 563:
#line 2952 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),":%s :%s",(yyvsp[-2].na),(yyvsp[0].na));}
#line 5489 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 564:
#line 2954 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),"::%s",(yyvsp[0].na));}
#line 5495 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 565:
#line 2956 "fortran.y" /* yacc.c:1646  */
    {strcpy((yyval.na),"");}
#line 5501 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 567:
#line 2974 "fortran.y" /* yacc.c:1646  */
    {in_complex_literal=0;}
#line 5507 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 568:
#line 2975 "fortran.y" /* yacc.c:1646  */
    {inallocate = 0;}
#line 5513 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 592:
#line 3045 "fortran.y" /* yacc.c:1646  */
    {in_complex_literal=0;}
#line 5519 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 593:
#line 3046 "fortran.y" /* yacc.c:1646  */
    {inallocate = 0;}
#line 5525 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 603:
#line 3076 "fortran.y" /* yacc.c:1646  */
    {
      strcpy((yyval.na),(yyvsp[0].v)->v_nomvar);
      if (strcasecmp(my_dim.last,""))
      {
      strcat((yyval.na),"(");
      strcat((yyval.na),my_dim.last);
      strcat((yyval.na),")");
      }
      }
#line 5539 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 607:
#line 3089 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na),"(%s)",(yyvsp[-1].na));}
#line 5545 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 608:
#line 3094 "fortran.y" /* yacc.c:1646  */
    {strcpy(my_dim.last,"");}
#line 5551 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 610:
#line 3100 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),"%s**%s",(yyvsp[-2].na),(yyvsp[0].na));}
#line 5557 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 612:
#line 3105 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na),"%s%s%s",(yyvsp[-2].na),(yyvsp[-1].na),(yyvsp[0].na)); }
#line 5563 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 614:
#line 3113 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na),"%s%s",(yyvsp[-1].na),(yyvsp[0].na)); }
#line 5569 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 615:
#line 3115 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na),"%s%s%s",(yyvsp[-2].na),(yyvsp[-1].na),(yyvsp[0].na)); }
#line 5575 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 617:
#line 3118 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na),"%s%s",(yyvsp[-1].na),(yyvsp[0].na)); }
#line 5581 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 619:
#line 3127 "fortran.y" /* yacc.c:1646  */
    {strcpy((yyval.na),"*");}
#line 5587 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 621:
#line 3133 "fortran.y" /* yacc.c:1646  */
    {strcpy((yyval.na),"+");}
#line 5593 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 622:
#line 3135 "fortran.y" /* yacc.c:1646  */
    {strcpy((yyval.na),"-");}
#line 5599 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 624:
#line 3141 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na),"%s%s%s",(yyvsp[-2].na),(yyvsp[-1].na),(yyvsp[0].na)); }
#line 5605 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 627:
#line 3150 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na),"%s%s%s",(yyvsp[-2].na),(yyvsp[-1].na),(yyvsp[0].na)); }
#line 5611 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 636:
#line 3163 "fortran.y" /* yacc.c:1646  */
    {strcpy((yyval.na),"<");}
#line 5617 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 638:
#line 3166 "fortran.y" /* yacc.c:1646  */
    {strcpy((yyval.na),">");}
#line 5623 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 641:
#line 3174 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na),"%s%s",(yyvsp[-1].na),(yyvsp[0].na)); }
#line 5629 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 643:
#line 3181 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na),"%s%s%s",(yyvsp[-2].na),(yyvsp[-1].na),(yyvsp[0].na)); }
#line 5635 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 645:
#line 3188 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na),"%s%s%s",(yyvsp[-2].na),(yyvsp[-1].na),(yyvsp[0].na)); }
#line 5641 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 647:
#line 3194 "fortran.y" /* yacc.c:1646  */
    { sprintf((yyval.na),"%s%s%s",(yyvsp[-2].na),(yyvsp[-1].na),(yyvsp[0].na)); }
#line 5647 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 657:
#line 3230 "fortran.y" /* yacc.c:1646  */
    {strcpy((yyval.na),"");}
#line 5653 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 660:
#line 3239 "fortran.y" /* yacc.c:1646  */
    {
     strcpy((yyval.na),(yyvsp[0].na));
     }
#line 5661 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 661:
#line 3246 "fortran.y" /* yacc.c:1646  */
    {strcpy((yyval.na),(yyvsp[0].na));}
#line 5667 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 790:
#line 3619 "fortran.y" /* yacc.c:1646  */
    {in_select_case_stmt++;}
#line 5673 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 792:
#line 3620 "fortran.y" /* yacc.c:1646  */
    {in_select_case_stmt++;}
#line 5679 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 796:
#line 3629 "fortran.y" /* yacc.c:1646  */
    {in_select_case_stmt--;}
#line 5685 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 798:
#line 3630 "fortran.y" /* yacc.c:1646  */
    {in_select_case_stmt--;}
#line 5691 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 800:
#line 3635 "fortran.y" /* yacc.c:1646  */
    {in_complex_literal=0;}
#line 5697 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 824:
#line 3698 "fortran.y" /* yacc.c:1646  */
    {close_or_connect = 1;}
#line 5703 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 825:
#line 3698 "fortran.y" /* yacc.c:1646  */
    {close_or_connect = 0;}
#line 5709 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 842:
#line 3728 "fortran.y" /* yacc.c:1646  */
    {close_or_connect = 1;}
#line 5715 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 843:
#line 3729 "fortran.y" /* yacc.c:1646  */
    {close_or_connect = 0;}
#line 5721 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 851:
#line 3746 "fortran.y" /* yacc.c:1646  */
    {
         in_io_control_spec = 0;
         }
#line 5729 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 853:
#line 3751 "fortran.y" /* yacc.c:1646  */
    {
         in_io_control_spec = 0;
         }
#line 5737 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 857:
#line 3761 "fortran.y" /* yacc.c:1646  */
    {
         in_io_control_spec = 0;
         }
#line 5745 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 859:
#line 3766 "fortran.y" /* yacc.c:1646  */
    {
         in_io_control_spec = 0;
         }
#line 5753 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 912:
#line 3883 "fortran.y" /* yacc.c:1646  */
    {in_inquire=0;}
#line 5759 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 914:
#line 3886 "fortran.y" /* yacc.c:1646  */
    {in_inquire=0;}
#line 5765 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 916:
#line 3890 "fortran.y" /* yacc.c:1646  */
    {in_inquire=1;}
#line 5771 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 931:
#line 3917 "fortran.y" /* yacc.c:1646  */
    {pos_endsubroutine=setposcur();}
#line 5777 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 935:
#line 3926 "fortran.y" /* yacc.c:1646  */
    {
            GlobalDeclaration = 0;
            strcpy(curmodulename,(yyvsp[0].na));
            strcpy(subroutinename,"");
            Add_NameOfModule_1((yyvsp[0].na));
            if ( inmoduledeclare == 0 )
            {
                /* To know if there are in the module declaration    */
                inmoduledeclare = 1;
                /* to know if a module has been met                  */
                inmodulemeet = 1;
                /* to know if we are after the keyword contains      */
                aftercontainsdeclare = 0 ;
            }
        }
#line 5797 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 937:
#line 3946 "fortran.y" /* yacc.c:1646  */
    {
            /* if we never meet the contains keyword               */
            if ( firstpass == 0 )
            {
                RemoveWordCUR_0(fortran_out, setposcur()-my_position);    // Remove word "end module"
                if ( inmoduledeclare && ! aftercontainsdeclare )
                {
                    Write_Closing_Module(1);
                }
                fprintf(fortran_out,"\n      end module %s\n", curmodulename);
                if ( module_declar && insubroutinedeclare == 0 )
                {
                    fclose(module_declar);
                }
            }
            inmoduledeclare = 0 ;
            inmodulemeet = 0 ;
            aftercontainsdeclare = 1;
            strcpy(curmodulename, "");
            GlobalDeclaration = 0 ;
        }
#line 5823 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 952:
#line 3998 "fortran.y" /* yacc.c:1646  */
    {if (firstpass == 0 && oldfortran_out) pos_curuseold = setposcurname(oldfortran_out);}
#line 5829 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 953:
#line 4003 "fortran.y" /* yacc.c:1646  */
    {
            if ( firstpass )
            {
                if ( insubroutinedeclare )
                {
                    if ((yyvsp[0].lc)) {
                      Add_CouplePointed_Var_1((yyvsp[-1].na),(yyvsp[0].lc));
                      coupletmp = (yyvsp[0].lc);
                      strcpy(ligne,"");
                      while ( coupletmp )
                      {
                        strcat(ligne, coupletmp->c_namevar);
                        strcat(ligne, " => ");
                        strcat(ligne, coupletmp->c_namepointedvar);
                        coupletmp = coupletmp->suiv;
                        if ( coupletmp ) strcat(ligne,",");
                      }
                      }
                  sprintf(charusemodule,"%s",(yyvsp[-1].na));
                }
                Add_NameOfModuleUsed_1((yyvsp[-1].na));
            }
            else
            {
                if ( insubroutinedeclare )
                {
                  copyuse_0((yyvsp[-1].na));
                    }

                if ( inmoduledeclare == 0 )
                {
                    pos_end = setposcur();
                    RemoveWordSET_0(fortran_out,my_position,pos_end-my_position);
                }
            }
    }
#line 5870 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 955:
#line 4041 "fortran.y" /* yacc.c:1646  */
    {
            if ( firstpass )
            {
                if ( insubroutinedeclare )
                {
                  if ((yyvsp[0].lc))
                  {
                    Add_CouplePointed_Var_1((yyvsp[-4].na),(yyvsp[0].lc));
                    coupletmp = (yyvsp[0].lc);
                    strcpy(ligne,"");
                    while ( coupletmp )
                    {
                        strcat(ligne,coupletmp->c_namevar);
                        if ( strcasecmp(coupletmp->c_namepointedvar,"") )   strcat(ligne," => ");
                        strcat(ligne,coupletmp->c_namepointedvar);
                        coupletmp = coupletmp->suiv;
                        if ( coupletmp ) strcat(ligne,",");
                    }
                  }
                  sprintf(charusemodule,"%s",(yyvsp[-4].na));
                }
                Add_NameOfModuleUsed_1((yyvsp[-4].na));
            }
            else
            {
                if ( insubroutinedeclare )
                    copyuseonly_0((yyvsp[-4].na));

                if ( inmoduledeclare == 0 )
                {
                    pos_end = setposcur();
                    RemoveWordSET_0(fortran_out,my_position,pos_end-my_position);
                    if ((yyvsp[0].lc))
                    {
                    if (oldfortran_out)  variableisglobalinmodule((yyvsp[0].lc),(yyvsp[-4].na),oldfortran_out,pos_curuseold);
                    }
                }
                else
                {
                  if ((yyvsp[0].lc))
                  {
                    /* if we are in the module declare and if the    */
                    /* onlylist is a list of global variable         */
                    variableisglobalinmodule((yyvsp[0].lc), (yyvsp[-4].na), fortran_out,my_position);
                  }
                }
            }
    }
#line 5923 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 960:
#line 4098 "fortran.y" /* yacc.c:1646  */
    {(yyval.lc)=NULL;}
#line 5929 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 961:
#line 4100 "fortran.y" /* yacc.c:1646  */
    {(yyval.lc)=(yyvsp[0].lc);}
#line 5935 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 967:
#line 4117 "fortran.y" /* yacc.c:1646  */
    {
            strcpy(subroutinename,(yyvsp[0].na));
            insubroutinedeclare = 1;
            inprogramdeclare = 1;
            /* in the second step we should write the head of       */
            /*    the subroutine sub_loop_<subroutinename>          */
            if ( ! firstpass )
                WriteBeginof_SubLoop();
        }
#line 5949 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 969:
#line 4130 "fortran.y" /* yacc.c:1646  */
    {pos_endsubroutine=my_position_before;}
#line 5955 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 970:
#line 4131 "fortran.y" /* yacc.c:1646  */
    {
            insubroutinedeclare = 0;
            inprogramdeclare = 0;
            pos_cur = setposcur();
            closeandcallsubloopandincludeit_0(3);
            functiondeclarationisdone = 0;
            strcpy(subroutinename,"");     
     }
#line 5968 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 977:
#line 4153 "fortran.y" /* yacc.c:1646  */
    {
    (yyval.lc)=NULL;
    }
#line 5976 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 978:
#line 4157 "fortran.y" /* yacc.c:1646  */
    {
    (yyval.lc)=(yyvsp[0].lc);
    }
#line 5984 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 979:
#line 4163 "fortran.y" /* yacc.c:1646  */
    {
     (yyval.lc)=(yyvsp[0].lc);
     }
#line 5992 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 980:
#line 4167 "fortran.y" /* yacc.c:1646  */
    {
     /* insert the variable in the list $1                 */
     (yyvsp[0].lc)->suiv = (yyvsp[-2].lc);
     (yyval.lc)=(yyvsp[0].lc);
     }
#line 6002 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 981:
#line 4176 "fortran.y" /* yacc.c:1646  */
    {
            coupletmp = (listcouple *) calloc(1,sizeof(listcouple));
            strcpy(coupletmp->c_namevar,(yyvsp[-2].na));
            strcpy(coupletmp->c_namepointedvar,(yyvsp[0].na));
            coupletmp->suiv = NULL;
            (yyval.lc) = coupletmp;
        }
#line 6014 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 982:
#line 4186 "fortran.y" /* yacc.c:1646  */
    {(yyval.lc)=(yyvsp[0].lc);}
#line 6020 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 983:
#line 4188 "fortran.y" /* yacc.c:1646  */
    {
            /* insert the variable in the list $1                 */
            (yyvsp[0].lc)->suiv = (yyvsp[-2].lc);
            (yyval.lc) = (yyvsp[0].lc);
        }
#line 6030 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 984:
#line 4197 "fortran.y" /* yacc.c:1646  */
    {
            coupletmp = (listcouple *)calloc(1,sizeof(listcouple));
            strcpy(coupletmp->c_namevar,(yyvsp[0].na));
            strcpy(coupletmp->c_namepointedvar,"");
            coupletmp->suiv = NULL;
            (yyval.lc) = coupletmp;
        }
#line 6042 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 985:
#line 4205 "fortran.y" /* yacc.c:1646  */
    {
            coupletmp = (listcouple *)calloc(1,sizeof(listcouple));
            strcpy(coupletmp->c_namevar,(yyvsp[0].na));
            strcpy(coupletmp->c_namepointedvar,"");
            coupletmp->suiv = NULL;
            (yyval.lc) = coupletmp;
        }
#line 6054 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 986:
#line 4213 "fortran.y" /* yacc.c:1646  */
    {
     (yyval.lc)=(yyvsp[0].lc);
     pointedvar = 1;
      Add_UsedInSubroutine_Var_1((yyvsp[0].lc)->c_namevar);
     }
#line 6064 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 999:
#line 4253 "fortran.y" /* yacc.c:1646  */
    {in_complex_literal=0;}
#line 6070 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 1000:
#line 4254 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),"%s(%s)",(yyvsp[-4].na),(yyvsp[-1].na));}
#line 6076 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 1001:
#line 4260 "fortran.y" /* yacc.c:1646  */
    {
            inagrifcallargument = 0 ;
            incalldeclare=0;
            if ( oldfortran_out && (callagrifinitgrids == 1) && (firstpass == 0) )
            {
                pos_end = setposcur();
                RemoveWordSET_0(fortran_out,pos_curcall,pos_end-pos_curcall);
                strcpy(subofagrifinitgrids,subroutinename);
            }
            Instanciation_0(sameagrifname);
        }
#line 6092 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 1003:
#line 4273 "fortran.y" /* yacc.c:1646  */
    {
            inagrifcallargument = 0 ;
            incalldeclare=0;
            if ( oldfortran_out && (callagrifinitgrids == 1) && (firstpass == 0) )
            {
                pos_end = setposcur();
                RemoveWordSET_0(fortran_out,pos_curcall,pos_end-pos_curcall);
                strcpy(subofagrifinitgrids,subroutinename);
            }
            Instanciation_0(sameagrifname);
        }
#line 6108 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 1005:
#line 4285 "fortran.y" /* yacc.c:1646  */
    {in_complex_literal=0;}
#line 6114 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 1006:
#line 4286 "fortran.y" /* yacc.c:1646  */
    {
            inagrifcallargument = 0 ;
            incalldeclare=0;
            if ( oldfortran_out && (callagrifinitgrids == 1) && (firstpass == 0) )
            {
                pos_end = setposcur();
                RemoveWordSET_0(fortran_out,pos_curcall,pos_end-pos_curcall);
                strcpy(subofagrifinitgrids,subroutinename);
            }
            Instanciation_0(sameagrifname);
        }
#line 6130 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 1008:
#line 4300 "fortran.y" /* yacc.c:1646  */
    {pos_curcall=my_position_before-strlen((yyvsp[-1].na))-4;}
#line 6136 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 1009:
#line 4301 "fortran.y" /* yacc.c:1646  */
    {
            if (!strcasecmp((yyvsp[0].na),"MPI_Init") )    callmpiinit = 1;
            else                                callmpiinit = 0;

            if (!strcasecmp((yyvsp[0].na),"Agrif_Init_Grids") )
            {
                callagrifinitgrids = 1;
                strcpy(meetagrifinitgrids,subroutinename);
            }
            else
            {
                callagrifinitgrids = 0;
            }
            if ( Vartonumber((yyvsp[0].na)) == 1 )
            {
                incalldeclare = 0;
                inagrifcallargument = 0 ;
                Add_SubroutineWhereAgrifUsed_1(subroutinename, curmodulename);
            }
        }
#line 6161 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 1014:
#line 4332 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),"%s,%s",(yyvsp[-2].na),(yyvsp[0].na));}
#line 6167 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 1015:
#line 4337 "fortran.y" /* yacc.c:1646  */
    {
            if ( callmpiinit == 1 )
            {
                strcpy(mpiinitvar,(yyvsp[0].na));
                if ( firstpass == 1 )  Add_UsedInSubroutine_Var_1 (mpiinitvar);
            }
        }
#line 6179 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 1016:
#line 4345 "fortran.y" /* yacc.c:1646  */
    {sprintf((yyval.na),"%s = %s",(yyvsp[-2].na),(yyvsp[0].na));
                 if ( callmpiinit == 1 )
            {
                strcpy(mpiinitvar,(yyvsp[0].na));
                if ( firstpass == 1 )  Add_UsedInSubroutine_Var_1 (mpiinitvar);
            }
            }
#line 6191 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 1018:
#line 4357 "fortran.y" /* yacc.c:1646  */
    {
     strcpy((yyval.na),(yyvsp[0].v)->v_nomvar);
     if ((yyvsp[0].v)->v_initialvalue_array)
     {
     strcat((yyval.na),"(");
     strcat((yyval.na),(yyvsp[0].v)->v_initialvalue_array->n_name);
     strcat((yyval.na),")");
     }
     }
#line 6205 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 1020:
#line 4369 "fortran.y" /* yacc.c:1646  */
    {isrecursive = 0;}
#line 6211 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 1024:
#line 4380 "fortran.y" /* yacc.c:1646  */
    {isrecursive = 0; functiondeclarationisdone = 1;}
#line 6217 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 1025:
#line 4382 "fortran.y" /* yacc.c:1646  */
    {isrecursive = 0;}
#line 6223 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 1026:
#line 4384 "fortran.y" /* yacc.c:1646  */
    {isrecursive = 1;}
#line 6229 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 1028:
#line 4393 "fortran.y" /* yacc.c:1646  */
    {in_complex_literal=0;}
#line 6235 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 1029:
#line 4394 "fortran.y" /* yacc.c:1646  */
    {
            insubroutinedeclare = 1;
            suborfun = 0;
            /* we should to list of the subroutine argument the  */
            /*    name of the function which has to be defined   */
            if ( firstpass )
            {
                Add_SubroutineArgument_Var_1((yyvsp[-2].l));
                if ( ! is_result_present )
                    Add_FunctionType_Var_1((yyvsp[-5].na));
            }
            else
            /* in the second step we should write the head of    */
            /*    the subroutine sub_loop_<subroutinename>       */
               {
                if (todebug == 1) fprintf(fortran_out,"      !DEBUG: Avant Writebeginof subloop\n");
                WriteBeginof_SubLoop();
                if (todebug == 1) fprintf(fortran_out,"      !DEBUG: Apres Writebeginof subloop\n");
                }
                strcpy(NamePrecision,"");
     }
#line 6261 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 1031:
#line 4419 "fortran.y" /* yacc.c:1646  */
    {
     if (strcmp(subroutinename,""))
     {
     strcpy(old_subroutinename,subroutinename); // can occur in internal-subprogram
     old_oldfortran_out=oldfortran_out;
     }
     else
     {
     old_oldfortran_out=(FILE *)NULL;
     }
     strcpy((yyval.na),(yyvsp[0].na));strcpy(subroutinename,(yyvsp[0].na));
     }
#line 6278 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 1032:
#line 4444 "fortran.y" /* yacc.c:1646  */
    {strcpy((yyval.na),(yyvsp[0].na));}
#line 6284 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 1033:
#line 4448 "fortran.y" /* yacc.c:1646  */
    {is_result_present = 0; }
#line 6290 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 1035:
#line 4454 "fortran.y" /* yacc.c:1646  */
    {is_result_present = 1;
                 if ( firstpass == 1 )
            {
                strcpy(nameinttypenameback,nameinttypename);
                strcpy(nameinttypename,"");
                curvar = createvar((yyvsp[-1].na),NULL);
                strcpy(nameinttypename,nameinttypenameback);
                strcpy(curvar->v_typevar,"");
                curlistvar = insertvar(NULL,curvar);
                Add_SubroutineArgument_Var_1(curlistvar);
            }
     }
#line 6307 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 1036:
#line 4470 "fortran.y" /* yacc.c:1646  */
    {strcpy(DeclType, "");}
#line 6313 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 1041:
#line 4484 "fortran.y" /* yacc.c:1646  */
    {
            insubroutinedeclare = 1;
            suborfun = 1;
            if ( firstpass )
                Add_SubroutineArgument_Var_1((yyvsp[0].l));
            else
              {
                WriteBeginof_SubLoop();
              }
        }
#line 6328 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 1043:
#line 4499 "fortran.y" /* yacc.c:1646  */
    {
     if (strcmp(subroutinename,""))
     {
     strcpy(old_subroutinename,subroutinename); // can occur in internal-subprogram
     old_oldfortran_out=oldfortran_out;
     }
     else
     {
     old_oldfortran_out=(FILE *)NULL;
     }
     strcpy((yyval.na),(yyvsp[0].na));strcpy(subroutinename,(yyvsp[0].na));
     }
#line 6345 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 1045:
#line 4520 "fortran.y" /* yacc.c:1646  */
    {pos_endsubroutine = my_position;
            GlobalDeclaration = 0 ;
            if ( firstpass == 0 && strcasecmp(subroutinename,"") )
            {
                if ( module_declar && insubroutinedeclare == 0 )    fclose(module_declar);
            }
            if ( strcasecmp(subroutinename,"") )
            {
                if ( inmodulemeet == 1 )
                {
                    /* we are in a module                                */
                    if ( insubroutinedeclare == 1 )
                    {
                        /* it is like an end subroutine <name>            */
                        insubroutinedeclare = 0 ;
                        pos_cur = setposcur();
                        closeandcallsubloopandincludeit_0(suborfun);
                        functiondeclarationisdone = 0;
                    }
                    else
                    {
                        /* it is like an end module <name>                */
                        inmoduledeclare = 0 ;
                        inmodulemeet = 0 ;
                    }
                }
                else
                {
                    insubroutinedeclare = 0;
                    pos_cur = setposcur();
                    closeandcallsubloopandincludeit_0(2);
                    functiondeclarationisdone = 0;
                }
            }
            strcpy(subroutinename,"");
            if (strcmp(old_subroutinename,""))
            {
            strcpy(subroutinename,old_subroutinename);
            strcpy(old_subroutinename,"");
            oldfortran_out=old_oldfortran_out;
            insubroutinedeclare=1;
            }
        }
#line 6393 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 1048:
#line 4569 "fortran.y" /* yacc.c:1646  */
    {if (firstpass) (yyval.l)=NULL;}
#line 6399 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 1049:
#line 4570 "fortran.y" /* yacc.c:1646  */
    {in_complex_literal=0;}
#line 6405 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 1050:
#line 4571 "fortran.y" /* yacc.c:1646  */
    {if (firstpass) (yyval.l)=(yyvsp[-1].l);}
#line 6411 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 1051:
#line 4575 "fortran.y" /* yacc.c:1646  */
    {if (firstpass) (yyval.l)=NULL;}
#line 6417 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 1052:
#line 4577 "fortran.y" /* yacc.c:1646  */
    {if (firstpass) (yyval.l)=(yyvsp[0].l);}
#line 6423 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 1053:
#line 4582 "fortran.y" /* yacc.c:1646  */
    {
            if ( firstpass == 1 )
            {
                strcpy(nameinttypenameback,nameinttypename);
                strcpy(nameinttypename,"");
                curvar = createvar((yyvsp[0].na),NULL);
                strcpy(nameinttypename,nameinttypenameback);
                curlistvar = insertvar(NULL,curvar);
                (yyval.l) = settype("",curlistvar);
            }
        }
#line 6439 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 1054:
#line 4594 "fortran.y" /* yacc.c:1646  */
    {
            if ( firstpass == 1 )
            {
                strcpy(nameinttypenameback,nameinttypename);
                strcpy(nameinttypename,"");
                curvar = createvar((yyvsp[0].na),NULL);
                strcpy(nameinttypename,nameinttypenameback);
                (yyval.l) = insertvar((yyvsp[-2].l),curvar);
            }
        }
#line 6454 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 1055:
#line 4608 "fortran.y" /* yacc.c:1646  */
    {strcpy((yyval.na),(yyvsp[0].na));}
#line 6460 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 1056:
#line 4610 "fortran.y" /* yacc.c:1646  */
    {strcpy((yyval.na),"*");}
#line 6466 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 1059:
#line 4620 "fortran.y" /* yacc.c:1646  */
    {
            if ( inside_type_declare ) break;
            if ( inmoduledeclare )
            {
                if ( firstpass == 0 )
                {
                    RemoveWordCUR_0(fortran_out,9);   // Remove word 'contains'
                    Write_Closing_Module(0);
                }
                inmoduledeclare = 0 ;
                aftercontainsdeclare = 1;
            }
            else if ( insubroutinedeclare )
            {
                incontainssubroutine = 1;
                insubroutinedeclare  = 0;
                incontainssubroutine = 0;
                functiondeclarationisdone = 0;

                if ( firstpass )
                    List_ContainsSubroutine = Addtolistnom(subroutinename, List_ContainsSubroutine, 0);
                else
                    closeandcallsubloop_contains_0();

                strcpy(subroutinename, "");
            }
            else printf("l.%4d -- TOK_CONTAINS -- MHCHECK\n",line_num_input);
        }
#line 6499 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 1061:
#line 4655 "fortran.y" /* yacc.c:1646  */
    {strcpy((yyval.na),"");}
#line 6505 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 1062:
#line 4656 "fortran.y" /* yacc.c:1646  */
    {strcpy((yyval.na),(yyvsp[0].na));}
#line 6511 "fortran.tab.c" /* yacc.c:1646  */
    break;

  case 1068:
#line 4784 "fortran.y" /* yacc.c:1646  */
    { afterpercent = 1; }
#line 6517 "fortran.tab.c" /* yacc.c:1646  */
    break;


#line 6521 "fortran.tab.c" /* yacc.c:1646  */
      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYEMPTY : YYTRANSLATE (yychar);

  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
# define YYSYNTAX_ERROR yysyntax_error (&yymsg_alloc, &yymsg, \
                                        yyssp, yytoken)
      {
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = YYSYNTAX_ERROR;
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == 1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = (char *) YYSTACK_ALLOC (yymsg_alloc);
            if (!yymsg)
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = 2;
              }
            else
              {
                yysyntax_error_status = YYSYNTAX_ERROR;
                yymsgp = yymsg;
              }
          }
        yyerror (yymsgp);
        if (yysyntax_error_status == 2)
          goto yyexhaustedlab;
      }
# undef YYSYNTAX_ERROR
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYTERROR;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;


      yydestruct ("Error: popping",
                  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#if !defined yyoverflow || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  yystos[*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  return yyresult;
}
#line 4881 "fortran.y" /* yacc.c:1906  */


void process_fortran(const char *input_file)
{
    extern FILE *fortran_in;
    extern FILE *fortran_out;

    char output_file[LONG_FNAME];
    char input_fullpath[LONG_FNAME];

    if ( todebug == 1 ) printf("Firstpass == %d \n", firstpass);

     yydebug=0;
/******************************************************************************/
/*  1-  Open input file                                                       */
/******************************************************************************/

    strcpy(cur_filename, input_file);
    sprintf(input_fullpath, "%s/%s", input_dir, input_file);

    fortran_in = fopen(input_fullpath, "r");
    if (! fortran_in)
    {
        printf("Error : File %s does not exist\n", input_fullpath);
        exit(1);
    }

/******************************************************************************/
/*  2-  Variables initialization                                              */
/******************************************************************************/

    line_num_input = 1;
    PublicDeclare = 0;
    PrivateDeclare = 0;
    ExternalDeclare = 0;
    SaveDeclare = 0;
    pointerdeclare = 0;
    optionaldeclare = 0;
    incalldeclare = 0;
    inside_type_declare = 0;
    Allocatabledeclare = 0 ;
    Targetdeclare = 0 ;
    VariableIsParameter =  0 ;
    strcpy(NamePrecision,"");
    c_star = 0 ;
    functiondeclarationisdone = 0;
    insubroutinedeclare = 0 ;
    strcpy(subroutinename," ");
    isrecursive = 0;
    InitialValueGiven = 0 ;
    GlobalDeclarationType = 0;
    inmoduledeclare = 0;
    incontainssubroutine = 0;
    afterpercent = 0;
    aftercontainsdeclare = 1;
    strcpy(nameinttypename,"");

/******************************************************************************/
/*  3-  Parsing of the input file (1 time)                                    */
/******************************************************************************/

    sprintf(output_file, "%s/%s", output_dir, input_file);

    if (firstpass == 0) fortran_out = fopen(output_file,"w");

    fortran_parse();

    if (firstpass == 0) NewModule_Creation_0();
    if (firstpass == 0) fclose(fortran_out);
}
#line 2 "fortran.yy.c"

#line 4 "fortran.yy.c"

#define  YY_INT_ALIGNED short int

/* A lexical scanner generated by flex */

#define yy_create_buffer fortran__create_buffer
#define yy_delete_buffer fortran__delete_buffer
#define yy_flex_debug fortran__flex_debug
#define yy_init_buffer fortran__init_buffer
#define yy_flush_buffer fortran__flush_buffer
#define yy_load_buffer_state fortran__load_buffer_state
#define yy_switch_to_buffer fortran__switch_to_buffer
#define yyin fortran_in
#define yyleng fortran_leng
#define yylex fortran_lex
#define yylineno fortran_lineno
#define yyout fortran_out
#define yyrestart fortran_restart
#define yytext fortran_text
#define yywrap fortran_wrap
#define yyalloc fortran_alloc
#define yyrealloc fortran_realloc
#define yyfree fortran_free

#define FLEX_SCANNER
#define YY_FLEX_MAJOR_VERSION 2
#define YY_FLEX_MINOR_VERSION 5
#define YY_FLEX_SUBMINOR_VERSION 35
#if YY_FLEX_SUBMINOR_VERSION > 0
#define FLEX_BETA
#endif

/* First, we deal with  platform-specific or compiler-specific issues. */

/* begin standard C headers. */
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <stdlib.h>

/* end standard C headers. */

/* flex integer type definitions */

#ifndef FLEXINT_H
#define FLEXINT_H

/* C99 systems have <inttypes.h>. Non-C99 systems may or may not. */

#if defined (__STDC_VERSION__) && __STDC_VERSION__ >= 199901L

/* C99 says to define __STDC_LIMIT_MACROS before including stdint.h,
 * if you want the limit (max/min) macros for int types. 
 */
#ifndef __STDC_LIMIT_MACROS
#define __STDC_LIMIT_MACROS 1
#endif

#include <inttypes.h>
typedef int8_t flex_int8_t;
typedef uint8_t flex_uint8_t;
typedef int16_t flex_int16_t;
typedef uint16_t flex_uint16_t;
typedef int32_t flex_int32_t;
typedef uint32_t flex_uint32_t;
typedef uint64_t flex_uint64_t;
#else
typedef signed char flex_int8_t;
typedef short int flex_int16_t;
typedef int flex_int32_t;
typedef unsigned char flex_uint8_t; 
typedef unsigned short int flex_uint16_t;
typedef unsigned int flex_uint32_t;
#endif /* ! C99 */

/* Limits of integral types. */
#ifndef INT8_MIN
#define INT8_MIN               (-128)
#endif
#ifndef INT16_MIN
#define INT16_MIN              (-32767-1)
#endif
#ifndef INT32_MIN
#define INT32_MIN              (-2147483647-1)
#endif
#ifndef INT8_MAX
#define INT8_MAX               (127)
#endif
#ifndef INT16_MAX
#define INT16_MAX              (32767)
#endif
#ifndef INT32_MAX
#define INT32_MAX              (2147483647)
#endif
#ifndef UINT8_MAX
#define UINT8_MAX              (255U)
#endif
#ifndef UINT16_MAX
#define UINT16_MAX             (65535U)
#endif
#ifndef UINT32_MAX
#define UINT32_MAX             (4294967295U)
#endif

#endif /* ! FLEXINT_H */

#ifdef __cplusplus

/* The "const" storage-class-modifier is valid. */
#define YY_USE_CONST

#else	/* ! __cplusplus */

/* C99 requires __STDC__ to be defined as 1. */
#if defined (__STDC__)

#define YY_USE_CONST

#endif	/* defined (__STDC__) */
#endif	/* ! __cplusplus */

#ifdef YY_USE_CONST
#define yyconst const
#else
#define yyconst
#endif

/* Returned upon end-of-file. */
#define YY_NULL 0

/* Promotes a possibly negative, possibly signed char to an unsigned
 * integer for use as an array index.  If the signed char is negative,
 * we want to instead treat it as an 8-bit unsigned char, hence the
 * double cast.
 */
#define YY_SC_TO_UI(c) ((unsigned int) (unsigned char) c)

/* Enter a start condition.  This macro really ought to take a parameter,
 * but we do it the disgusting crufty way forced on us by the ()-less
 * definition of BEGIN.
 */
#define BEGIN (yy_start) = 1 + 2 *

/* Translate the current start state into a value that can be later handed
 * to BEGIN to return to the state.  The YYSTATE alias is for lex
 * compatibility.
 */
#define YY_START (((yy_start) - 1) / 2)
#define YYSTATE YY_START

/* Action number for EOF rule of a given start state. */
#define YY_STATE_EOF(state) (YY_END_OF_BUFFER + state + 1)

/* Special action meaning "start processing a new file". */
#define YY_NEW_FILE fortran_restart(fortran_in  )

#define YY_END_OF_BUFFER_CHAR 0

/* Size of default input buffer. */
#ifndef YY_BUF_SIZE
#define YY_BUF_SIZE 16384
#endif

/* The state buf must be large enough to hold one state per character in the main buffer.
 */
#define YY_STATE_BUF_SIZE   ((YY_BUF_SIZE + 2) * sizeof(yy_state_type))

#ifndef YY_TYPEDEF_YY_BUFFER_STATE
#define YY_TYPEDEF_YY_BUFFER_STATE
typedef struct yy_buffer_state *YY_BUFFER_STATE;
#endif

#ifndef YY_TYPEDEF_YY_SIZE_T
#define YY_TYPEDEF_YY_SIZE_T
typedef size_t yy_size_t;
#endif

extern yy_size_t fortran_leng;

extern FILE *fortran_in, *fortran_out;

#define EOB_ACT_CONTINUE_SCAN 0
#define EOB_ACT_END_OF_FILE 1
#define EOB_ACT_LAST_MATCH 2

    #define YY_LESS_LINENO(n)
    
/* Return all but the first "n" matched characters back to the input stream. */
#define yyless(n) \
	do \
		{ \
		/* Undo effects of setting up fortran_text. */ \
        int yyless_macro_arg = (n); \
        YY_LESS_LINENO(yyless_macro_arg);\
		*yy_cp = (yy_hold_char); \
		YY_RESTORE_YY_MORE_OFFSET \
		(yy_c_buf_p) = yy_cp = yy_bp + yyless_macro_arg - YY_MORE_ADJ; \
		YY_DO_BEFORE_ACTION; /* set up fortran_text again */ \
		} \
	while ( 0 )

#define unput(c) yyunput( c, (yytext_ptr)  )

#ifndef YY_STRUCT_YY_BUFFER_STATE
#define YY_STRUCT_YY_BUFFER_STATE
struct yy_buffer_state
	{
	FILE *yy_input_file;

	char *yy_ch_buf;		/* input buffer */
	char *yy_buf_pos;		/* current position in input buffer */

	/* Size of input buffer in bytes, not including room for EOB
	 * characters.
	 */
	yy_size_t yy_buf_size;

	/* Number of characters read into yy_ch_buf, not including EOB
	 * characters.
	 */
	yy_size_t yy_n_chars;

	/* Whether we "own" the buffer - i.e., we know we created it,
	 * and can realloc() it to grow it, and should free() it to
	 * delete it.
	 */
	int yy_is_our_buffer;

	/* Whether this is an "interactive" input source; if so, and
	 * if we're using stdio for input, then we want to use getc()
	 * instead of fread(), to make sure we stop fetching input after
	 * each newline.
	 */
	int yy_is_interactive;

	/* Whether we're considered to be at the beginning of a line.
	 * If so, '^' rules will be active on the next match, otherwise
	 * not.
	 */
	int yy_at_bol;

    int yy_bs_lineno; /**< The line count. */
    int yy_bs_column; /**< The column count. */
    
	/* Whether to try to fill the input buffer when we reach the
	 * end of it.
	 */
	int yy_fill_buffer;

	int yy_buffer_status;

#define YY_BUFFER_NEW 0
#define YY_BUFFER_NORMAL 1
	/* When an EOF's been seen but there's still some text to process
	 * then we mark the buffer as YY_EOF_PENDING, to indicate that we
	 * shouldn't try reading from the input source any more.  We might
	 * still have a bunch of tokens to match, though, because of
	 * possible backing-up.
	 *
	 * When we actually see the EOF, we change the status to "new"
	 * (via fortran_restart()), so that the user can continue scanning by
	 * just pointing fortran_in at a new input file.
	 */
#define YY_BUFFER_EOF_PENDING 2

	};
#endif /* !YY_STRUCT_YY_BUFFER_STATE */

/* Stack of input buffers. */
static size_t yy_buffer_stack_top = 0; /**< index of top of stack. */
static size_t yy_buffer_stack_max = 0; /**< capacity of stack. */
static YY_BUFFER_STATE * yy_buffer_stack = 0; /**< Stack as an array. */

/* We provide macros for accessing buffer states in case in the
 * future we want to put the buffer states in a more general
 * "scanner state".
 *
 * Returns the top of the stack, or NULL.
 */
#define YY_CURRENT_BUFFER ( (yy_buffer_stack) \
                          ? (yy_buffer_stack)[(yy_buffer_stack_top)] \
                          : NULL)

/* Same as previous macro, but useful when we know that the buffer stack is not
 * NULL or when we need an lvalue. For internal use only.
 */
#define YY_CURRENT_BUFFER_LVALUE (yy_buffer_stack)[(yy_buffer_stack_top)]

/* yy_hold_char holds the character lost when fortran_text is formed. */
static char yy_hold_char;
static yy_size_t yy_n_chars;		/* number of characters read into yy_ch_buf */
yy_size_t fortran_leng;

/* Points to current character in buffer. */
static char *yy_c_buf_p = (char *) 0;
static int yy_init = 0;		/* whether we need to initialize */
static int yy_start = 0;	/* start state number */

/* Flag which is used to allow fortran_wrap()'s to do buffer switches
 * instead of setting up a fresh fortran_in.  A bit of a hack ...
 */
static int yy_did_buffer_switch_on_eof;

void fortran_restart (FILE *input_file  );
void fortran__switch_to_buffer (YY_BUFFER_STATE new_buffer  );
YY_BUFFER_STATE fortran__create_buffer (FILE *file,int size  );
void fortran__delete_buffer (YY_BUFFER_STATE b  );
void fortran__flush_buffer (YY_BUFFER_STATE b  );
void fortran_push_buffer_state (YY_BUFFER_STATE new_buffer  );
void fortran_pop_buffer_state (void );

static void fortran_ensure_buffer_stack (void );
static void fortran__load_buffer_state (void );
static void fortran__init_buffer (YY_BUFFER_STATE b,FILE *file  );

#define YY_FLUSH_BUFFER fortran__flush_buffer(YY_CURRENT_BUFFER )

YY_BUFFER_STATE fortran__scan_buffer (char *base,yy_size_t size  );
YY_BUFFER_STATE fortran__scan_string (yyconst char *yy_str  );
YY_BUFFER_STATE fortran__scan_bytes (yyconst char *bytes,yy_size_t len  );

void *fortran_alloc (yy_size_t  );
void *fortran_realloc (void *,yy_size_t  );
void fortran_free (void *  );

#define yy_new_buffer fortran__create_buffer

#define yy_set_interactive(is_interactive) \
	{ \
	if ( ! YY_CURRENT_BUFFER ){ \
        fortran_ensure_buffer_stack (); \
		YY_CURRENT_BUFFER_LVALUE =    \
            fortran__create_buffer(fortran_in,YY_BUF_SIZE ); \
	} \
	YY_CURRENT_BUFFER_LVALUE->yy_is_interactive = is_interactive; \
	}

#define yy_set_bol(at_bol) \
	{ \
	if ( ! YY_CURRENT_BUFFER ){\
        fortran_ensure_buffer_stack (); \
		YY_CURRENT_BUFFER_LVALUE =    \
            fortran__create_buffer(fortran_in,YY_BUF_SIZE ); \
	} \
	YY_CURRENT_BUFFER_LVALUE->yy_at_bol = at_bol; \
	}

#define YY_AT_BOL() (YY_CURRENT_BUFFER_LVALUE->yy_at_bol)

/* Begin user sect3 */

#define fortran_wrap(n) 1
#define YY_SKIP_YYWRAP

typedef unsigned char YY_CHAR;

FILE *fortran_in = (FILE *) 0, *fortran_out = (FILE *) 0;

typedef int yy_state_type;

extern int fortran_lineno;

int fortran_lineno = 1;

extern char *fortran_text;
#define yytext_ptr fortran_text

static yy_state_type yy_get_previous_state (void );
static yy_state_type yy_try_NUL_trans (yy_state_type current_state  );
static int yy_get_next_buffer (void );
static void yy_fatal_error (yyconst char msg[]  );

/* Done after the current pattern has been matched and before the
 * corresponding action - sets up fortran_text.
 */
#define YY_DO_BEFORE_ACTION \
	(yytext_ptr) = yy_bp; \
	fortran_leng = (yy_size_t) (yy_cp - yy_bp); \
	(yy_hold_char) = *yy_cp; \
	*yy_cp = '\0'; \
	(yy_c_buf_p) = yy_cp;

#define YY_NUM_RULES 178
#define YY_END_OF_BUFFER 179
/* This struct is not used in this scanner,
   but its presence is necessary. */
struct yy_trans_info
	{
	flex_int32_t yy_verify;
	flex_int32_t yy_nxt;
	};
static yyconst flex_int16_t yy_acclist[1588] =
    {   0,
      143,  143,  179,  178,  167,  178,  166,  178,  177,  178,
      178,  156,  178,  160,  178,  170,  178,  178,  159,  178,
      159,  178,  159,  178,  162,  178,  157,  178,  140,  178,
      155,  178,  159,  178,  161,  178,  164,  178,  163,  178,
      165,  178,  151,  178,  151,  178,  151,  178,  151,  178,
      151,  178,  151,  178,  151,  178,  151,  178,  151,  178,
      151,  178,  151,  178,  151,  178,  151,  178,  151,  178,
      151,  178,  151,  178,  151,  178,  151,  178,  151,  178,
      151,  178,  151,  178,  167,  178,  166,  176,  178,  177,
      178,  151,  178,  151,  178,  151,  178,  151,  178,  151,

      178,  178,  178,  174,  178,  178,  178,  149,  178,  178,
      178,  143,  178,  144,  178,  178,  166,  178,  151,  178,
      151,  178,  151,  178,  151,  178,  151,  178,  151,  178,
      151,  178,  151,  178,  151,  178,  151,  178,  151,  178,
      151,  178,  151,  178,  151,  178,  151,  178,  151,  178,
      151,  178,  151,  178,  151,  178,  151,  178,  151,  178,
      166,  176,  178,  167,  178,  159,  178,  155,  178,  151,
      178,  151,  178,  151,  178,  151,  178,  151,  178,  167,
      178,  155,  178,  167,  177,  177,  177,  146,  170,  145,
      138,   20,  154,  139,  137,   34,  155,  136,   35,   33,

       18,   36,  151,  151,  151,  151,  151,  151,  151,  151,
      151,  151,  151,  151,  151,  151,   42,  151,  151,  151,
      151,  151,  151,  151,  151,  151,  151,  151,  151,  151,
      151,  151,   91,  151,  151,  151,  151,  151,  151,  151,
      151,  151,  151,  151,  151,  151,  151,  151,  151,  151,
      151,  151,  151,  151,  151,  151,  151,  151,  151,  151,
      151,  151,  167,  176,  177,  177,  177,  177,  151,  151,
      151,  151,   91,  151,  151,  174,  149,  143,  142,  151,
      151,  151,  151,  151,  151,  151,  151,  151,  151,  151,
      151,  151,  151,   42,  151,  151,  151,  151,  151,  151,

      151,  151,  151,  151,  151,  151,  151,  151,  151,   91,
      151,  151,  151,  151,  151,  151,  151,  151,  151,  151,
      151,  151,  151,  151,  151,  151,  151,  151,  151,  151,
      151,  151,  151,  151,  151,  151,  151,  151,  151,  176,
      167,  167,  175,   20,  155,  175,  151,  151,  151,  151,
      151,  151,  151,  151,  151,  151,   91,  151,  151,  167,
      155,  177,  177,  141,  145,  153,  152,  153,  154,  154,
      151,  151,  151,  151,  151,  151,  151,  151,  151,  151,
      151,  151,  151,  151,  151,  151,  151,  151,  151,    9,
      151,  151,  151,  151,  151,  151,  151,  151,  151,  151,

      151,  151,  103,16485,  151,  151,  151,  151,  151,  151,
      151,  151,  151,  151,  151,  151,  151,  151,  151,  151,
      151,  151,  151,  151,   94,  151,  151,  151,  151,  151,
      151,  151,  151,  151,  151,  151,  151,  151,  151,  151,
      151,  151,  151,  151,  151,  151,  151,   11,  151,  151,
      151,  151,  177,  177,  177,  151,  151,  151,  151,  151,
      151,  151,  151,  151,  151,  151,  151,  151,  151,  151,
      151,  151,  151,  151,  151,  151,  151,  151,  151,  151,
        9,  151,  151,  151,  151,  151,  151,  151,  151,  151,
      151,  151,  151,  151,  151,  151,  151,  151,  151,  151,

      151,  151,  151,  151,  151,  151,  151,  151,  151,  151,
      151,  151,  151,   94,  151,  151,  151,  151,  151,  151,
      151,  151,  151,  151,  151,  151,  151,  151,  151,  151,
      151,  151,  151,  151,  151,  151,   11,  151,  151,  151,
      151,  167,  167,  155,  151,  151,  151,  151,  151,  151,
      151,  151,  151,  151,  151,  151,  151,  177,  177,  154,
       22,   24,   23,   26,   25,   28,   30,  151,  151,  151,
      151,  151,  151,  151,   15,  151,  151,  151,  151,  151,
      151,  151,  151,  151,  151,  151,   41,   41,  151,  151,
       99,  151,  116,  151,  151,  151,  151,  151,  117,  151,

      126,  151,  151,   79,  151,  151,  151,  151,  114,  151,
      151,   93,  151,  151,  151,  151,  151,  151,  151,  151,
      151,  151,  151,  118,  151,  151,  151,  151,  115,   14,
      151,  151,   63,  151,   77,  151,  151,  151,  151,  151,
      151,  151,  151,  151,  151,   83,  151,   43,  151,  130,
      151,  151,  151,  151,  151,   72,  151,  151,  151,   76,
      151,   57,  151,  151,  151,   97,  151,  151,  151,  151,
      151,   47,  177,  177,  177,  105,  151,  151,  151,  151,
      151,  151,16458,  151,  151,  151,  151,  151,  151,  151,
       15,  151,  151,  151,  151,  151,  151,  151,  151,  151,

      151,  151,   41,  151,  151,   99,  151,  151,  151,  151,
      151,  151,  151,  151,  151,   79,  151,  151,  151,  151,
      151,  151,   93,  151,  151,  151,  151,  151,  151,  151,
      151,  151,  151,  151,  151,  151,  151,  151,   14,  151,
      151,   63,  151,   77,  151,  151,  151,  151,  151,  151,
      151,  151,  151,  151,   83,  151,   43,  151,  151,  151,
      151,  151,  151,   72,  151,  151,  151,   76,  151,   57,
      151,  151,  151,   97,  151,  151,  151,  151,  151,  167,
      155,   15,  151,  105,  151,  151,  151,  151,  151,  151,
      151,  151,  151,  151,  151,  151,  151,  151,16458,  177,

      177,  158,   32,   21,   29,   31,  151,  151,  151,  151,
      151,  151,  151,  151,   52,  151,  151,  151,  151,  151,
      134,  151,  151,  151,  151,  151,  151,  151,   40,  151,
      100,  151,  151,  151,  151,  151,  151,  151,  151,  108,
       87,  151,  127,  151,   93,  102,  151,  151,   95,  151,
      151,  151,  151,  151,  151,  151,  151,  119,  151,  151,
      121,  128,  151,  151,  151,  151,  151,   55,  151,  151,
      151,   80,  151,  151,  151,  151,   82,  129,  151,  151,
      151,  151,  151,  151,  151,  151,  151,  112,   58,  151,
       38,  151,   86,  151,  105,16458,  177,  177,  177,  105,

      151,   92,  151,  151, 8266,   73, 8266,  151,  151,  151,
      151,  151,  151,  151,  151,   52,  151,  151,  151,  151,
      151,  134,  151,  151,  151,  151,  151,  151,  151,   40,
      151,  100,  151,  151,  151,  151,  151,  151,  151,  151,
       87,  151,  151,  151,  151,   95,  151,  151,  151,  151,
      151,  151,  151,  151,  151,  151,  151,  151,  151,  151,
      151,   55,  151,  151,  151,   80,  151,  151,  151,  151,
      151,  151,  151,  151,  151,  151,  151,  151,  151,   58,
      151,   38,  151,   86,  151,  167,  155,  105,  151,  151,
       52,  151,  151,  151,  151,  151,  151,  151,  134,  151,

      151,  151,   16,  177,   16,  177,   16,   16,  146,   16,
       16,   16,  145,   16,   16,   16,   16,   16,   16,   27,
      151,  151,  151,  151,  151,   16,  151,  151,  151,   66,
      151,  151,  151,  151,  151,  151,  151,  151,   98,  151,
      151,   40,  100,  151,  151,  151,  151,  151,  133,  151,
      151,  102, 8293,  102,  151,  151,  151,  151,   69,  151,
      151,  151,  124,  151,  151,   37,  151,  151,  151,  151,
      151,  151,  151,  151,  151,  151,   89,  151,  151,    7,
      151,   78,  151,   12,  151,  151,  151,  132,  151,  151,
       88,  151,   85,  177,  177,   16,  177,  151,  151,  151,

      151,  151,  151,  151,  151,   16,  151,  151,  151,   66,
      151,  151,  151,  151,  151,  151,  151,  151,   98,  151,
      151,  151,  151,  151,  151,  151,  151,  151,  151,  151,
      151,  151,   69,  151,  151,  151,  151,  151,   37,  151,
      151,  151,  151,  151,  151,  151,  151,  151,  151,   89,
      151,  151,    7,  151,   78,  151,   12,  151,  151,  151,
      132,  151,  151,   88,  151,   16,  151,  151,   66,  151,
      151,  151,  151,  151,   16,  151,  151,  151,   17,   17,
      177,   17,   17,  146,   17,   17,   17,  145,   17,   17,
       17,   17,   17,   17,  109,  110,   17,  151,  151,  151,

      151,  151,   50,  151,  151,  151,  151,  106,  151,  151,
      151,  151,   98,  151,  151,   75,  151,  151,  151,  120,
      151,  151, 8293,  151,   10,  151,   53,  151,   44,  151,
      151,  151,  125,   45,  151,  151,  151,    5,  151,  113,
      151,  151,   70,  151,  151,   90,  151,    2,  151,  151,
      151,  122,  131,  151,  177,   17,  177,  151,   67,  151,
      171,   17,  151,  151,  151,  151,  151,   50,  151,  151,
      151,  151,  106,  151,  151,  151,  151,  151,  151,   75,
      151,  151,  151,  151,  151,  151,   10,  151,   53,  151,
       44,  151,  151,  151,   45,  151,  151,  151,    5,  151,

      151,  151,   70,  151,  151,   90,  151,    2,  151,  151,
      151,  151,  171,   17,   17,  151,  151,   50,  151,  151,
      151,  151,  151,  151,    3,  151,  151,  151,  151,  151,
        4,  151,  151,  151,  151,  151,  151,   75,  151,   59,
      151,  151,   68,  151,    8,  151,   13,  151,  151,  151,
      151,   84,  151,   71,  151,  151,  151,  151,  151,  151,
      177,   62,  151,  151,  151,    3,  151,  151,  151,  151,
      151,    4,  151,  151,  151,  151,  151,  151,  151,   59,
      151,  151,   68,  151,    8,  151,   13,  151,  151,  151,
      151,   84,  151,   71,  151,  151,  151,  151,  151,  151,

      151,  151,   62,  151,    4,  151,  151,  137,  151,  151,
      135,  151,   46,  151,  151,  151,   54,  151,  151,  151,
       61,  151,   59,  107,  151,  151,   96,  151,  111,  151,
       64,  151,  123,   65,  151,  151,  151,   62,  177,  147,
      151,  150,  151,  151,  135,  151,   46,  151,  151,  151,
       54,  151,  151,  151,   61,  151,  107,  151,  151,   96,
      151,  151,   64,  151,   65,  151,  151,  151,   46,  151,
      151,  147,  151,  169,  137,  151,  151,   39,  151,    6,
      151,  151,  151,   61,   60,  107,  151,  151,  104,  151,
        1,  151,  147,  177,  151,  151,   39,  151,    6,  151,

      151,  151,  151,  151,  104,  151,    1,  151,  168,   39,
      151,   51,  151,  151,  151,   56,  151,  151,  104,  177,
       51,  151,  151,  151,   56,  151,  151,  169,  151,  151,
      151,  177,  151,  151,  151,  168,   19,   49,  151,  151,
      151,  177,  148,  149,   49,  151,  151,  151,  168,  168,
       49,  151,  151,  177,  151,  151,   48,  151,   81,  151,
      177,   48,  151,   81,  151,  168,   48,   81,  177,  177,
      177,  177,  177,  177,  172,  177,  172,  172,  175,  172,
      176,  177,  175,  173,  174,  173,  174
    } ;

static yyconst flex_int16_t yy_accept[1884] =
    {   0,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    2,    3,    3,    3,    3,    3,    4,    5,    7,
        9,   11,   12,   14,   16,   18,   19,   21,   23,   25,
       27,   29,   31,   33,   35,   37,   39,   41,   43,   45,
       47,   49,   51,   53,   55,   57,   59,   61,   63,   65,
       67,   69,   71,   73,   75,   77,   79,   81,   83,   85,
       87,   90,   92,   94,   96,   98,  100,  102,  103,  104,
      106,  107,  108,  110,  111,  112,  114,  116,  117,  119,
      121,  123,  125,  127,  129,  131,  133,  135,  137,  139,
      141,  143,  145,  147,  149,  151,  153,  155,  157,  159,

      161,  164,  166,  168,  170,  172,  174,  176,  178,  180,
      182,  184,  184,  184,  185,  186,  187,  188,  188,  189,
      189,  189,  190,  190,  190,  190,  190,  191,  191,  191,
      191,  191,  192,  192,  192,  192,  193,  193,  194,  194,
      194,  194,  194,  194,  194,  194,  194,  194,  194,  195,
      196,  197,  197,  198,  198,  199,  200,  201,  202,  203,
      204,  205,  206,  207,  208,  209,  210,  211,  212,  213,
      214,  215,  216,  217,  219,  220,  221,  222,  223,  224,
      225,  226,  227,  228,  229,  230,  231,  232,  233,  235,
      236,  237,  238,  239,  240,  241,  242,  243,  244,  245,

      246,  247,  248,  249,  250,  251,  252,  253,  254,  255,
      256,  257,  258,  259,  260,  261,  262,  263,  263,  264,
      265,  265,  265,  265,  265,  265,  265,  265,  266,  266,
      267,  268,  269,  269,  270,  271,  272,  273,  275,  276,
      276,  277,  277,  277,  278,  278,  278,  278,  279,  279,
      280,  280,  280,  280,  280,  280,  280,  281,  282,  283,
      284,  285,  286,  287,  288,  289,  290,  291,  292,  293,
      294,  296,  297,  298,  299,  300,  301,  302,  303,  304,
      305,  306,  307,  308,  309,  310,  312,  313,  314,  315,
      316,  317,  318,  319,  320,  321,  322,  323,  324,  325,

      326,  327,  328,  329,  330,  331,  332,  333,  334,  335,
      336,  337,  338,  339,  340,  340,  341,  341,  341,  342,
      343,  343,  343,  344,  345,  345,  345,  345,  345,  346,
      347,  347,  348,  349,  350,  351,  352,  353,  354,  355,
      356,  357,  359,  360,  361,  361,  361,  362,  362,  362,
      362,  363,  364,  364,  364,  364,  364,  364,  364,  364,
      364,  366,  366,  366,  366,  366,  366,  366,  366,  366,
      366,  366,  366,  366,  366,  366,  366,  366,  366,  366,
      366,  366,  366,  366,  366,  366,  366,  366,  366,  366,
      366,  366,  366,  367,  370,  370,  371,  372,  373,  374,

      375,  376,  377,  378,  379,  380,  381,  382,  383,  384,
      385,  386,  387,  387,  388,  389,  390,  392,  393,  394,
      395,  396,  397,  398,  399,  400,  401,  402,  402,  403,
      403,  405,  406,  407,  408,  409,  410,  411,  412,  413,
      414,  415,  416,  417,  418,  419,  420,  421,  422,  423,
      424,  425,  427,  428,  429,  430,  431,  432,  433,  434,
      435,  436,  437,  438,  439,  440,  441,  442,  443,  444,
      445,  446,  447,  448,  450,  451,  452,  453,  453,  453,
      453,  453,  453,  453,  453,  453,  453,  454,  455,  456,
      456,  457,  458,  459,  460,  461,  462,  462,  462,  462,

      462,  462,  462,  462,  462,  462,  462,  462,  462,  463,
      464,  465,  466,  467,  468,  469,  470,  471,  472,  473,
      474,  475,  476,  477,  478,  479,  480,  481,  483,  484,
      485,  486,  487,  488,  489,  490,  491,  492,  493,  494,
      495,  496,  497,  498,  499,  500,  501,  502,  503,  504,
      505,  506,  507,  508,  509,  510,  511,  512,  513,  514,
      516,  517,  518,  519,  520,  521,  522,  523,  524,  525,
      526,  527,  528,  529,  530,  531,  532,  533,  534,  535,
      536,  537,  539,  540,  541,  542,  542,  542,  542,  542,
      543,  543,  544,  544,  544,  544,  544,  544,  544,  545,

      545,  546,  547,  548,  549,  550,  551,  552,  553,  554,
      555,  556,  557,  558,  558,  558,  558,  558,  559,  560,
      560,  560,  560,  560,  560,  560,  560,  560,  560,  560,
      560,  560,  560,  560,  560,  560,  560,  560,  561,  561,
      561,  562,  562,  563,  564,  565,  566,  566,  567,  567,
      567,  568,  568,  568,  568,  568,  568,  568,  568,  568,
      568,  568,  568,  569,  570,  571,  572,  573,  574,  575,
      577,  578,  579,  580,  581,  582,  583,  584,  585,  586,
      587,  588,  590,  591,  593,  593,  594,  595,  596,  597,
      598,  599,  599,  600,  601,  601,  602,  603,  604,  606,

      607,  608,  609,  609,  610,  611,  612,  612,  614,  614,
      614,  614,  614,  615,  616,  617,  618,  619,  620,  621,
      622,  623,  624,  624,  625,  626,  627,  628,  629,  629,
      630,  632,  633,  635,  637,  638,  639,  640,  641,  642,
      643,  644,  645,  646,  648,  650,  650,  651,  652,  653,
      654,  655,  656,  658,  659,  660,  662,  664,  665,  666,
      668,  669,  670,  671,  672,  673,  673,  673,  673,  673,
      673,  673,  674,  675,  676,  676,  678,  679,  680,  681,
      682,  684,  684,  684,  684,  684,  684,  684,  684,  684,
      684,  685,  686,  687,  688,  689,  690,  691,  693,  694,

      695,  696,  697,  698,  699,  700,  701,  702,  703,  705,
      706,  708,  709,  710,  711,  712,  713,  714,  715,  716,
      718,  719,  720,  721,  722,  723,  725,  726,  727,  728,
      729,  730,  731,  732,  733,  734,  735,  736,  737,  738,
      739,  741,  742,  744,  746,  747,  748,  749,  750,  751,
      752,  753,  754,  755,  757,  759,  760,  761,  762,  763,
      764,  766,  767,  768,  770,  772,  773,  774,  776,  777,
      778,  779,  780,  780,  780,  780,  781,  781,  781,  781,
      781,  781,  782,  782,  784,  786,  787,  788,  789,  790,
      791,  792,  793,  794,  795,  796,  797,  798,  800,  800,

      800,  800,  801,  802,  802,  802,  802,  802,  802,  803,
      803,  803,  803,  803,  803,  803,  804,  805,  805,  806,
      807,  807,  807,  807,  807,  807,  807,  808,  809,  810,
      811,  812,  813,  814,  815,  817,  818,  819,  820,  821,
      823,  824,  825,  826,  827,  827,  828,  829,  829,  829,
      829,  829,  829,  831,  833,  834,  835,  836,  837,  838,
      839,  840,  840,  841,  843,  843,  844,  845,  846,  846,
      846,  846,  847,  848,  849,  851,  852,  853,  854,  855,
      856,  857,  858,  858,  859,  860,  861,  861,  862,  862,
      863,  864,  865,  866,  867,  868,  870,  871,  872,  874,

      875,  876,  877,  877,  878,  878,  879,  880,  881,  882,
      883,  884,  885,  886,  887,  888,  888,  889,  891,  893,
      895,  896,  896,  896,  896,  896,  897,  898,  899,  900,
      900,  901,  902,  903,  904,  905,  906,  907,  908,  908,
      908,  908,  908,  908,  908,  909,  910,  911,  912,  913,
      914,  915,  916,  918,  919,  920,  921,  922,  924,  925,
      926,  927,  928,  929,  930,  932,  934,  935,  936,  937,
      938,  939,  940,  941,  943,  944,  945,  946,  948,  949,
      950,  951,  952,  953,  954,  955,  956,  957,  958,  959,
      960,  961,  962,  964,  965,  966,  968,  969,  970,  971,

      972,  973,  974,  975,  976,  977,  978,  979,  980,  982,
      984,  986,  986,  986,  986,  987,  987,  987,  987,  987,
      987,  988,  988,  989,  990,  991,  993,  994,  995,  996,
      997,  998,  999, 1001, 1002, 1003, 1003, 1003, 1004, 1005,
     1007, 1007, 1008, 1010, 1010, 1011, 1012, 1014, 1014, 1014,
     1014, 1014, 1015, 1016, 1017, 1018, 1019, 1020, 1021, 1021,
     1021, 1021, 1021, 1022, 1023, 1024, 1025, 1026, 1028, 1029,
     1030, 1032, 1033, 1034, 1035, 1036, 1037, 1038, 1039, 1039,
     1039, 1041, 1042, 1043, 1044, 1044, 1044, 1044, 1045, 1046,
     1047, 1048, 1049, 1049, 1050, 1051, 1052, 1052, 1053, 1053,

     1053, 1053, 1053, 1054, 1055, 1056, 1057, 1058, 1059, 1061,
     1062, 1063, 1063, 1064, 1065, 1066, 1068, 1069, 1070, 1071,
     1072, 1073, 1074, 1075, 1076, 1077, 1079, 1080, 1082, 1084,
     1086, 1087, 1088, 1090, 1091, 1093, 1093, 1094, 1094, 1094,
     1094, 1095, 1096, 1098, 1098, 1099, 1100, 1101, 1101, 1101,
     1101, 1101, 1101, 1101, 1102, 1103, 1104, 1105, 1106, 1108,
     1109, 1110, 1112, 1113, 1114, 1115, 1116, 1117, 1118, 1119,
     1121, 1122, 1123, 1124, 1125, 1126, 1127, 1128, 1129, 1130,
     1131, 1132, 1133, 1135, 1136, 1137, 1138, 1139, 1141, 1142,
     1143, 1144, 1145, 1146, 1147, 1148, 1149, 1150, 1152, 1153,

     1155, 1157, 1159, 1160, 1161, 1163, 1164, 1166, 1166, 1166,
     1166, 1166, 1167, 1167, 1168, 1169, 1171, 1172, 1173, 1174,
     1175, 1177, 1178, 1179, 1179, 1180, 1182, 1183, 1185, 1186,
     1187, 1189, 1189, 1190, 1191, 1192, 1193, 1194, 1195, 1195,
     1195, 1195, 1195, 1195, 1196, 1196, 1197, 1199, 1200, 1201,
     1202, 1203, 1205, 1206, 1207, 1208, 1210, 1211, 1211, 1212,
     1213, 1214, 1214, 1215, 1215, 1215, 1215, 1216, 1218, 1219,
     1220, 1220, 1221, 1222, 1223, 1224, 1224, 1224, 1225, 1227,
     1229, 1231, 1232, 1233, 1233, 1234, 1236, 1236, 1237, 1238,
     1240, 1240, 1241, 1242, 1243, 1245, 1246, 1248, 1250, 1251,

     1251, 1252, 1252, 1253, 1253, 1254, 1255, 1255, 1255, 1255,
     1256, 1258, 1258, 1259, 1260, 1261, 1261, 1261, 1261, 1262,
     1262, 1262, 1264, 1265, 1266, 1267, 1268, 1270, 1271, 1272,
     1273, 1275, 1276, 1277, 1278, 1279, 1280, 1282, 1283, 1284,
     1285, 1286, 1287, 1289, 1291, 1293, 1294, 1295, 1297, 1298,
     1299, 1301, 1302, 1303, 1305, 1306, 1308, 1310, 1311, 1312,
     1313, 1313, 1314, 1314, 1315, 1315, 1317, 1318, 1320, 1321,
     1322, 1323, 1324, 1324, 1324, 1324, 1324, 1324, 1325, 1327,
     1328, 1329, 1330, 1331, 1333, 1334, 1335, 1335, 1335, 1336,
     1337, 1337, 1338, 1338, 1339, 1339, 1340, 1342, 1343, 1345,

     1347, 1347, 1349, 1350, 1351, 1351, 1352, 1354, 1356, 1357,
     1358, 1359, 1359, 1360, 1361, 1361, 1361, 1362, 1362, 1364,
     1365, 1365, 1365, 1365, 1365, 1365, 1366, 1368, 1369, 1370,
     1371, 1372, 1374, 1375, 1376, 1377, 1378, 1379, 1380, 1382,
     1383, 1385, 1387, 1389, 1390, 1391, 1392, 1394, 1396, 1397,
     1398, 1399, 1400, 1401, 1401, 1401, 1401, 1402, 1403, 1405,
     1407, 1408, 1408, 1408, 1408, 1408, 1408, 1408, 1408, 1409,
     1410, 1411, 1413, 1415, 1416, 1417, 1419, 1419, 1419, 1420,
     1421, 1421, 1423, 1423, 1424, 1426, 1427, 1429, 1429, 1430,
     1430, 1431, 1433, 1433, 1434, 1436, 1436, 1437, 1438, 1439,

     1439, 1440, 1440, 1442, 1442, 1442, 1442, 1443, 1444, 1445,
     1447, 1449, 1450, 1451, 1453, 1454, 1455, 1457, 1459, 1460,
     1462, 1463, 1465, 1467, 1468, 1469, 1469, 1469, 1469, 1471,
     1472, 1474, 1474, 1474, 1475, 1475, 1475, 1475, 1476, 1477,
     1478, 1480, 1482, 1482, 1482, 1483, 1484, 1485, 1485, 1486,
     1487, 1488, 1488, 1489, 1489, 1491, 1493, 1494, 1495, 1495,
     1495, 1495, 1496, 1497, 1499, 1501, 1502, 1503, 1504, 1505,
     1507, 1509, 1509, 1509, 1509, 1510, 1510, 1512, 1512, 1512,
     1512, 1512, 1512, 1512, 1514, 1514, 1514, 1514, 1514, 1515,
     1516, 1518, 1518, 1519, 1520, 1521, 1521, 1521, 1521, 1523,

     1524, 1525, 1527, 1528, 1528, 1528, 1528, 1528, 1528, 1528,
     1528, 1528, 1528, 1528, 1528, 1529, 1529, 1529, 1529, 1529,
     1530, 1531, 1531, 1532, 1533, 1533, 1533, 1533, 1534, 1535,
     1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536,
     1536, 1536, 1537, 1537, 1537, 1537, 1537, 1537, 1538, 1538,
     1538, 1540, 1541, 1541, 1542, 1543, 1543, 1543, 1543, 1545,
     1547, 1548, 1549, 1549, 1549, 1549, 1549, 1549, 1549, 1550,
     1550, 1550, 1550, 1551, 1551, 1551, 1551, 1551, 1552, 1552,
     1553, 1553, 1554, 1555, 1555, 1555, 1556, 1557, 1557, 1557,
     1557, 1557, 1557, 1557, 1557, 1557, 1557, 1557, 1557, 1559,

     1559, 1561, 1562, 1562, 1562, 1564, 1566, 1566, 1566, 1566,
     1566, 1566, 1566, 1567, 1567, 1568, 1569, 1570, 1570, 1570,
     1570, 1570, 1570, 1570, 1570, 1570, 1571, 1571, 1571, 1571,
     1571, 1571, 1571, 1571, 1571, 1572, 1572, 1572, 1572, 1572,
     1573, 1573, 1573, 1573, 1574, 1574, 1574, 1574, 1575, 1576,
     1577, 1577, 1578, 1578, 1578, 1578, 1580, 1580, 1580, 1582,
     1582, 1583, 1583, 1583, 1583, 1583, 1583, 1583, 1584, 1584,
     1584, 1584, 1584, 1586, 1586, 1586, 1587, 1587, 1587, 1588,
     1588, 1588, 1588
    } ;

static yyconst flex_int32_t yy_ec[256] =
    {   0,
        1,    1,    1,    1,    1,    1,    1,    1,    2,    3,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    4,    5,    6,    7,    8,    9,   10,   11,   12,
       13,   14,   15,   16,   17,   18,   19,   20,   20,   20,
       20,   20,   20,   20,   20,   20,   20,   21,   22,   23,
       24,   25,    1,    1,   26,   27,   28,   29,   30,   31,
       32,   33,   34,   35,   36,   37,   38,   39,   40,   41,
       42,   43,   44,   45,   46,   47,   48,   49,   50,   51,
       52,    1,   53,    1,   54,    1,   55,   56,   57,   58,

       59,   60,   61,   62,   63,   35,   64,   65,   66,   67,
       68,   69,   70,   71,   72,   73,   74,   75,   76,   77,
       78,   79,    1,   80,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,

        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1
    } ;

static yyconst flex_int32_t yy_meta[81] =
    {   0,
        1,    2,    3,    2,    4,    5,    4,    4,    1,    4,
        6,    7,    8,    4,    9,   10,   11,   12,   13,   14,
        1,    4,    1,    1,    1,   15,   14,   14,   14,   14,
       15,   16,   17,   17,   17,   17,   16,   17,   16,   16,
       17,   17,   16,   16,   16,   16,   17,   17,   17,   17,
       17,    1,    1,   18,   15,   14,   14,   14,   14,   15,
       16,   17,   17,   17,   16,   17,   16,   16,   17,   17,
       16,   16,   16,   16,   17,   17,   17,   17,   17,    5
    } ;

static yyconst flex_int16_t yy_base[2051] =
    {   0,
        0,   79,    0,    0,    0,  151, 2913,   82, 2895,   86,
       89,   92,  224,  303,    0,  375, 2885,   70,  102, 9608,
       78,  113,   86,   90,  308,  311,  355,  129,  147,  137,
      447,  386,  440,  145,  146,  285,  302,  361,  444,  356,
      499,  497,  547,  594,  382,  352,  535,  495,  503,  582,
      618,  630,  639,  657,  688,  667,  692,  708,  445,  780,
      123,  538,  583,  760,  756,  811,  813, 9608, 2872, 9608,
       94, 2868, 9608,  491,  102,  110, 9608, 2857,  862,  852,
      772,  923,  871,  972,  921,  154,  729,  854,  384,  870,
      873,  992, 1022,  926,  989, 1039, 1073,  126, 1072,  968,

     1122,  316, 1056,  347, 1179,   94, 1107,   90, 1234,  433,
      437,  161,  128,  130,    0,  289,  281, 2857, 2852,  448,
      322,  459, 2833,  542, 1149,  594, 2792,  626,  735,  631,
      740, 9608, 1260, 1277, 1302, 9608, 1303, 1051,  302,  321,
      676,  917,  736,  355,  362, 1152, 1321, 1334, 9608, 9608,
     9608, 1271, 1330,  323, 9608, 9608, 9608, 9608, 9608,    0,
      638,  299,  446,  447,  480,  366,  545,  590,  553,  821,
      613,  922,  581,  971,  636,  614,  653,  680,  726, 1085,
      734,  761,  781,  791,  831, 1001, 1261,  854, 1326, 1320,
      876,  879,  731, 1025,  899,  905,  969, 1013, 1027, 1331,

     1027, 1068, 1321, 1306,  791, 1353, 1093, 1112, 1224, 1369,
      825, 1239,  876,  894,  915, 1357,  964,    0, 1372, 1431,
     2797, 1387,  978, 1246, 1276, 1368, 1445, 2784, 1449, 1402,
     1404, 1372, 1112, 1419, 1422, 1393, 1401, 1430, 1421, 2727,
     9608, 1418, 2720, 9608, 1474, 1428, 1305,  199, 2707, 2702,
     1118, 1478, 1159, 2707, 2702, 1506, 1476, 1500, 1468, 1519,
     1525, 1534, 1473, 1543, 1557, 1561, 1565, 1566, 1609, 1605,
     1613, 1600, 1618, 1608, 1639, 1649, 1669, 1662, 1672, 1676,
     1680, 1702, 1732, 1736, 1711, 1743, 1729, 1751, 1775, 1778,
     1799, 1793, 1789, 1813, 1832, 1837, 1833, 1742, 1855, 1851,

     1859, 1890, 1893, 1897, 1923, 1928, 1936, 1930, 1949, 1954,
     1955, 1980, 1987, 1995, 1622, 2031, 2052, 2648, 1495,  144,
     1570, 2644, 9608, 2632, 1526, 1516, 2012, 2045, 2060, 2079,
     1593, 2136, 2216, 2048, 2060, 1803, 2057, 2091, 2136, 2137,
     2138, 2139, 2214, 1691, 2067, 2183, 2243, 1634, 1428, 1428,
     1485, 1491, 2615, 1521, 1609, 2606, 1258, 2106, 2166, 1610,
     2594, 2547, 2241, 2246, 1851, 1761, 2246, 2273, 2530, 2303,
     2306, 1610, 1347, 1916, 1503, 2307, 2523, 2443, 2434, 2433,
     2313, 1643, 2430, 1672, 1993, 2330, 2170, 2444, 2403, 2343,
     2362, 2388, 9608, 2355, 2323, 2319, 1774, 1850, 1707, 1714,

     1857, 1751, 1809, 1844, 1868, 1871, 2229, 2343, 2001, 2005,
     1904, 1919, 2357, 2359, 1934, 1989, 2431, 1084, 2043, 2312,
     2297, 2141, 2156, 2007, 2280, 2025, 2208, 2347, 2218, 2201,
     2325, 2253, 2336, 2343, 2347, 2356, 2300, 2350, 2350, 2345,
     2394, 2371, 2362, 2380, 2380, 2422, 2381, 2391, 2379, 2392,
     2378,    0, 2411, 2398, 2404, 2410, 2411, 2413, 2412, 2425,
     2478, 2407, 2417, 2430, 2437, 2438, 2429, 2439, 2444, 2445,
     2437, 2453, 2450,    0, 2456, 2464, 2460, 2318, 2456, 2309,
     2464, 2470, 2465, 2471, 2469, 2476, 2515, 2484, 2516, 2490,
     2495, 2512, 2500, 2503, 2501, 2502, 2532, 2563, 2283, 2566,

     2573, 2277, 2276, 2577, 2584, 2590, 2273, 2251, 2543, 2550,
     2571, 2560, 2577, 2545, 2580, 2593, 2596, 2598, 2607, 2604,
     2600, 2601, 2617, 2603, 2611, 2619, 2620, 2683, 2650, 2626,
     2678, 2624, 2660, 2661, 2654, 2690, 2668, 2631, 2615, 2692,
     2696, 2675, 2697, 2705, 2706, 2698, 2700, 2708, 2736, 2719,
     2701, 2721, 2722, 2765, 2748, 2751, 2712, 2755, 2729, 2243,
     2769, 2771, 2774, 2777, 2780, 2788, 2781, 2794, 2802, 2782,
     2783, 2798, 2795, 2805, 2799, 2793, 2796, 2807, 2830, 2806,
     2827, 2196, 2831, 2814, 2841, 2878, 2885, 2189, 2890, 2946,
     2488,  330, 1951, 2895, 2866, 2850, 2903, 2907, 2940, 2874,

     3019, 3099, 2871, 2882, 2893, 2902, 3017, 2926, 2880, 2961,
     2857, 2943, 2954, 2753, 2919, 2922, 2924, 2945, 2929, 2995,
     3067, 1819, 2187, 2180, 3047, 3124, 3044, 3049, 3126, 3078,
     2977, 2110, 3143, 3146, 3050, 3061, 2101, 2091, 2082, 3138,
     9608, 2081, 9608, 9608, 9608, 9608, 3151, 9608, 3002, 2049,
     9608, 2043, 2917, 3081, 2038, 2028, 3188, 3199, 3218, 2027,
     2002, 3228, 3016, 3097, 3105, 2948, 3064, 3101, 3113,    0,
     3112, 3140, 3128, 3141, 3136, 3154, 3165, 3147, 3140, 3169,
     1947, 1932, 3174, 3238, 3313, 9608, 3176, 3188, 3195, 3179,
     3193, 3170, 9608, 3187, 3239, 9608, 3201, 3201,    0, 3206,

     3256, 3202, 3257, 9608, 3263, 3205, 3211,    0, 3263, 1932,
     1927, 3280, 3235, 3222, 3226, 3255, 3268, 3256, 3246, 3259,
     3268, 3301, 3304, 9608, 3274, 3251, 3308, 3322, 3332, 9608,
        0, 3275,    0, 3281, 3281, 3284, 3303, 3282, 3294, 3296,
     3317, 3302, 3314, 3169,    0, 3350, 9608, 3364, 3308, 3316,
     3320, 3325,    0, 3337, 3342, 3329,    0, 3338, 3350,    0,
     3380, 3353, 3360, 3363, 9608, 3365, 3352, 3372, 3373, 3371,
     3372, 3393, 3402, 3404, 3367,  219, 3382,  577, 3385, 3389,
     3431, 3410, 3455, 3412, 3437, 3463, 3467, 3471, 1931, 1904,
     3411, 3414, 3441, 3420, 3417, 3458, 3459, 1900, 3467, 3477,

     3481, 3463, 3483, 3489, 3484, 3488, 3487, 3491,  326, 3495,
     3493, 3498, 3503, 3496, 3501, 3502, 3499, 3506, 3509, 1873,
     3505, 3568, 3534, 3581, 3517, 1816, 3539, 3513, 3536, 3541,
     3562, 3565, 3574, 3583, 3577, 3614, 3586, 3585, 3622, 3631,
     1809, 3597, 1804, 3590, 3605, 3592, 3607, 3594, 3603, 3608,
     3616, 3604, 3648, 3660, 1789, 3675, 3609, 3624, 3651, 3618,
     1755, 3656, 3662, 3666, 1752, 3653, 3667, 1747, 3684, 3671,
     3680, 3688, 3700, 3712, 3476, 3746, 3705, 3725, 3732, 3689,
     3725, 3740, 3729, 3819, 3899, 3739, 3733, 3860, 3777, 3810,
     3793, 3923, 3924, 3928, 3846, 3750, 3928, 3849, 3657, 3481,

        0, 3708,    0, 3756,  532, 3926, 2614, 3853, 9608, 3946,
     3978, 1711, 3785, 3972, 4032, 9608, 9608, 1710, 9608, 9608,
     3854, 3872, 3876, 3936, 1721, 4001, 3721, 3729, 3743, 3828,
     3751, 4112, 3819, 3835,    0, 3829, 3839, 3890, 3895,    0,
     3895, 3905, 3900, 3935, 4026, 3937, 3948, 3905, 3953, 3963,
     3897, 4002,    0,    0, 4022, 4021, 4033, 4039, 4034, 3696,
     4029, 4067, 9608,    0, 4073, 9608, 4036, 9608, 4136, 4137,
     4154, 4160, 4044, 4047,    0, 4035, 4049, 4036, 4043, 4071,
     4144, 4038, 4172, 9608, 4089, 4129, 4177, 9608, 4178, 9608,
     4128, 4139, 4154, 4146, 4156,    0, 4159, 4156,    0, 4148,

     4168, 4167, 3885, 9608, 4196, 9608, 4153, 4158, 4165, 4177,
     4160, 4177, 4165, 4164, 4166, 4224, 9608,    0,    0, 4005,
     1172, 4188, 1284, 4196, 4184, 4240, 4226, 4227, 1706, 4207,
     2022, 4211, 2071, 4210, 4218, 4260, 9608, 4261, 4250, 4243,
     3880, 3889, 4014, 4019, 4247, 4251, 4255, 4266, 4256, 4323,
     4302, 4265, 1702, 4312, 4347, 4309, 4314, 1698, 4317, 4346,
     4351, 4356, 4353, 4355, 1695, 1665, 4357, 4352, 4359, 4361,
     4358, 4396, 4362, 1658, 4363, 4379, 4367, 1653, 4366, 4375,
     4365, 4388, 4370, 4437, 4369, 4409, 4421, 4425, 4427, 4440,
     4405, 4430, 1646, 4443, 4429, 1639, 4445, 4442, 4446, 4447,

     4449, 4444, 4450, 4451, 4454, 4462, 4453, 4459, 1606, 1592,
     4294, 4297, 4305, 4228, 4533, 2129, 4508, 4512, 4490, 1575,
     4527, 4467, 4473, 4606, 4686, 4335, 4487, 4493, 4438, 4514,
     4531, 4766, 4443, 4514, 4522, 4452,    0, 9608,    0,    0,
      582, 1548, 1545, 4530, 4563, 4573, 1506, 4630, 4631, 4635,
     4846, 4585, 4010, 4652, 4658, 4659, 1492, 9608, 4598, 4636,
     4713, 4717, 4556, 4643, 4926, 4238, 4614,    0, 4520, 4608,
        0, 4606, 4614, 4678, 4535, 4680, 4608, 4758, 4695, 4695,
        0, 4699, 9608, 9608, 4694, 4692, 4704, 4705, 4735, 4749,
     4756, 4793, 4798, 9608, 4770, 4758, 4799, 4727, 4872, 4815,

     4592, 1492, 4889, 4905, 4770, 4775, 4776, 4765,    0, 4765,
     4765, 4873, 9608, 4879, 4776, 4841, 4800, 4834, 4951, 4785,
     4841, 4845, 4906, 4819, 4916,    0, 4846,    0,    0,    0,
     4955, 4956, 4960, 4916,    0, 4143, 9608, 4856, 4926, 4863,
     4962, 1483, 1482, 4935, 4929, 2200, 4944, 4969, 4964, 2295,
     2810, 4743, 4999, 4984, 5007, 5031, 4953, 5013, 1476, 5002,
     5014, 1468, 5011, 4987, 5015, 5062, 5018, 5059, 5064, 1465,
     5066, 5067, 5068, 5061, 5069, 5071, 5073, 5074, 5075, 5077,
     5078, 5097, 1461, 5079, 5098, 5142, 5083, 5113, 5091, 5099,
     5154, 5115, 5107, 5110, 5121, 5100, 5109, 1420, 5128, 1411,

     1379, 1364, 5170, 5181, 5184, 5147, 1344, 5191, 5197, 4947,
     1325, 1324, 5166, 5227, 5307, 5387, 4943, 4954, 5100, 5130,
        0, 5207, 5221, 5134, 9608,    0, 1315, 1303, 5203, 5212,
     1279, 5011, 5251, 5254, 5273, 5279, 5280, 1265, 3085, 3091,
     5334, 5338, 5261, 9608, 5262, 9608,    0, 5319, 5160, 5197,
     5229,    0, 5231, 5242, 5247,    0, 5238, 5379, 5295, 5304,
     9608, 5244, 5308, 5325, 5326, 5311, 5358,    0, 5383, 5384,
     5414, 9608, 5380, 5380, 5420, 5349, 5447, 5380,    0,    0,
        0, 5389, 5393, 5430, 9608,    0, 5454, 5398, 5401,    0,
     5459, 9608, 5420, 5435,    0, 5429,    0,    0, 5423, 5471,

     5446, 5478, 9608, 5482, 9608, 5442, 5440, 4096, 5455,  671,
     1269, 1212, 5443, 4269, 5460,  777, 5475, 5363, 9608, 4662,
     4752, 1175, 5486, 5463, 5501, 5489, 1170, 5494, 5500, 5487,
     1166, 5511, 5517, 5506, 5518, 5505, 1162, 5508, 5522, 5523,
     5529, 5528, 1154, 1128, 1094, 5531, 5533, 1074, 5532, 5537,
     1053, 5534, 5553, 1031, 5541, 1021, 1017, 5540, 5552, 5566,
     5367,  990, 5518,  947,  807,    0, 5517,    0, 5534, 5526,
     5554, 5569, 4903, 5426, 5614, 5617, 5635, 5575,    0, 5547,
     5575, 5561, 5584,    0, 5580, 5589, 5590, 5592, 5603, 5612,
     5601, 5616, 5619, 9608, 5618, 5605,    0, 5613,    0,    0,

     5666,    0, 5627, 5658, 5614, 5619,    0,    0, 5618, 5662,
     5635, 5641, 5629, 5635, 5644, 5663, 5689, 5664,    0, 5665,
     5695, 5696, 5701, 5705,    0, 5694,  937, 5697, 5701, 5705,
     5710,  905, 5708, 5712, 5714, 5709, 5711, 5716,  879, 5720,
      845,  833,  827, 5727, 5741, 5723,  819,  810, 5726, 5769,
     5737, 5730, 5739, 5796, 5258, 5754, 5691, 5699,    0,    0,
     5755, 5801,  859, 5790, 5805, 5813, 5817, 5835, 5831, 5745,
     5780,    0,    0, 5783, 5745,    0, 5791, 5792, 5789, 5798,
     5801, 5840, 5798, 9608,    0, 5818,    0, 5784, 9608, 5808,
     5821,    0, 5857, 9608,    0, 5818, 5833, 5834, 9608, 5835,

     5844, 5827,    0, 5863, 5865, 5868,    0, 5866, 5871,  785,
      774, 5874, 5876,  770, 5877, 5879, 5884,  769, 5880,  764,
     5882,  755,  747, 5894, 5895, 5897, 3005, 5901,    0, 5844,
      717,  997, 5946, 1409, 5924, 5919, 5958,  712, 5881, 5953,
        0,    0, 5864, 5885, 5908, 5918, 5962, 5968, 9608, 9608,
     5928, 5939, 5941, 5945,    0,    0, 9608, 1348,  655, 5973,
     5975, 5976, 5980,  698,  686, 5982, 5983, 5985, 5986,  650,
      627, 5988, 6006, 6020, 6026, 1521,    0, 6038, 6051, 6047,
     6063, 6067,  623,    0, 5993, 6072, 5953, 5965, 6019, 5949,
        0, 6029, 6033, 9608, 6055, 6013, 1545, 6072,  603, 6076,

     6081,  597, 6080, 6088, 6109, 6095,  596,  591, 6116, 6137,
     6095, 6104, 6142, 6154, 6159, 6159, 6171, 6071, 6060, 6067,
     6083, 6119, 6108, 6130, 6128, 6131,  544, 6175, 6146, 6133,
     6188, 6195,  519,  477, 6200, 6212, 6225, 6198, 6229, 6231,
     6243, 6247, 6203, 6260, 6272, 6248, 6276, 9608, 6142, 6186,
        0, 6191, 6209, 6223, 6234, 6222, 6251,  463, 9608,  462,
     6265, 6269, 6282, 6288, 6296, 6311, 6307, 6324, 6322, 6328,
     6340, 6345, 6343, 6357, 6361, 6366, 6278, 9608, 6262, 6269,
     6268, 6322, 1707,  391, 1795, 6354, 6369, 6378, 6382, 6398,
     6402, 6420, 6432, 6375, 6445, 6406, 1834, 6353,    0, 6368,

        0, 6393, 6368, 6387,  441,  437, 4827, 6489, 6459, 6471,
     6467, 6513, 6475, 6414, 9608, 9608, 6428, 6386, 6411, 6463,
     6517, 6569, 6534, 6494, 6440, 6505, 6380, 6479, 6527, 6546,
     6550, 6552, 6593, 6554, 6563, 6470, 2353, 6597, 6565, 6531,
     6488, 6542, 6543, 6622, 6626, 6574, 6630, 6634, 6638,  416,
     6648, 6652,  405, 6616, 6656, 6660,  370, 6664, 6668,  352,
      316, 6674, 6678,  212, 6682, 6671,  208, 6686,  201, 6689,
     6691, 6696, 6700,  178, 6704, 6708,  119,  115, 6712,   83,
     6716, 9608, 6762, 6780, 6798, 6816, 6834, 6852, 6869, 6873,
     6891, 6909, 6927, 6945, 6961, 6979, 6997, 7015, 7033, 7051,

     7069, 7086, 7103, 7108,   84, 7126, 7144, 7162, 7180, 7198,
     7216, 7234, 7252, 7270, 7288, 7306, 7324, 7342, 7360, 7378,
     7396, 7413, 7429, 7434, 7451, 7469, 7487, 7505, 7510, 7528,
     7541, 7556, 7574, 7592, 7610, 7628, 7646, 7664, 7682, 7700,
     7716, 7734, 7752, 7770, 7788, 7806, 7824, 7842, 7860, 7877,
     7893, 7910, 7928, 7946, 7964, 7982, 7987, 8005, 8023, 8041,
     8059, 8077, 8095, 8113, 8131, 8149, 8167, 8185, 8203, 8221,
     8239, 8257, 8275, 8293, 8310, 8315, 8331, 8348, 8366, 8384,
     8402, 8420, 8438, 8456, 8474, 8492, 8510, 8528, 8546, 8564,
     8582, 8600, 8618, 8636, 8654, 8672, 8690, 8708, 8726, 8744,

     8762, 8779, 8797, 8814, 8830, 8835, 8852, 8870, 8888, 8906,
     8924, 8942, 8960, 8978, 8996, 9013, 9030, 9048, 9066, 9084,
     9102, 9120, 9138, 9156, 9173, 9190, 9206, 9211, 9227, 9243,
     9260, 9265, 9283, 9301, 9319, 9337, 9355, 9373, 9391, 9409,
     9427, 9445, 9463, 9481, 9499, 9517, 9535, 9553, 9571, 9589
    } ;

static yyconst flex_int16_t yy_def[2051] =
    {   0,
     1882,    1, 1883, 1883,    1,    1, 1884, 1884, 1885, 1885,
     1883, 1883, 1882,   13,    1,    1, 1882, 1882, 1882, 1882,
     1886, 1887, 1882, 1882, 1882, 1888, 1889, 1882, 1882, 1882,
     1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1890, 1890,
     1890, 1890, 1890, 1890, 1890, 1890, 1890, 1890, 1890, 1890,
     1890, 1890,   51, 1890, 1890, 1890, 1890, 1890, 1890, 1882,
     1882, 1891,   41, 1890, 1890, 1890, 1890, 1882, 1892, 1882,
     1892, 1893, 1882, 1893, 1893, 1882, 1882, 1894, 1882, 1895,
     1895, 1895, 1895,   83,   83,   83, 1895, 1895,   83,   83,
       83,   83, 1895,   92,   83,   83, 1895,   93, 1895, 1895,

     1882,   60, 1896,   33, 1882,   83,   83,   88,   82,   60,
       33, 1882, 1882, 1882, 1897, 1897, 1897, 1898, 1882, 1898,
     1898, 1882, 1899, 1900, 1901, 1900, 1882, 1900, 1900, 1902,
     1902, 1882, 1902, 1902, 1902, 1882, 1882, 1882, 1882, 1882,
     1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882,
     1882, 1903, 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1904,
     1904, 1904, 1904, 1904, 1904, 1904, 1904, 1904, 1904, 1904,
     1904, 1904, 1904, 1904, 1904, 1904, 1904, 1904, 1904, 1904,
     1904, 1904, 1904, 1904, 1904, 1904, 1904, 1904, 1904, 1904,
     1904, 1904, 1904, 1904, 1904, 1904, 1904, 1904, 1904, 1904,

     1904, 1904, 1904, 1904, 1904, 1904, 1904, 1904, 1904, 1904,
     1904, 1904, 1904, 1904, 1904, 1904, 1904, 1905,   60, 1882,
     1906, 1882, 1882, 1882, 1882, 1882, 1882, 1907, 1882, 1907,
     1907, 1907, 1882, 1904, 1904, 1904, 1904, 1904, 1904, 1908,
     1882, 1908, 1909, 1882, 1909, 1909, 1909, 1882, 1910, 1882,
     1882, 1882, 1882, 1911, 1912, 1882,   88,   88,  258,  258,
      258,  258,  258,  258,  258,  258,  258,  258,  258,  258,
      258,  258,  258,  258,  258,  258,  258,  258,  258,  258,
      258,  258,  258,  258,  258,  258,  258,  258,  258,  258,
      258,  258,  258,  258,  258,  258,  258,  258,  258,  258,

      258,  258,  258,  258,  258,  258,  258,  258,  258,  258,
      258,  258,  258,  258, 1882, 1882, 1882, 1913,  219,  319,
     1882, 1914, 1882, 1914, 1914, 1914, 1882, 1882, 1882, 1882,
     1914, 1915, 1915,  333,  333,  333,  333,  333,  333,  258,
      258,  258,  258,  219, 1882, 1882, 1882, 1882, 1882, 1882,
     1916, 1916, 1917, 1917, 1917, 1918, 1919, 1919, 1919, 1919,
     1882, 1920, 1921, 1921, 1882, 1922, 1882, 1923, 1924, 1923,
     1923, 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882,
     1882, 1882, 1882, 1882, 1882, 1882, 1882, 1925, 1926, 1882,
     1882, 1927, 1882, 1928, 1882, 1882, 1929, 1929, 1929, 1929,

     1929, 1929, 1929, 1929, 1929, 1929, 1929, 1929, 1929, 1929,
     1929, 1929, 1882, 1929, 1929, 1929, 1929, 1929, 1929, 1929,
     1929, 1929, 1929, 1929, 1929, 1929, 1929, 1882, 1929, 1882,
     1930, 1929, 1929, 1929, 1929, 1929, 1929, 1929, 1929, 1929,
     1929, 1929, 1929, 1929, 1929, 1929, 1929, 1929, 1929, 1929,
     1929, 1929, 1929, 1929, 1929, 1929, 1929, 1929, 1929, 1929,
     1929, 1929, 1929, 1929, 1929, 1929, 1929, 1929, 1929, 1929,
     1929, 1929, 1929, 1929, 1929, 1929, 1929, 1931, 1882, 1932,
     1882, 1882, 1882, 1882, 1882, 1882, 1933, 1933, 1933, 1882,
     1929, 1929, 1929, 1929, 1929, 1929, 1934, 1935, 1936, 1882,

     1882, 1937, 1938, 1882, 1882, 1882, 1939, 1940, 1941, 1941,
     1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941,
     1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941,
     1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941,
     1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941,
     1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941,
     1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941,
     1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941,
     1941, 1941, 1941, 1941, 1941, 1882, 1882, 1942, 1882, 1882,
     1882,  590, 1882, 1882, 1943, 1943, 1882, 1882, 1882, 1943,

     1944, 1944,  602,  602,  602,  602,  602,  602,  602, 1941,
     1941, 1941, 1941, 1882, 1882, 1882, 1882, 1945, 1945, 1946,
     1946, 1947, 1948, 1949, 1948, 1948, 1950, 1950, 1950, 1882,
     1882, 1951, 1952, 1952, 1882, 1882, 1882, 1882, 1882, 1882,
     1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882,
     1882, 1882, 1882, 1882, 1953, 1954, 1882, 1882, 1882, 1955,
     1956, 1882, 1957, 1957, 1957, 1957, 1957, 1957, 1957, 1957,
     1957, 1957, 1957, 1957, 1957, 1957, 1957, 1957, 1957, 1957,
     1882, 1957, 1957, 1957, 1882, 1882, 1957, 1957, 1957, 1957,
     1957, 1882, 1882, 1957, 1882, 1882, 1957, 1957, 1957, 1957,

     1957, 1957, 1882, 1882, 1957, 1957, 1882, 1957, 1958, 1959,
     1960, 1958, 1957, 1957, 1957, 1957, 1957, 1957, 1957, 1957,
     1957, 1957, 1882, 1882, 1957, 1957, 1957, 1957, 1882, 1882,
     1957, 1957, 1957, 1957, 1957, 1957, 1957, 1957, 1957, 1957,
     1957, 1957, 1957, 1957, 1957, 1882, 1882, 1957, 1957, 1957,
     1957, 1957, 1957, 1957, 1957, 1957, 1957, 1957, 1957, 1957,
     1957, 1957, 1957, 1957, 1882, 1882, 1882, 1882, 1882, 1882,
     1882, 1961, 1961, 1961, 1882, 1957, 1957, 1957, 1957, 1957,
     1957, 1962, 1963, 1963, 1882, 1882, 1882, 1882, 1964, 1965,
     1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941,

     1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941,
     1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941,
     1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941,
     1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941,
     1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941,
     1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941,
     1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941,
     1941, 1941, 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1966,
     1966, 1882, 1966, 1967, 1967,  885,  885,  885,  885,  885,
      885,  885,  885,  885, 1941, 1941, 1941, 1941, 1882, 1882,

     1968, 1969, 1970, 1971, 1972, 1973, 1974, 1882, 1882, 1882,
     1975, 1976, 1977, 1978, 1979, 1882, 1882, 1882, 1882, 1882,
     1882, 1882, 1882, 1882, 1980, 1882, 1957, 1957, 1957, 1957,
     1957, 1981, 1957, 1957, 1957, 1957, 1957, 1957, 1957, 1957,
     1957, 1957, 1957, 1957, 1882, 1957, 1957, 1882, 1882, 1882,
     1882, 1882, 1957, 1957, 1957, 1957, 1957, 1957, 1957, 1957,
     1957, 1882, 1882, 1957, 1882, 1882, 1957, 1882, 1982, 1983,
     1984, 1985, 1957, 1957, 1957, 1957, 1957, 1957, 1957, 1957,
     1957, 1957, 1882, 1882, 1957, 1957, 1882, 1882, 1882, 1882,
     1957, 1957, 1957, 1957, 1957, 1957, 1957, 1957, 1957, 1957,

     1957, 1957, 1882, 1882, 1882, 1882, 1957, 1957, 1957, 1957,
     1957, 1957, 1957, 1957, 1957, 1882, 1882, 1957, 1957, 1957,
     1882, 1882, 1882, 1882, 1882, 1882, 1986, 1986, 1987, 1882,
     1882, 1957, 1882, 1957, 1957, 1882, 1882, 1882, 1988, 1989,
     1882, 1882, 1882, 1882, 1941, 1941, 1941, 1941, 1941, 1990,
     1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941,
     1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941,
     1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941,
     1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941,
     1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941,

     1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941,
     1941, 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1991, 1992,
     1882, 1991, 1991, 1993, 1993, 1125, 1125, 1125, 1125, 1125,
     1125, 1994, 1125, 1941, 1941, 1882, 1995, 1882, 1996, 1997,
     1998, 1999, 1882, 2000, 2001, 2001, 1882, 1882, 1882, 2002,
     2003, 1882, 2004, 1882, 2005, 2005, 2006, 1882, 1882, 1882,
     1882, 1882, 1957, 1957, 2007, 1957, 1957, 1957, 1957, 1957,
     1957, 1957, 1957, 1957, 1957, 1957, 1957, 1957, 1882, 1882,
     1957, 1957, 1882, 1882, 1882, 1882, 1882, 1957, 1957, 1957,
     1957, 1957, 1882, 1882, 1957, 1957, 2008, 2008, 2009, 2010,

     2011, 2010, 2011, 2011, 1957, 1957, 1957, 1957, 1957, 1957,
     1957, 1882, 1882, 1957, 1957, 1957, 1957, 1957, 1957, 1957,
     1957, 1957, 1957, 1957, 1957, 1957, 1957, 1957, 1957, 1957,
     1957, 1957, 1957, 1957, 1957, 1882, 1882, 1882, 1882, 1882,
     2012, 2013, 2012, 1882, 1957, 1957, 1957, 2014, 2015, 1882,
     2016, 1882, 1882, 1941, 1941, 2017, 1941, 1941, 1941, 1941,
     1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941,
     1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941,
     1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941,
     1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941,

     1941, 1941, 1941, 1941, 1941, 1941, 1941, 1882, 2016, 1882,
     2018, 2019, 2019, 2020, 2021, 2021, 1316, 1316, 1316, 1316,
     1316, 1941, 1941, 1882, 1882, 2022, 2023, 1882, 2024, 2024,
     1882, 2025, 1882, 2026, 1882, 2027, 2027, 2028, 1882, 2029,
     1882, 1882, 1882, 1882, 1882, 1882, 1957, 1957, 1957, 1957,
     1957, 1957, 1957, 1957, 1957, 1957, 1957, 1882, 1957, 1957,
     1882, 1882, 1957, 1882, 1882, 1882, 1957, 1957, 1957, 1957,
     1882, 1882, 1957, 1957, 2010, 2010, 2011, 1957, 1957, 1957,
     1957, 1957, 1957, 1882, 1882, 1957, 1882, 1957, 1957, 1957,
     1882, 1882, 1957, 1957, 1957, 1957, 1957, 1957, 1957, 1882,

     1957, 1882, 1882, 1882, 1882, 1957, 1882, 1882, 1882, 2012,
     2012, 1882, 1957, 1882, 1957, 2014, 2015, 1882, 1882, 1882,
     2030, 1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941,
     1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941,
     1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941,
     1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941,
     1882, 2031, 1882, 2019, 2019, 1316, 1316, 1316, 1316, 1316,
     1316, 1941, 1882, 1882, 1882, 1882, 2029, 1957, 1957, 1957,
     1957, 1957, 1957, 1957, 1957, 1957, 1882, 1882, 1957, 1957,
     1882, 1957, 1882, 1882, 1882, 1957, 1957, 1957, 1957, 1957,

     2010, 1957, 1957, 1957, 1882, 1957, 1957, 1957, 1957, 1957,
     1957, 1882, 1957, 1957, 1882, 1882, 2012, 1882, 1957, 1957,
     2014, 2015, 1882, 1882, 2032, 1941, 1941, 1941, 1941, 1941,
     1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941,
     1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941,
     1941, 1941, 1941, 1882, 1882, 2019, 1316, 1316, 1316, 1316,
     1941, 1882, 2033, 1882, 1882, 1882, 2034, 1882, 1882, 1957,
     1957, 1957, 1957, 1957, 1957, 1957, 1882, 1882, 1957, 1957,
     1882, 1957, 1882, 1882, 1957, 1957, 1957, 1882, 1882, 1882,
     1957, 1957, 1882, 1882, 1957, 1882, 1957, 1957, 1882, 1882,

     2012, 1882, 1957, 2014, 2015, 1882, 2032, 1941, 1941, 1941,
     1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941,
     1941, 1941, 1941, 1941, 1941, 1882, 2035, 2019, 1316, 1316,
     1941, 2033, 2033, 2033, 1882, 2034, 2034, 2034, 1957, 1957,
     1957, 1957, 1882, 1882, 1957, 1957, 1882, 1882, 1882, 1882,
     1957, 1882, 1957, 1882, 1957, 1957, 1882, 2012, 1882, 2014,
     2015, 1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941, 1941,
     1941, 2035, 1882, 2035, 2035, 2019, 1316, 2033, 2033, 2036,
     2034, 1882, 2034, 1957, 1882, 1882, 1882, 1882, 1957, 1957,
     1957, 1882, 1957, 1882, 2012, 1882, 2014, 2015, 1941, 1941,

     1941, 1941, 1941, 1882, 1882, 1882, 2037, 2038, 2035, 2035,
     2039, 2019, 2036, 2036, 2036, 1882, 1882, 1882, 1882, 1957,
     1957, 1882, 1957, 2012, 1882, 2014, 2040, 1941, 1941, 1941,
     1882, 1882, 2037, 2038, 2035, 2035, 2035, 2041, 2042, 2039,
     2039, 2039, 2019, 2036, 2033, 2036, 1882, 1882, 1882, 1882,
     1957, 1957, 1882, 1957, 2012, 1882, 2014, 2040, 1882, 1941,
     1941, 1941, 1882, 1882, 2035, 2035, 2041, 2041, 2041, 2042,
     1882, 2042, 2042, 2039, 2035, 2039, 2019, 1882, 1882, 1957,
     1882, 1957, 2012, 1882, 2014, 1941, 1941, 1882, 1882, 2035,
     2035, 2041, 2035, 2041, 2042, 2043, 2019, 1882, 1957, 1882,

     1957, 2012, 1882, 2014, 1941, 1941, 1882, 2035, 2035, 2035,
     2043, 2043, 2043, 2019, 1882, 1882, 2012, 1882, 2014, 1882,
     2035, 2044, 2043, 2043, 2019, 2012, 1882, 2014, 1882, 2035,
     2039, 2035, 2035, 2019, 2012, 1882, 2014, 2035, 2019, 2012,
     1882, 2014, 2019, 2012, 1882, 2014, 2019, 2012, 1882, 2045,
     1882, 1882, 2046, 2014, 2019, 1882, 2047, 1882, 1882, 2048,
     2045, 1882, 1882, 2046, 1882, 2014, 2047, 1882, 2048, 2014,
     2014, 2014, 1882, 2049, 1882, 1882, 2050, 2049, 1882, 2050,
     1882,    0, 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882,
     1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882,

     1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882,
     1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882,
     1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882,
     1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882,
     1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882,
     1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882,
     1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882,
     1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882,
     1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882,
     1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882,

     1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882,
     1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882,
     1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882,
     1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882,
     1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882
    } ;

static yyconst flex_int16_t yy_nxt[9689] =
    {   0,
       18,   19,   20,   19,   21,   22,   18,   23,   24,   25,
       26,   27,   28,   29,   28,   30,   28,   31,   32,   33,
       34,   35,   36,   37,   38,   39,   40,   41,   42,   43,
       44,   45,   46,   47,   46,   48,   49,   50,   51,   52,
       53,   46,   54,   55,   56,   57,   46,   58,   46,   46,
       59,   28,   28,   28,   39,   40,   41,   42,   43,   44,
       45,   46,   47,   48,   49,   50,   51,   52,   53,   46,
       54,   55,   56,   57,   46,   58,   46,   46,   59,   18,
       60,   61,   60,   62,   70, 1881,   71,   74,   73,   74,
       76,   77,   76,   76,   77,   76,  241,  478,  478,   78,

      112,  242,   78,  114,  244,  114,   63,   64,  116,   71,
       65,  248,   66,  248,  113,   75,  112, 1879,  119,  340,
      112, 1881,  117,   67,  227,  220,  227,  221,  342,  112,
      113,  114,  112,  114,  113,   63,   64,  116,   71,   65,
      247,   66,  113,  120,   75,  112,  113,  592,  340,  112,
      117,   67,   60,   61,   60,   62,  342,  121,  113,  112,
      136,  112,  113,  593,  311,  155,  257,  112,  247,  312,
      350,  257,  120,  113,  113,  112,  112,  112,   63,   64,
     1879,  113,   65,  257,   66,  121,  349,  257,  112,  113,
      113,  113,  311,  283,  257,   67,  112,  312,  350,  257,

      248,  113,  248, 1862,  112,  112,  112,   63,   64,  113,
     1868,   65,  257,   66, 1865,  349,  257,  113,  113,  113,
     1031,  283, 1031,   67,   18,   19,   79,   19,   21,   22,
       18,   23,   24,   25,   26,   27,   28,   29,   28,   30,
       28,   31,   32,   33,   34,   35,   36,   37,   38,   80,
       81,   82,   83,   84,   85,   86,   87,   88,   87,   89,
       90,   91,   92,   93,   94,   87,   95,   96,   97,   98,
       87,   99,   87,   87,  100,   28,   28,   28,   80,   81,
       82,   83,   84,   85,   86,   87,   88,   89,   90,   91,
       92,   93,   94,   87,   95,   96,   97,   98,   87,   99,

       87,   87,  100,   18,   60,  101,  102,   62,  156,  122,
      122,  122,  123,  125,  351,  112,  103,  319, 1862,  320,
      126,  127,  104,  352,  399,  157,  158,  119,  256,  113,
      105,  106,  112,  876,  107,  321,  108,  395,  112,  395,
      375,  128,  396,  351,  112,  809,  113,  109,  327,  877,
      328,  352,  113,  399, 1862,  129,  130,  113,  130,  105,
      106,  112,  376,  107,  355,  108,  329,  112,  375,  131,
      128,  131, 1868,  132,  113,  109,  110,   61,  110,   62,
      113,  166,  162,  129,  159,  134,  162,  146,  147,  146,
      376,  112,  355,  403,  111,  148,  165,  383,  149,  135,

      165,  384,   63,   64,  150,  113,   65, 1865,   66,  151,
      166,  162,  162,  257,  134,  162,  112,  288, 1862,   67,
      112,  186,  403,  257,  165,  383,  165,  135,  165,  384,
      113,   63,   64,  113,  344,   65,  344,   66,  346,  256,
      346,  162,  257,  256, 1803,  112,  288,   67,  137,  186,
      137,  257,  345,  119,  165,  218,  347,  152,  113,  153,
      122,  122,  122,  123,  256, 1759,  138,  348,  154,  154,
      112,  161,  139,  354,  162,  162,  140,  112,  141, 1705,
      163,  154,  400,  142,  113,  143,  144,  164,  165,  165,
      401,  113,  245,  244,  245,  145,  348,  154,  154,  112,

      161,  139,  354,  162,  162,  140,  112,  141,  163,  154,
      400,  142,  113,  143,  144,  164,  165,  165,  401,  113,
      246, 1705,  402,  145,  167,  162,  172,  162,  191,  162,
      173,  168,  192,  162,  118,  169,  174, 1143,  170,  165,
      229,  165,  193,  165,  125,  230, 1759,  165,  171,  246,
      402,  126,  127,  167,  162,  172,  162,  191,  162,  173,
      168,  192,  162,  169,  174,  187,  170,  165,  231,  165,
      193,  165,  188,  189,  190,  165,  171,  162, 1033,  165,
     1033,  404,  232,  175,  118,  176,  177, 1328,  178,  179,
      233,  165,  406, 1705,  187,  180,  125,  231, 1705,  256,

      188,  189,  190,  126,  127,  256,  162,  165,  234,  404,
      232,  175,  162,  176,  177,  405,  178,  179,  412,  165,
      406,  194,  235,  180,  162,  662,  165,  181,  125,  256,
      182,  183,  130,  184,  130,  126,  127,  234,  165,  185,
      409,  162,  417,  195,  405,  131,  412,  131,  162,  194,
      235,  363,  256,  162,  165,  196,  181,  197,  182,  183,
      162,  184,  165,  198,  202,  397,  165,  185,  199,  409,
      200,  417,  195,  229,  165,  201,  160,  162,  203,  416,
      363,  204,  398,  196,  205,  197,  206,  162,  256,  162,
      165,  198,  212,  202,  397,  418,  199,  162,  200,  213,

      256,  165,  165,  201,  160,  377,  203,  416, 1696,  204,
      398,  165,  205,  207,  662,  206,  162,  208,  162,  256,
      378,  212,  162,  418, 1517,  419,  162,  209,  213,  165,
      214,  256,  210,  211,  377,  215,  165,  125,  162,  165,
      216, 1882,  207, 1882,  126,  127,  208,  162,  378,  256,
      217,  162,  165,  419, 1882,  209, 1882,  256,  214,  259,
      210,  211,  442,  215,  165,  381,  256,  162,  420,  216,
      423,  256,  256,  262,  256,  382,  256,  364,  217,  241,
      165,  219,  220,  219,  221,  236,  162,  256,  259,  172,
      162,  442,  237,  173,  381,  186,  420,  263,  423,  174,

      165,  262,  259,  382,  165,  364,  424,  222,  223,  323,
      112,  224,  256,  225,  236,  162,  262,  459,  172,  162,
      237,  256,  173,  186,  226,  425,  263,  174,  165,  256,
     1521,  259,  165,  426,  424,  256,  222,  223,  212,  112,
      224,  187,  225,  162,  262,  213,  459,  256,  188,  238,
      190,  470,  226,  425,  256,  165,  256,  165,  407,  408,
     1556,  426,  239,  251,  252,  253,  254,  212, 1633,  427,
      187, 1634,  162,  256,  213,  255,  188,  238,  190,  258,
      470,  256,  259,  165,  284,  165,  407,  408,  260,  255,
      239,  285,  286,  287,  432,  261,  262,  427,  262,  289,

      269,  259,  257,  257,  270,  472,  257,  256,  258,  290,
      271,  259,  291,  284,  440,  262,  260,  441,  255,  285,
      286,  287,  432,  261,  262,  256,  262,  473,  289,  269,
      259,  257,  257,  270,  472,  257,  445,  290,  271,  256,
      291,  446,  440,  262,  474,  441,  379,  410,  264,  323,
      257,  299,  411,  259,  278,  265,  473,  279,  280,  266,
      281,  380,  267,  257,  445,  300,  282,  262,  301,  446,
      256,  302,  268,  474,  413,  379,  410,  264,  218,  257,
      299,  411,  259,  278,  265,  279,  280,  266,  281,  380,
      267,  257,  229,  300,  282,  262,  301,  477,  259,  302,

      268,  257,  428,  483,  428,  257, 1633,  447,  272, 1634,
      273,  274,  262,  275,  276,  414,  415,  292,  303,  256,
      277,  257,  257,  256,  256,  257,  477,  259,  257,  293,
      257,  294,  483,  256,  257,  447,  272,  295,  273,  274,
      262,  275,  276,  414,  415,  429,  292,  303,  277,  448,
      257,  257,  259,  443,  257,  256,  257,  293,  323,  294,
      296,  444,  297,  449,  304,  295,  262,  298,  305,  324,
      138,  452,  257,  429,  256,  256,  256,  448,  306,  374,
      374,  259,  443,  307,  308,  692,  325,  692,  296,  444,
      297,  449,  374,  304,  262,  298,  256,  305,  309,  452,

      326,  257,  259,  259,  313,  310,  306,  693,  374,  374,
      453,  307,  308,  454,  314,  325,  262,  262,  421,  500,
      374,  500,  254,  315,  316,  317,  318,  309,  326,  422,
      256,  259,  259,  313,  310,  255,  257,  490,  453,  465,
      257,  454,  314,  341,  262,  262,  283,  421,  466,  255,
      358,  359,  358,  146,  147,  146,  256,  422,  360,  361,
      500,  148,  501,  254,  256,  257,  490,  465,  256,  257,
      150,  341,  256, 1031,  283, 1031,  466,  256,  255,  322,
      322,  330,  322,  322,  322,  322,  331,  322,  322,  322,
      322,  322,  322,  322,  322,  322,  322,  322,  332,  322,

      322,  322,  322,  322,  333,  332,  332,  332,  332,  334,
      332,  335,  332,  332,  332,  336,  332,  332,  337,  332,
      332,  332,  332,  338,  332,  332,  332,  332,  339,  332,
      322,  322,  332,  333,  332,  332,  332,  332,  334,  332,
      335,  332,  332,  336,  332,  332,  337,  332,  332,  332,
      332,  338,  332,  332,  332,  332,  339,  332,  322,  309,
      359,  365,  430,  365,  430, 1518,  310,  622,  361,  467,
      257,  229,  431,  257, 1882,  366, 1882,  367,  365,  368,
      365,  471,  484,  343,  631, 1033,  399, 1033,  309,  362,
      394, 1882,  366, 1882,  367,  310,  368,  467,  257,  154,

      154,  257,  370,  365,  137,  365,  137,  244,  353,  471,
      484,  343,  154,  369,  485,  399, 1882,  366, 1882,  367,
      119,  368,  385,  386,  387,  388,  323,  323,  154,  154,
      369,  370,  372,  498,  389,  390,  391,  390,  392,  457,
      154,  373,  485,  148,  371,  458,  256,  152,  389,  153,
      229,  145,  150,  433,  455,  369,  437,  438,  154,  154,
      450,  372,  498,  439,  456,  434,  256,  435,  457,  373,
      436,  154,  371,  458,  229,  451,  636,  389,  460,  145,
      461,  256,  433,  455,  437,  438,  475,  154,  154,  450,
      476,  439,  456,  434,  468,  435,  462,  463,  436,  154,

      464, 1695, 1882,  451,  229,  636,  229,  460,  469,  461,
      350,  402,  481,  256,  489,  475,  479,  486, 1633,  476,
      241, 1634,  256,  468,  462,  463,  482,  487,  464,  488,
      244, 1882,  227,  220,  227,  221,  469,  493,  350,  402,
      494,  481,  489,  497,  479,  486,  227,  220,  227,  221,
      227,  220,  227,  221,  482,  404,  487,  433,  488,  407,
      492,  496,  491,  256,  616,  493,  247,  256,  494,  434,
      256,  435,  497,  617,  495,  245,  244,  245,  256,  251,
      252,  253,  254,  404,  229,  229,  433,  407,  492,  496,
      491,  255,  616,  511,  247,  257,  590,  434,  590,  435,

      515,  617,  495,  246, 1375,  255,  257,  504,  505,  506,
      507,  631,  257,  257,  257,  257,  362,  257,  323,  508,
      257,  618,  511,  323,  257,  591,  119,  509,  323,  515,
      257,  639,  246,  508,  255,  257,  619,  257,  257,  257,
      257,  257,  257,  257,  510,  257,  257,  241,  257,  618,
      353,  595,  257,  119,  591,  512,  509,  620,  596,  257,
      639,  257,  508,  257,  619,  257,  257,  257,  513,  257,
      257,  327,  510,  594, 1712,  257,  514,  323,  257,  516,
      595,  257,  517,  512,  257,  620,  596,  257,  257,  593,
      257,  257,  257,  521,  256,  323,  513,  257, 1726,  257,

      518,  257,  519,  520,  514,  257,  257,  516,  256,  257,
      257,  517,  359,  257,  119,  257,  413,  257,  600,  622,
      361,  257,  521,  586,  220,  586,  318,  257,  518,  257,
      519,  520,  257,  257,  522,  257,  257,  257,  257,  523,
      257,  256,  524,  527,  257,  257,  528,  600,  256,  257,
      529,  635,  257,  257,  621,  256,  257,  525,  526,  349,
      256,  257,  257,  522,  257,  257,  257,  256,  523,  257,
      524,  527,  257,  615,  257,  528,  257,  257,  529,  635,
      257,  257,  621,  257,  530,  525,  526,  650,  349,  257,
      257,  531,  344,  257,  344,  257,  257,  256,  534,  257,

      256,  615,  532,  257,  256,  257,  257,  257,  229,  229,
      345,  257,  530,  533,  652,  650,  257,  535,  257,  531,
      536,  257,  537,  926,  257,  257,  534, 1158,  257,  257,
     1149,  532,  257,  428,  257,  428,  257,  430,  257,  430,
      538,  533,  652,  665,  257,  535,  257,  431,  536,  256,
      537,  540,  257,  666,  256,  257,  257,  256,  257,  257,
     1802,  511,  627,  257,  627,  545,  546,  257,  538,  257,
      541,  665,  547,  257,  257,  628,  539,  628,  257,  540,
      257,  666,  542,  257,  543,  257,  560,  544,  257,  548,
      511,  256,  257,  545,  546,  257,  668,  241,  257,  541,

      547,  257,  257,  663,  539,  257,  256,  257,  257,  550,
      542,  256,  543,  549,  560,  544,  257,  548,  256,  257,
      257,  359,  257,  257,  668,  554,  257,  551,  622,  361,
      553,  257,  663,  257,  257,  552,  323,  257,  550,  332,
      257,  549,  605,  257,  669,  257,  332,  257, 1804,  257,
      257,  555,  365,  554,  365,  257,  551,  257,  553,  257,
      257,  257,  558,  552,  257,  257,  366,  332,  556,  257,
      605,  257,  669,  557,  332,  256,  257,  559,  257,  555,
      670,  257,  257,  664,  563,  257,  257, 1814,  257,  257,
      667,  558,  565,  257,  564,  257,  556,  561,  566,  257,

      562,  557,  256,  257,  257,  559,  505,  257,  670,  257,
      671,  257,  664,  563,  672,  257,  567,  257,  568,  667,
      569,  565,  564,  257,  257,  561,  566,  257,  562,  679,
      637,  257,  637,  505,  257,  638,  570,  571,  671,  970,
      572,  257,  672,  573,  969,  567,  257,  568,  680,  569,
      257,  682,  327,  257,  878,  257,  578,  257,  679,  574,
      683,  576,  257,  257,  570,  571,  681,  257,  572,  257,
      877,  573,  257,  575,  257,  577,  257,  680,  514,  257,
      257,  257,  257,  580,  257,  578,  257,  574,  581,  683,
      576,  579,  257,  257,  653,  257,  653,  388,  257,  257,

      257,  575,  257,  577,  662,  257,  514,  257,  257,  582,
      257,  257,  580,  597,  257,  597,  583,  581,  684,  579,
      584,  257,  257, 1031,  257, 1031,  257,  257,  585,  926,
      386,  257,  315,  316,  317,  318,  257,  677,  582,  257,
      386,  678,  591,  257,  255,  583,  597,  684,  598,  584,
      702,  257,  257,  586,  220,  587,  318,  585,  255,  257,
      920,  327,  705,  594,  593,  677,  919,  257,  346,  678,
      346,  591, 1033,  603, 1033,  591,  694,  152,  702,  599,
      504,  505,  506,  507,  332,  604,  345,  255,  154,  154,
      705,  332,  508,  332,  606,  607,  332,  614,  917,  916,

      332,  154,  603,  332,  591,  694,  508,  358,  359,  358,
      638,  365,  332,  365,  604,  360,  361,  154,  154,  332,
      638,  332,  606,  607,  332,  366,  614,  332,  332,  154,
      327,  332,  327,  608,  332,  508,  322,  322,  330,  322,
      322,  322,  322,  322,  322,  322,  322,  322,  322,  322,
      322,  322,  322,  322,  322,  332,  322,  322,  322,  322,
      322,  608,  332,  609,  257,  257,  541,  358,  359,  358,
      700,  653,  332,  654,  388,  360,  361,  611,  542,  332,
      543,  610,  257,  612,  346,  701,  346,  322,  322,  125,
      127,  589,  609,  257,  257,  541,  126,  127,  256,  700,

      332, 1414,  430, 1414,  430,  611,  542,  332,  543,  610,
      257,  612,  431,  614,  701,  322,  322,  322,  330,  322,
      322,  322,  322,  322,  322,  322,  322,  322,  322,  322,
      322,  322,  322,  322,  322,  706,  322,  322,  322,  322,
      322,  257,  614,  125,  346,  256,  346,  365,  125,  365,
      126,  127,  601,  505,  613,  126,  127,  708,  257,  602,
      152,  366,  347,  367,  706,  630,  673,  322,  322,  674,
      257,  154,  154,  614,  365,  505,  365,  625,  252,  252,
      601,  703,  613,  703,  154,  708,  257,  602,  366,  713,
      367,  626,  368,  250,  673,  322, 1418,  674, 1418,  369,

      154,  154,  614,  704,  365,  625,  365,  365,  640,  365,
      640,  229,  154,  695,  647,  695,  647,  713,  366,  626,
      367,  366,  368,  367,  641,  368,  369,  710,  765,  719,
      648,  385,  386,  387,  388,  696,  711,  712,  396,  633,
      698,  699,  396,  389,  390,  391,  390,  392,  428,  697,
      428,  634,  148,  642,  649,  241,  369,  389,  719,  369,
      413,  150,  413,  657,  658,  659,  660,  633,  698,  699,
      675,  148,  714,  722,  394,  389,  681,  697,  682,  634,
      150,  642,  649,  374,  374,  717,  389,  676,  715,  389,
      662,  707,  716,  720,  721,  723,  374,  723,  718,  675,

      714,  413,  722,  414,  725,  386, 1842,  726,  727,  728,
      731,  735,  374,  374,  717,  676,  715,  724,  389,  707,
      716,  720,  721,  729,  374,  729,  718,  732,  733,  413,
      734,  414,  685,  725,  685,  726,  736,  727,  728,  731,
      735,  737,  738,  739,  742,  730,  386,  651,  743,  740,
      646,  645,  750,  744,  686,  732,  733,  741,  734,  687,
      644,  745,  751,  752,  688,  736,  753,  754,  759,  737,
      738,  755,  739,  742,  689,  690,  743,  740,  691,  746,
      750,  746,  744,  756,  757,  741,  229,  758,  687,  745,
      751,  760,  752,  688,  761,  753,  754,  759,  762,  755,

      763,  747,  689,  690,  764,  486,  691,  766,  767,  768,
      769,  756,  757,  770,  748,  758,  771,  229,  229,  760,
      773,  775,  761,  749,  776,  778,  762,  875,  763,  779,
      780,  781,  764,  486,  241,  766,  767,  768,  769,  675,
      643,  770,  748,  718,  771,  256,  772,  256,  773,  631,
      775,  749,  256,  776,  778,  875,  777,  127,  779,  780,
      781,  774,  256,  782,  783,  244,  783,  785,  675,  785,
      254,  718,  791,  256,  785,  772,  786,  254,  787,  256,
      787,  507,  256,  792,  777,  504,  505,  506,  507,  774,
      796,  787,  782,  788,  507,  256,  784,  508,  256,  794,

      256,  791,  256,  256,  623,  256,  256,  793,  122,  256,
      795,  508,  792,  256,  413,  797,  125,  256,  796,  256,
      119,  256,  256, 1146, 1147,  784,  256,  794,  256,  798,
      809,  803,  808,  256,  323,  793,  805,  806,  799,  795,
      508,  800,  807,  797,  801,  810,  323,  802,  804,  811,
      589,  692,  256,  692,  826,  525,  256,  798,  825,  817,
      803,  808,  256,  256,  805,  806,  799,  819,  820,  800,
      256,  807,  801,  693,  810,  802,  804,  256,  811,  695,
      256,  695,  826,  525,  685,  256,  685,  825,  817,  821,
      822,  703,  256,  703,  256,  819,  820,  823,  256,  256,

      256,  696,  256,  256,  252,  824,  686,  256,  256,  252,
      256,  812,  499,  704,  256,  818,  813,  250,  821,  822,
      829,  256,  244,  256,  256,  823,  814,  815,  827,  241,
      816,  256,  828,  824,  831,  833,  836,  723,  256,  723,
      812,  834,  830,  818,  835,  813,  838,  832,  829,  839,
      256,  840,  837,  256,  814,  815,  827,  256,  816,  724,
      828,  843,  845,  831,  833,  836,  729,  256,  729,  834,
      830,  256,  835,  256,  838,  832,  256,  841,  839,  256,
      840,  837,  256,  256,  256,  256,  229,  842,  730,  843,
      256,  845,  615,  844,  846,  256,  256,  256,  256,  229,

      256,  256,  362,  746,  256,  746,  841,  256,  256,  256,
      849, 1418,  848, 1418,  847,  842,  256,  853,  850,  852,
      615,  844,  854,  846,  861,  747,  851,  858,  859,  256,
      855,  860,  256,  256,  862,  122,  865,  864,  856,  849,
      848,  863,  847,  256,  868,  853,  850,  857,  852,  866,
      871,  854,  323,  861,  851,  858,  859,  353,  855,  256,
      860,  867,  119,  862,  865,  864,  856,  250,  323,  863,
      244,  869,  868,  870,  241,  857,  323,  866,  871,  873,
      220,  873,  318,  896, 1882,  872,  873,  220,  874,  318,
      867,  315,  316,  317,  318,  881,  597,   73,  879,  869,

      332,  870,  880,  255,  597,  883,  597,  886,  597,  332,
      879,  332,  896,  872,  877,   70,  894,  255,  921, 1882,
      921,  388,  332,  881,  887,  591,  877, 1882, 1882,  332,
      880,  332, 1882,  591,  883,  886,  888,  591,  332,  889,
      332,  327,  890,  878,  894,  256,  255,  590,  220,  590,
      221,  332,  887,  901,  591,  332,  256,  152,  903,  882,
      332,  899,  591,  256,  888,  900,  591,  889,  154,  154,
      890,  893,  897,  222,  223,  930,  591,  224,  365,  225,
      365,  154,  901,  898,  332,  832,  895,  903,  902,  899,
      479, 1882,  366,  900, 1882, 1882,  631,  154,  154,  893,

      119,  897,  222,  223,  930,  591,  224, 1673,  225,  154,
     1882, 1882,  898,  832, 1674,  895,  902, 1675,  479,  322,
      322,  330,  322,  322,  322,  322,  322,  322,  322,  322,
      322,  322,  322,  322,  322,  322,  322,  322,  904,  322,
      322,  322,  322,  322,  891,  627,  332,  627,  918,  125,
     1882,  640, 1882,  640, 1882,  884,  126,  127,  628,  927,
      628,  892,  647, 1882,  647, 1882,  904,  641, 1882, 1882,
      322,  322,  119,  891, 1882,  332,  918, 1882,  648,  365,
     1882,  365,  921,  884,  922,  388, 1474,  927, 1474,  892,
      906, 1882, 1474,  366, 1474,  931,  905,  630,  322,  322,

      322,  330,  322,  322,  322,  322,  322,  322,  322,  322,
      322,  322,  322,  322,  322,  322,  322,  322,  906,  322,
      322,  322,  322,  322,  931,  905,  125,  908,  885,  908,
      932,  369, 1882,  126,  127, 1882,  928,  934,  909,  640,
     1882,  640, 1882,  910,  365,  911,  365,  365,  929,  365,
      322,  322,  647,  907,  647,  641,  933,  885,  366,  932,
      367,  366,  368,  367,  928,  368,  934,  936,  648,  935,
     1003,  692, 1003,  692, 1882,  915,  929,  937,  322,  912,
     1004,  938,  907,  941,  933,  942,  914,  939, 1882,  923,
      391,  923,  660,  693,  940,  936,  369,  148,  935,  369,

      657,  658,  659,  660,  915,  937,  150,  943,  148,  938,
      944,  941,  389,  942,  914,  953,  939,  150,  954,  923,
      391,  924,  660,  940,  955,  957,  389,  148,  956,  390,
      391,  390,  392,  958,  964,  943,  150,  148,  944,  945,
      695,  945,  695,  953,  959,  960,  150,  954,  961,  967,
      968, 1882, 1882,  955,  957,  389,  956,  962,  703,  962,
      703,  958,  696,  964,  965,  710,  965,  974,  973, 1882,
      975,  946,  959,  960,  711,  712,  961,  967,  968,  963,
      704,  971,  710,  971,  980,  947,  966,  986,  976,  979,
      981,  711,  712,  982,  972,  974,  972,  973,  975,  977,

      946,  985,  983,  972,  983,  723,  978,  723,  992,  987,
      993,  987,  980,  947,  685,  986,  685,  976,  979,  981,
      994,  995,  982,  989,  984,  989,  997,  724,  977, 1882,
      985,  988,  996,  729,  978,  729,  686,  992,  998,  993,
      999,  948, 1000, 1882, 1001,  990,  949, 1002,  994,  995,
     1007,  746, 1008,  746,  997,  730,  950,  951,  991,  972,
      952,  996, 1009, 1010, 1011, 1005,  998, 1005,  999, 1012,
      948, 1000, 1001,  747, 1013,  949, 1002, 1014, 1007, 1015,
     1008, 1016, 1018, 1016,  950,  951,  991, 1006,  952, 1019,
     1009, 1010, 1020, 1011, 1021,  229, 1022, 1023, 1012, 1024,

     1025, 1026, 1013, 1017,  229, 1014,  229, 1032, 1015, 1030,
     1034, 1018,  241,  256,  244,  939,  256, 1882, 1019,  256,
      977, 1020,  256, 1021, 1022, 1882, 1023,  978, 1024, 1025,
     1026, 1035, 1036, 1029, 1036, 1027, 1032, 1030, 1041, 1034,
     1041,  254, 1037,  256,  939, 1028, 1038, 1048, 1049,  977,
     1040, 1038, 1039, 1046, 1045,  978,  783,  244,  783, 1035,
      256,  256, 1029, 1027, 1041,  256, 1042,  254, 1043,  256,
     1043,  507, 1043, 1028, 1044,  507, 1048, 1049, 1040,  256,
     1039, 1046, 1045,  256, 1047,  256,  256, 1050,  784,  256,
      256,  256, 1052,  256,  945,  256,  945,  256,  256, 1055,

      256,  256, 1051,  256,  256,  256, 1053,  256,  256, 1882,
     1137,  256, 1047, 1058, 1882,  256, 1050,  784, 1114,  256,
     1054, 1052, 1057, 1882, 1059, 1067, 1063, 1055, 1056, 1061,
     1051, 1062, 1060, 1066, 1069, 1053,  256, 1065,  256, 1137,
     1064,  256, 1058,  256, 1882, 1070, 1114, 1073, 1054, 1071,
     1068, 1057, 1059, 1072, 1067, 1063, 1056, 1061, 1077, 1062,
     1060, 1075, 1066, 1069,  256, 1065, 1074,  256, 1064,  962,
      256,  962, 1076, 1070, 1079, 1073,  256, 1071, 1068,  256,
     1078, 1072,  965,  256,  965,  256, 1077,  256,  256, 1075,
     1882,  963,  256, 1080,  256, 1074,  256, 1882, 1082,  256,

     1081, 1076, 1085, 1079,  966,  256,  256,  256, 1078,  256,
      256,  256, 1083, 1086, 1084,  983,  256,  983,  256, 1090,
      256, 1087, 1080,  987,  256,  987,  256, 1082, 1081, 1092,
     1089, 1085,  989,  256,  989, 1882, 1093,  984, 1094, 1882,
     1083, 1097, 1086, 1084, 1091,  988, 1098, 1095, 1090, 1087,
      256, 1100, 1096,  256,  990,  256, 1103, 1092,  256, 1089,
     1101, 1003,  256, 1003,  256, 1093, 1094, 1088,  256,  256,
     1097, 1004, 1091,  256, 1098, 1095, 1005,  256, 1005, 1100,
     1096, 1099,  256, 1104, 1103, 1016,  256, 1016, 1101, 1105,
      256,  323, 1107, 1102, 1136, 1088, 1108, 1193, 1006, 1193,

     1109, 1112,  220, 1112,  318, 1882,  327, 1017, 1117, 1110,
     1099, 1106, 1104, 1112,  220, 1113,  318, 1111, 1105, 1194,
     1107, 1102, 1136, 1882, 1116, 1108,  597,  323, 1118, 1109,
     1882,  323, 1119,  597,  322, 1118,  322, 1139, 1110, 1106,
      322,  327,  322, 1117, 1116, 1882, 1111,  590,  220, 1115,
      221, 1116,  256, 1882, 1120,  591, 1882,  152, 1125, 1121,
     1119,  119,  591, 1882, 1163, 1116, 1139, 1164,  154,  154,
     1882, 1122, 1165,  222,  223, 1134,  591,  224,  322,  225,
      322,  154, 1124, 1120,  591, 1141,  365, 1125,  365, 1167,
      479,  591, 1163, 1882,  322, 1164,  322,  154,  154, 1122,

      366, 1165,  222,  223, 1134,  591,  224, 1882,  225,  154,
     1124,  322, 1882,  322, 1141, 1882, 1127, 1167,  479,  322,
      322,  330,  322,  322,  322,  322,  322,  322,  322,  322,
      322,  322,  322,  322,  322,  322,  322,  322, 1129,  322,
      322,  322,  322,  322, 1127, 1882, 1128, 1033,  256, 1033,
     1036,  256, 1036, 1166,  908, 1159,  908, 1159,  388, 1169,
     1037,  322, 1170,  322, 1038,  909, 1129, 1171, 1172, 1038,
      322,  322, 1882, 1159, 1128, 1160,  388, 1161,  391, 1161,
      660, 1250, 1166, 1250,  254,  148, 1003, 1169, 1003, 1126,
     1250, 1170, 1251,  254,  150, 1171, 1004, 1172,  322,  322,

     1123,  330, 1123,  322,  322,  322,  322,  322,  322,  322,
      322,  322,  322,  322,  322,  322,  322,  322, 1126,  322,
      322,  322,  322,  322,  322,  322,  322,  322,  125,  322,
      256,  322, 1173, 1174, 1175,  126,  127, 1161,  391, 1162,
      660, 1176, 1882, 1177, 1183,  148, 1186,  908, 1130,  908,
      322,  322, 1882, 1132,  150, 1144, 1131, 1133,  909, 1080,
     1173, 1174, 1175,  910, 1178, 1148, 1081, 1181, 1882, 1176,
     1135, 1177, 1183,  365, 1186,  365, 1882, 1130,  322,  908,
     1182,  908, 1132, 1184, 1144, 1131, 1133,  366, 1080,  367,
      909,  368, 1185, 1178, 1081,  910, 1181,  911, 1135,  912,

     1882, 1151,  657,  658,  659,  660, 1236, 1882, 1236, 1182,
      148,  627, 1184,  627,  389, 1252, 1237, 1252,  507,  150,
     1252, 1185, 1253,  507,  628,  369,  628,  945,  389,  945,
     1151,  912, 1138, 1152, 1187, 1152, 1138, 1138, 1138, 1138,
     1138, 1138, 1138, 1138, 1138, 1138, 1138, 1153, 1138, 1154,
     1138, 1155, 1138, 1138, 1138, 1138, 1138,  389, 1188, 1179,
     1882, 1189, 1190, 1187, 1191, 1192, 1882, 1195,  962, 1196,
      962, 1205, 1882, 1180,  965, 1206,  965, 1207, 1208, 1882,
     1209, 1210, 1214, 1138, 1138, 1157, 1188, 1882, 1179, 1189,
      963, 1190, 1882, 1191, 1192, 1195,  966, 1414, 1196, 1414,

     1205, 1180, 1211, 1882, 1206, 1207, 1882, 1208, 1209, 1210,
     1214, 1138, 1138, 1138, 1215, 1138, 1138, 1138, 1138, 1138,
     1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138,
     1138, 1211, 1138, 1138, 1138, 1138, 1138, 1197, 1199, 1197,
     1199, 1882, 1882, 1215, 1236, 1212, 1236, 1212,  969,  970,
     1198, 1200, 1198, 1200, 1237,  971,  710,  971, 1216, 1198,
     1200, 1217,  710, 1138, 1138,  711,  712, 1213,  972, 1218,
      972, 1202, 1203,  983, 1204,  983, 1204,  972,  987,  989,
      987,  989, 1219, 1204, 1220, 1221, 1882, 1216, 1222, 1223,
     1217, 1138, 1224, 1225, 1226,  984, 1227, 1005, 1218, 1005,

      988,  990, 1228, 1229, 1231, 1230, 1232, 1882, 1233, 1234,
     1235, 1219, 1220, 1238, 1221, 1198, 1200, 1222, 1223, 1006,
     1224, 1239, 1225, 1226, 1227, 1016, 1240, 1016,  229,  229,
     1228, 1229, 1231,  972, 1230, 1232, 1233, 1234, 1235, 1204,
     1244, 1036, 1238, 1036, 1245,  244, 1246, 1017, 1247,  256,
     1239, 1037,  241,  256, 1240, 1038, 1242,  256,  256, 1241,
     1038, 1036, 1038, 1036, 1038, 1310, 1882,  256,  256, 1244,
     1414, 1037, 1414, 1245, 1246, 1038, 1038, 1247, 1882, 1882,
     1038, 1038, 1348, 1248, 1256, 1242, 1882, 1249, 1241, 1255,
     1254, 1257, 1261, 1310, 1258, 1236,  256, 1236, 1308,  220,

     1308,  318, 1882, 1882,  256, 1237, 1308,  220, 1309,  318,
     1348,  256, 1248, 1256,  256, 1249,  256, 1255, 1254,  256,
     1257, 1261, 1258, 1138, 1138,  256, 1138, 1138, 1138, 1138,
     1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138,
     1138, 1138, 1260, 1138, 1138, 1138, 1138, 1138,  256,  256,
     1262, 1264, 1265,  256,  256,  256, 1266,  256,  256,  256,
      256,  256,  332,  256,  256,  256, 1882,  256,  256,  256,
     1260,  256,  256, 1882, 1138, 1138, 1263,  256, 1262, 1264,
     1265,  256, 1267, 1270, 1266, 1269, 1275, 1271, 1274, 1276,
      256,  332, 1273, 1272, 1268, 1280, 1278, 1193,  256, 1193,

     1277, 1285, 1138, 1882, 1282, 1263, 1279,  256, 1281, 1283,
     1267,  256, 1270, 1286, 1269, 1275, 1271, 1274, 1276, 1194,
     1273, 1272, 1268,  256, 1280, 1278, 1284,  256, 1277,  256,
     1285,  256,  256, 1282, 1287, 1279, 1281, 1283, 1212,  256,
     1212, 1286,  256, 1292,  256,  256,  256,  256,  256,  256,
     1288,  256,  256,  256, 1284,  256,  256, 1290, 1289, 1293,
     1213,  256, 1295, 1287,  256,  332, 1882, 1297, 1291,  323,
      332, 1292, 1294, 1298, 1123,  323, 1123, 1324, 1302, 1288,
     1318, 1882, 1301, 1304, 1882, 1882, 1290, 1289, 1293, 1296,
     1299, 1295,  323, 1300,  332, 1303, 1297, 1291, 1306,  332,

     1313, 1294, 1298, 1307, 1882, 1305, 1324, 1302, 1318,  597,
     1301,  597, 1304,  597,  332,  597,  256, 1296, 1299, 1311,
      332, 1300, 1317, 1303,  256, 1316, 1306, 1882,  327, 1313,
      327, 1307,  125, 1305,  590,  220,  590,  221,  591, 1330,
     1331,  332,  591,  332,  152, 1350,  153, 1319, 1311,  332,
     1322, 1317, 1323, 1316, 1882,  154,  154, 1343,  332, 1343,
      222,  223, 1355,  591,  224,  125,  225,  591,  154, 1320,
      332,  591,  126,  127, 1350,  125, 1319,  479, 1322, 1344,
     1882, 1323,  126,  127,  154,  154,  365,  332,  365,  222,
      223, 1355,  591,  224,  710,  225,  154, 1320, 1882, 1339,

      366, 1339,  388, 1202, 1203,  479,  322,  322,  330,  322,
      322,  322,  322,  322,  322,  322,  322,  322,  322,  322,
      322,  322,  322,  322,  322, 1882,  322,  322,  322,  322,
      322,  908,  908,  908,  908, 1314,  908, 1339,  908, 1340,
      388, 1357,  909,  909, 1345, 1882, 1345,  909, 1882, 1148,
     1149, 1349, 1351,  365, 1352,  365, 1353,  322,  322,  365,
      365,  365,  365, 1524, 1314, 1524, 1346,  366, 1882,  367,
     1357,  630, 1882,  366,  366,  367,  367,  368,  368, 1349,
     1351, 1882, 1352,  912, 1353,  322,  322,  322,  330,  322,
      322,  322,  322,  322,  322,  322,  322,  322,  322,  322,

      322,  322,  322,  322,  322,  369,  322,  322,  322,  322,
      322,  369,  369, 1315, 1341,  391, 1341,  660, 1341,  391,
     1342,  660,  148, 1354, 1356, 1361,  148, 1362, 1363, 1882,
     1364,  150, 1365, 1366, 1367,  150, 1882,  322,  322,  969,
     1882, 1198, 1315, 1198, 1420, 1882, 1420,  507, 1882, 1882,
     1198, 1354, 1356, 1524, 1361, 1524, 1362, 1363, 1364, 1358,
     1365, 1358, 1366, 1367, 1368,  322, 1312, 1312,  330, 1312,
     1312, 1312, 1312, 1312, 1312, 1312, 1312, 1312, 1312, 1312,
     1312, 1312, 1312, 1312, 1312, 1359, 1312, 1312, 1312, 1312,
     1312, 1369, 1370, 1368, 1371, 1373, 1371, 1374, 1360, 1193,

     1197, 1193, 1197, 1378, 1379, 1380, 1198, 1381, 1382, 1383,
     1393,  969, 1386, 1198, 1359, 1198, 1372, 1312, 1312, 1369,
     1370, 1194, 1198, 1882, 1373, 1374, 1360, 1375, 1820, 1376,
     1820, 1376, 1378, 1379, 1380, 1381, 1382, 1383, 1376, 1393,
     1386, 1882, 1387, 1389, 1387, 1312, 1325, 1333, 1397, 1333,
     1325, 1325, 1325, 1325, 1325, 1325, 1325, 1325, 1325, 1325,
     1325, 1334, 1325, 1335, 1325, 1336, 1325, 1325, 1325, 1325,
     1325, 1389, 1882, 1199, 1212, 1199, 1212, 1397, 1198, 1399,
     1384, 1388, 1384, 1390,  970, 1394, 1200, 1395, 1200, 1407,
     1377,  710, 1377, 1409, 1376, 1200, 1213, 1325, 1325, 1338,

     1202, 1203, 1385, 1204, 1562, 1204, 1562,  710, 1399, 1388,
     1882, 1390, 1204, 1394, 1563, 1395, 1202, 1203, 1407, 1204,
     1882, 1204, 1409, 1882, 1882, 1325, 1325, 1325, 1204, 1325,
     1325, 1325, 1325, 1325, 1325, 1325, 1325, 1325, 1325, 1325,
     1325, 1325, 1325, 1325, 1325, 1396, 1325, 1325, 1325, 1325,
     1325, 1200, 1391, 1398, 1391,  256, 1400, 1402, 1400, 1402,
     1406, 1404, 1408, 1404,  229, 1412,  244, 1413, 1204, 1415,
     1882,  241, 1463, 1396, 1392, 1882, 1882, 1325, 1325, 1403,
     1882, 1398, 1401, 1405, 1204, 1343,  256, 1343, 1406,  256,
     1408, 1468, 1410, 1417, 1412, 1413, 1469, 1423, 1415, 1416,

     1420, 1463, 1421,  507,  256, 1325, 1882, 1344, 1345,  256,
     1345, 1401,  908,  256,  908,  256,  256,  256, 1882, 1468,
      256, 1410, 1417,  909, 1469, 1423, 1882, 1425, 1416, 1428,
     1346, 1325, 1325,  256, 1325, 1325, 1325, 1325, 1325, 1325,
     1325, 1325, 1325, 1325, 1325, 1325, 1325, 1325, 1325, 1325,
     1424, 1325, 1325, 1325, 1325, 1325, 1425, 1428, 1426, 1427,
     1429,  256, 1431,  256,  256, 1358,  256, 1358,  256,  256,
      256,  256, 1371,  256, 1371,  256,  256,  256, 1424,  256,
      256,  256, 1325, 1325, 1882,  256, 1426, 1427, 1429, 1430,
     1431, 1433, 1432,  256, 1372, 1435, 1436, 1437, 1440,  256,

      256,  256,  256, 1438, 1434, 1439, 1443, 1444, 1442,  256,
     1325,  256,  256, 1441, 1387,  256, 1387,  256, 1430, 1448,
     1433, 1432, 1446,  256, 1435, 1436, 1437, 1440, 1882, 1456,
      256, 1438, 1434, 1439, 1450, 1443, 1444, 1442, 1470, 1445,
     1452, 1441, 1447, 1384,  256, 1384, 1457, 1448, 1451,  256,
     1446, 1453, 1454, 1449, 1882, 1391,  256, 1391, 1456, 1882,
     1455, 1458, 1450, 1882, 1882, 1385, 1470, 1445,  323, 1452,
     1447, 1400,  256, 1400, 1457, 1471, 1451, 1392, 1473, 1453,
     1454, 1449, 1402,  256, 1402, 1404,  256, 1404, 1455, 1480,
     1458, 1460, 1461,  220, 1461,  221, 1465, 1459, 1461,  220,

     1461, 1462, 1882, 1471, 1403,  125, 1473, 1405, 1414,  256,
     1414, 1882,  126,  127,  125, 1882, 1882, 1882, 1480, 1460,
     1882,  126,  127,  256, 1481, 1465, 1459, 1464, 1464,  330,
     1464, 1464, 1464, 1464, 1464, 1464, 1464, 1464, 1464, 1464,
     1464, 1464, 1464, 1464, 1464, 1464, 1472, 1464, 1464, 1464,
     1464, 1464,  365, 1481,  365,  627, 1882,  627, 1482, 1626,
     1483, 1626, 1343, 1345, 1343, 1345,  366, 1882,  628, 1627,
      628, 1484, 1485, 1491,  365, 1472,  365, 1486, 1464, 1464,
      365,  365,  365,  365, 1344, 1346, 1882, 1482,  366, 1483,
      367, 1882,  630, 1882,  366,  366,  367,  367,  368,  368,

     1484, 1485, 1491, 1882, 1882, 1486, 1464,  322,  322,  330,
      322,  322,  322,  322,  322,  322,  322,  322,  322,  322,
      322,  322,  322,  322,  322,  322,  369,  322,  322,  322,
      322,  322,  369,  369, 1489, 1476,  391, 1476,  660, 1476,
      391, 1477,  660,  148, 1478, 1882, 1490,  148, 1479, 1882,
     1492, 1467,  150, 1495, 1493, 1494,  150, 1882,  322,  322,
     1882, 1375, 1489, 1376, 1523, 1376, 1523,  254, 1554,  220,
     1554,  318, 1376, 1478, 1490, 1882, 1882, 1479, 1492, 1467,
     1358, 1495, 1358, 1493, 1494, 1496,  322,  322,  322,  330,
      322,  322,  322,  322,  322,  322,  322,  322,  322,  322,

      322,  322,  322,  322,  322,  322, 1487,  322,  322,  322,
      322,  322, 1497, 1498, 1496, 1371, 1499, 1371, 1500, 1488,
     1882, 1501, 1503, 1501, 1502, 1504, 1882, 1564, 1376, 1564,
      388, 1384, 1375, 1384, 1376, 1487, 1376, 1372,  322,  322,
     1506, 1497, 1498, 1376, 1499, 1507, 1500, 1488, 1377,  710,
     1377, 1503, 1502, 1385, 1504, 1387, 1508, 1387, 1202, 1203,
     1391, 1204, 1391, 1204, 1509,  256,  322, 1510, 1506, 1511,
     1204, 1513, 1400, 1507, 1400, 1514, 1882,  244, 1515, 1402,
     1516, 1402, 1392, 1404, 1508, 1404, 1519, 1520,  256,  256,
     1882,  256, 1528, 1509, 1505, 1510,  256, 1511, 1512, 1376,

     1513, 1403,  256,  256, 1514, 1405, 1515,  256,  256, 1516,
      256, 1526, 1533,  256, 1519, 1527, 1520, 1522, 1530,  256,
      256, 1528, 1505, 1531,  256,  256, 1204, 1512, 1529, 1532,
      256,  256, 1538,  256,  256,  256,  256, 1539, 1882,  256,
     1526, 1533,  256,  256, 1527, 1522, 1557, 1530, 1536, 1882,
     1534, 1540, 1531, 1882,  256,  256, 1535, 1529, 1532, 1541,
     1537, 1538, 1555, 1558, 1544, 1545, 1539, 1542,  256, 1559,
     1548,  256, 1543, 1882, 1546, 1557, 1536, 1552, 1534, 1550,
     1540, 1547, 1549, 1560, 1535, 1571, 1551, 1541, 1537, 1882,
     1555, 1882, 1558, 1544, 1545, 1542, 1561, 1559, 1548, 1553,

     1543, 1570, 1546, 1573, 1572, 1882, 1552, 1550, 1882, 1547,
     1882, 1549, 1560, 1571, 1551, 1565,  147, 1565, 1566,  391,
     1566,  392, 1574,  148, 1575, 1561,  148, 1576, 1553, 1577,
     1570, 1573,  150, 1572, 1578,  150, 1566,  391, 1566, 1567,
     1579, 1580, 1882, 1581, 1568, 1582, 1583, 1584, 1882, 1585,
     1574, 1586, 1575, 1569, 1587, 1576, 1590, 1577, 1591, 1588,
     1592, 1588, 1578, 1593, 1595, 1593, 1596, 1501, 1579, 1501,
     1580, 1581, 1597, 1598, 1582, 1583, 1584, 1585, 1375, 1586,
     1376, 1589, 1376, 1587, 1590, 1594, 1591, 1599, 1592, 1376,
     1600,  229, 1602, 1595, 1603, 1596,  256,  241,  244,  256,

     1597, 1598, 1523,  256, 1523,  254, 1606,  256, 1606,  507,
      256,  256,  256,  256,  256, 1599,  256, 1601,  256, 1600,
     1608, 1602,  256, 1603, 1604,  256, 1605, 1882,  256,  256,
     1610, 1882,  256, 1629, 1882, 1609, 1882, 1630, 1616,  256,
     1617,  256, 1588,  256, 1588, 1376, 1601, 1611, 1612, 1608,
     1614, 1615, 1613, 1604, 1620, 1605,  323,  256, 1619, 1610,
     1618, 1629, 1621, 1609, 1589, 1630, 1623, 1616, 1622, 1617,
     1593,  256, 1593, 1624, 1642, 1611, 1612, 1625, 1614, 1615,
     1613, 1639, 1628, 1620, 1631, 1588, 1619, 1588, 1618, 1882,
     1621, 1564, 1594, 1564,  388, 1623, 1622, 1554,  220, 1554,

      318, 1624, 1562, 1642, 1562, 1625, 1565, 1589, 1565, 1639,
     1882, 1628, 1563, 1631, 1635,  391, 1635,  660, 1636,  391,
     1636, 1644,  148,  150, 1640, 1646, 1637, 1641, 1643, 1645,
     1647,  150, 1565,  147, 1565, 1638,  390,  391,  390,  392,
      148, 1648, 1650, 1648,  148, 1651,  229, 1652, 1653,  150,
     1644, 1649, 1640,  150, 1646, 1641, 1643, 1645, 1593, 1647,
     1593, 1654, 1655, 1656, 1657,  241, 1659,  244,  256, 1606,
     1650, 1606,  507,  256, 1651, 1652,  256, 1653,  256,  256,
     1594,  256,  256, 1658,  256, 1648,  256, 1648, 1677, 1654,
     1661, 1655, 1656, 1657, 1659, 1649,  256,  256, 1626, 1882,

     1626, 1660, 1662,  323, 1687, 1665, 1667, 1668, 1627, 1669,
     1684, 1658, 1688, 1882, 1882, 1663, 1677, 1666, 1664, 1661,
     1636,  662, 1636, 1670, 1671, 1635,  391, 1635,  660, 1660,
     1662, 1882, 1687,  148, 1665, 1667, 1668, 1638, 1669, 1684,
     1676, 1688,  150, 1663, 1689, 1666, 1664, 1678, 1679, 1678,
     1680, 1690, 1670, 1671, 1685, 1633, 1685, 1691, 1634, 1681,
     1682, 1681, 1683, 1648, 1686, 1648, 1692, 1637, 1676, 1648,
     1693, 1648, 1689, 1649, 1694,  241, 1638,  244,  256, 1649,
     1690, 1685,  256, 1685,  256,  256, 1691,  256,  256, 1718,
     1673, 1686, 1721, 1882, 1685, 1692, 1685, 1674, 1719, 1693,

     1675, 1697, 1698, 1694, 1686, 1699, 1882, 1704, 1705, 1706,
     1707, 1882, 1882, 1882, 1702, 1703, 1701, 1718, 1700, 1708,
     1721, 1709, 1710, 1709, 1711, 1882, 1882, 1719, 1673, 1674,
     1697, 1698, 1675, 1708, 1699, 1674, 1882, 1882, 1675, 1678,
     1679, 1678, 1680, 1702, 1703, 1701, 1700, 1633, 1720, 1679,
     1634, 1725, 1678, 1679, 1678, 1680, 1714,  229, 1722, 1715,
     1633, 1723, 1708, 1634, 1681, 1682, 1681, 1683,  657,  658,
      659,  660, 1637, 1716,  244, 1716,  148, 1720,  256, 1725,
      389, 1638,  256,  256, 1882,  150, 1717, 1722, 1717, 1731,
     1723, 1731, 1707, 1724,  389, 1717, 1731, 1710, 1732, 1707,

     1749, 1727, 1882, 1750, 1741, 1728,  323, 1742, 1730, 1882,
     1704, 1705, 1706, 1707, 1882, 1751, 1752, 1709, 1710, 1709,
     1711, 1724, 1708,  389, 1729, 1674, 1882, 1882, 1675, 1749,
     1727, 1750,  229,  241, 1728,  256, 1708, 1730, 1735, 1736,
     1737, 1738, 1743, 1751, 1679, 1752, 1674, 1753,  256, 1675,
     1739, 1714, 1729, 1754, 1715, 1744, 1745, 1744, 1746, 1757,
     1716, 1679, 1716, 1714, 1739, 1708, 1715, 1756, 1714, 1755,
     1743, 1715, 1747, 1717, 1747, 1717, 1753,  256, 1762, 1761,
     1882, 1754, 1717, 1748, 1882, 1717, 1882, 1717, 1757, 1763,
     1778, 1763, 1707, 1739, 1717, 1756, 1763, 1755, 1764, 1707,

     1736, 1765, 1710, 1765, 1738,  323, 1762, 1768, 1761, 1674,
     1769, 1882, 1675, 1735, 1736, 1737, 1738, 1882, 1778, 1779,
     1882, 1674, 1882, 1760, 1675, 1739, 1765, 1710, 1766, 1738,
     1780, 1771, 1882, 1710, 1674, 1882,  229, 1675, 1772, 1739,
     1741, 1773, 1777, 1742, 1774, 1775, 1774, 1776, 1779, 1710,
     1679, 1760, 1741,  241, 1781, 1742, 1741, 1714, 1780, 1742,
     1715, 1744, 1745, 1744, 1746, 1782, 1784,  256, 1739, 1714,
     1777,  256, 1715, 1678, 1679, 1678, 1680, 1747, 1783, 1747,
      323, 1633, 1781, 1788, 1634, 1788, 1707, 1882, 1748, 1788,
     1785, 1789, 1707, 1782, 1784, 1882, 1882, 1790, 1710, 1790,

     1738, 1798, 1882, 1882, 1786, 1674, 1783, 1799, 1675, 1736,
     1800, 1787, 1790, 1710, 1791, 1738, 1768, 1882, 1785, 1769,
     1674, 1882, 1797, 1675, 1736, 1792, 1793, 1792, 1794, 1798,
     1771, 1768, 1786, 1768, 1769, 1799, 1769, 1772, 1800, 1787,
     1773, 1704, 1705, 1706, 1707, 1771, 1795, 1736, 1795, 1796,
     1797, 1801, 1772, 1708, 1772, 1773,  256, 1773, 1774, 1775,
     1774, 1776, 1735, 1736, 1737, 1738, 1741, 1708, 1710, 1742,
     1674,  256, 1882, 1675, 1739, 1741, 1882, 1736, 1742, 1807,
     1801, 1807, 1707, 1807, 1768, 1808, 1707, 1769, 1739,  241,
     1882, 1815, 1805, 1882, 1882,  229, 1708, 1816, 1806, 1809,

     1710, 1809, 1738, 1809, 1710, 1810, 1738, 1674, 1736, 1836,
     1675, 1674, 1818,  241, 1675, 1812,  323, 1739, 1813, 1815,
     1805, 1792, 1793, 1792, 1794, 1819, 1816, 1806, 1827, 1768,
      229, 1882, 1769, 1735, 1736, 1737, 1738, 1817, 1836, 1882,
     1818, 1674,  323, 1882, 1675, 1739, 1795, 1736, 1795, 1796,
     1828, 1882, 1882, 1819, 1772, 1882, 1827, 1773, 1825, 1739,
     1821, 1710, 1821, 1738, 1829, 1817, 1829, 1707, 1674, 1736,
     1826, 1675, 1821, 1710, 1822, 1738, 1812, 1736, 1828, 1813,
     1674,  241, 1834, 1675, 1812, 1882, 1825, 1813, 1739, 1882,
     1820, 1882, 1820, 1882, 1882, 1841, 1736, 1882, 1826, 1882,

     1882, 1882, 1882, 1812, 1882, 1882, 1813,  229, 1882, 1882,
     1834, 1882, 1882, 1882, 1823, 1793, 1823, 1824, 1830, 1710,
     1830, 1711, 1812, 1837, 1841, 1813, 1674, 1882, 1829, 1675,
     1829, 1707, 1845,  229, 1835, 1823, 1793, 1823, 1824, 1882,
     1882, 1882, 1882, 1812,  241,  323, 1813, 1838, 1710, 1838,
     1738, 1837, 1710, 1882, 1673, 1674,  323, 1882, 1675, 1741,
     1845, 1674, 1742, 1835, 1675,  229, 1882,  323, 1882, 1672,
     1830, 1710, 1830, 1831, 1672, 1844,  241, 1672, 1833, 1672,
     1672, 1675, 1882, 1839, 1672, 1672, 1846, 1847, 1840, 1672,
     1843, 1672, 1672, 1672, 1709, 1710, 1709, 1711, 1838, 1710,

     1838, 1738, 1674, 1844, 1882, 1675, 1674, 1882, 1882, 1675,
     1882, 1882, 1839, 1882, 1846, 1847, 1854, 1840,  241, 1843,
     1672, 1672, 1672, 1848, 1849, 1848, 1850, 1851, 1852, 1851,
     1853, 1855, 1856, 1855, 1857, 1848, 1849, 1848, 1850, 1858,
     1859, 1858, 1860, 1882, 1854, 1866, 1882, 1882, 1672, 1851,
     1852, 1851, 1853, 1863, 1852, 1863, 1853, 1855, 1856, 1855,
     1857, 1863, 1852, 1863, 1853, 1858, 1859, 1858, 1860, 1858,
     1859, 1858, 1860,  241, 1866, 1858, 1859, 1858, 1860, 1863,
     1852, 1863, 1853, 1863, 1852, 1863, 1853, 1863, 1852, 1863,
     1853,  241, 1872, 1873, 1872, 1874, 1870, 1872, 1873, 1872,

     1874, 1875, 1876, 1875, 1877, 1875, 1876, 1875, 1877, 1875,
     1876, 1875, 1877, 1875, 1876, 1875, 1877, 1875, 1876, 1875,
     1877, 1882, 1882, 1882, 1882, 1870, 1882, 1882, 1882, 1882,
     1882, 1882, 1882, 1871, 1882, 1882, 1882, 1882, 1882, 1882,
     1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882,
     1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882,
     1882, 1871,   68,   68,   68,   68,   68,   68,   68,   68,
       68,   68,   68,   68,   68,   68,   68,   68,   68,   68,
       69,   69,   69,   69,   69,   69,   69,   69,   69,   69,
       69,   69,   69,   69,   69,   69,   69,   69,   72,   72,

       72,   72,   72,   72,   72,   72,   72,   72,   72,   72,
       72,   72,   72,   72,   72,   72,  115,  115, 1882,  115,
      115,  115,  115,  115,  115,  115,  115,  115,  115,  115,
      115,  115,  115,  115,  118,  118,  118,  118,  118,  118,
      118,  118,  118,  118,  118,  118,  118,  118,  118,  118,
      118,  118,  124,  124,  124,  124,  124,  124,  124,  124,
      124,  124,  124,  124,  124,  124,  124,  124,  124,  124,
      133, 1882, 1882, 1882, 1882, 1882, 1882,  133, 1882,  133,
     1882,  133,  133,  133,  133,  133,  160,  160,  160,  160,
      160,  228,  228,  228,  228,  228,  228,  228,  228,  228,

      228,  228,  228,  228,  228,  228,  228,  228,  228,  240,
      240,  240,  240,  240,  240,  240,  240,  240,  240,  240,
      240,  240,  240,  240,  240,  240,  240,  243,  243,  243,
      243,  243,  243,  243,  243,  243,  243,  243,  243,  243,
      243,  243,  243,  243,  243,  249,  249,  249,  249,  249,
      249,  249,  249,  249,  249,  249,  249,  249,  249,  249,
      249,  249,  249,  257, 1882, 1882, 1882, 1882, 1882, 1882,
     1882, 1882, 1882, 1882,  257,  257,  257,  257,  257,  322,
      322,  322,  322,  322,  322,  322,  322,  322,  322,  322,
      322,  322,  322,  322,  322,  322,  322,  115,  115, 1882,

      115,  115,  115,  115,  115,  115,  115,  115,  115,  115,
      115,  115,  115,  115,  115,  118,  118,  118,  118,  118,
      118,  118,  118,  118,  118,  118,  118,  118,  118,  118,
      118,  118,  118,  356,  356,  356,  356,  356,  356,  356,
      356,  356,  356,  356,  356,  356,  356,  356,  356,  356,
      356,  124,  124,  124,  124,  124,  124,  124,  124,  124,
      124,  124,  124,  124,  124,  124,  124,  124,  124,  357,
      357,  357,  357,  357,  357,  357,  357,  357,  357,  357,
      357,  357,  357,  357,  357,  357,  357,  133, 1882, 1882,
     1882, 1882, 1882, 1882,  133, 1882,  133, 1882, 1882,  133,

      133,  133,  133,  393,  393,  393,  393, 1882,  393,  393,
      393,  393,  393,  393, 1882,  393,  393, 1882, 1882,  393,
      393,  160,  160,  160,  160,  160,  480,  480,  480,  480,
      480,  480,  480,  480,  480,  480,  480,  480,  480,  480,
      480,  480,  480,  480,  228,  228,  228,  228,  228,  228,
      228,  228,  228,  228,  228,  228,  228,  228,  228,  228,
      228,  228,  240,  240,  240,  240,  240,  240,  240,  240,
      240,  240,  240,  240,  240,  240,  240,  240,  240,  240,
      243,  243,  243,  243,  243,  243,  243,  243,  243,  243,
      243,  243,  243,  243,  243,  243,  243,  243,  249,  249,

      249,  249,  249,  249,  249,  249,  249,  249,  249,  249,
      249,  249,  249,  249,  249,  249,  502,  502,  502,  502,
      502,  502,  502,  502,  502,  502,  502,  502,  502,  502,
      502,  502,  502,  502,  503,  503,  503,  503,  503,  503,
      503,  503,  503,  503,  503,  503,  503,  503,  503,  503,
      503,  503,  588,  588,  588,  588,  588,  588,  588,  588,
      588,  588,  588,  588,  588,  588,  588,  588,  588,  588,
      322,  322,  322,  322,  322,  322,  322,  322,  322,  322,
      322,  322,  322,  322,  322,  322,  322,  322,  332,  332,
      332,  332,  332,  332,  332,  332,  332,  332,  332,  332,

      332,  332,  332,  332,  332,  332,  115,  115, 1882,  115,
      115,  115,  115,  115,  115,  115,  115,  115,  115,  115,
      115,  115,  115,  115,  118,  118,  118,  118,  118,  118,
      118,  118,  118,  118,  118,  118,  118,  118,  118,  118,
      118,  118,  356,  356,  356,  356,  356,  356,  356,  356,
      356,  356,  356,  356,  356,  356,  356,  356,  356,  356,
      357,  357,  357,  357,  357,  357,  357,  357,  357,  357,
      357,  357,  357,  357,  357,  357,  357,  357,  624,  624,
      624,  624,  624,  624,  624,  624,  624,  624,  624,  624,
      624,  624,  624,  624,  624,  624,  124,  124,  124,  124,

      124,  124,  124,  124,  124,  124,  124,  124,  124,  124,
      124,  124,  124,  124,  629, 1882, 1882, 1882, 1882, 1882,
     1882,  629, 1882,  629, 1882, 1882,  629,  629,  629,  629,
      133, 1882, 1882, 1882, 1882, 1882, 1882, 1882,  133, 1882,
      133, 1882,  133,  133,  133,  133,  133,  632,  632,  632,
      632,  655,  655,  655,  655,  655,  655,  655,  655,  655,
      655,  655,  655,  655,  655,  655,  655,  655,  655,  656,
      656,  656,  656,  656,  656,  656,  656,  656,  656,  656,
      656,  656,  656,  656,  656,  656,  656,  661,  661,  661,
      661,  661,  661,  661,  661,  661,  661,  661,  661,  661,

      661,  661,  661,  661,  661,  393,  393,  393,  393, 1882,
      393,  393,  393,  393,  393,  393, 1882,  393,  393, 1882,
     1882,  393,  393,  160,  160,  160,  160,  160,  709,  709,
      709,  709,  709,  709,  709,  709,  709,  709,  709,  709,
      709,  709,  709,  709,  709,  709,  478, 1882, 1882, 1882,
     1882, 1882, 1882, 1882,  478,  478,  480,  480,  480,  480,
      480,  480,  480,  480,  480,  480,  480,  480,  480,  480,
      480,  480,  480,  480,  228,  228,  228,  228,  228,  228,
      228,  228,  228,  228,  228,  228,  228,  228,  228,  228,
      228,  228,  240,  240,  240,  240,  240,  240,  240,  240,

      240,  240,  240,  240,  240,  240,  240,  240,  240,  240,
      243,  243,  243,  243,  243,  243,  243,  243,  243,  243,
      243,  243,  243,  243,  243,  243,  243,  243,  249,  249,
      249,  249,  249,  249,  249,  249,  249,  249,  249,  249,
      249,  249,  249,  249,  249,  249,  502,  502,  502,  502,
      502,  502,  502,  502,  502,  502,  502,  502,  502,  502,
      502,  502,  502,  502,  503,  503,  503,  503,  503,  503,
      503,  503,  503,  503,  503,  503,  503,  503,  503,  503,
      503,  503,  789,  789,  789,  789,  789,  789,  789,  789,
      789,  789,  789,  789,  789,  789,  789,  789,  789,  789,

      790,  790,  790,  790,  790,  790,  790,  790,  790,  790,
      790,  790,  790,  790,  790,  790,  790,  790,  257, 1882,
     1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882,  257,
      257,  257,  257,  257,  588,  588,  588,  588,  588,  588,
      588,  588,  588,  588,  588,  588,  588,  588,  588,  588,
      588,  588,  322,  322,  322,  322,  322,  322,  322,  322,
      322,  322,  322,  322,  322,  322,  322,  322,  322,  322,
      332,  332,  332,  332,  332,  332,  332,  332,  332,  332,
      332,  332,  332,  332,  332,  332,  332,  332,  115,  115,
     1882,  115,  115,  115,  115,  115,  115,  115,  115,  115,

      115,  115,  115,  115,  115,  115,  118,  118,  118,  118,
      118,  118,  118,  118,  118,  118,  118,  118,  118,  118,
      118,  118,  118,  118,  357,  357,  357,  357,  357,  357,
      357,  357,  357,  357,  357,  357,  357,  357,  357,  357,
      357,  357,  124,  124,  124,  124,  124,  124,  124,  124,
      124,  124,  124,  124,  124,  124,  124,  124,  124,  124,
      624,  624,  624,  624,  624,  624,  624,  624,  624,  624,
      624,  624,  624,  624,  624,  624,  624,  624,  629, 1882,
     1882, 1882, 1882, 1882, 1882,  629, 1882,  629, 1882, 1882,
      629,  629,  629,  629,  913, 1882, 1882, 1882, 1882, 1882,

     1882, 1882,  913, 1882, 1882, 1882,  913,  913,  913,  913,
      913,  133, 1882, 1882, 1882, 1882, 1882, 1882, 1882,  133,
     1882,  133, 1882,  133,  133,  133,  133,  133,  655,  655,
      655,  655,  655,  655,  655,  655,  655,  655,  655,  655,
      655,  655,  655,  655,  655,  655,  656,  656,  656,  656,
      656,  656,  656,  656,  656,  656,  656,  656,  656,  656,
      656,  656,  656,  656,  925,  925,  925,  925,  925,  925,
      925,  925,  925,  925,  925,  925,  925,  925,  925,  925,
      925,  925,  661,  661,  661,  661,  661,  661,  661,  661,
      661,  661,  661,  661,  661,  661,  661,  661,  661,  661,

      160,  160,  160,  160,  160,  709,  709,  709,  709,  709,
      709,  709,  709,  709,  709,  709,  709,  709,  709,  709,
      709,  709,  709,  710,  710,  710,  710,  710,  710, 1882,
      710,  710,  710,  710,  710,  710,  710,  710,  710,  710,
      710,  711,  711, 1882,  711,  711,  711,  711,  711,  711,
      711,  711,  711,  711,  711,  711,  711,  711,  711,  228,
      228,  228,  228,  228,  228,  228,  228,  228,  228,  228,
      228,  228,  228,  228,  228,  228,  228,  240,  240,  240,
      240,  240,  240,  240,  240,  240,  240,  240,  240,  240,
      240,  240,  240,  240,  240,  243,  243,  243,  243,  243,

      243,  243,  243,  243,  243,  243,  243,  243,  243,  243,
      243,  243,  243,  789,  789,  789,  789,  789,  789,  789,
      789,  789,  789,  789,  789,  789,  789,  789,  789,  789,
      789,  790,  790,  790,  790,  790,  790,  790,  790,  790,
      790,  790,  790,  790,  790,  790,  790,  790,  790,  322,
      322,  322,  322,  322,  322,  322,  322,  322,  322,  322,
      322,  322,  322,  322,  322,  322,  322,  332,  332,  332,
      332,  332,  332,  332,  332,  332,  332,  332,  332,  332,
      332,  332,  332,  332,  332, 1138, 1138, 1882, 1138, 1138,
     1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138,

     1138, 1138, 1138,  115,  115, 1882,  115,  115,  115,  115,
      115,  115,  115,  115,  115,  115,  115,  115,  115,  115,
      115, 1140, 1140, 1882, 1140, 1140, 1140, 1140, 1140, 1140,
     1140, 1140, 1140, 1140, 1140, 1140, 1140, 1140, 1140,  118,
      118,  118,  118,  118,  118,  118,  118,  118,  118,  118,
      118,  118,  118,  118,  118,  118,  118, 1142, 1142, 1142,
     1142, 1142, 1142, 1142, 1142, 1142, 1142, 1142, 1142, 1142,
     1142, 1142, 1142, 1142, 1142,  124,  124,  124,  124,  124,
      124,  124,  124,  124,  124,  124,  124,  124,  124,  124,
      124,  124,  124, 1145, 1145, 1145, 1145, 1145, 1145, 1145,

     1145, 1145, 1145, 1145, 1145, 1145, 1145, 1145, 1145, 1145,
     1145,  629, 1882, 1882, 1882, 1882, 1882,  629, 1882, 1882,
     1882,  629, 1882,  629,  629,  629,  629,  629, 1150, 1150,
     1150, 1150,  913, 1882, 1882, 1882, 1882, 1882, 1882, 1882,
      913, 1882, 1882, 1882,  913,  913,  913,  913,  913,  133,
     1882, 1882, 1882, 1882, 1882, 1882, 1882,  133, 1882,  133,
     1882,  133,  133,  133,  133,  133, 1156, 1156, 1882, 1156,
     1156, 1156, 1156, 1156, 1156, 1156, 1156, 1156, 1156, 1156,
     1156, 1156, 1156, 1156,  925,  925,  925,  925,  925,  925,
      925,  925,  925,  925,  925,  925,  925,  925,  925,  925,

      925,  925, 1168, 1168, 1882, 1168, 1168, 1168, 1168, 1168,
     1168, 1168, 1168, 1168, 1168, 1168, 1168, 1168, 1168, 1168,
      710,  710,  710,  710,  710,  710, 1882,  710,  710,  710,
      710,  710,  710,  710,  710,  710,  710,  710,  711,  711,
     1882,  711,  711,  711,  711,  711,  711,  711,  711,  711,
      711,  711,  711,  711,  711,  711,  709,  709,  709,  709,
      709,  709,  709,  709,  709,  709,  709,  709,  709,  709,
      709,  709,  709,  709, 1201, 1201, 1201, 1201, 1201, 1201,
     1201, 1201, 1201, 1201, 1201, 1201, 1201, 1201, 1201, 1201,
     1201, 1201,  228,  228,  228,  228,  228,  228,  228,  228,

      228,  228,  228,  228,  228,  228,  228,  228,  228,  228,
     1243, 1243, 1243, 1243, 1243, 1243, 1243, 1243, 1243, 1243,
     1243, 1243, 1243, 1243, 1243, 1243, 1243, 1243,  240,  240,
      240,  240,  240,  240,  240,  240,  240,  240,  240,  240,
      240,  240,  240,  240,  240,  240,  243,  243,  243,  243,
      243,  243,  243,  243,  243,  243,  243,  243,  243,  243,
      243,  243,  243,  243, 1259, 1259, 1259, 1259, 1259, 1259,
     1259, 1259, 1259, 1259, 1259, 1259, 1259, 1259, 1259, 1259,
     1259, 1259,  322,  322,  322,  322,  322,  322,  322,  322,
      322,  322,  322,  322,  322,  322,  322,  322,  322,  322,

     1312, 1312, 1312, 1312, 1312, 1312, 1312, 1312, 1312, 1312,
     1312, 1312, 1312, 1312, 1312, 1312, 1312, 1312,  332,  332,
      332,  332,  332,  332,  332,  332,  332,  332,  332,  332,
      332,  332,  332,  332,  332,  332, 1321, 1321, 1321, 1321,
     1321, 1321, 1321, 1321, 1321, 1321, 1321, 1321, 1321, 1321,
     1321, 1321, 1321, 1321, 1325, 1325, 1882, 1325, 1325, 1325,
     1325, 1325, 1325, 1325, 1325, 1325, 1325, 1325, 1325, 1325,
     1325, 1325, 1326, 1326, 1882, 1326, 1326, 1326, 1326, 1326,
     1326, 1326, 1326, 1326, 1326, 1326, 1326, 1326, 1326, 1326,
      115,  115, 1882,  115,  115,  115,  115,  115,  115,  115,

      115,  115,  115,  115,  115,  115,  115,  115, 1327, 1327,
     1327, 1327, 1327, 1327, 1327, 1327, 1327, 1327, 1327, 1327,
     1327, 1327, 1327, 1327, 1327, 1327,  118,  118,  118,  118,
      118,  118,  118,  118,  118,  118,  118,  118,  118,  118,
      118,  118,  118,  118, 1329, 1329, 1329, 1329, 1329, 1329,
     1329, 1329, 1329, 1329, 1329, 1329, 1329, 1329, 1329, 1329,
     1329, 1329,  124,  124,  124,  124,  124,  124,  124,  124,
      124,  124,  124,  124,  124,  124,  124,  124,  124,  124,
     1332, 1882, 1882, 1882, 1882, 1882, 1332, 1882, 1882, 1882,
     1882, 1882, 1332, 1332, 1332, 1332, 1332, 1337, 1337, 1882,

     1337, 1337, 1337, 1337, 1337, 1337, 1337, 1337, 1337, 1337,
     1337, 1337, 1337, 1337, 1337,  629, 1882, 1882, 1882, 1882,
     1882, 1882,  629, 1882,  629, 1882, 1882,  629,  629,  629,
      629,  133, 1882, 1882, 1882, 1882, 1882, 1882, 1882,  133,
     1882,  133, 1882,  133,  133,  133,  133,  133,  632,  632,
      632,  632, 1347, 1347, 1882, 1347, 1347, 1347, 1347, 1347,
     1347, 1347, 1347, 1347, 1347, 1347, 1347, 1347, 1347, 1347,
      710,  710,  710,  710,  710,  710, 1882,  710,  710,  710,
      710,  710,  710,  710,  710,  710,  710,  710,  711,  711,
     1882,  711,  711,  711,  711,  711,  711,  711,  711,  711,

      711,  711,  711,  711,  711,  711, 1202, 1202, 1882, 1202,
     1202, 1202, 1202, 1202, 1202, 1202, 1202, 1202, 1202, 1202,
     1202, 1202, 1202, 1202, 1201, 1201, 1201, 1201, 1201, 1201,
     1201, 1201, 1201, 1201, 1201, 1201, 1201, 1201, 1201, 1201,
     1201, 1201,  228,  228,  228,  228,  228,  228,  228,  228,
      228,  228,  228,  228,  228,  228,  228,  228,  228,  228,
     1411, 1411, 1411, 1411, 1411, 1411, 1411, 1411, 1411, 1411,
     1411, 1411, 1411, 1411, 1411, 1411, 1411, 1411,  240,  240,
      240,  240,  240,  240,  240,  240,  240,  240,  240,  240,
      240,  240,  240,  240,  240,  240,  243,  243,  243,  243,

      243,  243,  243,  243,  243,  243,  243,  243,  243,  243,
      243,  243,  243,  243, 1419, 1882, 1419, 1882, 1882, 1882,
     1882, 1419, 1882, 1882, 1419, 1419, 1419, 1419, 1419, 1419,
     1422, 1422, 1422, 1422, 1422, 1422, 1422, 1422, 1422, 1422,
     1422, 1422, 1422, 1422, 1422, 1422, 1422, 1422, 1464, 1464,
     1464, 1464, 1464, 1464, 1464, 1464, 1464, 1464, 1464, 1464,
     1464, 1464, 1464, 1464, 1464, 1464,  322,  322,  322,  322,
      322,  322,  322,  322,  322,  322,  322,  322,  322,  322,
      322,  322,  322,  322, 1466, 1466, 1466, 1466, 1466, 1466,
     1466, 1466, 1466, 1466, 1466, 1466, 1466, 1466, 1466, 1466,

     1466, 1466,  332,  332,  332,  332,  332,  332,  332,  332,
      332,  332,  332,  332,  332,  332,  332,  332,  332,  332,
      115,  115, 1882,  115,  115,  115,  115,  115,  115,  115,
      115,  115,  115,  115,  115,  115,  115,  115,  118,  118,
      118,  118,  118,  118,  118,  118,  118,  118,  118,  118,
      118,  118,  118,  118,  118,  118,  124,  124,  124,  124,
      124,  124,  124,  124,  124,  124,  124,  124,  124,  124,
      124,  124,  124,  124, 1332, 1882, 1882, 1882, 1882, 1882,
     1332, 1882, 1882, 1882, 1882, 1882, 1332, 1332, 1332, 1332,
     1332,  629, 1882, 1882, 1882, 1882, 1882, 1882,  629, 1882,

      629, 1882, 1882,  629,  629,  629,  629,  133, 1882, 1882,
     1882, 1882, 1882, 1882, 1882,  133, 1882,  133, 1882,  133,
      133,  133,  133,  133,  632,  632,  632,  632, 1475, 1882,
     1475, 1882, 1882, 1882, 1882, 1475, 1882, 1882, 1475, 1475,
     1475, 1475, 1475, 1475, 1525, 1882, 1525, 1882, 1882, 1882,
     1882, 1525, 1882, 1882, 1525, 1525, 1525, 1525, 1525, 1525,
      480,  480,  480,  480,  480,  480,  480,  480,  480,  480,
      480,  480,  480,  480,  480,  480,  480,  480, 1607, 1607,
     1607, 1607, 1607, 1632, 1632, 1882, 1632, 1632, 1632, 1632,
     1632, 1632, 1632, 1632, 1632, 1632, 1632, 1632, 1632, 1632,

     1632,  661,  661,  661,  661,  661,  661,  661,  661,  661,
      661,  661,  661,  661,  661,  661,  661,  661,  661, 1672,
     1672, 1672, 1672, 1672, 1672, 1672, 1672, 1672, 1672, 1672,
     1672, 1672, 1672, 1672, 1672, 1672, 1672, 1713, 1713, 1713,
     1713, 1713, 1713, 1713, 1713, 1713, 1713, 1713, 1713, 1713,
     1713, 1713, 1713, 1713, 1713, 1733, 1733, 1733, 1733, 1733,
     1733, 1733, 1733, 1733, 1733, 1733, 1733, 1733, 1733, 1733,
     1733, 1733, 1733, 1734, 1734, 1734, 1734, 1734, 1734, 1734,
     1734, 1734, 1734, 1734, 1734, 1734, 1734, 1734, 1734, 1734,
     1734, 1740, 1740, 1740, 1740, 1740, 1740, 1740, 1740, 1740,

     1740, 1740, 1740, 1740, 1740, 1740, 1740, 1740, 1740, 1758,
     1758, 1758, 1758, 1758, 1758, 1758, 1758, 1758, 1758, 1758,
     1758, 1758, 1758, 1758, 1758, 1758, 1758, 1767, 1767, 1767,
     1767, 1767, 1767, 1767, 1767, 1767, 1767, 1767, 1767, 1767,
     1767, 1767, 1767, 1767, 1767, 1770, 1770, 1770, 1770, 1770,
     1770, 1770, 1770, 1770, 1770, 1770, 1770, 1770, 1770, 1770,
     1770, 1770, 1770, 1811, 1811, 1811, 1811, 1811, 1811, 1811,
     1811, 1811, 1811, 1811, 1811, 1811, 1811, 1811, 1811, 1811,
     1811, 1832, 1832, 1832, 1832, 1832, 1832, 1832, 1832, 1832,
     1832, 1832, 1832, 1832, 1832, 1832, 1832, 1832, 1832, 1861,

     1861, 1861, 1861, 1861, 1861, 1861, 1861, 1861, 1861, 1861,
     1861, 1861, 1861, 1861, 1861, 1861, 1861, 1864, 1864, 1864,
     1864, 1864, 1864, 1864, 1864, 1864, 1864, 1864, 1864, 1864,
     1864, 1864, 1864, 1864, 1864, 1867, 1867, 1867, 1867, 1867,
     1867, 1867, 1867, 1867, 1867, 1867, 1867, 1867, 1867, 1867,
     1867, 1867, 1867, 1869, 1869, 1869, 1869, 1869, 1869, 1869,
     1869, 1869, 1869, 1869, 1869, 1869, 1869, 1869, 1869, 1869,
     1869, 1878, 1878, 1878, 1878, 1878, 1878, 1878, 1878, 1878,
     1878, 1878, 1878, 1878, 1878, 1878, 1878, 1878, 1878, 1880,
     1880, 1880, 1880, 1880, 1880, 1880, 1880, 1880, 1880, 1880,

     1880, 1880, 1880, 1880, 1880, 1880, 1880,   17, 1882, 1882,
     1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882,
     1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882,
     1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882,
     1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882,
     1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882,
     1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882,
     1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882,
     1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882
    } ;

static yyconst flex_int16_t yy_chk[9689] =
    {   0,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        2,    2,    2,    2,    8, 1880,    8,   10,   10,   10,
       11,   11,   11,   12,   12,   12,   71, 1905, 1905,   11,

       18,   71,   12,   19,   75,   19,    2,    2,   21,    8,
        2,   76,    2,   76,   18,   10,   23, 1878,   22,  106,
       24, 1877,   21,    2,   61,   61,   61,   61,  108,   18,
       23,  114,   19,  114,   24,    2,    2,   21,    8,    2,
       75,    2,   18,   22,   10,   23,   19,  320,  106,   24,
       21,    2,    6,    6,    6,    6,  108,   22,   23,   28,
       29,   19,   24,  320,   98,   34,   98,   30,   75,   98,
      113,   98,   22,   28,   19,   34,   35,   29,    6,    6,
     1874,   30,    6,   86,    6,   22,  112,   86,   28,   34,
       35,   29,   98,   86,   98,    6,   30,   98,  113,   98,

      248,   28,  248, 1869,   34,   35,   29,    6,    6,   30,
     1867,    6,   86,    6, 1864,  112,   86,   34,   35,   29,
      776,   86,  776,    6,   13,   13,   13,   13,   13,   13,
       13,   13,   13,   13,   13,   13,   13,   13,   13,   13,
       13,   13,   13,   13,   13,   13,   13,   13,   13,   13,
       13,   13,   13,   13,   13,   13,   13,   13,   13,   13,
       13,   13,   13,   13,   13,   13,   13,   13,   13,   13,
       13,   13,   13,   13,   13,   13,   13,   13,   13,   13,
       13,   13,   13,   13,   13,   13,   13,   13,   13,   13,
       13,   13,   13,   13,   13,   13,   13,   13,   13,   13,

       13,   13,   13,   13,   14,   14,   14,   14,   36,   25,
       25,   25,   25,   26,  116,   36,   14,  102, 1861,  102,
       26,   26,   14,  117,  162,   37,   37,  121,  809,   36,
       14,   14,   37,  592,   14,  102,   14,  154,   25,  154,
      139,   26,  154,  116,   36,  809,   37,   14,  104,  592,
      104,  117,   25,  162, 1860,   26,   27,   36,   27,   14,
       14,   37,  140,   14,  121,   14,  104,   25,  139,   27,
       26,   27, 1857,   27,   37,   14,   16,   16,   16,   16,
       25,   40,   46,   26,   38,   27,   40,   32,   32,   32,
      140,   38,  121,  166,   16,   32,   46,  144,   32,   27,

       40,  145,   16,   16,   32,   38,   16, 1853,   16,   32,
       40,   46,   45,   89,   27,   40,   32,   89, 1850,   16,
       38,   45,  166,   89,   46,  144,   45,   27,   40,  145,
       32,   16,   16,   38,  110,   16,  110,   16,  111, 1806,
      111,   45,   89, 1805, 1784,   32,   89,   16,   31,   45,
       31,   89,  110,  120,   45,   59,  111,   33,   32,   33,
      122,  122,  122,  122, 1760, 1758,   31,  111,   33,   33,
       33,   39,   31,  120,   39,   59,   31,   31,   31, 1734,
       39,   33,  163,   31,   33,   31,   31,   39,   39,   59,
      164,   31,   74,   74,   74,   31,  111,   33,   33,   33,

       39,   31,  120,   39,   59,   31,   31,   31,   39,   33,
      163,   31,   33,   31,   31,   39,   39,   59,  164,   31,
       74, 1733,  165,   31,   41,   48,   42,   42,   48,   41,
       42,   41,   49,   49,  905,   41,   42,  905,   41,   48,
       62,   42,   49,   41,  124,   62, 1727,   49,   41,   74,
      165,  124,  124,   41,   48,   42,   42,   48,   41,   42,
       41,   49,   49,   41,   42,   47,   41,   48,   62,   42,
       49,   41,   47,   47,   47,   49,   41,   43,  778,   47,
      778,  167,   62,   43, 1141,   43,   43, 1141,   43,   43,
       63,   43,  169, 1708,   47,   43,  126,   62, 1707, 1702,

       47,   47,   47,  126,  126, 1699,   43,   47,   63,  167,
       62,   43,   50,   43,   43,  168,   43,   43,  173,   43,
      169,   50,   63,   43,   44, 1683,   50,   44,  128, 1671,
       44,   44,  130,   44,  130,  128,  128,   63,   44,   44,
      171,   50,  176,   51,  168,  130,  173,  130,   51,   50,
       63,  128, 1670,   44,   50,   51,   44,   51,   44,   44,
       52,   44,   51,   51,   53,  161,   44,   44,   52,  171,
       52,  176,   51, 1410,   52,   52,   53,   51,   53,  175,
      128,   53,  161,   51,   53,   51,   54,   54, 1665,   52,
       51,   51,   56,   53,  161,  177,   52,   56,   52,   56,

     1664,   54,   52,   52,   53,  141,   53,  175, 1659,   53,
      161,   56,   53,   55, 1638,   54,   54,   55,   55, 1631,
      141,   56,   57,  177, 1410,  178,   56,   55,   56,   54,
       57,   87,   55,   55,  141,   57,   57,  129,   58,   56,
       58,  131,   55,  131,  129,  129,   55,   55,  141, 1623,
       58,   57,   58,  178,  131,   55,  131, 1622,   57,   87,
       55,   55,  193,   57,   57,  143, 1620,   58,  179,   58,
      181, 1618, 1614,   87,   81,  143, 1611,  129,   58, 1416,
       58,   60,   60,   60,   60,   64,   65, 1610,   87,   64,
       64,  193,   65,   64,  143,   65,  179,   81,  181,   64,

       65,   87,   81,  143,   64,  129,  182,   60,   60, 1465,
       60,   60, 1548,   60,   64,   65,   81,  205,   64,   64,
       65, 1547,   64,   65,   60,  183,   81,   64,   65, 1543,
     1416,   81,   64,  184,  182, 1542,   60,   60,   67,   60,
       60,   66,   60,   67,   81,   67,  205, 1541,   66,   66,
       66,  211,   60,  183,   80,   66,   88,   67,  170,  170,
     1465,  184,   67,   79,   79,   79,   79,   67, 1563,  185,
       66, 1563,   67,   83,   67,   79,   66,   66,   66,   80,
      211, 1539,   80,   66,   88,   67,  170,  170,   80,   79,
       67,   88,   88,   88,  188,   80,   80,  185,   88,   90,

       83,   83,   91,   90,   83,  213,   91, 1532,   80,   90,
       83,   80,   91,   88,  191,   83,   80,  192,   79,   88,
       88,   88,  188,   80,   80,   82,   88,  214,   90,   83,
       83,   91,   90,   83,  213,   91,  195,   90,   83, 1527,
       91,  196,  191,   83,  215,  192,  142,  172,   82, 1464,
       85,   94,  172,   82,   85,   82,  214,   85,   85,   82,
       85,  142,   82,   94,  195,   94,   85,   82,   94,  196,
      100,   94,   82,  215,  174,  142,  172,   82,  100,   85,
       94,  172,   82,   85,   82,   85,   85,   82,   85,  142,
       82,   94, 1462,   94,   85,   82,   94,  217,  100,   94,

       82,   84,  186,  223,  186,   84, 1632,  197,   84, 1632,
       84,   84,  100,   84,   84,  174,  174,   92,   95, 1457,
       84,   92,   95, 1456,   93,   92,  217,  100,   95,   92,
       84,   92,  223, 1454,   84,  197,   84,   92,   84,   84,
      100,   84,   84,  174,  174,  186,   92,   95,   84,  198,
       92,   95,   93,  194,   92, 1451,   95,   92,  103,   92,
       93,  194,   93,  199,   96,   92,   93,   93,   96,  103,
      138,  201,   96,  186,   99,   97, 1448,  198,   96,  138,
      138,   93,  194,   96,   96,  418,  103,  418,   93,  194,
       93,  199,  138,   96,   93,   93, 1445,   96,   97,  201,

      103,   96,   99,   97,   99,   97,   96,  418,  138,  138,
      202,   96,   96,  202,   99,  103,   99,   97,  180,  251,
      138,  251,  251,  101,  101,  101,  101,   97,  103,  180,
     1444,   99,   97,   99,   97,  101,  107,  233,  202,  207,
      107,  202,   99,  107,   99,   97,  107,  180,  208,  101,
      125,  125,  125,  146,  146,  146, 1443,  180,  125,  125,
      253,  146,  253,  253, 1437,  107,  233,  207, 1431,  107,
      146,  107, 1427, 1021,  107, 1021,  208, 1422,  101,  105,
      105,  105,  105,  105,  105,  105,  105,  105,  105,  105,
      105,  105,  105,  105,  105,  105,  105,  105,  105,  105,

      105,  105,  105,  105,  105,  105,  105,  105,  105,  105,
      105,  105,  105,  105,  105,  105,  105,  105,  105,  105,
      105,  105,  105,  105,  105,  105,  105,  105,  105,  105,
      105,  105,  105,  105,  105,  105,  105,  105,  105,  105,
      105,  105,  105,  105,  105,  105,  105,  105,  105,  105,
      105,  105,  105,  105,  105,  105,  105,  105,  105,  109,
      357,  133,  187,  133,  187, 1412,  109,  357,  357,  209,
      109, 1411,  187,  109,  133,  133,  133,  133,  134,  133,
      134,  212,  224,  109, 1338, 1023,  187, 1023,  109, 1331,
      152,  134,  134,  134,  134,  109,  134,  209,  109,  152,

      152,  109,  134,  135,  137,  135,  137,  247, 1328,  212,
      224,  109,  152,  133,  225,  187,  135,  135,  135,  135,
     1327,  135,  147,  147,  147,  147, 1312, 1311,  152,  152,
      134,  134,  137,  247,  147,  148,  148,  148,  148,  204,
      152,  137,  225,  148,  135,  204, 1307,  153,  147,  153,
     1658,  137,  148,  189,  203,  135,  190,  190,  153,  153,
      200,  137,  247,  190,  203,  189, 1302,  189,  204,  137,
      189,  153,  135,  204,  232,  200,  373,  147,  206,  137,
      206, 1301,  189,  203,  190,  190,  216,  153,  153,  200,
      216,  190,  203,  189,  210,  189,  206,  206,  189,  153,

      206, 1658,  219,  200,  230,  373,  231,  206,  210,  206,
      226,  210,  222, 1300,  232,  216,  219,  226, 1634,  216,
      242, 1634, 1298,  210,  206,  206,  222,  230,  206,  231,
      246,  219,  220,  220,  220,  220,  210,  236,  226,  210,
      237,  222,  232,  242,  219,  226,  227,  227,  227,  227,
      229,  229,  229,  229,  222,  234,  230,  238,  231,  235,
      235,  239,  234, 1283,  349,  236,  246, 1270,  237,  238,
     1262,  238,  242,  350,  238,  245,  245,  245, 1259,  252,
      252,  252,  252,  234, 1243, 1242,  238,  235,  235,  239,
      234,  252,  349,  259,  246,  259,  319,  238,  319,  238,

      263,  350,  238,  245, 1202,  252,  257,  256,  256,  256,
      256, 1157,  259,  257,  257,  257, 1147,  263,  326,  256,
      257,  351,  259, 1676,  259,  319,  354,  258,  325,  263,
      258,  375,  245,  256,  252,  257,  352,  258,  258,  258,
      259,  257,  257,  257,  258,  263,  260, 1697,  257,  351,
     1143,  325,  261, 1142,  319,  260,  258,  354,  326,  258,
      375,  262,  256,  260,  352,  258,  258,  258,  261,  261,
      264,  321,  258,  321, 1676,  260,  262, 1120,  262,  264,
      325,  261,  265,  260,  265,  354,  326,  264,  266,  321,
      262,  260,  267,  268, 1110,  331,  261,  261, 1697,  264,

      266,  265,  267,  267,  262,  266,  262,  264, 1109,  267,
      268,  265,  360,  265,  355,  264,  271,  266,  331,  360,
      360,  267,  268,  315,  315,  315,  315,  272,  266,  265,
      267,  267,  270,  266,  269,  274,  269,  267,  268,  269,
      271, 1096,  270,  272,  272,  273,  273,  331, 1093,  270,
      274,  372,  274,  269,  355, 1078,  272,  271,  271,  348,
     1074,  270,  273,  269,  274,  269,  275, 1066,  269,  271,
      270,  272,  272,  348,  273,  273,  276,  270,  274,  372,
      274,  269,  355,  275,  275,  271,  271,  382,  348,  278,
      273,  276,  344,  276,  344,  275,  277, 1065,  278,  279,

     1058,  348,  277,  280, 1053,  276,  278,  281, 1029, 1783,
      344,  275,  275,  277,  384,  382,  279,  279,  278,  276,
      280,  276,  281,  925,  281,  277,  278,  918,  279,  282,
      912,  277,  280,  283,  278,  283,  281,  284,  285,  284,
      282,  277,  384,  399,  279,  279,  282,  284,  280,  868,
      281,  285,  281,  400,  865,  285,  287,  861,  282,  283,
     1783,  284,  366,  284,  366,  287,  287,  285,  282,  298,
      286,  399,  287,  287,  282,  366,  283,  366,  288,  285,
      284,  400,  286,  285,  286,  287,  298,  286,  283,  288,
      284,  855,  284,  287,  287,  288,  402, 1785,  298,  286,

      287,  287,  289,  397,  283,  290,  843,  288,  284,  290,
      286,  841,  286,  289,  298,  286,  293,  288,  826,  289,
      292,  622,  290,  288,  402,  293,  291,  291,  622,  622,
      292,  289,  397,  293,  290,  291, 1797,  292,  290,  336,
      294,  289,  336,  291,  403,  293,  336,  289, 1785,  292,
      290,  294,  365,  293,  365,  291,  291,  294,  292,  295,
      297,  293,  297,  291,  296,  292,  365,  336,  295,  294,
      336,  291,  403,  296,  336,  820,  295,  297,  300,  294,
      404,  296,  299,  398,  300,  294,  301, 1797,  295,  297,
      401,  297,  301,  296,  300,  300,  295,  299,  301,  299,

      299,  296,  798,  301,  295,  297,  790,  300,  404,  296,
      405,  299,  398,  300,  406,  301,  302,  302,  303,  401,
      303,  301,  300,  300,  304,  299,  301,  299,  299,  411,
      374,  301,  374,  789,  302,  374,  303,  303,  405,  711,
      303,  304,  406,  304,  710,  302,  302,  303,  412,  303,
      305,  682,  593,  304,  593,  306,  308,  308,  411,  305,
      415,  307,  302,  307,  303,  303,  681,  305,  303,  304,
      593,  304,  306,  306,  308,  307,  309,  412,  307,  305,
      307,  310,  311,  310,  306,  308,  308,  305,  311,  415,
      307,  309,  307,  309,  385,  305,  385,  385,  310,  311,

      306,  306,  308,  307,  661,  309,  307,  312,  307,  312,
      310,  311,  310,  327,  313,  327,  313,  311,  416,  309,
      313,  309,  314, 1031,  312, 1031,  310,  311,  314,  660,
      656,  313,  316,  316,  316,  316,  312,  409,  312,  314,
      655,  410,  327,  313,  316,  313,  328,  416,  328,  313,
      424,  314,  312,  317,  317,  317,  317,  314,  316,  313,
      652,  329,  426,  329,  328,  409,  650,  314,  345,  410,
      345,  327, 1033,  334, 1033,  328,  419,  329,  424,  329,
      330,  330,  330,  330,  334,  335,  345,  316,  329,  329,
      426,  334,  330,  337,  337,  337,  335,  345,  642,  639,

      337,  329,  334,  335,  328,  419,  330,  358,  358,  358,
      638,  632,  334,  632,  335,  358,  358,  329,  329,  334,
      637,  337,  337,  337,  335,  632,  345,  338,  337,  329,
     1116,  335, 1116,  338,  338,  330,  332,  332,  332,  332,
      332,  332,  332,  332,  332,  332,  332,  332,  332,  332,
      332,  332,  332,  332,  332,  338,  332,  332,  332,  332,
      332,  338,  338,  339,  340,  341,  342,  359,  359,  359,
      422,  387,  339,  387,  387,  359,  359,  341,  342,  339,
      342,  340,  341,  342,  346,  423,  346,  332,  332,  623,
      624,  588,  339,  340,  341,  342,  623,  623,  582,  422,

      339, 1246,  430, 1246,  430,  341,  342,  339,  342,  340,
      341,  342,  430,  346,  423,  332,  333,  333,  333,  333,
      333,  333,  333,  333,  333,  333,  333,  333,  333,  333,
      333,  333,  333,  333,  333,  427,  333,  333,  333,  333,
      333,  343,  346,  363,  347,  560,  347,  367,  364,  367,
      363,  363,  333,  508,  343,  364,  364,  429,  343,  333,
      347,  367,  347,  367,  427,  367,  407,  333,  333,  407,
      343,  347,  347,  347,  368,  507,  368,  363,  503,  502,
      333,  425,  343,  425,  347,  429,  343,  333,  368,  432,
      368,  364,  368,  499,  407,  333, 1250,  407, 1250,  367,

      347,  347,  347,  425,  370,  363,  370,  371,  376,  371,
      376,  480,  347,  420,  381,  420,  381,  432,  370,  364,
      370,  371,  370,  371,  376,  371,  368,  431,  478,  437,
      381,  386,  386,  386,  386,  420,  431,  431,  396,  370,
      421,  421,  395,  386,  390,  390,  390,  390,  428,  420,
      428,  371,  390,  376,  381, 1837,  370,  386,  437,  371,
      413,  390,  414,  391,  391,  391,  391,  370,  421,  421,
      408,  391,  433,  440,  394,  391,  413,  420,  414,  371,
      391,  376,  381,  394,  394,  436,  386,  408,  434,  391,
      392,  428,  435,  438,  439,  441,  394,  441,  436,  408,

      433,  413,  440,  414,  442,  389, 1837,  443,  444,  445,
      447,  451,  394,  394,  436,  408,  434,  441,  391,  428,
      435,  438,  439,  446,  394,  446,  436,  448,  449,  413,
      450,  414,  417,  442,  417,  443,  453,  444,  445,  447,
      451,  454,  455,  456,  458,  446,  388,  383,  459,  457,
      380,  379,  462,  460,  417,  448,  449,  457,  450,  417,
      378,  460,  463,  464,  417,  453,  465,  466,  471,  454,
      455,  467,  456,  458,  417,  417,  459,  457,  417,  461,
      462,  461,  460,  468,  469,  457,  488,  470,  417,  460,
      463,  472,  464,  417,  473,  465,  466,  471,  475,  467,

      476,  461,  417,  417,  477,  479,  417,  481,  482,  483,
      484,  468,  469,  485,  461,  470,  486,  487,  489,  472,
      488,  490,  473,  461,  491,  493,  475,  591,  476,  494,
      495,  496,  477,  479,  497,  481,  482,  483,  484,  492,
      377,  485,  461,  495,  486,  509,  487,  514,  488,  369,
      490,  461,  510,  491,  493,  591,  492,  362,  494,  495,
      496,  489,  512,  497,  498,  498,  498,  500,  492,  500,
      500,  495,  509,  511,  501,  487,  501,  501,  504,  513,
      504,  504,  515,  510,  492,  505,  505,  505,  505,  489,
      514,  506,  497,  506,  506,  516,  498,  505,  517,  512,

      518,  509,  521,  522,  361,  524,  520,  511,  356,  519,
      513,  505,  510,  525,  525,  515,  907,  539,  514,  523,
      353,  526,  527,  907,  907,  498,  532,  512,  530,  516,
      525,  520,  524,  538,  324,  511,  521,  522,  517,  513,
      505,  518,  523,  515,  519,  526,  322,  519,  520,  527,
      318,  529,  529,  529,  539,  525,  535,  516,  538,  530,
      520,  524,  533,  534,  521,  522,  517,  532,  532,  518,
      537,  523,  519,  529,  526,  519,  520,  542,  527,  531,
      531,  531,  539,  525,  528,  528,  528,  538,  530,  533,
      534,  536,  536,  536,  540,  532,  532,  535,  541,  543,

      546,  531,  547,  551,  255,  537,  528,  544,  545,  254,
      548,  528,  250,  536,  557,  531,  528,  249,  533,  534,
      542,  550,  243,  552,  553,  535,  528,  528,  540,  240,
      528,  559,  541,  537,  544,  545,  548,  549,  549,  549,
      528,  546,  543,  531,  547,  528,  551,  544,  542,  552,
      555,  553,  550,  556,  528,  528,  540,  558,  528,  549,
      541,  557,  559,  544,  545,  548,  554,  554,  554,  546,
      543,  561,  547,  562,  551,  544,  563,  555,  552,  564,
      553,  550,  565,  567,  570,  571,  228,  556,  554,  557,
      566,  559,  614,  558,  561,  576,  568,  573,  577,  221,

      572,  575,  127,  569,  569,  569,  555,  574,  580,  578,
      564, 1251,  563, 1251,  562,  556,  584,  567,  565,  566,
      614,  558,  568,  561,  573,  569,  565,  570,  571,  581,
      568,  572,  579,  583,  574,  123,  577,  576,  569,  564,
      563,  575,  562,  585,  580,  567,  565,  569,  566,  578,
      584,  568,  596,  573,  565,  570,  571,  119,  568,  611,
      572,  579,  118,  574,  577,  576,  569,   78,  595,  575,
       72,  581,  580,  583,   69,  569,  600,  578,  584,  586,
      586,  586,  586,  611,   17,  585,  587,  587,  587,  587,
      579,  589,  589,  589,  589,  596,  594,    9,  594,  581,

      603,  583,  595,  589,  597,  600,  597,  603,  598,  609,
      598,  604,  611,  585,  594,    7,  609,  589,  653,    0,
      653,  653,  605,  596,  604,  594,  598,    0,    0,  603,
      595,  606,    0,  597,  600,  603,  605,  598,  609,  606,
      604,  599,  606,  599,  609,  612,  589,  590,  590,  590,
      590,  605,  604,  617,  594,  608,  613,  599,  619,  599,
      606,  615,  597,  610,  605,  616,  598,  606,  599,  599,
      606,  608,  612,  590,  590,  666,  590,  590,  631,  590,
      631,  599,  617,  613,  608,  612,  610,  619,  618,  615,
      590,    0,  631,  616,    0,    0,  631,  599,  599,  608,

      620,  612,  590,  590,  666,  590,  590, 1627,  590,  599,
        0,    0,  613,  612, 1627,  610,  618, 1627,  590,  601,
      601,  601,  601,  601,  601,  601,  601,  601,  601,  601,
      601,  601,  601,  601,  601,  601,  601,  601,  620,  601,
      601,  601,  601,  601,  607,  627,  607,  627,  649,  625,
      628,  635,  628,  635,    0,  601,  625,  625,  627,  663,
      627,  607,  636,  628,  636,  628,  620,  635,    0,    0,
      601,  601,  621,  607,    0,  607,  649,    0,  636,  630,
        0,  630,  654,  601,  654,  654, 1339,  663, 1339,  607,
      625,    0, 1340,  630, 1340,  667,  621,  630,  601,  602,

      602,  602,  602,  602,  602,  602,  602,  602,  602,  602,
      602,  602,  602,  602,  602,  602,  602,  602,  625,  602,
      602,  602,  602,  602,  667,  621,  626,  629,  602,  629,
      668,  630,    0,  626,  626,    0,  664,  671,  629,  640,
      629,  640,  629,  629,  633,  629,  633,  634,  665,  634,
      602,  602,  647,  626,  647,  640,  669,  602,  633,  668,
      633,  634,  633,  634,  664,  634,  671,  673,  647,  672,
      744,  692,  744,  692,    0,  634,  665,  674,  602,  629,
      744,  675,  626,  678,  669,  679,  633,  676,    0,  657,
      657,  657,  657,  692,  677,  673,  633,  657,  672,  634,

      658,  658,  658,  658,  634,  674,  657,  680,  658,  675,
      683,  678,  658,  679,  633,  687,  676,  658,  688,  659,
      659,  659,  659,  677,  689,  691,  658,  659,  690,  662,
      662,  662,  662,  694,  702,  680,  659,  662,  683,  684,
      695,  684,  695,  687,  697,  698,  662,  688,  700,  706,
      707,    0,    0,  689,  691,  658,  690,  701,  703,  701,
      703,  694,  695,  702,  705,  709,  705,  714,  713,    0,
      715,  684,  697,  698,  709,  709,  700,  706,  707,  701,
      703,  712,  712,  712,  719,  684,  705,  726,  716,  718,
      720,  712,  712,  721,  712,  714,  712,  713,  715,  717,

      684,  725,  722,  712,  722,  723,  717,  723,  732,  727,
      734,  727,  719,  684,  685,  726,  685,  716,  718,  720,
      735,  736,  721,  728,  722,  728,  738,  723,  717,    0,
      725,  727,  737,  729,  717,  729,  685,  732,  739,  734,
      740,  685,  741,    0,  742,  728,  685,  743,  735,  736,
      749,  746,  750,  746,  738,  729,  685,  685,  728,  712,
      685,  737,  751,  752,  754,  748,  739,  748,  740,  755,
      685,  741,  742,  746,  756,  685,  743,  758,  749,  759,
      750,  761,  762,  761,  685,  685,  728,  748,  685,  763,
      751,  752,  764,  754,  766,  772,  767,  768,  755,  769,

      770,  771,  756,  761,  773,  758,  774,  777,  759,  775,
      779,  762,  782,  791,  784,  777,  792,    0,  763,  795,
      780,  764,  794,  766,  767,    0,  768,  780,  769,  770,
      771,  780,  781,  774,  781,  772,  777,  775,  785,  779,
      785,  785,  781,  793,  777,  773,  781,  794,  795,  780,
      784,  781,  782,  792,  791,  780,  783,  783,  783,  780,
      796,  797,  774,  772,  786,  802,  786,  786,  787,  799,
      787,  787,  788,  773,  788,  788,  794,  795,  784,  800,
      782,  792,  791,  801,  793,  803,  805,  796,  783,  807,
      806,  804,  799,  808,  811,  811,  811,  810,  814,  802,

      812,  817,  797,  815,  816,  813,  800,  821,  818,    0,
      900,  819,  793,  805,    0,  828,  796,  783,  875,  825,
      801,  799,  804,    0,  806,  814,  811,  802,  803,  808,
      797,  810,  807,  813,  816,  800,  823,  812,  829,  900,
      811,  827,  805,  830,    0,  817,  875,  821,  801,  818,
      815,  804,  806,  819,  814,  811,  803,  808,  828,  810,
      807,  825,  813,  816,  831,  812,  823,  832,  811,  822,
      822,  822,  827,  817,  830,  821,  833,  818,  815,  835,
      829,  819,  824,  824,  824,  834,  828,  838,  837,  825,
        0,  822,  844,  831,  846,  823,  848,    0,  832,  842,

      831,  827,  835,  830,  824,  849,  852,  845,  829,  847,
      850,  857,  833,  837,  834,  836,  836,  836,  851,  844,
      860,  838,  831,  839,  839,  839,  858,  832,  831,  846,
      842,  835,  840,  840,  840,    0,  847,  836,  848,    0,
      833,  851,  837,  834,  845,  839,  852,  849,  844,  838,
      853,  857,  850,  859,  840,  866,  860,  846,  862,  842,
      858,  854,  854,  854,  863,  847,  848,  840,  864,  867,
      851,  854,  845,  870,  852,  849,  856,  856,  856,  857,
      850,  853,  871,  862,  860,  869,  869,  869,  858,  863,
      872,  880,  866,  859,  899,  840,  867,  960,  856,  960,

      870,  873,  873,  873,  873,    0,  877,  869,  877,  871,
      853,  864,  862,  874,  874,  874,  874,  872,  863,  960,
      866,  859,  899,    0,  877,  867,  878,  881,  878,  870,
        0,  883,  880,  879,  887,  879,  887,  902,  871,  864,
      886,  882,  886,  882,  878,    0,  872,  876,  876,  876,
      876,  879,  896,    0,  881,  878,    0,  882,  887,  882,
      880,  904,  879,    0,  927,  876,  902,  928,  882,  882,
        0,  883,  929,  876,  876,  896,  876,  876,  889,  876,
      889,  882,  886,  881,  878,  904,  913,  887,  913,  931,
      876,  879,  927,    0,  891,  928,  891,  882,  882,  883,

      913,  929,  876,  876,  896,  876,  876,    0,  876,  882,
      886,  890,    0,  890,  904,    0,  889,  931,  876,  884,
      884,  884,  884,  884,  884,  884,  884,  884,  884,  884,
      884,  884,  884,  884,  884,  884,  884,  884,  891,  884,
      884,  884,  884,  884,  889,    0,  890,  895,  895,  895,
      898,  898,  898,  930,  908,  921,  908,  921,  921,  933,
      898,  888,  934,  888,  898,  908,  891,  936,  937,  898,
      884,  884,    0,  922,  890,  922,  922,  923,  923,  923,
      923, 1041,  930, 1041, 1041,  923, 1003,  933, 1003,  888,
     1042,  934, 1042, 1042,  923,  936, 1003,  937,  884,  885,

      885,  885,  885,  885,  885,  885,  885,  885,  885,  885,
      885,  885,  885,  885,  885,  885,  885,  885,  888,  885,
      885,  885,  885,  885,  892,  893,  892,  893,  906,  894,
      897,  894,  938,  939,  941,  906,  906,  924,  924,  924,
      924,  942,    0,  943,  948,  924,  951,  910,  892,  910,
      885,  885,    0,  893,  924,  906,  892,  894,  910,  897,
      938,  939,  941,  910,  944,  910,  897,  946,    0,  942,
      897,  943,  948,  914,  951,  914,    0,  892,  885,  911,
      947,  911,  893,  949,  906,  892,  894,  914,  897,  914,
      911,  914,  950,  944,  897,  911,  946,  911,  897,  910,

        0,  914,  926,  926,  926,  926, 1020,    0, 1020,  947,
      926, 1153,  949, 1153,  926, 1043, 1020, 1043, 1043,  926,
     1044,  950, 1044, 1044, 1153,  914, 1153,  945,  926,  945,
      914,  911,  915,  915,  952,  915,  915,  915,  915,  915,
      915,  915,  915,  915,  915,  915,  915,  915,  915,  915,
      915,  915,  915,  915,  915,  915,  915,  926,  955,  945,
        0,  956,  957,  952,  958,  959,    0,  961,  962,  967,
      962,  973,    0,  945,  965,  974,  965,  976,  977,    0,
      978,  979,  982,  915,  915,  915,  955,    0,  945,  956,
      962,  957,    0,  958,  959,  961,  965, 1408,  967, 1408,

      973,  945,  980,    0,  974,  976,    0,  977,  978,  979,
      982,  915,  932,  932,  985,  932,  932,  932,  932,  932,
      932,  932,  932,  932,  932,  932,  932,  932,  932,  932,
      932,  980,  932,  932,  932,  932,  932,  969,  970,  969,
      970,    0,    0,  985, 1236,  981, 1236,  981,  969,  970,
      969,  970,  969,  970, 1236,  971,  971,  971,  986,  969,
      970,  991,  972,  932,  932,  971,  971,  981,  971,  992,
      971,  972,  972,  983,  972,  983,  972,  971,  987,  989,
      987,  989,  993,  972,  994,  995,    0,  986,  997,  998,
      991,  932, 1000, 1001, 1002,  983, 1007, 1005,  992, 1005,

      987,  989, 1008, 1009, 1011, 1010, 1012,    0, 1013, 1014,
     1015,  993,  994, 1022,  995,  969,  970,  997,  998, 1005,
     1000, 1024, 1001, 1002, 1007, 1016, 1025, 1016, 1027, 1028,
     1008, 1009, 1011,  971, 1010, 1012, 1013, 1014, 1015,  972,
     1030, 1026, 1022, 1026, 1032, 1040, 1034, 1016, 1035, 1045,
     1024, 1026, 1039, 1046, 1025, 1026, 1028, 1047, 1049, 1027,
     1026, 1036, 1038, 1036, 1038, 1114,    0, 1052, 1048, 1030,
     1414, 1036, 1414, 1032, 1034, 1036, 1038, 1035,    0,    0,
     1036, 1038, 1166, 1039, 1047, 1028,    0, 1040, 1027, 1046,
     1045, 1048, 1052, 1114, 1049, 1111, 1111, 1111, 1112, 1112,

     1112, 1112,    0,    0, 1051, 1111, 1113, 1113, 1113, 1113,
     1166, 1056, 1039, 1047, 1054, 1040, 1057, 1046, 1045, 1059,
     1048, 1052, 1049, 1050, 1050, 1050, 1050, 1050, 1050, 1050,
     1050, 1050, 1050, 1050, 1050, 1050, 1050, 1050, 1050, 1050,
     1050, 1050, 1051, 1050, 1050, 1050, 1050, 1050, 1060, 1055,
     1054, 1056, 1057, 1061, 1068, 1063, 1059, 1064, 1062, 1067,
     1071, 1069, 1126, 1070, 1073, 1075,    0, 1081, 1079, 1077,
     1051, 1085, 1083,    0, 1050, 1050, 1055, 1080, 1054, 1056,
     1057, 1076, 1060, 1063, 1059, 1062, 1070, 1064, 1069, 1071,
     1082, 1126, 1068, 1067, 1061, 1077, 1075, 1072, 1072, 1072,

     1073, 1083, 1050,    0, 1080, 1055, 1076, 1091, 1079, 1081,
     1060, 1086, 1063, 1085, 1062, 1070, 1064, 1069, 1071, 1072,
     1068, 1067, 1061, 1087, 1077, 1075, 1082, 1088, 1073, 1089,
     1083, 1095, 1092, 1080, 1086, 1076, 1079, 1081, 1084, 1084,
     1084, 1085, 1090, 1091, 1098, 1094, 1102, 1097, 1099, 1100,
     1087, 1101, 1103, 1104, 1082, 1107, 1105, 1089, 1088, 1092,
     1084, 1108, 1095, 1086, 1106, 1129,    0, 1098, 1090, 1122,
     1133, 1091, 1094, 1099, 1123, 1123, 1123, 1136, 1103, 1087,
     1129,    0, 1102, 1105,    0,    0, 1089, 1088, 1092, 1097,
     1100, 1095, 1119, 1101, 1129, 1104, 1098, 1090, 1107, 1133,

     1122, 1094, 1099, 1108,    0, 1106, 1136, 1103, 1129, 1117,
     1102, 1117, 1105, 1118, 1127, 1118, 1134, 1097, 1100, 1119,
     1128, 1101, 1128, 1104, 1135, 1127, 1107,    0, 1121, 1122,
     1121, 1108, 1144, 1106, 1115, 1115, 1115, 1115, 1117, 1144,
     1144, 1130, 1118, 1127, 1121, 1169, 1121, 1130, 1119, 1128,
     1134, 1128, 1135, 1127,    0, 1121, 1121, 1163, 1131, 1163,
     1115, 1115, 1175, 1115, 1115, 1145, 1115, 1117, 1121, 1131,
     1130, 1118, 1145, 1145, 1169, 1146, 1130, 1115, 1134, 1163,
        0, 1135, 1146, 1146, 1121, 1121, 1152, 1131, 1152, 1115,
     1115, 1175, 1115, 1115, 1201, 1115, 1121, 1131,    0, 1159,

     1152, 1159, 1159, 1201, 1201, 1115, 1124, 1124, 1124, 1124,
     1124, 1124, 1124, 1124, 1124, 1124, 1124, 1124, 1124, 1124,
     1124, 1124, 1124, 1124, 1124,    0, 1124, 1124, 1124, 1124,
     1124, 1148, 1149, 1148, 1149, 1124, 1150, 1160, 1150, 1160,
     1160, 1177, 1148, 1149, 1164,    0, 1164, 1150,    0, 1148,
     1149, 1167, 1170, 1154, 1172, 1154, 1173, 1124, 1124, 1155,
     1156, 1155, 1156, 1420, 1124, 1420, 1164, 1154,    0, 1154,
     1177, 1154,    0, 1155, 1156, 1155, 1156, 1155, 1156, 1167,
     1170,    0, 1172, 1148, 1173, 1124, 1125, 1125, 1125, 1125,
     1125, 1125, 1125, 1125, 1125, 1125, 1125, 1125, 1125, 1125,

     1125, 1125, 1125, 1125, 1125, 1154, 1125, 1125, 1125, 1125,
     1125, 1155, 1156, 1125, 1161, 1161, 1161, 1161, 1162, 1162,
     1162, 1162, 1161, 1174, 1176, 1179, 1162, 1180, 1182,    0,
     1185, 1161, 1186, 1187, 1188, 1162,    0, 1125, 1125, 1198,
        0, 1198, 1125, 1198, 1252,    0, 1252, 1252,    0,    0,
     1198, 1174, 1176, 1421, 1179, 1421, 1180, 1182, 1185, 1178,
     1186, 1178, 1187, 1188, 1189, 1125, 1132, 1132, 1132, 1132,
     1132, 1132, 1132, 1132, 1132, 1132, 1132, 1132, 1132, 1132,
     1132, 1132, 1132, 1132, 1132, 1178, 1132, 1132, 1132, 1132,
     1132, 1190, 1191, 1189, 1192, 1195, 1192, 1196, 1178, 1193,

     1197, 1193, 1197, 1205, 1206, 1207, 1198, 1208, 1210, 1211,
     1220, 1197, 1215, 1197, 1178, 1197, 1192, 1132, 1132, 1190,
     1191, 1193, 1197,    0, 1195, 1196, 1178, 1200, 1807, 1200,
     1807, 1200, 1205, 1206, 1207, 1208, 1210, 1211, 1200, 1220,
     1215,    0, 1216, 1217, 1216, 1132, 1151, 1151, 1224, 1151,
     1151, 1151, 1151, 1151, 1151, 1151, 1151, 1151, 1151, 1151,
     1151, 1151, 1151, 1151, 1151, 1151, 1151, 1151, 1151, 1151,
     1151, 1217,    0, 1199, 1212, 1199, 1212, 1224, 1197, 1227,
     1214, 1216, 1214, 1218, 1199, 1221, 1199, 1222, 1199, 1238,
     1203, 1203, 1203, 1240, 1200, 1199, 1212, 1151, 1151, 1151,

     1203, 1203, 1214, 1203, 1473, 1203, 1473, 1204, 1227, 1216,
        0, 1218, 1203, 1221, 1473, 1222, 1204, 1204, 1238, 1204,
        0, 1204, 1240,    0,    0, 1151, 1165, 1165, 1204, 1165,
     1165, 1165, 1165, 1165, 1165, 1165, 1165, 1165, 1165, 1165,
     1165, 1165, 1165, 1165, 1165, 1223, 1165, 1165, 1165, 1165,
     1165, 1199, 1219, 1225, 1219, 1257, 1231, 1232, 1231, 1232,
     1234, 1233, 1239, 1233, 1241, 1244, 1249, 1245, 1203, 1247,
        0, 1248, 1310, 1223, 1219,    0,    0, 1165, 1165, 1232,
        0, 1225, 1231, 1233, 1204, 1254, 1254, 1254, 1234, 1264,
     1239, 1317, 1241, 1249, 1244, 1245, 1318, 1257, 1247, 1248,

     1253, 1310, 1253, 1253, 1260, 1165,    0, 1254, 1255, 1255,
     1255, 1231, 1332, 1263, 1332, 1258, 1261, 1265,    0, 1317,
     1267, 1241, 1249, 1332, 1318, 1257,    0, 1260, 1248, 1264,
     1255, 1256, 1256, 1256, 1256, 1256, 1256, 1256, 1256, 1256,
     1256, 1256, 1256, 1256, 1256, 1256, 1256, 1256, 1256, 1256,
     1258, 1256, 1256, 1256, 1256, 1256, 1260, 1264, 1261, 1263,
     1265, 1268, 1267, 1274, 1266, 1269, 1269, 1269, 1271, 1272,
     1273, 1275, 1276, 1276, 1276, 1277, 1278, 1279, 1258, 1280,
     1281, 1284, 1256, 1256,    0, 1287, 1261, 1263, 1265, 1266,
     1267, 1269, 1268, 1289, 1276, 1271, 1272, 1273, 1277, 1282,

     1285, 1290, 1296, 1274, 1269, 1275, 1280, 1281, 1279, 1293,
     1256, 1297, 1294, 1278, 1288, 1288, 1288, 1292, 1266, 1287,
     1269, 1268, 1284, 1295, 1271, 1272, 1273, 1277,    0, 1296,
     1299, 1274, 1269, 1275, 1289, 1280, 1281, 1279, 1319, 1282,
     1292, 1278, 1285, 1286, 1286, 1286, 1297, 1287, 1290, 1306,
     1284, 1293, 1294, 1288,    0, 1291, 1291, 1291, 1296,    0,
     1295, 1299, 1289,    0,    0, 1286, 1319, 1282, 1313, 1292,
     1285, 1303, 1303, 1303, 1297, 1320, 1290, 1291, 1324, 1293,
     1294, 1288, 1304, 1304, 1304, 1305, 1305, 1305, 1295, 1349,
     1299, 1306, 1308, 1308, 1308, 1308, 1313, 1303, 1309, 1309,

     1309, 1309,    0, 1320, 1304, 1329, 1324, 1305, 1322, 1322,
     1322,    0, 1329, 1329, 1330,    0,    0,    0, 1349, 1306,
        0, 1330, 1330, 1323, 1350, 1313, 1303, 1314, 1314, 1314,
     1314, 1314, 1314, 1314, 1314, 1314, 1314, 1314, 1314, 1314,
     1314, 1314, 1314, 1314, 1314, 1314, 1323, 1314, 1314, 1314,
     1314, 1314, 1333, 1350, 1333, 1334,    0, 1334, 1351, 1555,
     1353, 1555, 1343, 1345, 1343, 1345, 1333,    0, 1334, 1555,
     1334, 1354, 1355, 1362, 1335, 1323, 1335, 1357, 1314, 1314,
     1336, 1337, 1336, 1337, 1343, 1345,    0, 1351, 1335, 1353,
     1335,    0, 1335,    0, 1336, 1337, 1336, 1337, 1336, 1337,

     1354, 1355, 1362,    0,    0, 1357, 1314, 1315, 1315, 1315,
     1315, 1315, 1315, 1315, 1315, 1315, 1315, 1315, 1315, 1315,
     1315, 1315, 1315, 1315, 1315, 1315, 1335, 1315, 1315, 1315,
     1315, 1315, 1336, 1337, 1359, 1341, 1341, 1341, 1341, 1342,
     1342, 1342, 1342, 1341, 1348,    0, 1360, 1342, 1348,    0,
     1363, 1315, 1341, 1366, 1364, 1365, 1342,    0, 1315, 1315,
        0, 1376, 1359, 1376, 1418, 1376, 1418, 1418, 1461, 1461,
     1461, 1461, 1376, 1348, 1360,    0,    0, 1348, 1363, 1315,
     1358, 1366, 1358, 1364, 1365, 1367, 1315, 1316, 1316, 1316,
     1316, 1316, 1316, 1316, 1316, 1316, 1316, 1316, 1316, 1316,

     1316, 1316, 1316, 1316, 1316, 1316, 1358, 1316, 1316, 1316,
     1316, 1316, 1369, 1370, 1367, 1371, 1373, 1371, 1374, 1358,
        0, 1375, 1382, 1375, 1378, 1383,    0, 1474, 1376, 1474,
     1474, 1384, 1375, 1384, 1375, 1358, 1375, 1371, 1316, 1316,
     1388, 1369, 1370, 1375, 1373, 1389, 1374, 1358, 1377, 1377,
     1377, 1382, 1378, 1384, 1383, 1387, 1393, 1387, 1377, 1377,
     1391, 1377, 1391, 1377, 1394, 1424, 1316, 1396, 1388, 1399,
     1377, 1401, 1400, 1389, 1400, 1406,    0, 1417, 1407, 1402,
     1409, 1402, 1391, 1404, 1393, 1404, 1413, 1415, 1423, 1430,
        0, 1426, 1424, 1394, 1387, 1396, 1428, 1399, 1400, 1375,

     1401, 1402, 1429, 1425, 1406, 1404, 1407, 1436, 1434, 1409,
     1438, 1423, 1430, 1432, 1413, 1423, 1415, 1417, 1426, 1433,
     1435, 1424, 1387, 1428, 1439, 1440, 1377, 1400, 1425, 1429,
     1442, 1441, 1436, 1446, 1449, 1447, 1452, 1438,    0, 1450,
     1423, 1430, 1458, 1455, 1423, 1417, 1467, 1426, 1434,    0,
     1432, 1439, 1428,    0, 1459, 1453, 1433, 1425, 1429, 1440,
     1435, 1436, 1463, 1469, 1446, 1447, 1438, 1441, 1460, 1470,
     1452, 1472, 1442,    0, 1449, 1467, 1434, 1459, 1432, 1455,
     1439, 1450, 1453, 1471, 1433, 1480, 1458, 1440, 1435,    0,
     1463,    0, 1469, 1446, 1447, 1441, 1472, 1470, 1452, 1460,

     1442, 1478, 1449, 1482, 1481,    0, 1459, 1455,    0, 1450,
        0, 1453, 1471, 1480, 1458, 1475, 1475, 1475, 1476, 1476,
     1476, 1476, 1483, 1475, 1485, 1472, 1476, 1486, 1460, 1487,
     1478, 1482, 1475, 1481, 1488, 1476, 1477, 1477, 1477, 1477,
     1489, 1490,    0, 1491, 1477, 1492, 1493, 1495,    0, 1496,
     1483, 1498, 1485, 1477, 1503, 1486, 1505, 1487, 1506, 1504,
     1509, 1504, 1488, 1510, 1511, 1510, 1512, 1501, 1489, 1501,
     1490, 1491, 1513, 1514, 1492, 1493, 1495, 1496, 1501, 1498,
     1501, 1504, 1501, 1503, 1505, 1510, 1506, 1515, 1509, 1501,
     1516, 1517, 1518, 1511, 1520, 1512, 1526, 1521, 1522, 1528,

     1513, 1514, 1523, 1529, 1523, 1523, 1524, 1530, 1524, 1524,
     1533, 1536, 1531, 1537, 1534, 1515, 1535, 1517, 1538, 1516,
     1526, 1518, 1540, 1520, 1521, 1546, 1522,    0, 1549, 1544,
     1529,    0, 1552, 1557,    0, 1528,    0, 1558, 1536, 1551,
     1537, 1553, 1545, 1545, 1545, 1501, 1517, 1530, 1531, 1526,
     1534, 1535, 1533, 1521, 1544, 1522, 1556, 1561, 1540, 1529,
     1538, 1557, 1546, 1528, 1545, 1558, 1551, 1536, 1549, 1537,
     1550, 1550, 1550, 1552, 1575, 1530, 1531, 1553, 1534, 1535,
     1533, 1570, 1556, 1544, 1561, 1588, 1540, 1588, 1538,    0,
     1546, 1564, 1550, 1564, 1564, 1551, 1549, 1554, 1554, 1554,

     1554, 1552, 1562, 1575, 1562, 1553, 1565, 1588, 1565, 1570,
        0, 1556, 1562, 1561, 1566, 1566, 1566, 1566, 1567, 1567,
     1567, 1578, 1566, 1565, 1571, 1580, 1567, 1574, 1577, 1579,
     1581, 1566, 1569, 1569, 1569, 1567, 1568, 1568, 1568, 1568,
     1569, 1582, 1583, 1582, 1568, 1586, 1601, 1590, 1591, 1569,
     1578, 1582, 1571, 1568, 1580, 1574, 1577, 1579, 1593, 1581,
     1593, 1596, 1597, 1598, 1600, 1604, 1602, 1605, 1608, 1606,
     1583, 1606, 1606, 1609, 1586, 1590, 1612, 1591, 1613, 1615,
     1593, 1616, 1619, 1601, 1621, 1617, 1617, 1617, 1630, 1596,
     1605, 1597, 1598, 1600, 1602, 1617, 1624, 1625, 1626,    0,

     1626, 1604, 1608, 1628, 1643, 1613, 1616, 1619, 1626, 1621,
     1639, 1601, 1644,    0,    0, 1609, 1630, 1615, 1612, 1605,
     1636, 1636, 1636, 1624, 1625, 1635, 1635, 1635, 1635, 1604,
     1608,    0, 1643, 1635, 1613, 1616, 1619, 1636, 1621, 1639,
     1628, 1644, 1635, 1609, 1645, 1615, 1612, 1633, 1633, 1633,
     1633, 1646, 1624, 1625, 1640, 1633, 1640, 1651, 1633, 1637,
     1637, 1637, 1637, 1647, 1640, 1647, 1652, 1637, 1628, 1648,
     1653, 1648, 1645, 1647, 1654, 1660, 1637, 1661, 1662, 1648,
     1646, 1663, 1663, 1663, 1666, 1667, 1651, 1668, 1669, 1687,
     1672, 1663, 1690,    0, 1685, 1652, 1685, 1672, 1688, 1653,

     1672, 1660, 1661, 1654, 1685, 1662,    0, 1673, 1673, 1673,
     1673,    0,    0,    0, 1668, 1669, 1667, 1687, 1666, 1673,
     1690, 1674, 1674, 1674, 1674,    0,    0, 1688, 1675, 1674,
     1660, 1661, 1674, 1673, 1662, 1675,    0,    0, 1675, 1678,
     1678, 1678, 1678, 1668, 1669, 1667, 1666, 1678, 1689, 1680,
     1678, 1696, 1679, 1679, 1679, 1679, 1680, 1695, 1692, 1680,
     1679, 1693, 1673, 1679, 1681, 1681, 1681, 1681, 1682, 1682,
     1682, 1682, 1681, 1686, 1698, 1686, 1682, 1689, 1700, 1696,
     1682, 1681, 1703, 1701,    0, 1682, 1686, 1692, 1686, 1704,
     1693, 1704, 1704, 1695, 1682, 1686, 1706, 1711, 1706, 1706,

     1718, 1698,    0, 1719, 1711, 1700, 1712, 1711, 1703,    0,
     1705, 1705, 1705, 1705,    0, 1720, 1721, 1709, 1709, 1709,
     1709, 1695, 1705, 1682, 1701, 1709,    0,    0, 1709, 1718,
     1698, 1719, 1724, 1726, 1700, 1730, 1705, 1703, 1710, 1710,
     1710, 1710, 1712, 1720, 1713, 1721, 1710, 1722, 1729, 1710,
     1710, 1713, 1701, 1723, 1713, 1714, 1714, 1714, 1714, 1726,
     1716, 1715, 1716, 1714, 1710, 1705, 1714, 1725, 1715, 1724,
     1712, 1715, 1717, 1716, 1717, 1716, 1722, 1728, 1730, 1729,
        0, 1723, 1716, 1717,    0, 1717,    0, 1717, 1726, 1731,
     1749, 1731, 1731, 1710, 1717, 1725, 1732, 1724, 1732, 1732,

     1738, 1735, 1735, 1735, 1735, 1743, 1730, 1738, 1729, 1735,
     1738,    0, 1735, 1736, 1736, 1736, 1736,    0, 1749, 1750,
        0, 1736,    0, 1728, 1736, 1736, 1737, 1737, 1737, 1737,
     1752, 1739,    0, 1740, 1737,    0, 1755, 1737, 1739, 1736,
     1740, 1739, 1743, 1740, 1741, 1741, 1741, 1741, 1750, 1742,
     1746, 1728, 1741, 1757, 1753, 1741, 1742, 1746, 1752, 1742,
     1746, 1744, 1744, 1744, 1744, 1754, 1756, 1761, 1736, 1744,
     1743, 1762, 1744, 1745, 1745, 1745, 1745, 1747, 1755, 1747,
     1777, 1745, 1753, 1763, 1745, 1763, 1763,    0, 1747, 1764,
     1757, 1764, 1764, 1754, 1756,    0,    0, 1765, 1765, 1765,

     1765, 1779,    0,    0, 1761, 1765, 1755, 1780, 1765, 1767,
     1781, 1762, 1766, 1766, 1766, 1766, 1767,    0, 1757, 1767,
     1766,    0, 1777, 1766, 1769, 1768, 1768, 1768, 1768, 1779,
     1770, 1769, 1761, 1768, 1769, 1780, 1768, 1770, 1781, 1762,
     1770, 1771, 1771, 1771, 1771, 1773, 1772, 1772, 1772, 1772,
     1777, 1782, 1773, 1771, 1772, 1773, 1786, 1772, 1774, 1774,
     1774, 1774, 1775, 1775, 1775, 1775, 1774, 1771, 1776, 1774,
     1775, 1787,    0, 1775, 1775, 1776,    0, 1794, 1776, 1788,
     1782, 1788, 1788, 1789, 1794, 1789, 1789, 1794, 1775, 1804,
        0, 1798, 1786,    0,    0, 1802, 1771, 1800, 1787, 1790,

     1790, 1790, 1790, 1791, 1791, 1791, 1791, 1790, 1796, 1827,
     1790, 1791, 1803, 1819, 1791, 1796, 1814, 1775, 1796, 1798,
     1786, 1792, 1792, 1792, 1792, 1804, 1800, 1787, 1818, 1792,
     1817,    0, 1792, 1793, 1793, 1793, 1793, 1802, 1827,    0,
     1803, 1793, 1825,    0, 1793, 1793, 1795, 1795, 1795, 1795,
     1819,    0,    0, 1804, 1795,    0, 1818, 1795, 1814, 1793,
     1809, 1809, 1809, 1809, 1820, 1802, 1820, 1820, 1809, 1811,
     1817, 1809, 1810, 1810, 1810, 1810, 1811, 1813, 1819, 1811,
     1810, 1828, 1825, 1810, 1813,    0, 1814, 1813, 1793, 1808,
     1808, 1808, 1808,    0, 1808, 1836, 1824, 1808, 1817, 1808,

     1808, 1808,    0, 1824, 1808, 1808, 1824, 1826,    0, 1808,
     1825, 1808, 1808, 1808, 1812, 1812, 1812, 1812, 1821, 1821,
     1821, 1821, 1812, 1828, 1836, 1812, 1821,    0, 1829, 1821,
     1829, 1829, 1841, 1840, 1826, 1823, 1823, 1823, 1823,    0,
     1808, 1808, 1808, 1823, 1842, 1843, 1823, 1830, 1830, 1830,
     1830, 1828, 1831,    0, 1832, 1830, 1834,    0, 1830, 1831,
     1841, 1832, 1831, 1826, 1832, 1835,    0, 1839, 1808, 1822,
     1822, 1822, 1822, 1822, 1822, 1840, 1846, 1822, 1822, 1822,
     1822, 1822,    0, 1834, 1822, 1822, 1842, 1843, 1835, 1822,
     1839, 1822, 1822, 1822, 1833, 1833, 1833, 1833, 1838, 1838,

     1838, 1838, 1833, 1840,    0, 1833, 1838,    0,    0, 1838,
        0,    0, 1834,    0, 1842, 1843, 1846, 1835, 1854, 1839,
     1822, 1822, 1822, 1844, 1844, 1844, 1844, 1845, 1845, 1845,
     1845, 1847, 1847, 1847, 1847, 1848, 1848, 1848, 1848, 1849,
     1849, 1849, 1849,    0, 1846, 1854,    0,    0, 1822, 1851,
     1851, 1851, 1851, 1852, 1852, 1852, 1852, 1855, 1855, 1855,
     1855, 1856, 1856, 1856, 1856, 1858, 1858, 1858, 1858, 1859,
     1859, 1859, 1859, 1866, 1854, 1862, 1862, 1862, 1862, 1863,
     1863, 1863, 1863, 1865, 1865, 1865, 1865, 1868, 1868, 1868,
     1868, 1870, 1871, 1871, 1871, 1871, 1866, 1872, 1872, 1872,

     1872, 1873, 1873, 1873, 1873, 1875, 1875, 1875, 1875, 1876,
     1876, 1876, 1876, 1879, 1879, 1879, 1879, 1881, 1881, 1881,
     1881,    0,    0,    0,    0, 1866,    0,    0,    0,    0,
        0,    0,    0, 1870,    0,    0,    0,    0,    0,    0,
        0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
        0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
        0, 1870, 1883, 1883, 1883, 1883, 1883, 1883, 1883, 1883,
     1883, 1883, 1883, 1883, 1883, 1883, 1883, 1883, 1883, 1883,
     1884, 1884, 1884, 1884, 1884, 1884, 1884, 1884, 1884, 1884,
     1884, 1884, 1884, 1884, 1884, 1884, 1884, 1884, 1885, 1885,

     1885, 1885, 1885, 1885, 1885, 1885, 1885, 1885, 1885, 1885,
     1885, 1885, 1885, 1885, 1885, 1885, 1886, 1886,    0, 1886,
     1886, 1886, 1886, 1886, 1886, 1886, 1886, 1886, 1886, 1886,
     1886, 1886, 1886, 1886, 1887, 1887, 1887, 1887, 1887, 1887,
     1887, 1887, 1887, 1887, 1887, 1887, 1887, 1887, 1887, 1887,
     1887, 1887, 1888, 1888, 1888, 1888, 1888, 1888, 1888, 1888,
     1888, 1888, 1888, 1888, 1888, 1888, 1888, 1888, 1888, 1888,
     1889,    0,    0,    0,    0,    0,    0, 1889,    0, 1889,
        0, 1889, 1889, 1889, 1889, 1889, 1890, 1890, 1890, 1890,
     1890, 1891, 1891, 1891, 1891, 1891, 1891, 1891, 1891, 1891,

     1891, 1891, 1891, 1891, 1891, 1891, 1891, 1891, 1891, 1892,
     1892, 1892, 1892, 1892, 1892, 1892, 1892, 1892, 1892, 1892,
     1892, 1892, 1892, 1892, 1892, 1892, 1892, 1893, 1893, 1893,
     1893, 1893, 1893, 1893, 1893, 1893, 1893, 1893, 1893, 1893,
     1893, 1893, 1893, 1893, 1893, 1894, 1894, 1894, 1894, 1894,
     1894, 1894, 1894, 1894, 1894, 1894, 1894, 1894, 1894, 1894,
     1894, 1894, 1894, 1895,    0,    0,    0,    0,    0,    0,
        0,    0,    0,    0, 1895, 1895, 1895, 1895, 1895, 1896,
     1896, 1896, 1896, 1896, 1896, 1896, 1896, 1896, 1896, 1896,
     1896, 1896, 1896, 1896, 1896, 1896, 1896, 1897, 1897,    0,

     1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897, 1897,
     1897, 1897, 1897, 1897, 1897, 1898, 1898, 1898, 1898, 1898,
     1898, 1898, 1898, 1898, 1898, 1898, 1898, 1898, 1898, 1898,
     1898, 1898, 1898, 1899, 1899, 1899, 1899, 1899, 1899, 1899,
     1899, 1899, 1899, 1899, 1899, 1899, 1899, 1899, 1899, 1899,
     1899, 1900, 1900, 1900, 1900, 1900, 1900, 1900, 1900, 1900,
     1900, 1900, 1900, 1900, 1900, 1900, 1900, 1900, 1900, 1901,
     1901, 1901, 1901, 1901, 1901, 1901, 1901, 1901, 1901, 1901,
     1901, 1901, 1901, 1901, 1901, 1901, 1901, 1902,    0,    0,
        0,    0,    0,    0, 1902,    0, 1902,    0,    0, 1902,

     1902, 1902, 1902, 1903, 1903, 1903, 1903,    0, 1903, 1903,
     1903, 1903, 1903, 1903,    0, 1903, 1903,    0,    0, 1903,
     1903, 1904, 1904, 1904, 1904, 1904, 1906, 1906, 1906, 1906,
     1906, 1906, 1906, 1906, 1906, 1906, 1906, 1906, 1906, 1906,
     1906, 1906, 1906, 1906, 1907, 1907, 1907, 1907, 1907, 1907,
     1907, 1907, 1907, 1907, 1907, 1907, 1907, 1907, 1907, 1907,
     1907, 1907, 1908, 1908, 1908, 1908, 1908, 1908, 1908, 1908,
     1908, 1908, 1908, 1908, 1908, 1908, 1908, 1908, 1908, 1908,
     1909, 1909, 1909, 1909, 1909, 1909, 1909, 1909, 1909, 1909,
     1909, 1909, 1909, 1909, 1909, 1909, 1909, 1909, 1910, 1910,

     1910, 1910, 1910, 1910, 1910, 1910, 1910, 1910, 1910, 1910,
     1910, 1910, 1910, 1910, 1910, 1910, 1911, 1911, 1911, 1911,
     1911, 1911, 1911, 1911, 1911, 1911, 1911, 1911, 1911, 1911,
     1911, 1911, 1911, 1911, 1912, 1912, 1912, 1912, 1912, 1912,
     1912, 1912, 1912, 1912, 1912, 1912, 1912, 1912, 1912, 1912,
     1912, 1912, 1913, 1913, 1913, 1913, 1913, 1913, 1913, 1913,
     1913, 1913, 1913, 1913, 1913, 1913, 1913, 1913, 1913, 1913,
     1914, 1914, 1914, 1914, 1914, 1914, 1914, 1914, 1914, 1914,
     1914, 1914, 1914, 1914, 1914, 1914, 1914, 1914, 1915, 1915,
     1915, 1915, 1915, 1915, 1915, 1915, 1915, 1915, 1915, 1915,

     1915, 1915, 1915, 1915, 1915, 1915, 1916, 1916,    0, 1916,
     1916, 1916, 1916, 1916, 1916, 1916, 1916, 1916, 1916, 1916,
     1916, 1916, 1916, 1916, 1917, 1917, 1917, 1917, 1917, 1917,
     1917, 1917, 1917, 1917, 1917, 1917, 1917, 1917, 1917, 1917,
     1917, 1917, 1918, 1918, 1918, 1918, 1918, 1918, 1918, 1918,
     1918, 1918, 1918, 1918, 1918, 1918, 1918, 1918, 1918, 1918,
     1919, 1919, 1919, 1919, 1919, 1919, 1919, 1919, 1919, 1919,
     1919, 1919, 1919, 1919, 1919, 1919, 1919, 1919, 1920, 1920,
     1920, 1920, 1920, 1920, 1920, 1920, 1920, 1920, 1920, 1920,
     1920, 1920, 1920, 1920, 1920, 1920, 1921, 1921, 1921, 1921,

     1921, 1921, 1921, 1921, 1921, 1921, 1921, 1921, 1921, 1921,
     1921, 1921, 1921, 1921, 1922,    0,    0,    0,    0,    0,
        0, 1922,    0, 1922,    0,    0, 1922, 1922, 1922, 1922,
     1923,    0,    0,    0,    0,    0,    0,    0, 1923,    0,
     1923,    0, 1923, 1923, 1923, 1923, 1923, 1924, 1924, 1924,
     1924, 1925, 1925, 1925, 1925, 1925, 1925, 1925, 1925, 1925,
     1925, 1925, 1925, 1925, 1925, 1925, 1925, 1925, 1925, 1926,
     1926, 1926, 1926, 1926, 1926, 1926, 1926, 1926, 1926, 1926,
     1926, 1926, 1926, 1926, 1926, 1926, 1926, 1927, 1927, 1927,
     1927, 1927, 1927, 1927, 1927, 1927, 1927, 1927, 1927, 1927,

     1927, 1927, 1927, 1927, 1927, 1928, 1928, 1928, 1928,    0,
     1928, 1928, 1928, 1928, 1928, 1928,    0, 1928, 1928,    0,
        0, 1928, 1928, 1929, 1929, 1929, 1929, 1929, 1930, 1930,
     1930, 1930, 1930, 1930, 1930, 1930, 1930, 1930, 1930, 1930,
     1930, 1930, 1930, 1930, 1930, 1930, 1931,    0,    0,    0,
        0,    0,    0,    0, 1931, 1931, 1932, 1932, 1932, 1932,
     1932, 1932, 1932, 1932, 1932, 1932, 1932, 1932, 1932, 1932,
     1932, 1932, 1932, 1932, 1933, 1933, 1933, 1933, 1933, 1933,
     1933, 1933, 1933, 1933, 1933, 1933, 1933, 1933, 1933, 1933,
     1933, 1933, 1934, 1934, 1934, 1934, 1934, 1934, 1934, 1934,

     1934, 1934, 1934, 1934, 1934, 1934, 1934, 1934, 1934, 1934,
     1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935,
     1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1936, 1936,
     1936, 1936, 1936, 1936, 1936, 1936, 1936, 1936, 1936, 1936,
     1936, 1936, 1936, 1936, 1936, 1936, 1937, 1937, 1937, 1937,
     1937, 1937, 1937, 1937, 1937, 1937, 1937, 1937, 1937, 1937,
     1937, 1937, 1937, 1937, 1938, 1938, 1938, 1938, 1938, 1938,
     1938, 1938, 1938, 1938, 1938, 1938, 1938, 1938, 1938, 1938,
     1938, 1938, 1939, 1939, 1939, 1939, 1939, 1939, 1939, 1939,
     1939, 1939, 1939, 1939, 1939, 1939, 1939, 1939, 1939, 1939,

     1940, 1940, 1940, 1940, 1940, 1940, 1940, 1940, 1940, 1940,
     1940, 1940, 1940, 1940, 1940, 1940, 1940, 1940, 1941,    0,
        0,    0,    0,    0,    0,    0,    0,    0,    0, 1941,
     1941, 1941, 1941, 1941, 1942, 1942, 1942, 1942, 1942, 1942,
     1942, 1942, 1942, 1942, 1942, 1942, 1942, 1942, 1942, 1942,
     1942, 1942, 1943, 1943, 1943, 1943, 1943, 1943, 1943, 1943,
     1943, 1943, 1943, 1943, 1943, 1943, 1943, 1943, 1943, 1943,
     1944, 1944, 1944, 1944, 1944, 1944, 1944, 1944, 1944, 1944,
     1944, 1944, 1944, 1944, 1944, 1944, 1944, 1944, 1945, 1945,
        0, 1945, 1945, 1945, 1945, 1945, 1945, 1945, 1945, 1945,

     1945, 1945, 1945, 1945, 1945, 1945, 1946, 1946, 1946, 1946,
     1946, 1946, 1946, 1946, 1946, 1946, 1946, 1946, 1946, 1946,
     1946, 1946, 1946, 1946, 1947, 1947, 1947, 1947, 1947, 1947,
     1947, 1947, 1947, 1947, 1947, 1947, 1947, 1947, 1947, 1947,
     1947, 1947, 1948, 1948, 1948, 1948, 1948, 1948, 1948, 1948,
     1948, 1948, 1948, 1948, 1948, 1948, 1948, 1948, 1948, 1948,
     1949, 1949, 1949, 1949, 1949, 1949, 1949, 1949, 1949, 1949,
     1949, 1949, 1949, 1949, 1949, 1949, 1949, 1949, 1950,    0,
        0,    0,    0,    0,    0, 1950,    0, 1950,    0,    0,
     1950, 1950, 1950, 1950, 1951,    0,    0,    0,    0,    0,

        0,    0, 1951,    0,    0,    0, 1951, 1951, 1951, 1951,
     1951, 1952,    0,    0,    0,    0,    0,    0,    0, 1952,
        0, 1952,    0, 1952, 1952, 1952, 1952, 1952, 1953, 1953,
     1953, 1953, 1953, 1953, 1953, 1953, 1953, 1953, 1953, 1953,
     1953, 1953, 1953, 1953, 1953, 1953, 1954, 1954, 1954, 1954,
     1954, 1954, 1954, 1954, 1954, 1954, 1954, 1954, 1954, 1954,
     1954, 1954, 1954, 1954, 1955, 1955, 1955, 1955, 1955, 1955,
     1955, 1955, 1955, 1955, 1955, 1955, 1955, 1955, 1955, 1955,
     1955, 1955, 1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956,
     1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956,

     1957, 1957, 1957, 1957, 1957, 1958, 1958, 1958, 1958, 1958,
     1958, 1958, 1958, 1958, 1958, 1958, 1958, 1958, 1958, 1958,
     1958, 1958, 1958, 1959, 1959, 1959, 1959, 1959, 1959,    0,
     1959, 1959, 1959, 1959, 1959, 1959, 1959, 1959, 1959, 1959,
     1959, 1960, 1960,    0, 1960, 1960, 1960, 1960, 1960, 1960,
     1960, 1960, 1960, 1960, 1960, 1960, 1960, 1960, 1960, 1961,
     1961, 1961, 1961, 1961, 1961, 1961, 1961, 1961, 1961, 1961,
     1961, 1961, 1961, 1961, 1961, 1961, 1961, 1962, 1962, 1962,
     1962, 1962, 1962, 1962, 1962, 1962, 1962, 1962, 1962, 1962,
     1962, 1962, 1962, 1962, 1962, 1963, 1963, 1963, 1963, 1963,

     1963, 1963, 1963, 1963, 1963, 1963, 1963, 1963, 1963, 1963,
     1963, 1963, 1963, 1964, 1964, 1964, 1964, 1964, 1964, 1964,
     1964, 1964, 1964, 1964, 1964, 1964, 1964, 1964, 1964, 1964,
     1964, 1965, 1965, 1965, 1965, 1965, 1965, 1965, 1965, 1965,
     1965, 1965, 1965, 1965, 1965, 1965, 1965, 1965, 1965, 1966,
     1966, 1966, 1966, 1966, 1966, 1966, 1966, 1966, 1966, 1966,
     1966, 1966, 1966, 1966, 1966, 1966, 1966, 1967, 1967, 1967,
     1967, 1967, 1967, 1967, 1967, 1967, 1967, 1967, 1967, 1967,
     1967, 1967, 1967, 1967, 1967, 1968, 1968,    0, 1968, 1968,
     1968, 1968, 1968, 1968, 1968, 1968, 1968, 1968, 1968, 1968,

     1968, 1968, 1968, 1969, 1969,    0, 1969, 1969, 1969, 1969,
     1969, 1969, 1969, 1969, 1969, 1969, 1969, 1969, 1969, 1969,
     1969, 1970, 1970,    0, 1970, 1970, 1970, 1970, 1970, 1970,
     1970, 1970, 1970, 1970, 1970, 1970, 1970, 1970, 1970, 1971,
     1971, 1971, 1971, 1971, 1971, 1971, 1971, 1971, 1971, 1971,
     1971, 1971, 1971, 1971, 1971, 1971, 1971, 1972, 1972, 1972,
     1972, 1972, 1972, 1972, 1972, 1972, 1972, 1972, 1972, 1972,
     1972, 1972, 1972, 1972, 1972, 1973, 1973, 1973, 1973, 1973,
     1973, 1973, 1973, 1973, 1973, 1973, 1973, 1973, 1973, 1973,
     1973, 1973, 1973, 1974, 1974, 1974, 1974, 1974, 1974, 1974,

     1974, 1974, 1974, 1974, 1974, 1974, 1974, 1974, 1974, 1974,
     1974, 1975,    0,    0,    0,    0,    0, 1975,    0,    0,
        0, 1975,    0, 1975, 1975, 1975, 1975, 1975, 1976, 1976,
     1976, 1976, 1977,    0,    0,    0,    0,    0,    0,    0,
     1977,    0,    0,    0, 1977, 1977, 1977, 1977, 1977, 1978,
        0,    0,    0,    0,    0,    0,    0, 1978,    0, 1978,
        0, 1978, 1978, 1978, 1978, 1978, 1979, 1979,    0, 1979,
     1979, 1979, 1979, 1979, 1979, 1979, 1979, 1979, 1979, 1979,
     1979, 1979, 1979, 1979, 1980, 1980, 1980, 1980, 1980, 1980,
     1980, 1980, 1980, 1980, 1980, 1980, 1980, 1980, 1980, 1980,

     1980, 1980, 1981, 1981,    0, 1981, 1981, 1981, 1981, 1981,
     1981, 1981, 1981, 1981, 1981, 1981, 1981, 1981, 1981, 1981,
     1982, 1982, 1982, 1982, 1982, 1982,    0, 1982, 1982, 1982,
     1982, 1982, 1982, 1982, 1982, 1982, 1982, 1982, 1983, 1983,
        0, 1983, 1983, 1983, 1983, 1983, 1983, 1983, 1983, 1983,
     1983, 1983, 1983, 1983, 1983, 1983, 1984, 1984, 1984, 1984,
     1984, 1984, 1984, 1984, 1984, 1984, 1984, 1984, 1984, 1984,
     1984, 1984, 1984, 1984, 1985, 1985, 1985, 1985, 1985, 1985,
     1985, 1985, 1985, 1985, 1985, 1985, 1985, 1985, 1985, 1985,
     1985, 1985, 1986, 1986, 1986, 1986, 1986, 1986, 1986, 1986,

     1986, 1986, 1986, 1986, 1986, 1986, 1986, 1986, 1986, 1986,
     1987, 1987, 1987, 1987, 1987, 1987, 1987, 1987, 1987, 1987,
     1987, 1987, 1987, 1987, 1987, 1987, 1987, 1987, 1988, 1988,
     1988, 1988, 1988, 1988, 1988, 1988, 1988, 1988, 1988, 1988,
     1988, 1988, 1988, 1988, 1988, 1988, 1989, 1989, 1989, 1989,
     1989, 1989, 1989, 1989, 1989, 1989, 1989, 1989, 1989, 1989,
     1989, 1989, 1989, 1989, 1990, 1990, 1990, 1990, 1990, 1990,
     1990, 1990, 1990, 1990, 1990, 1990, 1990, 1990, 1990, 1990,
     1990, 1990, 1991, 1991, 1991, 1991, 1991, 1991, 1991, 1991,
     1991, 1991, 1991, 1991, 1991, 1991, 1991, 1991, 1991, 1991,

     1992, 1992, 1992, 1992, 1992, 1992, 1992, 1992, 1992, 1992,
     1992, 1992, 1992, 1992, 1992, 1992, 1992, 1992, 1993, 1993,
     1993, 1993, 1993, 1993, 1993, 1993, 1993, 1993, 1993, 1993,
     1993, 1993, 1993, 1993, 1993, 1993, 1994, 1994, 1994, 1994,
     1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994, 1994,
     1994, 1994, 1994, 1994, 1995, 1995,    0, 1995, 1995, 1995,
     1995, 1995, 1995, 1995, 1995, 1995, 1995, 1995, 1995, 1995,
     1995, 1995, 1996, 1996,    0, 1996, 1996, 1996, 1996, 1996,
     1996, 1996, 1996, 1996, 1996, 1996, 1996, 1996, 1996, 1996,
     1997, 1997,    0, 1997, 1997, 1997, 1997, 1997, 1997, 1997,

     1997, 1997, 1997, 1997, 1997, 1997, 1997, 1997, 1998, 1998,
     1998, 1998, 1998, 1998, 1998, 1998, 1998, 1998, 1998, 1998,
     1998, 1998, 1998, 1998, 1998, 1998, 1999, 1999, 1999, 1999,
     1999, 1999, 1999, 1999, 1999, 1999, 1999, 1999, 1999, 1999,
     1999, 1999, 1999, 1999, 2000, 2000, 2000, 2000, 2000, 2000,
     2000, 2000, 2000, 2000, 2000, 2000, 2000, 2000, 2000, 2000,
     2000, 2000, 2001, 2001, 2001, 2001, 2001, 2001, 2001, 2001,
     2001, 2001, 2001, 2001, 2001, 2001, 2001, 2001, 2001, 2001,
     2002,    0,    0,    0,    0,    0, 2002,    0,    0,    0,
        0,    0, 2002, 2002, 2002, 2002, 2002, 2003, 2003,    0,

     2003, 2003, 2003, 2003, 2003, 2003, 2003, 2003, 2003, 2003,
     2003, 2003, 2003, 2003, 2003, 2004,    0,    0,    0,    0,
        0,    0, 2004,    0, 2004,    0,    0, 2004, 2004, 2004,
     2004, 2005,    0,    0,    0,    0,    0,    0,    0, 2005,
        0, 2005,    0, 2005, 2005, 2005, 2005, 2005, 2006, 2006,
     2006, 2006, 2007, 2007,    0, 2007, 2007, 2007, 2007, 2007,
     2007, 2007, 2007, 2007, 2007, 2007, 2007, 2007, 2007, 2007,
     2008, 2008, 2008, 2008, 2008, 2008,    0, 2008, 2008, 2008,
     2008, 2008, 2008, 2008, 2008, 2008, 2008, 2008, 2009, 2009,
        0, 2009, 2009, 2009, 2009, 2009, 2009, 2009, 2009, 2009,

     2009, 2009, 2009, 2009, 2009, 2009, 2010, 2010,    0, 2010,
     2010, 2010, 2010, 2010, 2010, 2010, 2010, 2010, 2010, 2010,
     2010, 2010, 2010, 2010, 2011, 2011, 2011, 2011, 2011, 2011,
     2011, 2011, 2011, 2011, 2011, 2011, 2011, 2011, 2011, 2011,
     2011, 2011, 2012, 2012, 2012, 2012, 2012, 2012, 2012, 2012,
     2012, 2012, 2012, 2012, 2012, 2012, 2012, 2012, 2012, 2012,
     2013, 2013, 2013, 2013, 2013, 2013, 2013, 2013, 2013, 2013,
     2013, 2013, 2013, 2013, 2013, 2013, 2013, 2013, 2014, 2014,
     2014, 2014, 2014, 2014, 2014, 2014, 2014, 2014, 2014, 2014,
     2014, 2014, 2014, 2014, 2014, 2014, 2015, 2015, 2015, 2015,

     2015, 2015, 2015, 2015, 2015, 2015, 2015, 2015, 2015, 2015,
     2015, 2015, 2015, 2015, 2016,    0, 2016,    0,    0,    0,
        0, 2016,    0,    0, 2016, 2016, 2016, 2016, 2016, 2016,
     2017, 2017, 2017, 2017, 2017, 2017, 2017, 2017, 2017, 2017,
     2017, 2017, 2017, 2017, 2017, 2017, 2017, 2017, 2018, 2018,
     2018, 2018, 2018, 2018, 2018, 2018, 2018, 2018, 2018, 2018,
     2018, 2018, 2018, 2018, 2018, 2018, 2019, 2019, 2019, 2019,
     2019, 2019, 2019, 2019, 2019, 2019, 2019, 2019, 2019, 2019,
     2019, 2019, 2019, 2019, 2020, 2020, 2020, 2020, 2020, 2020,
     2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020,

     2020, 2020, 2021, 2021, 2021, 2021, 2021, 2021, 2021, 2021,
     2021, 2021, 2021, 2021, 2021, 2021, 2021, 2021, 2021, 2021,
     2022, 2022,    0, 2022, 2022, 2022, 2022, 2022, 2022, 2022,
     2022, 2022, 2022, 2022, 2022, 2022, 2022, 2022, 2023, 2023,
     2023, 2023, 2023, 2023, 2023, 2023, 2023, 2023, 2023, 2023,
     2023, 2023, 2023, 2023, 2023, 2023, 2024, 2024, 2024, 2024,
     2024, 2024, 2024, 2024, 2024, 2024, 2024, 2024, 2024, 2024,
     2024, 2024, 2024, 2024, 2025,    0,    0,    0,    0,    0,
     2025,    0,    0,    0,    0,    0, 2025, 2025, 2025, 2025,
     2025, 2026,    0,    0,    0,    0,    0,    0, 2026,    0,

     2026,    0,    0, 2026, 2026, 2026, 2026, 2027,    0,    0,
        0,    0,    0,    0,    0, 2027,    0, 2027,    0, 2027,
     2027, 2027, 2027, 2027, 2028, 2028, 2028, 2028, 2029,    0,
     2029,    0,    0,    0,    0, 2029,    0,    0, 2029, 2029,
     2029, 2029, 2029, 2029, 2030,    0, 2030,    0,    0,    0,
        0, 2030,    0,    0, 2030, 2030, 2030, 2030, 2030, 2030,
     2031, 2031, 2031, 2031, 2031, 2031, 2031, 2031, 2031, 2031,
     2031, 2031, 2031, 2031, 2031, 2031, 2031, 2031, 2032, 2032,
     2032, 2032, 2032, 2033, 2033,    0, 2033, 2033, 2033, 2033,
     2033, 2033, 2033, 2033, 2033, 2033, 2033, 2033, 2033, 2033,

     2033, 2034, 2034, 2034, 2034, 2034, 2034, 2034, 2034, 2034,
     2034, 2034, 2034, 2034, 2034, 2034, 2034, 2034, 2034, 2035,
     2035, 2035, 2035, 2035, 2035, 2035, 2035, 2035, 2035, 2035,
     2035, 2035, 2035, 2035, 2035, 2035, 2035, 2036, 2036, 2036,
     2036, 2036, 2036, 2036, 2036, 2036, 2036, 2036, 2036, 2036,
     2036, 2036, 2036, 2036, 2036, 2037, 2037, 2037, 2037, 2037,
     2037, 2037, 2037, 2037, 2037, 2037, 2037, 2037, 2037, 2037,
     2037, 2037, 2037, 2038, 2038, 2038, 2038, 2038, 2038, 2038,
     2038, 2038, 2038, 2038, 2038, 2038, 2038, 2038, 2038, 2038,
     2038, 2039, 2039, 2039, 2039, 2039, 2039, 2039, 2039, 2039,

     2039, 2039, 2039, 2039, 2039, 2039, 2039, 2039, 2039, 2040,
     2040, 2040, 2040, 2040, 2040, 2040, 2040, 2040, 2040, 2040,
     2040, 2040, 2040, 2040, 2040, 2040, 2040, 2041, 2041, 2041,
     2041, 2041, 2041, 2041, 2041, 2041, 2041, 2041, 2041, 2041,
     2041, 2041, 2041, 2041, 2041, 2042, 2042, 2042, 2042, 2042,
     2042, 2042, 2042, 2042, 2042, 2042, 2042, 2042, 2042, 2042,
     2042, 2042, 2042, 2043, 2043, 2043, 2043, 2043, 2043, 2043,
     2043, 2043, 2043, 2043, 2043, 2043, 2043, 2043, 2043, 2043,
     2043, 2044, 2044, 2044, 2044, 2044, 2044, 2044, 2044, 2044,
     2044, 2044, 2044, 2044, 2044, 2044, 2044, 2044, 2044, 2045,

     2045, 2045, 2045, 2045, 2045, 2045, 2045, 2045, 2045, 2045,
     2045, 2045, 2045, 2045, 2045, 2045, 2045, 2046, 2046, 2046,
     2046, 2046, 2046, 2046, 2046, 2046, 2046, 2046, 2046, 2046,
     2046, 2046, 2046, 2046, 2046, 2047, 2047, 2047, 2047, 2047,
     2047, 2047, 2047, 2047, 2047, 2047, 2047, 2047, 2047, 2047,
     2047, 2047, 2047, 2048, 2048, 2048, 2048, 2048, 2048, 2048,
     2048, 2048, 2048, 2048, 2048, 2048, 2048, 2048, 2048, 2048,
     2048, 2049, 2049, 2049, 2049, 2049, 2049, 2049, 2049, 2049,
     2049, 2049, 2049, 2049, 2049, 2049, 2049, 2049, 2049, 2050,
     2050, 2050, 2050, 2050, 2050, 2050, 2050, 2050, 2050, 2050,

     2050, 2050, 2050, 2050, 2050, 2050, 2050, 1882, 1882, 1882,
     1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882,
     1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882,
     1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882,
     1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882,
     1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882,
     1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882,
     1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882,
     1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882
    } ;

extern int fortran__flex_debug;
int fortran__flex_debug = 0;

static yy_state_type *yy_state_buf=0, *yy_state_ptr=0;
static char *yy_full_match;
static int yy_lp;
static int yy_looking_for_trail_begin = 0;
static int yy_full_lp;
static int *yy_full_state;
#define YY_TRAILING_MASK 0x2000
#define YY_TRAILING_HEAD_MASK 0x4000
#define REJECT \
{ \
*yy_cp = (yy_hold_char); /* undo effects of setting up fortran_text */ \
yy_cp = (yy_full_match); /* restore poss. backed-over text */ \
(yy_lp) = (yy_full_lp); /* restore orig. accepting pos. */ \
(yy_state_ptr) = (yy_full_state); /* restore orig. state */ \
yy_current_state = *(yy_state_ptr); /* restore curr. state */ \
++(yy_lp); \
goto find_rule; \
}

#define yymore() yymore_used_but_not_detected
#define YY_MORE_ADJ 0
#define YY_RESTORE_YY_MORE_OFFSET
char *fortran_text;
#line 1 "fortran.lex"
/******************************************************************************/
/*                                                                            */
/*     CONV (converter) for Agrif (Adaptive Grid Refinement In Fortran)       */
/*                                                                            */
/* Copyright or   or Copr. Laurent Debreu (Laurent.Debreu@imag.fr)            */
/*                        Cyril Mazauric (Cyril_Mazauric@yahoo.fr)            */
/* This software is governed by the CeCILL-C license under French law and     */
/* abiding by the rules of distribution of free software.  You can  use,      */
/* modify and/ or redistribute the software under the terms of the CeCILL-C   */
/* license as circulated by CEA, CNRS and INRIA at the following URL          */
/* "http://www.cecill.info".                                                  */
/*                                                                            */
/* As a counterpart to the access to the source code and  rights to copy,     */
/* modify and redistribute granted by the license, users are provided only    */
/* with a limited warranty  and the software's author,  the holder of the     */
/* economic rights,  and the successive licensors  have only  limited         */
/* liability.                                                                 */
/*                                                                            */
/* In this respect, the user's attention is drawn to the risks associated     */
/* with loading,  using,  modifying and/or developing or reproducing the      */
/* software by the user in light of its specific status of free software,     */
/* that may mean  that it is complicated to manipulate,  and  that  also      */
/* therefore means  that it is reserved for developers  and  experienced      */
/* professionals having in-depth computer knowledge. Users are therefore      */
/* encouraged to load and test the software's suitability as regards their    */
/* requirements in conditions enabling the security of their systems and/or   */
/* data to be ensured and,  more generally, to use and operate it in the      */
/* same conditions as regards security.                                       */
/*                                                                            */
/* The fact that you are presently reading this means that you have had       */
/* knowledge of the CeCILL-C license and that you accept its terms.           */
/******************************************************************************/
/* version 1.7                                                                */
/******************************************************************************/







#line 46 "fortran.lex"
#include <math.h>
#include <stdlib.h>
#include <string.h>
extern FILE * fortran_in;
#define MAX_INCLUDE_DEPTH 30
#define YY_BUF_SIZE 64000
YY_BUFFER_STATE include_stack[MAX_INCLUDE_DEPTH];
int line_num_input = 0;
int newlinef90 = 0;
int tmpc;

int lastwasendofstmt = 1;

extern char linebuf1[1024];
extern char linebuf2[1024];

int count_newlines(const char* str_in)
{
    int k, i = 0;
    for( k=0 ; k<strlen(str_in) ; k++)
        if (str_in[k] == '\n') i++;
    return i;
}

#define PRINT_LINE_NUM()   //  { fprintf(stderr,"== Parsing l.%4d...\n", line_num_input); }
#define INCREMENT_LINE_NUM() { line_num_input+=count_newlines(fortran_text) ; PRINT_LINE_NUM(); }
#define YY_USER_ACTION       { if (increment_nbtokens !=0) token_since_endofstmt++; increment_nbtokens = 1; if (token_since_endofstmt>=1) lastwasendofstmt=0; /*printf("VALLIJSDFLSD = %d %d %s \n",lastwasendofstmt,token_since_endofstmt,fortran_text); */ if (firstpass) { strcpy(linebuf1, linebuf2); strncpy(linebuf2, fortran_text,80);} \
                               else {my_position_before=setposcur();/*printf("muposition = %d\n",my_position_before);*/ECHO;} }
#define YY_BREAK {/*printf("VALL = %d %d\n",lastwasendofstmt,token_since_endofstmt);*/if (token_since_endofstmt>=1) lastwasendofstmt=0; break;}

void out_of_donottreat(void);

#line 3526 "fortran.yy.c"

#define INITIAL 0
#define parameter 1
#define character 2
#define donottreat 3
#define donottreat_interface 4
#define includestate 5
#define fortran77style 6
#define fortran90style 7

#ifndef YY_NO_UNISTD_H
/* Special case for "unistd.h", since it is non-ANSI. We include it way
 * down here because we want the user's section 1 to have been scanned first.
 * The user has a chance to override it with an option.
 */
#include <unistd.h>
#endif

#ifndef YY_EXTRA_TYPE
#define YY_EXTRA_TYPE void *
#endif

static int yy_init_globals (void );

/* Accessor methods to globals.
   These are made visible to non-reentrant scanners for convenience. */

int fortran_lex_destroy (void );

int fortran_get_debug (void );

void fortran_set_debug (int debug_flag  );

YY_EXTRA_TYPE fortran_get_extra (void );

void fortran_set_extra (YY_EXTRA_TYPE user_defined  );

FILE *fortran_get_in (void );

void fortran_set_in  (FILE * in_str  );

FILE *fortran_get_out (void );

void fortran_set_out  (FILE * out_str  );

yy_size_t fortran_get_leng (void );

char *fortran_get_text (void );

int fortran_get_lineno (void );

void fortran_set_lineno (int line_number  );

/* Macros after this point can all be overridden by user definitions in
 * section 1.
 */

#ifndef YY_SKIP_YYWRAP
#ifdef __cplusplus
extern "C" int fortran_wrap (void );
#else
extern int fortran_wrap (void );
#endif
#endif

    static void yyunput (int c,char *buf_ptr  );
    
#ifndef yytext_ptr
static void yy_flex_strncpy (char *,yyconst char *,int );
#endif

#ifdef YY_NEED_STRLEN
static int yy_flex_strlen (yyconst char * );
#endif

#ifndef YY_NO_INPUT

#ifdef __cplusplus
static int yyinput (void );
#else
static int input (void );
#endif

#endif

/* Amount of stuff to slurp up with each read. */
#ifndef YY_READ_BUF_SIZE
#define YY_READ_BUF_SIZE 8192
#endif

/* Copy whatever the last rule matched to the standard output. */
#ifndef ECHO
/* This used to be an fputs(), but since the string might contain NUL's,
 * we now use fwrite().
 */
#define ECHO fwrite( fortran_text, fortran_leng, 1, fortran_out )
#endif

/* Gets input and stuffs it into "buf".  number of characters read, or YY_NULL,
 * is returned in "result".
 */
#ifndef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
	if ( YY_CURRENT_BUFFER_LVALUE->yy_is_interactive ) \
		{ \
		int c = '*'; \
		yy_size_t n; \
		for ( n = 0; n < max_size && \
			     (c = getc( fortran_in )) != EOF && c != '\n'; ++n ) \
			buf[n] = (char) c; \
		if ( c == '\n' ) \
			buf[n++] = (char) c; \
		if ( c == EOF && ferror( fortran_in ) ) \
			YY_FATAL_ERROR( "input in flex scanner failed" ); \
		result = n; \
		} \
	else \
		{ \
		errno=0; \
		while ( (result = fread(buf, 1, max_size, fortran_in))==0 && ferror(fortran_in)) \
			{ \
			if( errno != EINTR) \
				{ \
				YY_FATAL_ERROR( "input in flex scanner failed" ); \
				break; \
				} \
			errno=0; \
			clearerr(fortran_in); \
			} \
		}\
\

#endif

/* No semi-colon after return; correct usage is to write "yyterminate();" -
 * we don't want an extra ';' after the "return" because that will cause
 * some compilers to complain about unreachable statements.
 */
#ifndef yyterminate
#define yyterminate() return YY_NULL
#endif

/* Number of entries by which start-condition stack grows. */
#ifndef YY_START_STACK_INCR
#define YY_START_STACK_INCR 25
#endif

/* Report a fatal error. */
#ifndef YY_FATAL_ERROR
#define YY_FATAL_ERROR(msg) yy_fatal_error( msg )
#endif

/* end tables serialization structures and prototypes */

/* Default declaration of generated scanner - a define so the user can
 * easily add parameters.
 */
#ifndef YY_DECL
#define YY_DECL_IS_OURS 1

extern int fortran_lex (void);

#define YY_DECL int fortran_lex (void)
#endif /* !YY_DECL */

/* Code executed at the beginning of each rule, after fortran_text and fortran_leng
 * have been set up.
 */
#ifndef YY_USER_ACTION
#define YY_USER_ACTION
#endif

/* Code executed at the end of each rule. */
#ifndef YY_BREAK
#define YY_BREAK break;
#endif

#define YY_RULE_SETUP \
	if ( fortran_leng > 0 ) \
		YY_CURRENT_BUFFER_LVALUE->yy_at_bol = \
				(fortran_text[fortran_leng - 1] == '\n'); \
	YY_USER_ACTION

/** The main scanner function which does all the work.
 */
YY_DECL
{
	register yy_state_type yy_current_state;
	register char *yy_cp, *yy_bp;
	register int yy_act;
    
#line 101 "fortran.lex"

  if (infixed) BEGIN(fortran77style) ;
  if (infree)  BEGIN(fortran90style) ;

#line 3723 "fortran.yy.c"

	if ( !(yy_init) )
		{
		(yy_init) = 1;

#ifdef YY_USER_INIT
		YY_USER_INIT;
#endif

        /* Create the reject buffer large enough to save one state per allowed character. */
        if ( ! (yy_state_buf) )
            (yy_state_buf) = (yy_state_type *)fortran_alloc(YY_STATE_BUF_SIZE  );
            if ( ! (yy_state_buf) )
                YY_FATAL_ERROR( "out of dynamic memory in fortran_lex()" );

		if ( ! (yy_start) )
			(yy_start) = 1;	/* first start state */

		if ( ! fortran_in )
			fortran_in = stdin;

		if ( ! fortran_out )
			fortran_out = stdout;

		if ( ! YY_CURRENT_BUFFER ) {
			fortran_ensure_buffer_stack ();
			YY_CURRENT_BUFFER_LVALUE =
				fortran__create_buffer(fortran_in,YY_BUF_SIZE );
		}

		fortran__load_buffer_state( );
		}

	while ( 1 )		/* loops until end-of-file is reached */
		{
		yy_cp = (yy_c_buf_p);

		/* Support of fortran_text. */
		*yy_cp = (yy_hold_char);

		/* yy_bp points to the position in yy_ch_buf of the start of
		 * the current run.
		 */
		yy_bp = yy_cp;

		yy_current_state = (yy_start);
		yy_current_state += YY_AT_BOL();

		(yy_state_ptr) = (yy_state_buf);
		*(yy_state_ptr)++ = yy_current_state;

yy_match:
		do
			{
			register YY_CHAR yy_c = yy_ec[YY_SC_TO_UI(*yy_cp)];
			while ( yy_chk[yy_base[yy_current_state] + yy_c] != yy_current_state )
				{
				yy_current_state = (int) yy_def[yy_current_state];
				if ( yy_current_state >= 1883 )
					yy_c = yy_meta[(unsigned int) yy_c];
				}
			yy_current_state = yy_nxt[yy_base[yy_current_state] + (unsigned int) yy_c];
			*(yy_state_ptr)++ = yy_current_state;
			++yy_cp;
			}
		while ( yy_base[yy_current_state] != 9608 );

yy_find_action:
		yy_current_state = *--(yy_state_ptr);
		(yy_lp) = yy_accept[yy_current_state];
goto find_rule; /* Shut up GCC warning -Wall */
find_rule: /* we branch to this label when backing up */
		for ( ; ; ) /* until we find what rule we matched */
			{
			if ( (yy_lp) && (yy_lp) < yy_accept[yy_current_state + 1] )
				{
				yy_act = yy_acclist[(yy_lp)];
				if ( yy_act & YY_TRAILING_HEAD_MASK ||
				     (yy_looking_for_trail_begin) )
					{
					if ( yy_act == (yy_looking_for_trail_begin) )
						{
						(yy_looking_for_trail_begin) = 0;
						yy_act &= ~YY_TRAILING_HEAD_MASK;
						break;
						}
					}
				else if ( yy_act & YY_TRAILING_MASK )
					{
					(yy_looking_for_trail_begin) = yy_act & ~YY_TRAILING_MASK;
					(yy_looking_for_trail_begin) |= YY_TRAILING_HEAD_MASK;
					}
				else
					{
					(yy_full_match) = yy_cp;
					(yy_full_state) = (yy_state_ptr);
					(yy_full_lp) = (yy_lp);
					break;
					}
				++(yy_lp);
				goto find_rule;
				}
			--yy_cp;
			yy_current_state = *--(yy_state_ptr);
			(yy_lp) = yy_accept[yy_current_state];
			}

		YY_DO_BEFORE_ACTION;

do_action:	/* This label is used only to access EOF actions. */

		switch ( yy_act )
	{ /* beginning of action switch */
case 1:
YY_RULE_SETUP
#line 105 "fortran.lex"
{ return TOK_SUBROUTINE; }
	YY_BREAK
case 2:
YY_RULE_SETUP
#line 106 "fortran.lex"
{ return TOK_PROGRAM; }
	YY_BREAK
case 3:
YY_RULE_SETUP
#line 107 "fortran.lex"
{ inallocate = 1; return TOK_ALLOCATE; }
	YY_BREAK
case 4:
YY_RULE_SETUP
#line 108 "fortran.lex"
{ return TOK_CONTINUE; }
	YY_BREAK
case 5:
YY_RULE_SETUP
#line 109 "fortran.lex"
{ return TOK_NULLIFY; }
	YY_BREAK
case 6:
YY_RULE_SETUP
#line 110 "fortran.lex"
{ inallocate = 1; return TOK_DEALLOCATE; }
	YY_BREAK
case 7:
YY_RULE_SETUP
#line 111 "fortran.lex"
{ return TOK_RESULT; }
	YY_BREAK
case 8:
YY_RULE_SETUP
#line 112 "fortran.lex"
{ return TOK_FUNCTION; }
	YY_BREAK
case 9:
YY_RULE_SETUP
#line 113 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_ENDUNIT;}
	YY_BREAK
case 10:
YY_RULE_SETUP
#line 114 "fortran.lex"
{ pos_curinclude = setposcur()-9; BEGIN(includestate); }
	YY_BREAK
case 11:
YY_RULE_SETUP
#line 115 "fortran.lex"
{ return TOK_USE;}
	YY_BREAK
case 12:
YY_RULE_SETUP
#line 116 "fortran.lex"
{ return TOK_REWIND; }
	YY_BREAK
case 13:
YY_RULE_SETUP
#line 117 "fortran.lex"
{ return TOK_IMPLICIT; }
	YY_BREAK
case 14:
YY_RULE_SETUP
#line 118 "fortran.lex"
{ return TOK_NONE; }
	YY_BREAK
case 15:
YY_RULE_SETUP
#line 119 "fortran.lex"
{ return TOK_CALL; }
	YY_BREAK
case 16:
YY_RULE_SETUP
#line 120 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_TRUE; }
	YY_BREAK
case 17:
YY_RULE_SETUP
#line 121 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_FALSE; }
	YY_BREAK
case 18:
YY_RULE_SETUP
#line 122 "fortran.lex"
{ return TOK_POINT_TO; }
	YY_BREAK
case 19:
YY_RULE_SETUP
#line 123 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_ASSIGNTYPE;}
	YY_BREAK
case 20:
YY_RULE_SETUP
#line 124 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_DASTER; }
	YY_BREAK
case 21:
YY_RULE_SETUP
#line 125 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_EQV; }
	YY_BREAK
case 22:
YY_RULE_SETUP
#line 126 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_EQ;  }
	YY_BREAK
case 23:
YY_RULE_SETUP
#line 127 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_GT;  }
	YY_BREAK
case 24:
YY_RULE_SETUP
#line 128 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_GE;  }
	YY_BREAK
case 25:
YY_RULE_SETUP
#line 129 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_LT;  }
	YY_BREAK
case 26:
YY_RULE_SETUP
#line 130 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_LE;  }
	YY_BREAK
case 27:
YY_RULE_SETUP
#line 131 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_NEQV;}
	YY_BREAK
case 28:
YY_RULE_SETUP
#line 132 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_NE;  }
	YY_BREAK
case 29:
YY_RULE_SETUP
#line 133 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_NOT; }
	YY_BREAK
case 30:
YY_RULE_SETUP
#line 134 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_OR;  }
	YY_BREAK
case 31:
YY_RULE_SETUP
#line 135 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_XOR; }
	YY_BREAK
case 32:
YY_RULE_SETUP
#line 136 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_AND; }
	YY_BREAK
case 33:
YY_RULE_SETUP
#line 137 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_EQUALEQUAL; }
	YY_BREAK
case 34:
YY_RULE_SETUP
#line 138 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_SLASHEQUAL; }
	YY_BREAK
case 35:
YY_RULE_SETUP
#line 139 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_INFEQUAL; }
	YY_BREAK
case 36:
YY_RULE_SETUP
#line 140 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_SUPEQUAL; }
	YY_BREAK
case 37:
YY_RULE_SETUP
#line 141 "fortran.lex"
{ return TOK_MODULE; }
	YY_BREAK
case 38:
YY_RULE_SETUP
#line 142 "fortran.lex"
{ return TOK_WHILE; }
	YY_BREAK
case 39:
YY_RULE_SETUP
#line 143 "fortran.lex"
{ return TOK_CONCURRENT; }
	YY_BREAK
case 40:
YY_RULE_SETUP
#line 144 "fortran.lex"
{ return TOK_ENDDO; }
	YY_BREAK
case 41:
YY_RULE_SETUP
#line 145 "fortran.lex"
{ strcpy(yylval.na,&fortran_text[2]);
                              if (testandextractfromlist(&List_Do_labels,&fortran_text[2]) == 1)
                              {
                              return TOK_PLAINDO_LABEL_DJVIEW;
                              }
                              else
                              {
                              List_Do_labels=Insertname(List_Do_labels,yylval.na,1);
                              return TOK_PLAINDO_LABEL;
                             }
                             }
	YY_BREAK
case 42:
YY_RULE_SETUP
#line 156 "fortran.lex"
{ increment_nbtokens = 0; return TOK_PLAINDO;}
	YY_BREAK
case 43:
YY_RULE_SETUP
#line 157 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_REAL; }
	YY_BREAK
case 44:
YY_RULE_SETUP
#line 158 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_INTEGER; }
	YY_BREAK
case 45:
YY_RULE_SETUP
#line 159 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_LOGICAL; }
	YY_BREAK
case 46:
YY_RULE_SETUP
#line 160 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_CHARACTER; }
	YY_BREAK
case 47:
YY_RULE_SETUP
#line 161 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_HEXA;}
	YY_BREAK
case 48:
YY_RULE_SETUP
#line 162 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_DOUBLEPRECISION; }
	YY_BREAK
case 49:
YY_RULE_SETUP
#line 163 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_DOUBLECOMPLEX; }
	YY_BREAK
case 50:
YY_RULE_SETUP
#line 164 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_COMPLEX; }
	YY_BREAK
case 51:
YY_RULE_SETUP
#line 165 "fortran.lex"
{ return TOK_ALLOCATABLE; }
	YY_BREAK
case 52:
YY_RULE_SETUP
#line 166 "fortran.lex"
{ return TOK_CLOSE; }
	YY_BREAK
case 53:
YY_RULE_SETUP
#line 167 "fortran.lex"
{ return TOK_INQUIRE; }
	YY_BREAK
case 54:
YY_RULE_SETUP
#line 168 "fortran.lex"
{ return TOK_DIMENSION; }
	YY_BREAK
case 55:
YY_RULE_SETUP
#line 169 "fortran.lex"
{ return TOK_PAUSE; }
	YY_BREAK
case 56:
YY_RULE_SETUP
#line 170 "fortran.lex"
{ return TOK_EQUIVALENCE; }
	YY_BREAK
case 57:
YY_RULE_SETUP
#line 171 "fortran.lex"
{ return TOK_STOP; }
	YY_BREAK
case 58:
YY_RULE_SETUP
#line 172 "fortran.lex"
{ return TOK_WHERE; }
	YY_BREAK
case 59:
YY_RULE_SETUP
#line 173 "fortran.lex"
{ return TOK_ENDWHERE; }
	YY_BREAK
case 60:
YY_RULE_SETUP
#line 174 "fortran.lex"
{ return TOK_ELSEWHEREPAR; }
	YY_BREAK
case 61:
YY_RULE_SETUP
#line 175 "fortran.lex"
{ return TOK_ELSEWHERE; }
	YY_BREAK
case 62:
YY_RULE_SETUP
#line 176 "fortran.lex"
{ return TOK_CONTAINS; }
	YY_BREAK
case 63:
YY_RULE_SETUP
#line 177 "fortran.lex"
{ return TOK_ONLY; }
	YY_BREAK
case 64:
YY_RULE_SETUP
#line 178 "fortran.lex"
{ return TOK_PARAMETER; }
	YY_BREAK
case 65:
YY_RULE_SETUP
#line 179 "fortran.lex"
{ return TOK_RECURSIVE; }
	YY_BREAK
case 66:
YY_RULE_SETUP
#line 180 "fortran.lex"
{ return TOK_COMMON; }
	YY_BREAK
case 67:
YY_RULE_SETUP
#line 181 "fortran.lex"
{ return TOK_GLOBAL; }
	YY_BREAK
case 68:
YY_RULE_SETUP
#line 182 "fortran.lex"
{ return TOK_EXTERNAL; }
	YY_BREAK
case 69:
YY_RULE_SETUP
#line 183 "fortran.lex"
{ intent_spec = 1; return TOK_INTENT; }
	YY_BREAK
case 70:
YY_RULE_SETUP
#line 184 "fortran.lex"
{ return TOK_POINTER; }
	YY_BREAK
case 71:
YY_RULE_SETUP
#line 185 "fortran.lex"
{ return TOK_OPTIONAL; }
	YY_BREAK
case 72:
YY_RULE_SETUP
#line 186 "fortran.lex"
{ return TOK_SAVE; }
	YY_BREAK
case 73:
YY_RULE_SETUP
#line 187 "fortran.lex"
{ pos_cur_decl = setposcur()-strlen(fortran_text); return TOK_TYPEPAR; }
	YY_BREAK
case 74:
YY_RULE_SETUP
#line 188 "fortran.lex"
{ return TOK_TYPE; }
	YY_BREAK
case 75:
YY_RULE_SETUP
#line 189 "fortran.lex"
{ return TOK_ENDTYPE; }
	YY_BREAK
case 76:
YY_RULE_SETUP
#line 190 "fortran.lex"
{ if (inallocate == 1) return TOK_STAT; else { strcpy(yylval.na,fortran_text); return TOK_NAME; } }
	YY_BREAK
case 77:
YY_RULE_SETUP
#line 191 "fortran.lex"
{ return TOK_OPEN; }
	YY_BREAK
case 78:
YY_RULE_SETUP
#line 192 "fortran.lex"
{ return TOK_RETURN; }
	YY_BREAK
case 79:
YY_RULE_SETUP
#line 193 "fortran.lex"
{ return TOK_EXIT; }
	YY_BREAK
case 80:
YY_RULE_SETUP
#line 194 "fortran.lex"
{ return TOK_PRINT; }
	YY_BREAK
case 81:
YY_RULE_SETUP
#line 195 "fortran.lex"
{ return TOK_PROCEDURE; }
	YY_BREAK
case 82:
YY_RULE_SETUP
#line 196 "fortran.lex"
{ in_io_control_spec = 1; return TOK_READ_PAR; }
	YY_BREAK
case 83:
YY_RULE_SETUP
#line 197 "fortran.lex"
{ return TOK_READ; }
	YY_BREAK
case 84:
YY_RULE_SETUP
#line 198 "fortran.lex"
{ return TOK_NAMELIST; }
	YY_BREAK
case 85:
YY_RULE_SETUP
#line 199 "fortran.lex"
{ in_io_control_spec = 1; return TOK_WRITE_PAR; }
	YY_BREAK
case 86:
YY_RULE_SETUP
#line 200 "fortran.lex"
{ return TOK_WRITE; }
	YY_BREAK
case 87:
YY_RULE_SETUP
#line 201 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_FLUSH; }
	YY_BREAK
case 88:
YY_RULE_SETUP
#line 202 "fortran.lex"
{ return TOK_TARGET; }
	YY_BREAK
case 89:
YY_RULE_SETUP
#line 203 "fortran.lex"
{ return TOK_PUBLIC; }
	YY_BREAK
case 90:
YY_RULE_SETUP
#line 204 "fortran.lex"
{ return TOK_PRIVATE; }
	YY_BREAK
case 91:
YY_RULE_SETUP
#line 205 "fortran.lex"
{ strcpy(yylval.na,fortran_text);
                               if (intent_spec==1)
                                {return TOK_IN; }
                              else
                              {
                              return TOK_NAME;
                              }
                            }
	YY_BREAK
case 92:
YY_RULE_SETUP
#line 213 "fortran.lex"
{ pos_curdata = setposcur()-strlen(fortran_text); /*Init_List_Data_Var();*/ return TOK_DATA; }
	YY_BREAK
case 93:
YY_RULE_SETUP
#line 214 "fortran.lex"
{ return TOK_PLAINGOTO; }
	YY_BREAK
case 94:
YY_RULE_SETUP
#line 215 "fortran.lex"
{ strcpy(yylval.na,fortran_text);
                               if (intent_spec==1)
                                {return TOK_OUT; }
                              else
                              {
                              return TOK_NAME;
                              }
                            }
	YY_BREAK
case 95:
YY_RULE_SETUP
#line 223 "fortran.lex"
{ strcpy(yylval.na,fortran_text);
                               if (intent_spec==1)
                                {return TOK_IN; }
                              else
                              {
                              return TOK_INOUT;
                              }
                            }
	YY_BREAK
case 96:
YY_RULE_SETUP
#line 231 "fortran.lex"
{ return TOK_INTRINSIC; }
	YY_BREAK
case 97:
YY_RULE_SETUP
#line 232 "fortran.lex"
{ return TOK_THEN; }
	YY_BREAK
case 98:
YY_RULE_SETUP
#line 233 "fortran.lex"
{ return TOK_ELSEIF; }
	YY_BREAK
case 99:
YY_RULE_SETUP
#line 234 "fortran.lex"
{ return TOK_ELSE; }
	YY_BREAK
case 100:
YY_RULE_SETUP
#line 235 "fortran.lex"
{ return TOK_ENDIF; }
	YY_BREAK
case 101:
YY_RULE_SETUP
#line 236 "fortran.lex"
{strcpy(yylval.na,fortran_text);
                            return TOK_LOGICALIF_PAR;
                            }
	YY_BREAK
case 102:
/* rule 102 can match eol */
*yy_cp = (yy_hold_char); /* undo effects of setting up fortran_text */
(yy_c_buf_p) = yy_cp = yy_bp + 2;
YY_DO_BEFORE_ACTION; /* set up fortran_text again */
YY_RULE_SETUP
#line 239 "fortran.lex"
{strcpy(yylval.na,fortran_text);
                            return TOK_NAME;
                            }
	YY_BREAK
case 103:
YY_RULE_SETUP
#line 242 "fortran.lex"
{strcpy(yylval.na,fortran_text);
                            return TOK_LOGICALIF_PAR;
                            }
	YY_BREAK
case 104:
YY_RULE_SETUP
#line 245 "fortran.lex"
{ return TOK_SELECTCASE; }
	YY_BREAK
case 105:
YY_RULE_SETUP
#line 246 "fortran.lex"
{ if (in_select_case_stmt > 0) return TOK_CASE ; else return TOK_NAME;}
	YY_BREAK
case 106:
YY_RULE_SETUP
#line 247 "fortran.lex"
{ return TOK_DEFAULT; }
	YY_BREAK
case 107:
YY_RULE_SETUP
#line 248 "fortran.lex"
{ return TOK_ENDSELECT; }
	YY_BREAK
case 108:
YY_RULE_SETUP
#line 249 "fortran.lex"
{ return TOK_FILE; }
	YY_BREAK
case 109:
YY_RULE_SETUP
#line 250 "fortran.lex"
{ return TOK_ACCESS; }
	YY_BREAK
case 110:
YY_RULE_SETUP
#line 251 "fortran.lex"
{ return TOK_ACTION; }
	YY_BREAK
case 111:
YY_RULE_SETUP
#line 252 "fortran.lex"
{ return TOK_IOLENGTH; }
	YY_BREAK
case 112:
YY_RULE_SETUP
#line 253 "fortran.lex"
{ return TOK_UNIT; }
	YY_BREAK
case 113:
YY_RULE_SETUP
#line 254 "fortran.lex"
{ return TOK_OPENED; }
	YY_BREAK
case 114:
YY_RULE_SETUP
#line 255 "fortran.lex"
{ return TOK_FMT; }
	YY_BREAK
case 115:
YY_RULE_SETUP
#line 256 "fortran.lex"
{ return TOK_NML; }
	YY_BREAK
case 116:
YY_RULE_SETUP
#line 257 "fortran.lex"
{ return TOK_END; }
	YY_BREAK
case 117:
YY_RULE_SETUP
#line 258 "fortran.lex"
{ return TOK_EOR; }
	YY_BREAK
case 118:
*yy_cp = (yy_hold_char); /* undo effects of setting up fortran_text */
(yy_c_buf_p) = yy_cp = yy_bp + 3;
YY_DO_BEFORE_ACTION; /* set up fortran_text again */
YY_RULE_SETUP
#line 259 "fortran.lex"
{
                            if (in_char_selector ==1)
                               return TOK_LEN;
                            else
                            {
                            strcpy(yylval.na,fortran_text); return TOK_NAME;
                            }
                            }
	YY_BREAK
case 119:
*yy_cp = (yy_hold_char); /* undo effects of setting up fortran_text */
(yy_c_buf_p) = yy_cp = yy_bp + 4;
YY_DO_BEFORE_ACTION; /* set up fortran_text again */
YY_RULE_SETUP
#line 267 "fortran.lex"
{
                            if ((in_char_selector==1) || (in_kind_selector == 1))
                               return TOK_KIND;
                            else
                            {
                            strcpy(yylval.na,fortran_text); return TOK_NAME;
                            }
                            }
	YY_BREAK
case 120:
YY_RULE_SETUP
#line 275 "fortran.lex"
{ return TOK_ERRMSG; }
	YY_BREAK
case 121:
YY_RULE_SETUP
#line 276 "fortran.lex"
{ return TOK_MOLD; }
	YY_BREAK
case 122:
YY_RULE_SETUP
#line 277 "fortran.lex"
{ return TOK_SOURCE; }
	YY_BREAK
case 123:
YY_RULE_SETUP
#line 278 "fortran.lex"
{ return TOK_POSITION; }
	YY_BREAK
case 124:
YY_RULE_SETUP
#line 279 "fortran.lex"
{ return TOK_IOMSG; }
	YY_BREAK
case 125:
YY_RULE_SETUP
#line 280 "fortran.lex"
{ return TOK_IOSTAT; }
	YY_BREAK
case 126:
YY_RULE_SETUP
#line 281 "fortran.lex"
{ return TOK_ERR; }
	YY_BREAK
case 127:
YY_RULE_SETUP
#line 282 "fortran.lex"
{ return TOK_FORM; }
	YY_BREAK
case 128:
*yy_cp = (yy_hold_char); /* undo effects of setting up fortran_text */
(yy_c_buf_p) = yy_cp = yy_bp + 4;
YY_DO_BEFORE_ACTION; /* set up fortran_text again */
YY_RULE_SETUP
#line 283 "fortran.lex"
{
                            if (in_inquire==1)
                               return TOK_NAME_EQ;
                            else
                            {
                            strcpy(yylval.na,fortran_text); return TOK_NAME;
                            }
                            }
	YY_BREAK
case 129:
YY_RULE_SETUP
#line 291 "fortran.lex"
{ return TOK_RECL; }
	YY_BREAK
case 130:
*yy_cp = (yy_hold_char); /* undo effects of setting up fortran_text */
(yy_c_buf_p) = yy_cp = yy_bp + 3;
YY_DO_BEFORE_ACTION; /* set up fortran_text again */
YY_RULE_SETUP
#line 292 "fortran.lex"
{ if (in_io_control_spec == 1)
                              return TOK_REC;
                             else
                             {
                             strcpy(yylval.na,fortran_text); return TOK_NAME;
                             }
                             }
	YY_BREAK
case 131:
*yy_cp = (yy_hold_char); /* undo effects of setting up fortran_text */
(yy_c_buf_p) = yy_cp = yy_bp + 6;
YY_DO_BEFORE_ACTION; /* set up fortran_text again */
YY_RULE_SETUP
#line 299 "fortran.lex"
{ if (close_or_connect == 1)
                              return TOK_STATUS;
                             else
                             {
                             strcpy(yylval.na,fortran_text); return TOK_NAME;
                             }
                             }
	YY_BREAK
case 132:
YY_RULE_SETUP
#line 306 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_NAME;}
	YY_BREAK
case 133:
YY_RULE_SETUP
#line 307 "fortran.lex"
{ return TOK_EXIST; }
	YY_BREAK
case 134:
YY_RULE_SETUP
#line 308 "fortran.lex"
{ return TOK_CYCLE; }
	YY_BREAK
case 135:
YY_RULE_SETUP
#line 309 "fortran.lex"
{ return TOK_BACKSPACE; }
	YY_BREAK
case 136:
YY_RULE_SETUP
#line 310 "fortran.lex"
{ return TOK_FOURDOTS;  }
	YY_BREAK
case 137:
/* rule 137 can match eol */
YY_RULE_SETUP
#line 311 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_DSLASH; }
	YY_BREAK
case 138:
YY_RULE_SETUP
#line 312 "fortran.lex"
{ return TOK_LEFTAB; }
	YY_BREAK
case 139:
YY_RULE_SETUP
#line 313 "fortran.lex"
{ return TOK_RIGHTAB; }
	YY_BREAK
case 140:
YY_RULE_SETUP
#line 314 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_SLASH; }
	YY_BREAK
case 141:
/* rule 141 can match eol */
YY_RULE_SETUP
#line 315 "fortran.lex"
{
                              INCREMENT_LINE_NUM() ; strcpy(yylval.na,fortran_text); return TOK_CHAR_CUT; }
	YY_BREAK
case 142:
/* rule 142 can match eol */
YY_RULE_SETUP
#line 317 "fortran.lex"
{Add_Include_1(fortran_text);}
	YY_BREAK
case 143:
YY_RULE_SETUP
#line 318 "fortran.lex"
{}
	YY_BREAK
case 144:
/* rule 144 can match eol */
YY_RULE_SETUP
#line 319 "fortran.lex"
{
                  if (inmoduledeclare == 0 )
                  {
                  pos_end=setposcur();
                  RemoveWordSET_0(fortran_out,pos_curinclude,pos_end-pos_curinclude);
                  }
                  out_of_donottreat();
                  }
	YY_BREAK
case 145:
/* rule 145 can match eol */
YY_RULE_SETUP
#line 327 "fortran.lex"
{ strcpy(yylval.na,fortran_text);return TOK_CHAR_CONSTANT; }
	YY_BREAK
case 146:
/* rule 146 can match eol */
YY_RULE_SETUP
#line 328 "fortran.lex"
{ strcpy(yylval.na,fortran_text);return TOK_CHAR_MESSAGE; }
	YY_BREAK
case 147:
YY_RULE_SETUP
#line 329 "fortran.lex"
{ BEGIN(donottreat_interface); }
	YY_BREAK
case 148:
/* rule 148 can match eol */
YY_RULE_SETUP
#line 330 "fortran.lex"
{ out_of_donottreat(); return '\n'; }
	YY_BREAK
case 149:
/* rule 149 can match eol */
YY_RULE_SETUP
#line 331 "fortran.lex"
{INCREMENT_LINE_NUM() ; }
	YY_BREAK
case 150:
/* rule 150 can match eol */
YY_RULE_SETUP
#line 332 "fortran.lex"
{strcpy(yylval.na,fortran_text); removenewline(yylval.na);
                            return TOK_NAME; }
	YY_BREAK
case 151:
YY_RULE_SETUP
#line 334 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_NAME; }
	YY_BREAK
case 152:
YY_RULE_SETUP
#line 335 "fortran.lex"
{strcpy(yylval.na,fortran_text); return TOK_CSTREAL; }
	YY_BREAK
case 153:
/* rule 153 can match eol */
*yy_cp = (yy_hold_char); /* undo effects of setting up fortran_text */
(yy_c_buf_p) = yy_cp -= 1;
YY_DO_BEFORE_ACTION; /* set up fortran_text again */
YY_RULE_SETUP
#line 336 "fortran.lex"
{  // REAL1
                              strcpy(yylval.na,fortran_text); return TOK_CSTREAL; }
	YY_BREAK
case 154:
YY_RULE_SETUP
#line 338 "fortran.lex"
{  // REAL2
                              strcpy(yylval.na,fortran_text); return TOK_CSTREAL; }
	YY_BREAK
case 155:
YY_RULE_SETUP
#line 340 "fortran.lex"
{ strcpy(yylval.na,fortran_text);
                             if (lastwasendofstmt == 0)
                              return TOK_CSTINT;
                             else
                              if (testandextractfromlist(&List_Do_labels,fortran_text) == 1)
                              {
                              removefromlist(&List_Do_labels,yylval.na);
                              return TOK_LABEL_DJVIEW;
                              }
                              else
                              {
                              return TOK_LABEL;
                              }
                             }
	YY_BREAK
case 156:
YY_RULE_SETUP
#line 354 "fortran.lex"
{}
	YY_BREAK
case 157:
YY_RULE_SETUP
#line 355 "fortran.lex"
{}
	YY_BREAK
case 158:
*yy_cp = (yy_hold_char); /* undo effects of setting up fortran_text */
(yy_c_buf_p) = yy_cp = yy_bp + 1;
YY_DO_BEFORE_ACTION; /* set up fortran_text again */
YY_RULE_SETUP
#line 356 "fortran.lex"
{
                            in_complex_literal = -1;
                            return (int) *fortran_text;
                            }
	YY_BREAK
case 159:
YY_RULE_SETUP
#line 360 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return (int) *fortran_text; }
	YY_BREAK
case 160:
YY_RULE_SETUP
#line 361 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return (int) *fortran_text; }
	YY_BREAK
case 161:
YY_RULE_SETUP
#line 362 "fortran.lex"
{ lastwasendofstmt=1; token_since_endofstmt = 0; return TOK_SEMICOLON; }
	YY_BREAK
case 162:
YY_RULE_SETUP
#line 363 "fortran.lex"
{ if (in_complex_literal==-1) {return TOK_COMMACOMPLEX; in_complex_literal=0;} else; return (int) *fortran_text; }
	YY_BREAK
case 163:
YY_RULE_SETUP
#line 364 "fortran.lex"
{ return (int) *fortran_text; }
	YY_BREAK
case 164:
YY_RULE_SETUP
#line 365 "fortran.lex"
{ return (int) *fortran_text; }
	YY_BREAK
case 165:
YY_RULE_SETUP
#line 366 "fortran.lex"
{ return (int) *fortran_text; }
	YY_BREAK
case 166:
/* rule 166 can match eol */
YY_RULE_SETUP
#line 367 "fortran.lex"
{ INCREMENT_LINE_NUM() ; lastwasendofstmt=1; token_since_endofstmt = 0; increment_nbtokens = 0; return '\n'; }
	YY_BREAK
case 167:
YY_RULE_SETUP
#line 368 "fortran.lex"
{increment_nbtokens = 0;}
	YY_BREAK
case 168:
/* rule 168 can match eol */
YY_RULE_SETUP
#line 369 "fortran.lex"
{
                              return TOK_LABEL_FORMAT; }
	YY_BREAK
case 169:
/* rule 169 can match eol */
YY_RULE_SETUP
#line 371 "fortran.lex"
{return TOK_LABEL_FORMAT; }
	YY_BREAK
case 170:
/* rule 170 can match eol */
YY_RULE_SETUP
#line 372 "fortran.lex"
{ INCREMENT_LINE_NUM() ; newlinef90=1; }
	YY_BREAK
case 171:
/* rule 171 can match eol */
YY_RULE_SETUP
#line 373 "fortran.lex"
{ INCREMENT_LINE_NUM() ;}
	YY_BREAK
case 172:
/* rule 172 can match eol */
YY_RULE_SETUP
#line 375 "fortran.lex"
{INCREMENT_LINE_NUM() ; BEGIN(donottreat); }
	YY_BREAK
case 173:
/* rule 173 can match eol */
YY_RULE_SETUP
#line 376 "fortran.lex"
{out_of_donottreat(); return '\n'; }
	YY_BREAK
case 174:
/* rule 174 can match eol */
YY_RULE_SETUP
#line 377 "fortran.lex"
{INCREMENT_LINE_NUM() ; }
	YY_BREAK
case 175:
/* rule 175 can match eol */
YY_RULE_SETUP
#line 378 "fortran.lex"
{INCREMENT_LINE_NUM() ; increment_nbtokens = 0;}
	YY_BREAK
case 176:
/* rule 176 can match eol */
YY_RULE_SETUP
#line 379 "fortran.lex"
{INCREMENT_LINE_NUM() ; increment_nbtokens = 0;}
	YY_BREAK
case 177:
YY_RULE_SETUP
#line 380 "fortran.lex"
{increment_nbtokens = 0;}
	YY_BREAK
case YY_STATE_EOF(INITIAL):
case YY_STATE_EOF(parameter):
case YY_STATE_EOF(character):
case YY_STATE_EOF(donottreat):
case YY_STATE_EOF(donottreat_interface):
case YY_STATE_EOF(includestate):
case YY_STATE_EOF(fortran77style):
case YY_STATE_EOF(fortran90style):
#line 381 "fortran.lex"
{endoffile = 1; yyterminate();}
	YY_BREAK
case 178:
YY_RULE_SETUP
#line 382 "fortran.lex"
ECHO;
	YY_BREAK
#line 4881 "fortran.yy.c"

	case YY_END_OF_BUFFER:
		{
		/* Amount of text matched not including the EOB char. */
		int yy_amount_of_matched_text = (int) (yy_cp - (yytext_ptr)) - 1;

		/* Undo the effects of YY_DO_BEFORE_ACTION. */
		*yy_cp = (yy_hold_char);
		YY_RESTORE_YY_MORE_OFFSET

		if ( YY_CURRENT_BUFFER_LVALUE->yy_buffer_status == YY_BUFFER_NEW )
			{
			/* We're scanning a new file or input source.  It's
			 * possible that this happened because the user
			 * just pointed fortran_in at a new source and called
			 * fortran_lex().  If so, then we have to assure
			 * consistency between YY_CURRENT_BUFFER and our
			 * globals.  Here is the right place to do so, because
			 * this is the first action (other than possibly a
			 * back-up) that will match for the new input source.
			 */
			(yy_n_chars) = YY_CURRENT_BUFFER_LVALUE->yy_n_chars;
			YY_CURRENT_BUFFER_LVALUE->yy_input_file = fortran_in;
			YY_CURRENT_BUFFER_LVALUE->yy_buffer_status = YY_BUFFER_NORMAL;
			}

		/* Note that here we test for yy_c_buf_p "<=" to the position
		 * of the first EOB in the buffer, since yy_c_buf_p will
		 * already have been incremented past the NUL character
		 * (since all states make transitions on EOB to the
		 * end-of-buffer state).  Contrast this with the test
		 * in input().
		 */
		if ( (yy_c_buf_p) <= &YY_CURRENT_BUFFER_LVALUE->yy_ch_buf[(yy_n_chars)] )
			{ /* This was really a NUL. */
			yy_state_type yy_next_state;

			(yy_c_buf_p) = (yytext_ptr) + yy_amount_of_matched_text;

			yy_current_state = yy_get_previous_state(  );

			/* Okay, we're now positioned to make the NUL
			 * transition.  We couldn't have
			 * yy_get_previous_state() go ahead and do it
			 * for us because it doesn't know how to deal
			 * with the possibility of jamming (and we don't
			 * want to build jamming into it because then it
			 * will run more slowly).
			 */

			yy_next_state = yy_try_NUL_trans( yy_current_state );

			yy_bp = (yytext_ptr) + YY_MORE_ADJ;

			if ( yy_next_state )
				{
				/* Consume the NUL. */
				yy_cp = ++(yy_c_buf_p);
				yy_current_state = yy_next_state;
				goto yy_match;
				}

			else
				{
				yy_cp = (yy_c_buf_p);
				goto yy_find_action;
				}
			}

		else switch ( yy_get_next_buffer(  ) )
			{
			case EOB_ACT_END_OF_FILE:
				{
				(yy_did_buffer_switch_on_eof) = 0;

				if ( fortran_wrap( ) )
					{
					/* Note: because we've taken care in
					 * yy_get_next_buffer() to have set up
					 * fortran_text, we can now set up
					 * yy_c_buf_p so that if some total
					 * hoser (like flex itself) wants to
					 * call the scanner after we return the
					 * YY_NULL, it'll still work - another
					 * YY_NULL will get returned.
					 */
					(yy_c_buf_p) = (yytext_ptr) + YY_MORE_ADJ;

					yy_act = YY_STATE_EOF(YY_START);
					goto do_action;
					}

				else
					{
					if ( ! (yy_did_buffer_switch_on_eof) )
						YY_NEW_FILE;
					}
				break;
				}

			case EOB_ACT_CONTINUE_SCAN:
				(yy_c_buf_p) =
					(yytext_ptr) + yy_amount_of_matched_text;

				yy_current_state = yy_get_previous_state(  );

				yy_cp = (yy_c_buf_p);
				yy_bp = (yytext_ptr) + YY_MORE_ADJ;
				goto yy_match;

			case EOB_ACT_LAST_MATCH:
				(yy_c_buf_p) =
				&YY_CURRENT_BUFFER_LVALUE->yy_ch_buf[(yy_n_chars)];

				yy_current_state = yy_get_previous_state(  );

				yy_cp = (yy_c_buf_p);
				yy_bp = (yytext_ptr) + YY_MORE_ADJ;
				goto yy_find_action;
			}
		break;
		}

	default:
		YY_FATAL_ERROR(
			"fatal flex scanner internal error--no action found" );
	} /* end of action switch */
		} /* end of scanning one token */
} /* end of fortran_lex */

/* yy_get_next_buffer - try to read in a new buffer
 *
 * Returns a code representing an action:
 *	EOB_ACT_LAST_MATCH -
 *	EOB_ACT_CONTINUE_SCAN - continue scanning from current position
 *	EOB_ACT_END_OF_FILE - end of file
 */
static int yy_get_next_buffer (void)
{
    	register char *dest = YY_CURRENT_BUFFER_LVALUE->yy_ch_buf;
	register char *source = (yytext_ptr);
	register int number_to_move, i;
	int ret_val;

	if ( (yy_c_buf_p) > &YY_CURRENT_BUFFER_LVALUE->yy_ch_buf[(yy_n_chars) + 1] )
		YY_FATAL_ERROR(
		"fatal flex scanner internal error--end of buffer missed" );

	if ( YY_CURRENT_BUFFER_LVALUE->yy_fill_buffer == 0 )
		{ /* Don't try to fill the buffer, so this is an EOF. */
		if ( (yy_c_buf_p) - (yytext_ptr) - YY_MORE_ADJ == 1 )
			{
			/* We matched a single character, the EOB, so
			 * treat this as a final EOF.
			 */
			return EOB_ACT_END_OF_FILE;
			}

		else
			{
			/* We matched some text prior to the EOB, first
			 * process it.
			 */
			return EOB_ACT_LAST_MATCH;
			}
		}

	/* Try to read more data. */

	/* First move last chars to start of buffer. */
	number_to_move = (int) ((yy_c_buf_p) - (yytext_ptr)) - 1;

	for ( i = 0; i < number_to_move; ++i )
		*(dest++) = *(source++);

	if ( YY_CURRENT_BUFFER_LVALUE->yy_buffer_status == YY_BUFFER_EOF_PENDING )
		/* don't do the read, it's not guaranteed to return an EOF,
		 * just force an EOF
		 */
		YY_CURRENT_BUFFER_LVALUE->yy_n_chars = (yy_n_chars) = 0;

	else
		{
			yy_size_t num_to_read =
			YY_CURRENT_BUFFER_LVALUE->yy_buf_size - number_to_move - 1;

		while ( num_to_read <= 0 )
			{ /* Not enough room in the buffer - grow it. */

			YY_FATAL_ERROR(
"input buffer overflow, can't enlarge buffer because scanner uses REJECT" );

			}

		if ( num_to_read > YY_READ_BUF_SIZE )
			num_to_read = YY_READ_BUF_SIZE;

		/* Read in more data. */
		YY_INPUT( (&YY_CURRENT_BUFFER_LVALUE->yy_ch_buf[number_to_move]),
			(yy_n_chars), num_to_read );

		YY_CURRENT_BUFFER_LVALUE->yy_n_chars = (yy_n_chars);
		}

	if ( (yy_n_chars) == 0 )
		{
		if ( number_to_move == YY_MORE_ADJ )
			{
			ret_val = EOB_ACT_END_OF_FILE;
			fortran_restart(fortran_in  );
			}

		else
			{
			ret_val = EOB_ACT_LAST_MATCH;
			YY_CURRENT_BUFFER_LVALUE->yy_buffer_status =
				YY_BUFFER_EOF_PENDING;
			}
		}

	else
		ret_val = EOB_ACT_CONTINUE_SCAN;

	if ((yy_size_t) ((yy_n_chars) + number_to_move) > YY_CURRENT_BUFFER_LVALUE->yy_buf_size) {
		/* Extend the array by 50%, plus the number we really need. */
		yy_size_t new_size = (yy_n_chars) + number_to_move + ((yy_n_chars) >> 1);
		YY_CURRENT_BUFFER_LVALUE->yy_ch_buf = (char *) fortran_realloc((void *) YY_CURRENT_BUFFER_LVALUE->yy_ch_buf,new_size  );
		if ( ! YY_CURRENT_BUFFER_LVALUE->yy_ch_buf )
			YY_FATAL_ERROR( "out of dynamic memory in yy_get_next_buffer()" );
	}

	(yy_n_chars) += number_to_move;
	YY_CURRENT_BUFFER_LVALUE->yy_ch_buf[(yy_n_chars)] = YY_END_OF_BUFFER_CHAR;
	YY_CURRENT_BUFFER_LVALUE->yy_ch_buf[(yy_n_chars) + 1] = YY_END_OF_BUFFER_CHAR;

	(yytext_ptr) = &YY_CURRENT_BUFFER_LVALUE->yy_ch_buf[0];

	return ret_val;
}

/* yy_get_previous_state - get the state just before the EOB char was reached */

    static yy_state_type yy_get_previous_state (void)
{
	register yy_state_type yy_current_state;
	register char *yy_cp;
    
	yy_current_state = (yy_start);
	yy_current_state += YY_AT_BOL();

	(yy_state_ptr) = (yy_state_buf);
	*(yy_state_ptr)++ = yy_current_state;

	for ( yy_cp = (yytext_ptr) + YY_MORE_ADJ; yy_cp < (yy_c_buf_p); ++yy_cp )
		{
		register YY_CHAR yy_c = (*yy_cp ? yy_ec[YY_SC_TO_UI(*yy_cp)] : 1);
		while ( yy_chk[yy_base[yy_current_state] + yy_c] != yy_current_state )
			{
			yy_current_state = (int) yy_def[yy_current_state];
			if ( yy_current_state >= 1883 )
				yy_c = yy_meta[(unsigned int) yy_c];
			}
		yy_current_state = yy_nxt[yy_base[yy_current_state] + (unsigned int) yy_c];
		*(yy_state_ptr)++ = yy_current_state;
		}

	return yy_current_state;
}

/* yy_try_NUL_trans - try to make a transition on the NUL character
 *
 * synopsis
 *	next_state = yy_try_NUL_trans( current_state );
 */
    static yy_state_type yy_try_NUL_trans  (yy_state_type yy_current_state )
{
	register int yy_is_jam;
    
	register YY_CHAR yy_c = 1;
	while ( yy_chk[yy_base[yy_current_state] + yy_c] != yy_current_state )
		{
		yy_current_state = (int) yy_def[yy_current_state];
		if ( yy_current_state >= 1883 )
			yy_c = yy_meta[(unsigned int) yy_c];
		}
	yy_current_state = yy_nxt[yy_base[yy_current_state] + (unsigned int) yy_c];
	yy_is_jam = (yy_current_state == 1882);
	if ( ! yy_is_jam )
		*(yy_state_ptr)++ = yy_current_state;

	return yy_is_jam ? 0 : yy_current_state;
}

    static void yyunput (int c, register char * yy_bp )
{
	register char *yy_cp;
    
    yy_cp = (yy_c_buf_p);

	/* undo effects of setting up fortran_text */
	*yy_cp = (yy_hold_char);

	if ( yy_cp < YY_CURRENT_BUFFER_LVALUE->yy_ch_buf + 2 )
		{ /* need to shift things up to make room */
		/* +2 for EOB chars. */
		register yy_size_t number_to_move = (yy_n_chars) + 2;
		register char *dest = &YY_CURRENT_BUFFER_LVALUE->yy_ch_buf[
					YY_CURRENT_BUFFER_LVALUE->yy_buf_size + 2];
		register char *source =
				&YY_CURRENT_BUFFER_LVALUE->yy_ch_buf[number_to_move];

		while ( source > YY_CURRENT_BUFFER_LVALUE->yy_ch_buf )
			*--dest = *--source;

		yy_cp += (int) (dest - source);
		yy_bp += (int) (dest - source);
		YY_CURRENT_BUFFER_LVALUE->yy_n_chars =
			(yy_n_chars) = YY_CURRENT_BUFFER_LVALUE->yy_buf_size;

		if ( yy_cp < YY_CURRENT_BUFFER_LVALUE->yy_ch_buf + 2 )
			YY_FATAL_ERROR( "flex scanner push-back overflow" );
		}

	*--yy_cp = (char) c;

	(yytext_ptr) = yy_bp;
	(yy_hold_char) = *yy_cp;
	(yy_c_buf_p) = yy_cp;
}

#ifndef YY_NO_INPUT
#ifdef __cplusplus
    static int yyinput (void)
#else
    static int input  (void)
#endif

{
	int c;
    
	*(yy_c_buf_p) = (yy_hold_char);

	if ( *(yy_c_buf_p) == YY_END_OF_BUFFER_CHAR )
		{
		/* yy_c_buf_p now points to the character we want to return.
		 * If this occurs *before* the EOB characters, then it's a
		 * valid NUL; if not, then we've hit the end of the buffer.
		 */
		if ( (yy_c_buf_p) < &YY_CURRENT_BUFFER_LVALUE->yy_ch_buf[(yy_n_chars)] )
			/* This was really a NUL. */
			*(yy_c_buf_p) = '\0';

		else
			{ /* need more input */
			yy_size_t offset = (yy_c_buf_p) - (yytext_ptr);
			++(yy_c_buf_p);

			switch ( yy_get_next_buffer(  ) )
				{
				case EOB_ACT_LAST_MATCH:
					/* This happens because yy_g_n_b()
					 * sees that we've accumulated a
					 * token and flags that we need to
					 * try matching the token before
					 * proceeding.  But for input(),
					 * there's no matching to consider.
					 * So convert the EOB_ACT_LAST_MATCH
					 * to EOB_ACT_END_OF_FILE.
					 */

					/* Reset buffer status. */
					fortran_restart(fortran_in );

					/*FALLTHROUGH*/

				case EOB_ACT_END_OF_FILE:
					{
					if ( fortran_wrap( ) )
						return 0;

					if ( ! (yy_did_buffer_switch_on_eof) )
						YY_NEW_FILE;
#ifdef __cplusplus
					return yyinput();
#else
					return input();
#endif
					}

				case EOB_ACT_CONTINUE_SCAN:
					(yy_c_buf_p) = (yytext_ptr) + offset;
					break;
				}
			}
		}

	c = *(unsigned char *) (yy_c_buf_p);	/* cast for 8-bit char's */
	*(yy_c_buf_p) = '\0';	/* preserve fortran_text */
	(yy_hold_char) = *++(yy_c_buf_p);

	YY_CURRENT_BUFFER_LVALUE->yy_at_bol = (c == '\n');

	return c;
}
#endif	/* ifndef YY_NO_INPUT */

/** Immediately switch to a different input stream.
 * @param input_file A readable stream.
 * 
 * @note This function does not reset the start condition to @c INITIAL .
 */
    void fortran_restart  (FILE * input_file )
{
    
	if ( ! YY_CURRENT_BUFFER ){
        fortran_ensure_buffer_stack ();
		YY_CURRENT_BUFFER_LVALUE =
            fortran__create_buffer(fortran_in,YY_BUF_SIZE );
	}

	fortran__init_buffer(YY_CURRENT_BUFFER,input_file );
	fortran__load_buffer_state( );
}

/** Switch to a different input buffer.
 * @param new_buffer The new input buffer.
 * 
 */
    void fortran__switch_to_buffer  (YY_BUFFER_STATE  new_buffer )
{
    
	/* TODO. We should be able to replace this entire function body
	 * with
	 *		fortran_pop_buffer_state();
	 *		fortran_push_buffer_state(new_buffer);
     */
	fortran_ensure_buffer_stack ();
	if ( YY_CURRENT_BUFFER == new_buffer )
		return;

	if ( YY_CURRENT_BUFFER )
		{
		/* Flush out information for old buffer. */
		*(yy_c_buf_p) = (yy_hold_char);
		YY_CURRENT_BUFFER_LVALUE->yy_buf_pos = (yy_c_buf_p);
		YY_CURRENT_BUFFER_LVALUE->yy_n_chars = (yy_n_chars);
		}

	YY_CURRENT_BUFFER_LVALUE = new_buffer;
	fortran__load_buffer_state( );

	/* We don't actually know whether we did this switch during
	 * EOF (fortran_wrap()) processing, but the only time this flag
	 * is looked at is after fortran_wrap() is called, so it's safe
	 * to go ahead and always set it.
	 */
	(yy_did_buffer_switch_on_eof) = 1;
}

static void fortran__load_buffer_state  (void)
{
    	(yy_n_chars) = YY_CURRENT_BUFFER_LVALUE->yy_n_chars;
	(yytext_ptr) = (yy_c_buf_p) = YY_CURRENT_BUFFER_LVALUE->yy_buf_pos;
	fortran_in = YY_CURRENT_BUFFER_LVALUE->yy_input_file;
	(yy_hold_char) = *(yy_c_buf_p);
}

/** Allocate and initialize an input buffer state.
 * @param file A readable stream.
 * @param size The character buffer size in bytes. When in doubt, use @c YY_BUF_SIZE.
 * 
 * @return the allocated buffer state.
 */
    YY_BUFFER_STATE fortran__create_buffer  (FILE * file, int  size )
{
	YY_BUFFER_STATE b;
    
	b = (YY_BUFFER_STATE) fortran_alloc(sizeof( struct yy_buffer_state )  );
	if ( ! b )
		YY_FATAL_ERROR( "out of dynamic memory in fortran__create_buffer()" );

	b->yy_buf_size = size;

	/* yy_ch_buf has to be 2 characters longer than the size given because
	 * we need to put in 2 end-of-buffer characters.
	 */
	b->yy_ch_buf = (char *) fortran_alloc(b->yy_buf_size + 2  );
	if ( ! b->yy_ch_buf )
		YY_FATAL_ERROR( "out of dynamic memory in fortran__create_buffer()" );

	b->yy_is_our_buffer = 1;

	fortran__init_buffer(b,file );

	return b;
}

/** Destroy the buffer.
 * @param b a buffer created with fortran__create_buffer()
 * 
 */
    void fortran__delete_buffer (YY_BUFFER_STATE  b )
{
    
	if ( ! b )
		return;

	if ( b == YY_CURRENT_BUFFER ) /* Not sure if we should pop here. */
		YY_CURRENT_BUFFER_LVALUE = (YY_BUFFER_STATE) 0;

	if ( b->yy_is_our_buffer )
		fortran_free((void *) b->yy_ch_buf  );

	fortran_free((void *) b  );
}

#ifndef __cplusplus
extern int isatty (int );
#endif /* __cplusplus */
    
/* Initializes or reinitializes a buffer.
 * This function is sometimes called more than once on the same buffer,
 * such as during a fortran_restart() or at EOF.
 */
    static void fortran__init_buffer  (YY_BUFFER_STATE  b, FILE * file )

{
	int oerrno = errno;
    
	fortran__flush_buffer(b );

	b->yy_input_file = file;
	b->yy_fill_buffer = 1;

    /* If b is the current buffer, then fortran__init_buffer was _probably_
     * called from fortran_restart() or through yy_get_next_buffer.
     * In that case, we don't want to reset the lineno or column.
     */
    if (b != YY_CURRENT_BUFFER){
        b->yy_bs_lineno = 1;
        b->yy_bs_column = 0;
    }

        b->yy_is_interactive = file ? (isatty( fileno(file) ) > 0) : 0;
    
	errno = oerrno;
}

/** Discard all buffered characters. On the next scan, YY_INPUT will be called.
 * @param b the buffer state to be flushed, usually @c YY_CURRENT_BUFFER.
 * 
 */
    void fortran__flush_buffer (YY_BUFFER_STATE  b )
{
    	if ( ! b )
		return;

	b->yy_n_chars = 0;

	/* We always need two end-of-buffer characters.  The first causes
	 * a transition to the end-of-buffer state.  The second causes
	 * a jam in that state.
	 */
	b->yy_ch_buf[0] = YY_END_OF_BUFFER_CHAR;
	b->yy_ch_buf[1] = YY_END_OF_BUFFER_CHAR;

	b->yy_buf_pos = &b->yy_ch_buf[0];

	b->yy_at_bol = 1;
	b->yy_buffer_status = YY_BUFFER_NEW;

	if ( b == YY_CURRENT_BUFFER )
		fortran__load_buffer_state( );
}

/** Pushes the new state onto the stack. The new state becomes
 *  the current state. This function will allocate the stack
 *  if necessary.
 *  @param new_buffer The new state.
 *  
 */
void fortran_push_buffer_state (YY_BUFFER_STATE new_buffer )
{
    	if (new_buffer == NULL)
		return;

	fortran_ensure_buffer_stack();

	/* This block is copied from fortran__switch_to_buffer. */
	if ( YY_CURRENT_BUFFER )
		{
		/* Flush out information for old buffer. */
		*(yy_c_buf_p) = (yy_hold_char);
		YY_CURRENT_BUFFER_LVALUE->yy_buf_pos = (yy_c_buf_p);
		YY_CURRENT_BUFFER_LVALUE->yy_n_chars = (yy_n_chars);
		}

	/* Only push if top exists. Otherwise, replace top. */
	if (YY_CURRENT_BUFFER)
		(yy_buffer_stack_top)++;
	YY_CURRENT_BUFFER_LVALUE = new_buffer;

	/* copied from fortran__switch_to_buffer. */
	fortran__load_buffer_state( );
	(yy_did_buffer_switch_on_eof) = 1;
}

/** Removes and deletes the top of the stack, if present.
 *  The next element becomes the new top.
 *  
 */
void fortran_pop_buffer_state (void)
{
    	if (!YY_CURRENT_BUFFER)
		return;

	fortran__delete_buffer(YY_CURRENT_BUFFER );
	YY_CURRENT_BUFFER_LVALUE = NULL;
	if ((yy_buffer_stack_top) > 0)
		--(yy_buffer_stack_top);

	if (YY_CURRENT_BUFFER) {
		fortran__load_buffer_state( );
		(yy_did_buffer_switch_on_eof) = 1;
	}
}

/* Allocates the stack if it does not exist.
 *  Guarantees space for at least one push.
 */
static void fortran_ensure_buffer_stack (void)
{
	yy_size_t num_to_alloc;
    
	if (!(yy_buffer_stack)) {

		/* First allocation is just for 2 elements, since we don't know if this
		 * scanner will even need a stack. We use 2 instead of 1 to avoid an
		 * immediate realloc on the next call.
         */
		num_to_alloc = 1;
		(yy_buffer_stack) = (struct yy_buffer_state**)fortran_alloc
								(num_to_alloc * sizeof(struct yy_buffer_state*)
								);
		if ( ! (yy_buffer_stack) )
			YY_FATAL_ERROR( "out of dynamic memory in fortran_ensure_buffer_stack()" );
								  
		memset((yy_buffer_stack), 0, num_to_alloc * sizeof(struct yy_buffer_state*));
				
		(yy_buffer_stack_max) = num_to_alloc;
		(yy_buffer_stack_top) = 0;
		return;
	}

	if ((yy_buffer_stack_top) >= ((yy_buffer_stack_max)) - 1){

		/* Increase the buffer to prepare for a possible push. */
		int grow_size = 8 /* arbitrary grow size */;

		num_to_alloc = (yy_buffer_stack_max) + grow_size;
		(yy_buffer_stack) = (struct yy_buffer_state**)fortran_realloc
								((yy_buffer_stack),
								num_to_alloc * sizeof(struct yy_buffer_state*)
								);
		if ( ! (yy_buffer_stack) )
			YY_FATAL_ERROR( "out of dynamic memory in fortran_ensure_buffer_stack()" );

		/* zero only the new slots.*/
		memset((yy_buffer_stack) + (yy_buffer_stack_max), 0, grow_size * sizeof(struct yy_buffer_state*));
		(yy_buffer_stack_max) = num_to_alloc;
	}
}

/** Setup the input buffer state to scan directly from a user-specified character buffer.
 * @param base the character buffer
 * @param size the size in bytes of the character buffer
 * 
 * @return the newly allocated buffer state object. 
 */
YY_BUFFER_STATE fortran__scan_buffer  (char * base, yy_size_t  size )
{
	YY_BUFFER_STATE b;
    
	if ( size < 2 ||
	     base[size-2] != YY_END_OF_BUFFER_CHAR ||
	     base[size-1] != YY_END_OF_BUFFER_CHAR )
		/* They forgot to leave room for the EOB's. */
		return 0;

	b = (YY_BUFFER_STATE) fortran_alloc(sizeof( struct yy_buffer_state )  );
	if ( ! b )
		YY_FATAL_ERROR( "out of dynamic memory in fortran__scan_buffer()" );

	b->yy_buf_size = size - 2;	/* "- 2" to take care of EOB's */
	b->yy_buf_pos = b->yy_ch_buf = base;
	b->yy_is_our_buffer = 0;
	b->yy_input_file = 0;
	b->yy_n_chars = b->yy_buf_size;
	b->yy_is_interactive = 0;
	b->yy_at_bol = 1;
	b->yy_fill_buffer = 0;
	b->yy_buffer_status = YY_BUFFER_NEW;

	fortran__switch_to_buffer(b  );

	return b;
}

/** Setup the input buffer state to scan a string. The next call to fortran_lex() will
 * scan from a @e copy of @a str.
 * @param yystr a NUL-terminated string to scan
 * 
 * @return the newly allocated buffer state object.
 * @note If you want to scan bytes that may contain NUL values, then use
 *       fortran__scan_bytes() instead.
 */
YY_BUFFER_STATE fortran__scan_string (yyconst char * yystr )
{
    
	return fortran__scan_bytes(yystr,strlen(yystr) );
}

/** Setup the input buffer state to scan the given bytes. The next call to fortran_lex() will
 * scan from a @e copy of @a bytes.
 * @param bytes the byte buffer to scan
 * @param len the number of bytes in the buffer pointed to by @a bytes.
 * 
 * @return the newly allocated buffer state object.
 */
YY_BUFFER_STATE fortran__scan_bytes  (yyconst char * yybytes, yy_size_t  _yybytes_len )
{
	YY_BUFFER_STATE b;
	char *buf;
	yy_size_t n, i;
    
	/* Get memory for full buffer, including space for trailing EOB's. */
	n = _yybytes_len + 2;
	buf = (char *) fortran_alloc(n  );
	if ( ! buf )
		YY_FATAL_ERROR( "out of dynamic memory in fortran__scan_bytes()" );

	for ( i = 0; i < _yybytes_len; ++i )
		buf[i] = yybytes[i];

	buf[_yybytes_len] = buf[_yybytes_len+1] = YY_END_OF_BUFFER_CHAR;

	b = fortran__scan_buffer(buf,n );
	if ( ! b )
		YY_FATAL_ERROR( "bad buffer in fortran__scan_bytes()" );

	/* It's okay to grow etc. this buffer, and we should throw it
	 * away when we're done.
	 */
	b->yy_is_our_buffer = 1;

	return b;
}

#ifndef YY_EXIT_FAILURE
#define YY_EXIT_FAILURE 2
#endif

static void yy_fatal_error (yyconst char* msg )
{
    	(void) fprintf( stderr, "%s\n", msg );
	exit( YY_EXIT_FAILURE );
}

/* Redefine yyless() so it works in section 3 code. */

#undef yyless
#define yyless(n) \
	do \
		{ \
		/* Undo effects of setting up fortran_text. */ \
        int yyless_macro_arg = (n); \
        YY_LESS_LINENO(yyless_macro_arg);\
		fortran_text[fortran_leng] = (yy_hold_char); \
		(yy_c_buf_p) = fortran_text + yyless_macro_arg; \
		(yy_hold_char) = *(yy_c_buf_p); \
		*(yy_c_buf_p) = '\0'; \
		fortran_leng = yyless_macro_arg; \
		} \
	while ( 0 )

/* Accessor  methods (get/set functions) to struct members. */

/** Get the current line number.
 * 
 */
int fortran_get_lineno  (void)
{
        
    return fortran_lineno;
}

/** Get the input stream.
 * 
 */
FILE *fortran_get_in  (void)
{
        return fortran_in;
}

/** Get the output stream.
 * 
 */
FILE *fortran_get_out  (void)
{
        return fortran_out;
}

/** Get the length of the current token.
 * 
 */
yy_size_t fortran_get_leng  (void)
{
        return fortran_leng;
}

/** Get the current token.
 * 
 */

char *fortran_get_text  (void)
{
        return fortran_text;
}

/** Set the current line number.
 * @param line_number
 * 
 */
void fortran_set_lineno (int  line_number )
{
    
    fortran_lineno = line_number;
}

/** Set the input stream. This does not discard the current
 * input buffer.
 * @param in_str A readable stream.
 * 
 * @see fortran__switch_to_buffer
 */
void fortran_set_in (FILE *  in_str )
{
        fortran_in = in_str ;
}

void fortran_set_out (FILE *  out_str )
{
        fortran_out = out_str ;
}

int fortran_get_debug  (void)
{
        return fortran__flex_debug;
}

void fortran_set_debug (int  bdebug )
{
        fortran__flex_debug = bdebug ;
}

static int yy_init_globals (void)
{
        /* Initialization is the same as for the non-reentrant scanner.
     * This function is called from fortran_lex_destroy(), so don't allocate here.
     */

    (yy_buffer_stack) = 0;
    (yy_buffer_stack_top) = 0;
    (yy_buffer_stack_max) = 0;
    (yy_c_buf_p) = (char *) 0;
    (yy_init) = 0;
    (yy_start) = 0;

    (yy_state_buf) = 0;
    (yy_state_ptr) = 0;
    (yy_full_match) = 0;
    (yy_lp) = 0;

/* Defined in main.c */
#ifdef YY_STDINIT
    fortran_in = stdin;
    fortran_out = stdout;
#else
    fortran_in = (FILE *) 0;
    fortran_out = (FILE *) 0;
#endif

    /* For future reference: Set errno on error, since we are called by
     * fortran_lex_init()
     */
    return 0;
}

/* fortran_lex_destroy is for both reentrant and non-reentrant scanners. */
int fortran_lex_destroy  (void)
{
    
    /* Pop the buffer stack, destroying each element. */
	while(YY_CURRENT_BUFFER){
		fortran__delete_buffer(YY_CURRENT_BUFFER  );
		YY_CURRENT_BUFFER_LVALUE = NULL;
		fortran_pop_buffer_state();
	}

	/* Destroy the stack itself. */
	fortran_free((yy_buffer_stack) );
	(yy_buffer_stack) = NULL;

    fortran_free ( (yy_state_buf) );
    (yy_state_buf)  = NULL;

    /* Reset the globals. This is important in a non-reentrant scanner so the next time
     * fortran_lex() is called, initialization will occur. */
    yy_init_globals( );

    return 0;
}

/*
 * Internal utility routines.
 */

#ifndef yytext_ptr
static void yy_flex_strncpy (char* s1, yyconst char * s2, int n )
{
	register int i;
	for ( i = 0; i < n; ++i )
		s1[i] = s2[i];
}
#endif

#ifdef YY_NEED_STRLEN
static int yy_flex_strlen (yyconst char * s )
{
	register int n;
	for ( n = 0; s[n]; ++n )
		;

	return n;
}
#endif

void *fortran_alloc (yy_size_t  size )
{
	return (void *) malloc( size );
}

void *fortran_realloc  (void * ptr, yy_size_t  size )
{
	/* The cast to (char *) in the following accommodates both
	 * implementations that use char* generic pointers, and those
	 * that use void* generic pointers.  It works with the latter
	 * because both ANSI C and C++ allow castless assignment from
	 * any pointer type to void*, and deal with argument conversions
	 * as though doing an assignment.
	 */
	return (void *) realloc( (char *) ptr, size );
}

void fortran_free (void * ptr )
{
	free( (char *) ptr );	/* see fortran_realloc() for (char *) cast */
}

#define YYTABLES_NAME "yytables"

#line 382 "fortran.lex"



void out_of_donottreat ( void )
{
    BEGIN(INITIAL);
    if (infixed) BEGIN(fortran77style) ;
    if (infree)  BEGIN(fortran90style) ;
    INCREMENT_LINE_NUM() ;
}

