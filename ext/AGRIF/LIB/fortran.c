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
#define yy_scan_buffer fortran__scan_buffer
#define yy_scan_string fortran__scan_string
#define yy_scan_bytes fortran__scan_bytes
#define yy_init_buffer fortran__init_buffer
#define yy_flush_buffer fortran__flush_buffer
#define yy_load_buffer_state fortran__load_buffer_state
#define yy_switch_to_buffer fortran__switch_to_buffer
#define yypush_buffer_state fortran_push_buffer_state
#define yypop_buffer_state fortran_pop_buffer_state
#define yyensure_buffer_stack fortran_ensure_buffer_stack
#define yy_flex_debug fortran__flex_debug
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
#define YY_FLEX_MINOR_VERSION 6
#define YY_FLEX_SUBMINOR_VERSION 4
#if YY_FLEX_SUBMINOR_VERSION > 0
#define FLEX_BETA
#endif

#ifdef yy_create_buffer
#define fortran__create_buffer_ALREADY_DEFINED
#else
#define yy_create_buffer fortran__create_buffer
#endif

#ifdef yy_delete_buffer
#define fortran__delete_buffer_ALREADY_DEFINED
#else
#define yy_delete_buffer fortran__delete_buffer
#endif

#ifdef yy_scan_buffer
#define fortran__scan_buffer_ALREADY_DEFINED
#else
#define yy_scan_buffer fortran__scan_buffer
#endif

#ifdef yy_scan_string
#define fortran__scan_string_ALREADY_DEFINED
#else
#define yy_scan_string fortran__scan_string
#endif

#ifdef yy_scan_bytes
#define fortran__scan_bytes_ALREADY_DEFINED
#else
#define yy_scan_bytes fortran__scan_bytes
#endif

#ifdef yy_init_buffer
#define fortran__init_buffer_ALREADY_DEFINED
#else
#define yy_init_buffer fortran__init_buffer
#endif

#ifdef yy_flush_buffer
#define fortran__flush_buffer_ALREADY_DEFINED
#else
#define yy_flush_buffer fortran__flush_buffer
#endif

#ifdef yy_load_buffer_state
#define fortran__load_buffer_state_ALREADY_DEFINED
#else
#define yy_load_buffer_state fortran__load_buffer_state
#endif

#ifdef yy_switch_to_buffer
#define fortran__switch_to_buffer_ALREADY_DEFINED
#else
#define yy_switch_to_buffer fortran__switch_to_buffer
#endif

#ifdef yypush_buffer_state
#define fortran_push_buffer_state_ALREADY_DEFINED
#else
#define yypush_buffer_state fortran_push_buffer_state
#endif

#ifdef yypop_buffer_state
#define fortran_pop_buffer_state_ALREADY_DEFINED
#else
#define yypop_buffer_state fortran_pop_buffer_state
#endif

#ifdef yyensure_buffer_stack
#define fortran_ensure_buffer_stack_ALREADY_DEFINED
#else
#define yyensure_buffer_stack fortran_ensure_buffer_stack
#endif

#ifdef yylex
#define fortran_lex_ALREADY_DEFINED
#else
#define yylex fortran_lex
#endif

#ifdef yyrestart
#define fortran_restart_ALREADY_DEFINED
#else
#define yyrestart fortran_restart
#endif

#ifdef yylex_init
#define fortran_lex_init_ALREADY_DEFINED
#else
#define yylex_init fortran_lex_init
#endif

#ifdef yylex_init_extra
#define fortran_lex_init_extra_ALREADY_DEFINED
#else
#define yylex_init_extra fortran_lex_init_extra
#endif

#ifdef yylex_destroy
#define fortran_lex_destroy_ALREADY_DEFINED
#else
#define yylex_destroy fortran_lex_destroy
#endif

#ifdef yyget_debug
#define fortran_get_debug_ALREADY_DEFINED
#else
#define yyget_debug fortran_get_debug
#endif

#ifdef yyset_debug
#define fortran_set_debug_ALREADY_DEFINED
#else
#define yyset_debug fortran_set_debug
#endif

#ifdef yyget_extra
#define fortran_get_extra_ALREADY_DEFINED
#else
#define yyget_extra fortran_get_extra
#endif

#ifdef yyset_extra
#define fortran_set_extra_ALREADY_DEFINED
#else
#define yyset_extra fortran_set_extra
#endif

#ifdef yyget_in
#define fortran_get_in_ALREADY_DEFINED
#else
#define yyget_in fortran_get_in
#endif

#ifdef yyset_in
#define fortran_set_in_ALREADY_DEFINED
#else
#define yyset_in fortran_set_in
#endif

#ifdef yyget_out
#define fortran_get_out_ALREADY_DEFINED
#else
#define yyget_out fortran_get_out
#endif

#ifdef yyset_out
#define fortran_set_out_ALREADY_DEFINED
#else
#define yyset_out fortran_set_out
#endif

#ifdef yyget_leng
#define fortran_get_leng_ALREADY_DEFINED
#else
#define yyget_leng fortran_get_leng
#endif

#ifdef yyget_text
#define fortran_get_text_ALREADY_DEFINED
#else
#define yyget_text fortran_get_text
#endif

#ifdef yyget_lineno
#define fortran_get_lineno_ALREADY_DEFINED
#else
#define yyget_lineno fortran_get_lineno
#endif

#ifdef yyset_lineno
#define fortran_set_lineno_ALREADY_DEFINED
#else
#define yyset_lineno fortran_set_lineno
#endif

#ifdef yywrap
#define fortran_wrap_ALREADY_DEFINED
#else
#define yywrap fortran_wrap
#endif

#ifdef yyalloc
#define fortran_alloc_ALREADY_DEFINED
#else
#define yyalloc fortran_alloc
#endif

#ifdef yyrealloc
#define fortran_realloc_ALREADY_DEFINED
#else
#define yyrealloc fortran_realloc
#endif

#ifdef yyfree
#define fortran_free_ALREADY_DEFINED
#else
#define yyfree fortran_free
#endif

#ifdef yytext
#define fortran_text_ALREADY_DEFINED
#else
#define yytext fortran_text
#endif

#ifdef yyleng
#define fortran_leng_ALREADY_DEFINED
#else
#define yyleng fortran_leng
#endif

#ifdef yyin
#define fortran_in_ALREADY_DEFINED
#else
#define yyin fortran_in
#endif

#ifdef yyout
#define fortran_out_ALREADY_DEFINED
#else
#define yyout fortran_out
#endif

#ifdef yy_flex_debug
#define fortran__flex_debug_ALREADY_DEFINED
#else
#define yy_flex_debug fortran__flex_debug
#endif

#ifdef yylineno
#define fortran_lineno_ALREADY_DEFINED
#else
#define yylineno fortran_lineno
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
#else
typedef signed char flex_int8_t;
typedef short int flex_int16_t;
typedef int flex_int32_t;
typedef unsigned char flex_uint8_t; 
typedef unsigned short int flex_uint16_t;
typedef unsigned int flex_uint32_t;

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

#ifndef SIZE_MAX
#define SIZE_MAX               (~(size_t)0)
#endif

#endif /* ! C99 */

#endif /* ! FLEXINT_H */

/* begin standard C++ headers. */

/* TODO: this is always defined, so inline it */
#define yyconst const

#if defined(__GNUC__) && __GNUC__ >= 3
#define yynoreturn __attribute__((__noreturn__))
#else
#define yynoreturn
#endif

/* Returned upon end-of-file. */
#define YY_NULL 0

/* Promotes a possibly negative, possibly signed char to an
 *   integer in range [0..255] for use as an array index.
 */
#define YY_SC_TO_UI(c) ((YY_CHAR) (c))

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
#define YY_NEW_FILE yyrestart( yyin  )
#define YY_END_OF_BUFFER_CHAR 0

/* Size of default input buffer. */
#ifndef YY_BUF_SIZE
#ifdef __ia64__
/* On IA-64, the buffer size is 16k, not 8k.
 * Moreover, YY_BUF_SIZE is 2*YY_READ_BUF_SIZE in the general case.
 * Ditto for the __ia64__ case accordingly.
 */
#define YY_BUF_SIZE 32768
#else
#define YY_BUF_SIZE 16384
#endif /* __ia64__ */
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

extern int yyleng;

extern FILE *yyin, *yyout;

#define EOB_ACT_CONTINUE_SCAN 0
#define EOB_ACT_END_OF_FILE 1
#define EOB_ACT_LAST_MATCH 2
    
    #define YY_LESS_LINENO(n)
    #define YY_LINENO_REWIND_TO(ptr)
    
/* Return all but the first "n" matched characters back to the input stream. */
#define yyless(n) \
	do \
		{ \
		/* Undo effects of setting up yytext. */ \
        int yyless_macro_arg = (n); \
        YY_LESS_LINENO(yyless_macro_arg);\
		*yy_cp = (yy_hold_char); \
		YY_RESTORE_YY_MORE_OFFSET \
		(yy_c_buf_p) = yy_cp = yy_bp + yyless_macro_arg - YY_MORE_ADJ; \
		YY_DO_BEFORE_ACTION; /* set up yytext again */ \
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
	int yy_buf_size;

	/* Number of characters read into yy_ch_buf, not including EOB
	 * characters.
	 */
	int yy_n_chars;

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
	 * (via yyrestart()), so that the user can continue scanning by
	 * just pointing yyin at a new input file.
	 */
#define YY_BUFFER_EOF_PENDING 2

	};
#endif /* !YY_STRUCT_YY_BUFFER_STATE */

/* Stack of input buffers. */
static size_t yy_buffer_stack_top = 0; /**< index of top of stack. */
static size_t yy_buffer_stack_max = 0; /**< capacity of stack. */
static YY_BUFFER_STATE * yy_buffer_stack = NULL; /**< Stack as an array. */

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

/* yy_hold_char holds the character lost when yytext is formed. */
static char yy_hold_char;
static int yy_n_chars;		/* number of characters read into yy_ch_buf */
int yyleng;

/* Points to current character in buffer. */
static char *yy_c_buf_p = NULL;
static int yy_init = 0;		/* whether we need to initialize */
static int yy_start = 0;	/* start state number */

/* Flag which is used to allow yywrap()'s to do buffer switches
 * instead of setting up a fresh yyin.  A bit of a hack ...
 */
static int yy_did_buffer_switch_on_eof;

void yyrestart ( FILE *input_file  );
void yy_switch_to_buffer ( YY_BUFFER_STATE new_buffer  );
YY_BUFFER_STATE yy_create_buffer ( FILE *file, int size  );
void yy_delete_buffer ( YY_BUFFER_STATE b  );
void yy_flush_buffer ( YY_BUFFER_STATE b  );
void yypush_buffer_state ( YY_BUFFER_STATE new_buffer  );
void yypop_buffer_state ( void );

static void yyensure_buffer_stack ( void );
static void yy_load_buffer_state ( void );
static void yy_init_buffer ( YY_BUFFER_STATE b, FILE *file  );
#define YY_FLUSH_BUFFER yy_flush_buffer( YY_CURRENT_BUFFER )

YY_BUFFER_STATE yy_scan_buffer ( char *base, yy_size_t size  );
YY_BUFFER_STATE yy_scan_string ( const char *yy_str  );
YY_BUFFER_STATE yy_scan_bytes ( const char *bytes, int len  );

void *yyalloc ( yy_size_t  );
void *yyrealloc ( void *, yy_size_t  );
void yyfree ( void *  );

#define yy_new_buffer yy_create_buffer
#define yy_set_interactive(is_interactive) \
	{ \
	if ( ! YY_CURRENT_BUFFER ){ \
        yyensure_buffer_stack (); \
		YY_CURRENT_BUFFER_LVALUE =    \
            yy_create_buffer( yyin, YY_BUF_SIZE ); \
	} \
	YY_CURRENT_BUFFER_LVALUE->yy_is_interactive = is_interactive; \
	}
#define yy_set_bol(at_bol) \
	{ \
	if ( ! YY_CURRENT_BUFFER ){\
        yyensure_buffer_stack (); \
		YY_CURRENT_BUFFER_LVALUE =    \
            yy_create_buffer( yyin, YY_BUF_SIZE ); \
	} \
	YY_CURRENT_BUFFER_LVALUE->yy_at_bol = at_bol; \
	}
#define YY_AT_BOL() (YY_CURRENT_BUFFER_LVALUE->yy_at_bol)

/* Begin user sect3 */

#define fortran_wrap() (/*CONSTCOND*/1)
#define YY_SKIP_YYWRAP
typedef flex_uint8_t YY_CHAR;

FILE *yyin = NULL, *yyout = NULL;

typedef int yy_state_type;

extern int yylineno;
int yylineno = 1;

extern char *yytext;
#ifdef yytext_ptr
#undef yytext_ptr
#endif
#define yytext_ptr yytext

static yy_state_type yy_get_previous_state ( void );
static yy_state_type yy_try_NUL_trans ( yy_state_type current_state  );
static int yy_get_next_buffer ( void );
static void yynoreturn yy_fatal_error ( const char* msg  );

/* Done after the current pattern has been matched and before the
 * corresponding action - sets up yytext.
 */
#define YY_DO_BEFORE_ACTION \
	(yytext_ptr) = yy_bp; \
	yyleng = (int) (yy_cp - yy_bp); \
	(yy_hold_char) = *yy_cp; \
	*yy_cp = '\0'; \
	(yy_c_buf_p) = yy_cp;
#define YY_NUM_RULES 177
#define YY_END_OF_BUFFER 178
/* This struct is not used in this scanner,
   but its presence is necessary. */
struct yy_trans_info
	{
	flex_int32_t yy_verify;
	flex_int32_t yy_nxt;
	};
static const flex_int16_t yy_acclist[1584] =
    {   0,
      143,  143,  178,  177,  166,  177,  165,  177,  176,  177,
      177,  155,  177,  159,  177,  169,  177,  177,  158,  177,
      158,  177,  158,  177,  161,  177,  156,  177,  140,  177,
      154,  177,  158,  177,  160,  177,  163,  177,  162,  177,
      164,  177,  150,  177,  150,  177,  150,  177,  150,  177,
      150,  177,  150,  177,  150,  177,  150,  177,  150,  177,
      150,  177,  150,  177,  150,  177,  150,  177,  150,  177,
      150,  177,  150,  177,  150,  177,  150,  177,  150,  177,
      150,  177,  150,  177,  166,  177,  165,  175,  177,  176,
      177,  150,  177,  150,  177,  150,  177,  150,  177,  150,

      177,  177,  177,  173,  177,  177,  177,  177,  143,  177,
      144,  177,  177,  165,  177,  150,  177,  150,  177,  150,
      177,  150,  177,  150,  177,  150,  177,  150,  177,  150,
      177,  150,  177,  150,  177,  150,  177,  150,  177,  150,
      177,  150,  177,  150,  177,  150,  177,  150,  177,  150,
      177,  150,  177,  150,  177,  150,  177,  165,  175,  177,
      166,  177,  158,  177,  154,  177,  150,  177,  150,  177,
      150,  177,  150,  177,  150,  177,  166,  177,  154,  177,
      166,  176,  176,  176,  146,  169,  145,  138,   20,  153,
      139,  137,   34,  154,  136,   35,   33,   18,   36,  150,

      150,  150,  150,  150,  150,  150,  150,  150,  150,  150,
      150,  150,  150,   42,  150,  150,  150,  150,  150,  150,
      150,  150,  150,  150,  150,  150,  150,  150,  150,   91,
      150,  150,  150,  150,  150,  150,  150,  150,  150,  150,
      150,  150,  150,  150,  150,  150,  150,  150,  150,  150,
      150,  150,  150,  150,  150,  150,  150,  150,  150,  166,
      175,  176,  176,  176,  176,  150,  150,  150,  150,   91,
      150,  150,  173,  143,  142,  150,  150,  150,  150,  150,
      150,  150,  150,  150,  150,  150,  150,  150,  150,   42,
      150,  150,  150,  150,  150,  150,  150,  150,  150,  150,

      150,  150,  150,  150,  150,   91,  150,  150,  150,  150,
      150,  150,  150,  150,  150,  150,  150,  150,  150,  150,
      150,  150,  150,  150,  150,  150,  150,  150,  150,  150,
      150,  150,  150,  150,  150,  175,  166,  166,  174,   20,
      154,  174,  150,  150,  150,  150,  150,  150,  150,  150,
      150,  150,   91,  150,  150,  166,  154,  176,  176,  141,
      145,  152,  151,  152,  153,  153,  150,  150,  150,  150,
      150,  150,  150,  150,  150,  150,  150,  150,  150,  150,
      150,  150,  150,  150,  150,    9,  150,  150,  150,  150,
      150,  150,  150,  150,  150,  150,  150,  150,  103,16485,

      150,  150,  150,  150,  150,  150,  150,  150,  150,  150,
      150,  150,  150,  150,  150,  150,  150,  150,  150,  150,
       94,  150,  150,  150,  150,  150,  150,  150,  150,  150,
      150,  150,  150,  150,  150,  150,  150,  150,  150,  150,
      150,  150,  150,   11,  150,  150,  150,  150,  176,  176,
      176,  150,  150,  150,  150,  150,  150,  150,  150,  150,
      150,  150,  150,  150,  150,  150,  150,  150,  150,  150,
      150,  150,  150,  150,  150,  150,    9,  150,  150,  150,
      150,  150,  150,  150,  150,  150,  150,  150,  150,  150,
      150,  150,  150,  150,  150,  150,  150,  150,  150,  150,

      150,  150,  150,  150,  150,  150,  150,  150,  150,   94,
      150,  150,  150,  150,  150,  150,  150,  150,  150,  150,
      150,  150,  150,  150,  150,  150,  150,  150,  150,  150,
      150,  150,   11,  150,  150,  150,  150,  166,  166,  154,
      150,  150,  150,  150,  150,  150,  150,  150,  150,  150,
      150,  150,  150,  176,  176,  153,   22,   24,   23,   26,
       25,   28,   30,  150,  150,  150,  150,  150,  150,  150,
       15,  150,  150,  150,  150,  150,  150,  150,  150,  150,
      150,  150,   41,   41,  150,  150,   99,  150,  116,  150,
      150,  150,  150,  150,  117,  150,  126,  150,  150,   79,

      150,  150,  150,  150,  114,  150,  150,   93,  150,  150,
      150,  150,  150,  150,  150,  150,  150,  150,  150,  118,
      150,  150,  150,  150,  115,   14,  150,  150,   63,  150,
       77,  150,  150,  150,  150,  150,  150,  150,  150,  150,
      150,   83,  150,   43,  150,  130,  150,  150,  150,  150,
      150,   72,  150,  150,  150,   76,  150,   57,  150,  150,
      150,   97,  150,  150,  150,  150,  150,   47,  176,  176,
      176,  105,  150,  150,  150,  150,  150,  150,16458,  150,
      150,  150,  150,  150,  150,  150,   15,  150,  150,  150,
      150,  150,  150,  150,  150,  150,  150,  150,   41,  150,

      150,   99,  150,  150,  150,  150,  150,  150,  150,  150,
      150,   79,  150,  150,  150,  150,  150,  150,   93,  150,
      150,  150,  150,  150,  150,  150,  150,  150,  150,  150,
      150,  150,  150,  150,   14,  150,  150,   63,  150,   77,
      150,  150,  150,  150,  150,  150,  150,  150,  150,  150,
       83,  150,   43,  150,  150,  150,  150,  150,  150,   72,
      150,  150,  150,   76,  150,   57,  150,  150,  150,   97,
      150,  150,  150,  150,  150,  166,  154,   15,  150,  105,
      150,  150,  150,  150,  150,  150,  150,  150,  150,  150,
      150,  150,  150,  150,16458,  176,  176,  157,   32,   21,

       29,   31,  150,  150,  150,  150,  150,  150,  150,  150,
       52,  150,  150,  150,  150,  150,  134,  150,  150,  150,
      150,  150,  150,  150,   40,  150,  100,  150,  150,  150,
      150,  150,  150,  150,  150,  108,   87,  150,  127,  150,
       93,  102,  150,  150,   95,  150,  150,  150,  150,  150,
      150,  150,  150,  119,  150,  150,  121,  128,  150,  150,
      150,  150,  150,   55,  150,  150,  150,   80,  150,  150,
      150,  150,   82,  129,  150,  150,  150,  150,  150,  150,
      150,  150,  150,  112,   58,  150,   38,  150,   86,  150,
      105,16458,  176,  176,  176,  105,  150,   92,  150,  150,

     8266,   73, 8266,  150,  150,  150,  150,  150,  150,  150,
      150,   52,  150,  150,  150,  150,  150,  134,  150,  150,
      150,  150,  150,  150,  150,   40,  150,  100,  150,  150,
      150,  150,  150,  150,  150,  150,   87,  150,  150,  150,
      150,   95,  150,  150,  150,  150,  150,  150,  150,  150,
      150,  150,  150,  150,  150,  150,  150,   55,  150,  150,
      150,   80,  150,  150,  150,  150,  150,  150,  150,  150,
      150,  150,  150,  150,  150,   58,  150,   38,  150,   86,
      150,  166,  154,  105,  150,  150,   52,  150,  150,  150,
      150,  150,  150,  150,  134,  150,  150,  150,   16,  176,

       16,  176,   16,   16,  146,   16,   16,   16,  145,   16,
       16,   16,   16,   16,   16,   27,  150,  150,  150,  150,
      150,   16,  150,  150,  150,   66,  150,  150,  150,  150,
      150,  150,  150,  150,   98,  150,  150,   40,  100,  150,
      150,  150,  150,  150,  133,  150,  150,  102, 8293,  102,
      150,  150,  150,  150,   69,  150,  150,  150,  124,  150,
      150,   37,  150,  150,  150,  150,  150,  150,  150,  150,
      150,  150,   89,  150,  150,    7,  150,   78,  150,   12,
      150,  150,  150,  132,  150,  150,   88,  150,   85,  176,
      176,   16,  176,  150,  150,  150,  150,  150,  150,  150,

      150,   16,  150,  150,  150,   66,  150,  150,  150,  150,
      150,  150,  150,  150,   98,  150,  150,  150,  150,  150,
      150,  150,  150,  150,  150,  150,  150,  150,   69,  150,
      150,  150,  150,  150,   37,  150,  150,  150,  150,  150,
      150,  150,  150,  150,  150,   89,  150,  150,    7,  150,
       78,  150,   12,  150,  150,  150,  132,  150,  150,   88,
      150,   16,  150,  150,   66,  150,  150,  150,  150,  150,
       16,  150,  150,  150,   17,   17,  176,   17,   17,  146,
       17,   17,   17,  145,   17,   17,   17,   17,   17,   17,
      109,  110,   17,  150,  150,  150,  150,  150,   50,  150,

      150,  150,  150,  106,  150,  150,  150,  150,   98,  150,
      150,   75,  150,  150,  150,  120,  150,  150, 8293,  150,
       10,  150,   53,  150,   44,  150,  150,  150,  125,   45,
      150,  150,  150,    5,  150,  113,  150,  150,   70,  150,
      150,   90,  150,    2,  150,  150,  150,  122,  131,  150,
      176,   17,  176,  150,   67,  150,  170,   17,  150,  150,
      150,  150,  150,   50,  150,  150,  150,  150,  106,  150,
      150,  150,  150,  150,  150,   75,  150,  150,  150,  150,
      150,  150,   10,  150,   53,  150,   44,  150,  150,  150,
       45,  150,  150,  150,    5,  150,  150,  150,   70,  150,

      150,   90,  150,    2,  150,  150,  150,  150,  170,   17,
       17,  150,  150,   50,  150,  150,  150,  150,  150,  150,
        3,  150,  150,  150,  150,  150,    4,  150,  150,  150,
      150,  150,  150,   75,  150,   59,  150,  150,   68,  150,
        8,  150,   13,  150,  150,  150,  150,   84,  150,   71,
      150,  150,  150,  150,  150,  150,  176,   62,  150,  150,
      150,    3,  150,  150,  150,  150,  150,    4,  150,  150,
      150,  150,  150,  150,  150,   59,  150,  150,   68,  150,
        8,  150,   13,  150,  150,  150,  150,   84,  150,   71,
      150,  150,  150,  150,  150,  150,  150,  150,   62,  150,

        4,  150,  150,  137,  150,  150,  135,  150,   46,  150,
      150,  150,   54,  150,  150,  150,   61,  150,   59,  107,
      150,  150,   96,  150,  111,  150,   64,  150,  123,   65,
      150,  150,  150,   62,  176,  147,  150,  149,  150,  150,
      135,  150,   46,  150,  150,  150,   54,  150,  150,  150,
       61,  150,  107,  150,  150,   96,  150,  150,   64,  150,
       65,  150,  150,  150,   46,  150,  150,  147,  150,  168,
      137,  150,  150,   39,  150,    6,  150,  150,  150,   61,
       60,  107,  150,  150,  104,  150,    1,  150,  147,  176,
      150,  150,   39,  150,    6,  150,  150,  150,  150,  150,

      104,  150,    1,  150,  167,   39,  150,   51,  150,  150,
      150,   56,  150,  150,  104,  176,   51,  150,  150,  150,
       56,  150,  150,  168,  150,  150,  150,  176,  150,  150,
      150,  167,   19,   49,  150,  150,  150,  176,  148,  173,
       49,  150,  150,  150,  167,  167,   49,  150,  150,  176,
      150,  150,   48,  150,   81,  150,  176,   48,  150,   81,
      150,  167,   48,   81,  176,  176,  176,  176,  176,  176,
      171,  176,  171,  171,  174,  171,  175,  176,  174,  172,
      173,  172,  173
    } ;

static const flex_int16_t yy_accept[1878] =
    {   0,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    2,
        3,    3,    3,    3,    3,    4,    5,    7,    9,   11,
       12,   14,   16,   18,   19,   21,   23,   25,   27,   29,
       31,   33,   35,   37,   39,   41,   43,   45,   47,   49,
       51,   53,   55,   57,   59,   61,   63,   65,   67,   69,
       71,   73,   75,   77,   79,   81,   83,   85,   87,   90,
       92,   94,   96,   98,  100,  102,  103,  104,  106,  107,
      108,  109,  111,  113,  114,  116,  118,  120,  122,  124,
      126,  128,  130,  132,  134,  136,  138,  140,  142,  144,
      146,  148,  150,  152,  154,  156,  158,  161,  163,  165,

      167,  169,  171,  173,  175,  177,  179,  181,  181,  181,
      182,  183,  184,  185,  185,  186,  186,  186,  187,  187,
      187,  187,  187,  188,  188,  188,  188,  188,  189,  189,
      189,  189,  190,  190,  191,  191,  191,  191,  191,  191,
      191,  191,  191,  191,  191,  192,  193,  194,  194,  195,
      195,  196,  197,  198,  199,  200,  201,  202,  203,  204,
      205,  206,  207,  208,  209,  210,  211,  212,  213,  214,
      216,  217,  218,  219,  220,  221,  222,  223,  224,  225,
      226,  227,  228,  229,  230,  232,  233,  234,  235,  236,
      237,  238,  239,  240,  241,  242,  243,  244,  245,  246,

      247,  248,  249,  250,  251,  252,  253,  254,  255,  256,
      257,  258,  259,  260,  260,  261,  262,  262,  262,  262,
      262,  262,  262,  262,  263,  263,  264,  265,  266,  266,
      267,  268,  269,  270,  272,  273,  273,  274,  274,  274,
      274,  274,  275,  275,  276,  276,  276,  276,  276,  276,
      276,  277,  278,  279,  280,  281,  282,  283,  284,  285,
      286,  287,  288,  289,  290,  292,  293,  294,  295,  296,
      297,  298,  299,  300,  301,  302,  303,  304,  305,  306,
      308,  309,  310,  311,  312,  313,  314,  315,  316,  317,
      318,  319,  320,  321,  322,  323,  324,  325,  326,  327,

      328,  329,  330,  331,  332,  333,  334,  335,  336,  336,
      337,  337,  337,  338,  339,  339,  339,  340,  341,  341,
      341,  341,  341,  342,  343,  343,  344,  345,  346,  347,
      348,  349,  350,  351,  352,  353,  355,  356,  357,  357,
      357,  358,  358,  358,  358,  359,  360,  360,  360,  360,
      360,  360,  360,  360,  360,  362,  362,  362,  362,  362,
      362,  362,  362,  362,  362,  362,  362,  362,  362,  362,
      362,  362,  362,  362,  362,  362,  362,  362,  362,  362,
      362,  362,  362,  362,  362,  362,  362,  363,  366,  366,
      367,  368,  369,  370,  371,  372,  373,  374,  375,  376,

      377,  378,  379,  380,  381,  382,  383,  383,  384,  385,
      386,  388,  389,  390,  391,  392,  393,  394,  395,  396,
      397,  398,  398,  399,  399,  401,  402,  403,  404,  405,
      406,  407,  408,  409,  410,  411,  412,  413,  414,  415,
      416,  417,  418,  419,  420,  421,  423,  424,  425,  426,
      427,  428,  429,  430,  431,  432,  433,  434,  435,  436,
      437,  438,  439,  440,  441,  442,  443,  444,  446,  447,
      448,  449,  449,  449,  449,  449,  449,  449,  449,  449,
      449,  450,  451,  452,  452,  453,  454,  455,  456,  457,
      458,  458,  458,  458,  458,  458,  458,  458,  458,  458,

      458,  458,  458,  459,  460,  461,  462,  463,  464,  465,
      466,  467,  468,  469,  470,  471,  472,  473,  474,  475,
      476,  477,  479,  480,  481,  482,  483,  484,  485,  486,
      487,  488,  489,  490,  491,  492,  493,  494,  495,  496,
      497,  498,  499,  500,  501,  502,  503,  504,  505,  506,
      507,  508,  509,  510,  512,  513,  514,  515,  516,  517,
      518,  519,  520,  521,  522,  523,  524,  525,  526,  527,
      528,  529,  530,  531,  532,  533,  535,  536,  537,  538,
      538,  538,  538,  538,  539,  539,  540,  540,  540,  540,
      540,  540,  540,  541,  541,  542,  543,  544,  545,  546,

      547,  548,  549,  550,  551,  552,  553,  554,  554,  554,
      554,  554,  555,  556,  556,  556,  556,  556,  556,  556,
      556,  556,  556,  556,  556,  556,  556,  556,  556,  556,
      556,  556,  557,  557,  557,  558,  558,  559,  560,  561,
      562,  562,  563,  563,  563,  564,  564,  564,  564,  564,
      564,  564,  564,  564,  564,  564,  564,  565,  566,  567,
      568,  569,  570,  571,  573,  574,  575,  576,  577,  578,
      579,  580,  581,  582,  583,  584,  586,  587,  589,  589,
      590,  591,  592,  593,  594,  595,  595,  596,  597,  597,
      598,  599,  600,  602,  603,  604,  605,  605,  606,  607,

      608,  608,  610,  610,  610,  610,  610,  611,  612,  613,
      614,  615,  616,  617,  618,  619,  620,  620,  621,  622,
      623,  624,  625,  625,  626,  628,  629,  631,  633,  634,
      635,  636,  637,  638,  639,  640,  641,  642,  644,  646,
      646,  647,  648,  649,  650,  651,  652,  654,  655,  656,
      658,  660,  661,  662,  664,  665,  666,  667,  668,  669,
      669,  669,  669,  669,  669,  669,  670,  671,  672,  672,
      674,  675,  676,  677,  678,  680,  680,  680,  680,  680,
      680,  680,  680,  680,  680,  681,  682,  683,  684,  685,
      686,  687,  689,  690,  691,  692,  693,  694,  695,  696,

      697,  698,  699,  701,  702,  704,  705,  706,  707,  708,
      709,  710,  711,  712,  714,  715,  716,  717,  718,  719,
      721,  722,  723,  724,  725,  726,  727,  728,  729,  730,
      731,  732,  733,  734,  735,  737,  738,  740,  742,  743,
      744,  745,  746,  747,  748,  749,  750,  751,  753,  755,
      756,  757,  758,  759,  760,  762,  763,  764,  766,  768,
      769,  770,  772,  773,  774,  775,  776,  776,  776,  776,
      777,  777,  777,  777,  777,  777,  778,  778,  780,  782,
      783,  784,  785,  786,  787,  788,  789,  790,  791,  792,
      793,  794,  796,  796,  796,  796,  797,  798,  798,  798,

      798,  798,  798,  799,  799,  799,  799,  799,  799,  799,
      800,  801,  801,  802,  803,  803,  803,  803,  803,  803,
      803,  804,  805,  806,  807,  808,  809,  810,  811,  813,
      814,  815,  816,  817,  819,  820,  821,  822,  823,  823,
      824,  825,  825,  825,  825,  825,  825,  827,  829,  830,
      831,  832,  833,  834,  835,  836,  836,  837,  839,  839,
      840,  841,  842,  842,  842,  842,  843,  844,  845,  847,
      848,  849,  850,  851,  852,  853,  854,  854,  855,  856,
      857,  857,  858,  858,  859,  860,  861,  862,  863,  864,
      866,  867,  868,  870,  871,  872,  873,  873,  874,  874,

      875,  876,  877,  878,  879,  880,  881,  882,  883,  884,
      884,  885,  887,  889,  891,  892,  892,  892,  892,  892,
      893,  894,  895,  896,  896,  897,  898,  899,  900,  901,
      902,  903,  904,  904,  904,  904,  904,  904,  904,  905,
      906,  907,  908,  909,  910,  911,  912,  914,  915,  916,
      917,  918,  920,  921,  922,  923,  924,  925,  926,  928,
      930,  931,  932,  933,  934,  935,  936,  937,  939,  940,
      941,  942,  944,  945,  946,  947,  948,  949,  950,  951,
      952,  953,  954,  955,  956,  957,  958,  960,  961,  962,
      964,  965,  966,  967,  968,  969,  970,  971,  972,  973,

      974,  975,  976,  978,  980,  982,  982,  982,  982,  983,
      983,  983,  983,  983,  983,  984,  984,  985,  986,  987,
      989,  990,  991,  992,  993,  994,  995,  997,  998,  999,
      999,  999, 1000, 1001, 1003, 1003, 1004, 1006, 1006, 1007,
     1008, 1010, 1010, 1010, 1010, 1010, 1011, 1012, 1013, 1014,
     1015, 1016, 1017, 1017, 1017, 1017, 1017, 1018, 1019, 1020,
     1021, 1022, 1024, 1025, 1026, 1028, 1029, 1030, 1031, 1032,
     1033, 1034, 1035, 1035, 1035, 1037, 1038, 1039, 1040, 1040,
     1040, 1040, 1041, 1042, 1043, 1044, 1045, 1045, 1046, 1047,
     1048, 1048, 1049, 1049, 1049, 1049, 1049, 1050, 1051, 1052,

     1053, 1054, 1055, 1057, 1058, 1059, 1059, 1060, 1061, 1062,
     1064, 1065, 1066, 1067, 1068, 1069, 1070, 1071, 1072, 1073,
     1075, 1076, 1078, 1080, 1082, 1083, 1084, 1086, 1087, 1089,
     1089, 1090, 1090, 1090, 1090, 1091, 1092, 1094, 1094, 1095,
     1096, 1097, 1097, 1097, 1097, 1097, 1097, 1097, 1098, 1099,
     1100, 1101, 1102, 1104, 1105, 1106, 1108, 1109, 1110, 1111,
     1112, 1113, 1114, 1115, 1117, 1118, 1119, 1120, 1121, 1122,
     1123, 1124, 1125, 1126, 1127, 1128, 1129, 1131, 1132, 1133,
     1134, 1135, 1137, 1138, 1139, 1140, 1141, 1142, 1143, 1144,
     1145, 1146, 1148, 1149, 1151, 1153, 1155, 1156, 1157, 1159,

     1160, 1162, 1162, 1162, 1162, 1162, 1163, 1163, 1164, 1165,
     1167, 1168, 1169, 1170, 1171, 1173, 1174, 1175, 1175, 1176,
     1178, 1179, 1181, 1182, 1183, 1185, 1185, 1186, 1187, 1188,
     1189, 1190, 1191, 1191, 1191, 1191, 1191, 1191, 1192, 1192,
     1193, 1195, 1196, 1197, 1198, 1199, 1201, 1202, 1203, 1204,
     1206, 1207, 1207, 1208, 1209, 1210, 1210, 1211, 1211, 1211,
     1211, 1212, 1214, 1215, 1216, 1216, 1217, 1218, 1219, 1220,
     1220, 1220, 1221, 1223, 1225, 1227, 1228, 1229, 1229, 1230,
     1232, 1232, 1233, 1234, 1236, 1236, 1237, 1238, 1239, 1241,
     1242, 1244, 1246, 1247, 1247, 1248, 1248, 1249, 1249, 1250,

     1251, 1251, 1251, 1251, 1252, 1254, 1254, 1255, 1256, 1257,
     1257, 1257, 1257, 1258, 1258, 1258, 1260, 1261, 1262, 1263,
     1264, 1266, 1267, 1268, 1269, 1271, 1272, 1273, 1274, 1275,
     1276, 1278, 1279, 1280, 1281, 1282, 1283, 1285, 1287, 1289,
     1290, 1291, 1293, 1294, 1295, 1297, 1298, 1299, 1301, 1302,
     1304, 1306, 1307, 1308, 1309, 1309, 1310, 1310, 1311, 1311,
     1313, 1314, 1316, 1317, 1318, 1319, 1320, 1320, 1320, 1320,
     1320, 1320, 1321, 1323, 1324, 1325, 1326, 1327, 1329, 1330,
     1331, 1331, 1331, 1332, 1333, 1333, 1334, 1334, 1335, 1335,
     1336, 1338, 1339, 1341, 1343, 1343, 1345, 1346, 1347, 1347,

     1348, 1350, 1352, 1353, 1354, 1355, 1355, 1356, 1357, 1357,
     1357, 1358, 1358, 1360, 1361, 1361, 1361, 1361, 1361, 1361,
     1362, 1364, 1365, 1366, 1367, 1368, 1370, 1371, 1372, 1373,
     1374, 1375, 1376, 1378, 1379, 1381, 1383, 1385, 1386, 1387,
     1388, 1390, 1392, 1393, 1394, 1395, 1396, 1397, 1397, 1397,
     1397, 1398, 1399, 1401, 1403, 1404, 1404, 1404, 1404, 1404,
     1404, 1404, 1404, 1405, 1406, 1407, 1409, 1411, 1412, 1413,
     1415, 1415, 1415, 1416, 1417, 1417, 1419, 1419, 1420, 1422,
     1423, 1425, 1425, 1426, 1426, 1427, 1429, 1429, 1430, 1432,
     1432, 1433, 1434, 1435, 1435, 1436, 1436, 1438, 1438, 1438,

     1438, 1439, 1440, 1441, 1443, 1445, 1446, 1447, 1449, 1450,
     1451, 1453, 1455, 1456, 1458, 1459, 1461, 1463, 1464, 1465,
     1465, 1465, 1465, 1467, 1468, 1470, 1470, 1470, 1471, 1471,
     1471, 1471, 1472, 1473, 1474, 1476, 1478, 1478, 1478, 1479,
     1480, 1481, 1481, 1482, 1483, 1484, 1484, 1485, 1485, 1487,
     1489, 1490, 1491, 1491, 1491, 1491, 1492, 1493, 1495, 1497,
     1498, 1499, 1500, 1501, 1503, 1505, 1505, 1505, 1505, 1506,
     1506, 1508, 1508, 1508, 1508, 1508, 1508, 1508, 1510, 1510,
     1510, 1510, 1510, 1511, 1512, 1514, 1514, 1515, 1516, 1517,
     1517, 1517, 1517, 1519, 1520, 1521, 1523, 1524, 1524, 1524,

     1524, 1524, 1524, 1524, 1524, 1524, 1524, 1524, 1524, 1525,
     1525, 1525, 1525, 1525, 1526, 1527, 1527, 1528, 1529, 1529,
     1529, 1529, 1530, 1531, 1532, 1532, 1532, 1532, 1532, 1532,
     1532, 1532, 1532, 1532, 1532, 1532, 1533, 1533, 1533, 1533,
     1533, 1533, 1534, 1534, 1534, 1536, 1537, 1537, 1538, 1539,
     1539, 1539, 1539, 1541, 1543, 1544, 1545, 1545, 1545, 1545,
     1545, 1545, 1545, 1546, 1546, 1546, 1546, 1547, 1547, 1547,
     1547, 1547, 1548, 1548, 1549, 1549, 1550, 1551, 1551, 1551,
     1552, 1553, 1553, 1553, 1553, 1553, 1553, 1553, 1553, 1553,
     1553, 1553, 1553, 1555, 1555, 1557, 1558, 1558, 1558, 1560,

     1562, 1562, 1562, 1562, 1562, 1562, 1562, 1563, 1563, 1564,
     1565, 1566, 1566, 1566, 1566, 1566, 1566, 1566, 1566, 1566,
     1567, 1567, 1567, 1567, 1567, 1567, 1567, 1567, 1567, 1568,
     1568, 1568, 1568, 1568, 1569, 1569, 1569, 1569, 1570, 1570,
     1570, 1570, 1571, 1572, 1573, 1573, 1574, 1574, 1574, 1574,
     1576, 1576, 1576, 1578, 1578, 1579, 1579, 1579, 1579, 1579,
     1579, 1579, 1580, 1580, 1580, 1580, 1580, 1582, 1582, 1582,
     1583, 1583, 1583, 1584, 1584, 1584, 1584
    } ;

static const YY_CHAR yy_ec[256] =
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

static const YY_CHAR yy_meta[81] =
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

static const flex_int16_t yy_base[2035] =
    {   0,
        0,   79,    0,    0,    0,  151, 3696,   84,   88,   91,
      224,  303,    0,  375, 3675,   65,   99, 9366,   73,  100,
       74,   90,  308,  117,  325,  126,  137,  133,  447,  386,
      382,  144,  143,  285,  390,  302,  425,  449,  499,  497,
      547,  594,  443,  324,  535,  495,  503,  582,  618,  630,
      639,  398,  685,  688,  697,  689,  450,  769,  216,  538,
      583,  749,  745,  800,  802, 9366, 3671, 9366,  789,  114,
      155,   96, 9366, 3646,  851,  841,  692,  912,  860,  961,
      910,  853,  858,  948,  895,  896,  977, 1009, 1020, 1036,
     1033, 1069, 1085, 1081, 1118, 1123, 1162,  203,  908,  316,

     1219,   71, 1145,  100, 1274,  339,  363,  106,  127,  198,
        0,  140,  144, 3621, 3616,  308,  348,  347, 3618,  181,
      698,  415, 3605,  434,  899,  631,  819, 9366, 1300, 1317,
     1342, 9366, 1343,  697,  307,  331,  446,  607,  613,  363,
      425, 1050, 1361, 1167, 9366, 9366, 9366, 1311, 1358,  304,
     9366, 9366, 9366, 9366, 9366,    0,  831,  302,  435,  473,
      508,  359,  516,  367,  542,  749,  411,  906,  550, 1142,
      549,  489,  576,  624,  653,  926,  698,  692,  723,  735,
      804, 1040, 1301,  805, 1366, 1360,  810,  857,  489,  976,
      861,  957,  958,  965,  972, 1060,  973, 1138, 1340, 1089,

      570, 1393, 1003, 1019, 1012, 1403,  573, 1027,  615,  649,
      765, 1292,  807,    0, 1362, 1190, 3608, 1404,  891, 1071,
     1092, 1390, 1195, 3557, 1449,  993, 1322, 1402,  957, 1405,
     1135, 1265, 1327, 1436, 1345, 3549, 9366, 1453, 1421, 1459,
     1460,  209, 3481, 3473, 1201, 1493, 1304, 3454, 3376, 1497,
     1489, 1507, 1458, 1496, 1514, 1520, 1515, 1553, 1540, 1554,
     1564, 1551, 1588, 1587, 1601, 1595, 1606, 1614, 1627, 1637,
     1658, 1660, 1650, 1555, 1661, 1681, 1712, 1725, 1702, 1716,
     1750, 1751, 1764, 1769, 1782, 1800, 1804, 1805, 1806, 1830,
     1840, 1807, 1848, 1870, 1872, 1880, 1902, 1904, 1911, 1808,

     1926, 1935, 1944, 1965, 1962, 1975, 1984, 1999, 1760, 2047,
     2061, 3358, 1652,  318,  782, 3300, 9366, 3296, 1464, 1533,
     1907, 2038, 2066, 2074, 1484, 2136, 2216, 2045, 2048, 2050,
     2125, 2062, 2128, 1913, 2138, 2139, 2214, 1748, 2241, 2000,
     2244, 1673, 1362, 1401, 1443, 1448, 3261, 1486, 1595, 3258,
     1380, 1802, 2247, 1913, 3243, 3236, 2070, 2267,  497,  880,
     2289, 2290, 3222, 2313, 2314, 1474, 1041, 1124, 1077, 2280,
     3110, 3074, 3067, 3060, 2293, 1533, 3019, 1546, 1772, 2334,
     1884, 3022, 3009, 2354, 2367, 2999, 9366, 2345, 2974, 2961,
     1271, 1316, 1601, 1609, 1346, 1616, 1634, 1654, 1668, 1711,

     1947, 2142, 1735, 1783, 1581, 1663, 2362, 2372, 1790, 1872,
     2444, 2035, 1892, 2381, 2129, 1914, 1952, 1812, 2252, 1852,
     1998, 2345, 1852, 1528, 1733, 1963, 2046, 2045, 2047, 2269,
     2067, 2057, 2130, 2147, 2261, 2256, 2134, 2293, 2293, 2322,
     2295, 2145, 2136, 2213, 2345,    0, 2354, 2317, 2345, 2359,
     2359, 2362, 2360, 2373, 2412, 2353, 2354, 2367, 2381, 2383,
     2386, 2387, 2392, 2394, 2389, 2400, 2396,    0, 2400, 2407,
     2406, 2933, 2402, 2935, 2409, 2415, 2410, 2416, 2414, 2421,
     2461, 2460, 2463, 2438, 2444, 2466, 2449, 2449, 2455, 2465,
     2497, 2528, 2920, 2508, 2531, 2871, 2870, 2536, 2540, 2544,

     2858, 2855, 2488, 2493, 2516, 2512, 2525, 2505, 2531, 2518,
     2547, 2554, 2558, 2557, 2563, 2566, 2560, 2562, 2567, 2579,
     2574, 2639, 2570, 2573, 2642, 2575, 2602, 2608, 2581, 2647,
     2619, 2631, 2561, 2572, 2649, 2636, 2652, 2661, 2659, 2651,
     2620, 2667, 2675, 2666, 2657, 2672, 2687, 2703, 2689, 2691,
     2671, 2706, 2713, 2844, 2710, 2678, 2721, 2730, 2731, 2734,
     2732, 2738, 2755, 2735, 2736, 2737, 2759, 2760, 2740, 2739,
     2758, 2751, 2782, 2741, 2771, 2810, 2774, 2750, 2783, 2821,
     2828, 2804, 2832, 2888, 2549,  388, 2110, 2837, 2801, 2802,
     2838, 2847, 2882, 2818, 2961, 3041, 2822, 2842, 2864, 2888,

     2959, 2869, 2845, 2851, 2900, 3031, 2850, 2815, 2871, 2881,
     2875, 2886, 2891, 2949, 2929, 2043, 2860, 2791, 2996, 3066,
     2988, 2993, 3018, 3079, 3007, 3013, 3085, 3102, 2654, 3080,
     2771, 2752, 2750, 3105, 9366, 2737, 9366, 9366, 9366, 9366,
     3106, 9366, 2885, 2734, 9366, 2733, 2860, 3109, 2673, 2613,
     3132, 3144, 3163, 2610, 2590, 3173, 2947, 2956, 2956, 2973,
     3054, 3058, 2995,    0, 3071, 3082, 3051, 3079, 3081, 3096,
     3101, 3106, 3106, 3114, 2486, 2481, 3125, 3136, 3211, 9366,
     3129, 3114, 3120, 3136, 3131, 3183, 9366, 3141, 3187, 9366,
     3150, 3150,    0, 3153, 3196, 3169, 3201, 9366, 3202, 3163,

     3169,    0, 3067, 2407, 2340, 3215, 3187, 3164, 3184, 3199,
     3209, 3200, 3204, 3212, 3220, 3256, 3262, 9366, 3221, 3214,
     3286, 3287, 3290, 9366,    0, 3218,    0, 3223, 3225, 3230,
     3242, 3232, 3240, 3252, 3272, 3257, 3268, 3304,    0, 3305,
     9366, 3313, 3261, 3275, 3275, 3280,    0, 3292, 3293, 3277,
        0, 3286, 3300,    0, 3331, 3304, 3306, 3308, 9366, 3309,
     3296, 3316, 3316, 3314, 3315, 3319, 3345, 3350, 3313,  462,
     3332,  602, 3331, 3349, 3381, 3357, 3373, 3367, 3394, 3399,
     3409, 3417, 2339, 2318, 3361, 3375, 3379, 3409, 3415, 3405,
     3388, 2317, 3422, 3423, 3391, 3421, 3424, 3427, 3426, 3435,

     3437, 3436,  427, 3441, 3465, 3439, 3442, 3451, 3438, 3447,
     3446, 3443, 3449, 2297, 3452, 3515, 3468, 3522, 3459, 2286,
     3471, 3462, 3486, 3493, 3511, 3517, 3494, 3526, 3509, 3545,
     3534, 3537, 3551, 3564, 2276, 3542, 2264, 3535, 3539, 3554,
     3541, 3567, 3570, 3574, 3578, 3560, 3579, 3581, 2263, 3593,
     3583, 3586, 3587, 3595, 2244, 3596, 3607, 3603, 2241, 3605,
     3606, 2233, 3635, 3611, 3622, 3625, 3664, 3683, 3420, 3687,
     3659, 3674, 3678, 3653, 3641, 3710, 3680, 3780, 3860, 3691,
     3646, 3706, 3702, 3709, 3754, 3825, 3885, 3888,  685, 3700,
     3774, 3818, 3580, 3672,    0, 3690,    0, 3748,  532, 3890,

     2885, 1789, 9366, 3901, 3904, 2193, 3725, 3907, 3966, 9366,
     9366, 2186, 9366, 9366, 3757, 3762, 3806, 3926, 2195, 3993,
     3588, 3657, 3713, 3788, 3773, 4050, 3777, 3787,    0, 3780,
     3793, 3783, 3819,    0, 3858, 3867, 3863, 3794, 3960, 3879,
     3901, 3901, 3962, 3969, 3951, 3969,    0,    0, 3967, 3964,
     3976, 3983, 3978, 3892, 3974, 4013, 9366,    0, 4023, 9366,
     3977, 9366, 4074, 4075, 4092, 4098, 3986, 3987,    0, 3999,
     3994, 4035, 4045, 4053, 4115, 4048, 4116, 9366, 4071, 4070,
     4121, 9366, 4129, 9366, 4072, 4096, 4099, 4085, 4102,    0,
     4104, 4102,    0, 4093, 4115, 4114, 3944, 9366, 4144, 9366,

     4103, 4104, 4112, 4115, 4113, 4129, 4118, 4118, 4122, 4172,
     9366,    0,    0, 3948, 1158, 4134, 2019, 4143, 4137, 4181,
     4147, 4179, 2191, 4150, 2606, 4153, 2935, 4154, 4163, 4199,
     9366, 3834, 4196, 4197, 3771, 3949, 4202, 4220, 4202, 4209,
     4214, 4211, 4215, 4282, 4240, 4223, 2174, 4269, 4272, 4268,
     4274, 2129, 4275, 4307, 4276, 4315, 4309, 4313, 2125, 2120,
     4314, 4311, 4319, 4316, 4321, 4225, 4318, 2113, 4320, 4327,
     4329, 2106, 4322, 4330, 4323, 4324, 4334, 4229, 4325, 4335,
     4347, 4370, 4361, 4373, 4378, 4356, 2064, 4364, 4385, 2042,
     4394, 4396, 4397, 4398, 4402, 4404, 4400, 4405, 4406, 4408,

     4407, 4410, 2033, 2027, 4253, 4258, 4457, 4198, 4482, 3071,
     4433, 4442, 4438, 1995, 4488, 4435, 4079, 4558, 4638, 4193,
     4438, 4444, 4466, 4400, 4468, 4718, 4211, 4485, 4473, 4214,
        0, 9366,    0,    0, 1208, 1959, 1958, 3846, 4509, 4518,
     1949, 4582, 4583, 4414, 4798, 4410, 4534, 4604, 4610, 4611,
     1881, 9366, 4552, 4587, 4665, 4669, 4243, 4595, 4878, 4479,
     4552,    0, 4394, 4548,    0, 4555, 4562, 4561, 4487, 4564,
     4560, 4710, 4645, 4644,    0, 4648, 9366, 9366, 4643, 4640,
     4652, 4656, 4657, 4642, 4652, 4742, 4745, 9366, 4719, 4708,
     4748, 4679, 4760, 4765, 3930, 1883, 4829, 4844, 4720, 4726,

     4727, 4710,    0, 4716, 4723, 4825, 9366, 4834, 4760, 4835,
     4782, 4784, 4903, 4732, 4790, 4800, 4830, 4765, 4868,    0,
     4767,    0,    0,    0, 4908, 4909, 4913, 4869,    0, 4428,
     9366, 4814, 4879, 4887, 4917, 1871, 1857, 4890, 4884, 3893,
     4899, 4925, 4905, 3955, 4487, 4596, 4695, 4936, 4959, 4985,
     4966, 4946, 1826, 4919, 4968, 1822, 4956, 4972, 4970, 5014,
     4973, 5011, 5019, 1796, 4923, 5021, 5022, 4977, 5024, 5026,
     4926, 5016, 5023, 5028, 5032, 5029, 1778, 5031, 5033, 5066,
     5046, 5051, 5060, 5063, 5091, 5070, 5037, 5056, 5061, 5094,
     5064, 1764, 5074, 1763, 1757, 1746, 5114, 5119, 5131, 5095,

     1739, 4788, 4862, 4915, 1732, 1699, 5076, 5171, 5251, 5331,
     5070, 5083, 5089, 5085,    0, 4870, 5136, 5093, 9366,    0,
     1675, 1670, 5141, 5147, 1639, 4953, 5144, 5195, 5217, 5223,
     5224, 1600, 4530, 4700, 5198, 5278, 5205, 9366, 5212, 9366,
        0, 5259, 4913, 5057, 5139,    0, 5175, 5188, 5194,    0,
     5164, 5323, 5182, 5236, 9366, 5254, 5244, 5260, 5261, 5250,
     5266,    0, 5269, 5270, 5306, 9366, 5261, 5318, 5357, 5345,
     5374, 5320,    0,    0,    0, 5332, 5334, 5386, 9366,    0,
     5371, 5328, 5334,    0, 5398, 9366, 5364, 5338,    0, 5364,
        0,    0, 5357, 5411, 5368, 5404, 9366, 5412, 9366, 5371,

     5378, 5159, 5383,  582, 1581, 1516, 5374, 5163, 5391,  796,
     5417, 5307, 9366, 5209, 5244, 1534, 5421, 5423, 5424, 5427,
     1523, 5432, 5439, 5418, 1514, 5438, 5447, 5440, 5446, 5456,
     1502, 5455, 5460, 5430, 5453, 5452, 1488, 1428, 1412, 5467,
     5461, 1398, 5462, 5458, 1365, 5463, 5496, 1358, 5468, 1346,
     1325, 5469, 5501, 5474, 5536, 1320, 5429, 1309,  804,    0,
     5463,    0, 5488, 5452, 5491, 5506, 5541, 5544, 5555, 5565,
     5575, 5505,    0, 5497, 5512, 5511, 5521,    0, 5517, 5533,
     5533, 5533, 5543, 5536, 5540, 5556, 5559, 9366, 5559, 5546,
        0, 5553,    0,    0, 5608,    0, 5565, 5603, 5553, 5558,

        0,    0, 5559, 5604, 5583, 5588, 5559, 5578, 5585, 5605,
     5631, 5606,    0, 5606, 5636, 5637, 5642, 5646,    0, 5634,
     1215, 5635, 5646, 5638, 5652, 1214, 5649, 5653, 5655, 5656,
     5666, 5667, 1135, 5660, 1114, 1109, 1094, 5650, 5680, 5668,
     1078, 1024, 5670, 5714, 5676, 5682, 5664, 5740, 5735, 5672,
     5629, 5684,    0,    0, 5725, 5744, 1469, 5748, 5730, 5755,
     5759, 5777, 5773, 5729, 5722,    0,    0, 5725, 5734,    0,
     5733, 5738, 5731, 5745, 5755, 5799, 5743, 9366,    0, 5758,
        0, 5802, 9366, 5749, 5762,    0, 5803, 9366,    0, 5764,
     5761, 5779, 9366, 5780, 5809, 5773,    0, 5815, 5818, 5820,

        0, 5820, 5825,  962,  931, 5826, 5828,  904, 5827, 5831,
     5843,  889, 5832,  878, 5834,  848,  834, 5845, 5848, 5838,
     5292, 5829,    0, 5788,  822, 1996, 5890, 2158, 5862, 5906,
     5909,  817, 5846, 5914,    0,    0, 5811, 5858, 5819, 5867,
     5919, 5920, 9366, 9366, 5876, 5889, 5890, 5897,    0,    0,
     9366,  938,  757, 5926, 5930, 5931, 5935,  778,  774, 5899,
     5937, 5938, 5939,  751,  743, 5940, 5971, 5976, 5941,  979,
        0, 5991, 6003, 5949, 6007, 6017,  726,    0, 6021, 6033,
     5923, 5914, 5933, 5958,    0, 5973, 5985, 9366, 5979, 5985,
     1003, 6012,  703, 6026, 6031,  690, 6035, 6049, 6056, 6061,

      638,  627, 6076, 6092, 6059, 6037, 6088, 6105, 6109, 6112,
     6122, 6009, 6005, 6006, 6010, 6053, 6054, 6085, 6077, 6118,
      602, 6084, 6089, 6096, 6136, 6146,  596,  591, 6152, 6164,
     6177, 6181, 6183, 6185, 6197, 6201, 6157, 6213, 6225, 6209,
     6130, 9366, 6081, 6110,    0, 6091, 6129, 6142, 6160, 6161,
     6168,  549, 9366,  451, 6169, 6217, 6237, 6241, 6245, 6249,
     6253, 6265, 6261, 6269, 6281, 6287, 6291, 6303, 6315, 6311,
     6254, 9366, 6225, 6248, 6255, 6202, 1153,  332, 1856, 6273,
     6293, 6328, 6332, 6343, 6355, 6371, 6375, 6341, 6388, 6384,
     1880, 6264,    0, 6214,    0, 6277, 6266, 6332,  379,  369,

     5508, 6432, 6402, 6406, 6410, 6456, 6411, 6392, 9366, 9366,
     6299, 6298, 6360, 6362, 6468, 6512, 6485, 6437, 6359, 6424,
     6251, 6419, 6472, 6497, 6493, 6517, 6536, 6449, 6436, 6370,
     2342, 6540, 6479, 6506, 6310, 6516, 6523, 6552, 6567, 6544,
     6571, 6578, 6595,  368, 6599, 6603,  322, 6501, 6607, 6614,
      312, 6618, 6622,  198,  178, 6626, 6630,  173, 6634, 6559,
      166, 6638,  146, 6610, 6642, 6646, 6654,  126, 6658, 6662,
      120,  106, 6666,   82, 6670, 9366, 6683, 6701, 6719, 6737,
     6755, 6772, 6776, 6794, 6812, 6830, 6846, 6864, 6882, 6900,
     6918, 6936, 6954, 6971, 6988, 6993,  101, 7011, 7029, 7047,

     7065, 7083, 7101, 7119, 7137, 7155, 7173, 7191, 7209, 7227,
     7245, 7263, 7280, 7296, 7301, 7318, 7336, 7354, 7372, 7377,
     7395, 7408, 7423, 7441, 7459, 7477, 7495, 7513, 7531, 7549,
     7565, 7583, 7601, 7619, 7637, 7655, 7673, 7691, 7709, 7726,
     7742, 7759, 7777, 7795, 7813, 7831, 7836, 7854, 7872, 7890,
     7908, 7926, 7944, 7962, 7980, 7998, 8016, 8034, 8052, 8070,
     8088, 8106, 8124, 8141, 8146, 8162, 8179, 8197, 8215, 8233,
     8251, 8269, 8287, 8305, 8323, 8341, 8359, 8377, 8395, 8413,
     8431, 8449, 8467, 8485, 8503, 8521, 8539, 8557, 8575, 8592,
     8610, 8627, 8643, 8648, 8665, 8683, 8701, 8719, 8737, 8755,

     8773, 8791, 8808, 8825, 8843, 8861, 8879, 8897, 8915, 8933,
     8951, 8968, 8985, 9001, 9018, 9023, 9041, 9059, 9077, 9095,
     9113, 9131, 9149, 9167, 9185, 9203, 9221, 9239, 9257, 9275,
     9293, 9311, 9329, 9347
    } ;

static const flex_int16_t yy_def[2035] =
    {   0,
     1876,    1, 1877, 1877,    1,    1, 1878, 1878, 1877, 1877,
     1876,   11,    1,    1, 1876, 1876, 1876, 1876, 1879, 1880,
     1876, 1876, 1876, 1881, 1882, 1876, 1876, 1876, 1876, 1876,
     1876, 1876, 1876, 1876, 1876, 1876, 1883, 1883, 1883, 1883,
     1883, 1883, 1883, 1883, 1883, 1883, 1883, 1883, 1883, 1883,
       49, 1883, 1883, 1883, 1883, 1883, 1883, 1876, 1876, 1884,
       39, 1883, 1883, 1883, 1883, 1876, 1885, 1876, 1885, 1885,
     1885, 1876, 1876, 1886, 1876, 1887, 1887, 1887, 1887,   79,
       79,   79, 1887, 1887,   79,   79,   79,   79, 1887,   88,
       79,   79, 1887,   89, 1887, 1887, 1876,   58, 1888,   31,

     1876,   79,   79,   84,   78,   58,   31, 1876, 1876, 1876,
     1889, 1889, 1889, 1890, 1876, 1890, 1890, 1876, 1891, 1892,
     1893, 1892, 1876, 1892, 1892, 1894, 1894, 1876, 1894, 1894,
     1894, 1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876,
     1876, 1876, 1876, 1876, 1876, 1876, 1876, 1895, 1876, 1876,
     1876, 1876, 1876, 1876, 1876, 1896, 1896, 1896, 1896, 1896,
     1896, 1896, 1896, 1896, 1896, 1896, 1896, 1896, 1896, 1896,
     1896, 1896, 1896, 1896, 1896, 1896, 1896, 1896, 1896, 1896,
     1896, 1896, 1896, 1896, 1896, 1896, 1896, 1896, 1896, 1896,
     1896, 1896, 1896, 1896, 1896, 1896, 1896, 1896, 1896, 1896,

     1896, 1896, 1896, 1896, 1896, 1896, 1896, 1896, 1896, 1896,
     1896, 1896, 1896, 1897,   58, 1876, 1898, 1876, 1876, 1876,
     1876, 1876, 1876, 1899, 1876, 1899, 1899, 1899, 1876, 1896,
     1896, 1896, 1896, 1896, 1896, 1900, 1876, 1900, 1900, 1900,
     1900, 1876, 1901, 1876, 1876, 1876, 1876, 1902, 1903, 1876,
       84,   84,  252,  252,  252,  252,  252,  252,  252,  252,
      252,  252,  252,  252,  252,  252,  252,  252,  252,  252,
      252,  252,  252,  252,  252,  252,  252,  252,  252,  252,
      252,  252,  252,  252,  252,  252,  252,  252,  252,  252,
      252,  252,  252,  252,  252,  252,  252,  252,  252,  252,

      252,  252,  252,  252,  252,  252,  252,  252, 1876, 1876,
     1876, 1904,  215,  313, 1876, 1905, 1876, 1905, 1905, 1905,
     1876, 1876, 1876, 1876, 1905, 1906, 1906,  327,  327,  327,
      327,  327,  327,  252,  252,  252,  252,  215, 1876, 1876,
     1876, 1876, 1876, 1876, 1907, 1907, 1908, 1908, 1908, 1909,
     1910, 1910, 1910, 1910, 1876, 1911, 1912, 1912, 1876, 1913,
     1876, 1914, 1915, 1914, 1914, 1876, 1876, 1876, 1876, 1876,
     1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876,
     1876, 1916, 1917, 1876, 1876, 1918, 1876, 1919, 1876, 1876,
     1920, 1920, 1920, 1920, 1920, 1920, 1920, 1920, 1920, 1920,

     1920, 1920, 1920, 1920, 1920, 1920, 1876, 1920, 1920, 1920,
     1920, 1920, 1920, 1920, 1920, 1920, 1920, 1920, 1920, 1920,
     1920, 1876, 1920, 1876, 1921, 1920, 1920, 1920, 1920, 1920,
     1920, 1920, 1920, 1920, 1920, 1920, 1920, 1920, 1920, 1920,
     1920, 1920, 1920, 1920, 1920, 1920, 1920, 1920, 1920, 1920,
     1920, 1920, 1920, 1920, 1920, 1920, 1920, 1920, 1920, 1920,
     1920, 1920, 1920, 1920, 1920, 1920, 1920, 1920, 1920, 1920,
     1920, 1922, 1876, 1923, 1876, 1876, 1876, 1876, 1876, 1876,
     1924, 1924, 1924, 1876, 1920, 1920, 1920, 1920, 1920, 1920,
     1925, 1925, 1926, 1876, 1876, 1927, 1928, 1876, 1876, 1876,

     1929, 1930, 1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931,
     1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931,
     1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931,
     1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931,
     1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931,
     1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931,
     1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931,
     1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931, 1876,
     1876, 1932, 1876, 1876, 1876,  584, 1876, 1876, 1933, 1933,
     1876, 1876, 1876, 1933, 1934, 1934,  596,  596,  596,  596,

      596,  596,  596, 1931, 1931, 1931, 1931, 1876, 1876, 1876,
     1876, 1935, 1935, 1936, 1936, 1937, 1938, 1939, 1938, 1938,
     1940, 1940, 1940, 1876, 1876, 1941, 1942, 1942, 1876, 1876,
     1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876,
     1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876, 1943, 1944,
     1876, 1876, 1876, 1945, 1946, 1876, 1947, 1947, 1947, 1947,
     1947, 1947, 1947, 1947, 1947, 1947, 1947, 1947, 1947, 1947,
     1947, 1947, 1947, 1947, 1876, 1947, 1947, 1947, 1876, 1876,
     1947, 1947, 1947, 1947, 1947, 1876, 1876, 1947, 1876, 1876,
     1947, 1947, 1947, 1947, 1947, 1947, 1876, 1876, 1947, 1947,

     1876, 1947, 1948, 1949, 1950, 1948, 1947, 1947, 1947, 1947,
     1947, 1947, 1947, 1947, 1947, 1947, 1876, 1876, 1947, 1947,
     1947, 1947, 1876, 1876, 1947, 1947, 1947, 1947, 1947, 1947,
     1947, 1947, 1947, 1947, 1947, 1947, 1947, 1947, 1947, 1876,
     1876, 1947, 1947, 1947, 1947, 1947, 1947, 1947, 1947, 1947,
     1947, 1947, 1947, 1947, 1947, 1947, 1947, 1947, 1876, 1876,
     1876, 1876, 1876, 1876, 1876, 1951, 1951, 1951, 1876, 1947,
     1947, 1947, 1947, 1947, 1947, 1952, 1952, 1952, 1876, 1876,
     1876, 1876, 1953, 1954, 1931, 1931, 1931, 1931, 1931, 1931,
     1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931,

     1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931,
     1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931,
     1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931,
     1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931,
     1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931,
     1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931,
     1931, 1931, 1931, 1931, 1931, 1931, 1876, 1876, 1876, 1876,
     1876, 1876, 1876, 1955, 1955, 1876, 1955, 1956, 1956,  879,
      879,  879,  879,  879,  879,  879,  879,  879, 1931, 1931,
     1931, 1931, 1876, 1876, 1957, 1958, 1959, 1960, 1961, 1962,

     1963, 1876, 1876, 1876, 1964, 1965, 1966, 1967, 1968, 1876,
     1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876, 1969, 1876,
     1947, 1947, 1947, 1947, 1947, 1970, 1947, 1947, 1947, 1947,
     1947, 1947, 1947, 1947, 1947, 1947, 1947, 1947, 1876, 1947,
     1947, 1876, 1876, 1876, 1876, 1876, 1947, 1947, 1947, 1947,
     1947, 1947, 1947, 1947, 1947, 1876, 1876, 1947, 1876, 1876,
     1947, 1876, 1971, 1972, 1973, 1974, 1947, 1947, 1947, 1947,
     1947, 1947, 1947, 1947, 1947, 1947, 1876, 1876, 1947, 1947,
     1876, 1876, 1876, 1876, 1947, 1947, 1947, 1947, 1947, 1947,
     1947, 1947, 1947, 1947, 1947, 1947, 1876, 1876, 1876, 1876,

     1947, 1947, 1947, 1947, 1947, 1947, 1947, 1947, 1947, 1876,
     1876, 1947, 1947, 1947, 1876, 1876, 1876, 1876, 1876, 1876,
     1975, 1975, 1976, 1876, 1876, 1947, 1876, 1947, 1947, 1876,
     1876, 1876, 1977, 1977, 1876, 1876, 1876, 1876, 1931, 1931,
     1931, 1931, 1931, 1978, 1931, 1931, 1931, 1931, 1931, 1931,
     1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931,
     1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931,
     1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931,
     1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931,
     1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931,

     1931, 1931, 1931, 1931, 1931, 1876, 1876, 1876, 1876, 1876,
     1876, 1876, 1979, 1980, 1876, 1979, 1979, 1981, 1981, 1119,
     1119, 1119, 1119, 1119, 1119, 1982, 1119, 1931, 1931, 1876,
     1983, 1876, 1984, 1985, 1986, 1987, 1876, 1988, 1989, 1989,
     1876, 1876, 1876, 1990, 1991, 1876, 1992, 1876, 1993, 1993,
     1994, 1876, 1876, 1876, 1876, 1876, 1947, 1947, 1995, 1947,
     1947, 1947, 1947, 1947, 1947, 1947, 1947, 1947, 1947, 1947,
     1947, 1947, 1876, 1876, 1947, 1947, 1876, 1876, 1876, 1876,
     1876, 1947, 1947, 1947, 1947, 1947, 1876, 1876, 1947, 1947,
     1996, 1996, 1997, 1998, 1999, 1998, 1999, 1999, 1947, 1947,

     1947, 1947, 1947, 1947, 1947, 1876, 1876, 1947, 1947, 1947,
     1947, 1947, 1947, 1947, 1947, 1947, 1947, 1947, 1947, 1947,
     1947, 1947, 1947, 1947, 1947, 1947, 1947, 1947, 1947, 1876,
     1876, 1876, 1876, 1876, 2000, 2001, 2000, 1876, 1947, 1947,
     1947, 2002, 2002, 1876, 2003, 1876, 1876, 1931, 1931, 2004,
     1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931,
     1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931,
     1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931,
     1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931,
     1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931,

     1931, 1876, 2003, 1876, 2005, 2006, 2006, 2007, 2008, 2008,
     1310, 1310, 1310, 1310, 1310, 1931, 1931, 1876, 1876, 2009,
     2010, 1876, 2011, 2011, 1876, 2012, 1876, 1992, 1876, 1993,
     1993, 1994, 1876, 2013, 1876, 1876, 1876, 1876, 1876, 1876,
     1947, 1947, 1947, 1947, 1947, 1947, 1947, 1947, 1947, 1947,
     1947, 1876, 1947, 1947, 1876, 1876, 1947, 1876, 1876, 1876,
     1947, 1947, 1947, 1947, 1876, 1876, 1947, 1947, 1998, 1998,
     1999, 1947, 1947, 1947, 1947, 1947, 1947, 1876, 1876, 1947,
     1876, 1947, 1947, 1947, 1876, 1876, 1947, 1947, 1947, 1947,
     1947, 1947, 1947, 1876, 1947, 1876, 1876, 1876, 1876, 1947,

     1876, 1876, 1876, 2000, 2000, 1876, 1947, 1876, 1947, 2002,
     2002, 1876, 1876, 1876, 2014, 1931, 1931, 1931, 1931, 1931,
     1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931,
     1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931,
     1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931,
     1931, 1931, 1931, 1931, 1876, 2015, 1876, 2006, 2006, 1310,
     1310, 1310, 1310, 1310, 1310, 1931, 1876, 1876, 1876, 1876,
     2013, 1947, 1947, 1947, 1947, 1947, 1947, 1947, 1947, 1947,
     1876, 1876, 1947, 1947, 1876, 1947, 1876, 1876, 1876, 1947,
     1947, 1947, 1947, 1947, 1998, 1947, 1947, 1947, 1876, 1947,

     1947, 1947, 1947, 1947, 1947, 1876, 1947, 1947, 1876, 1876,
     2000, 1876, 1947, 1947, 2002, 2002, 1876, 1876, 2016, 1931,
     1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931,
     1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931,
     1931, 1931, 1931, 1931, 1931, 1931, 1931, 1876, 1876, 2006,
     1310, 1310, 1310, 1310, 1931, 1876, 2017, 1876, 1876, 1876,
     2018, 1876, 1876, 1947, 1947, 1947, 1947, 1947, 1947, 1947,
     1876, 1876, 1947, 1947, 1876, 1947, 1876, 1876, 1947, 1947,
     1947, 1876, 1876, 1876, 1947, 1947, 1876, 1876, 1947, 1876,
     1947, 1947, 1876, 1876, 2000, 1876, 1947, 2002, 2002, 1876,

     2016, 1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931,
     1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931, 1931, 1876,
     2019, 2006, 1310, 1310, 1931, 2017, 2017, 2017, 1876, 2018,
     2018, 2018, 1947, 1947, 1947, 1947, 1876, 1876, 1947, 1947,
     1876, 1876, 1876, 1876, 1947, 1876, 1947, 1876, 1947, 1947,
     1876, 2000, 1876, 2002, 2002, 1931, 1931, 1931, 1931, 1931,
     1931, 1931, 1931, 1931, 1931, 2019, 1876, 2019, 2019, 2006,
     1310, 2017, 2017, 2020, 2018, 1876, 2018, 1947, 1876, 1876,
     1876, 1876, 1947, 1947, 1947, 1876, 1947, 1876, 2000, 1876,
     2002, 2002, 1931, 1931, 1931, 1931, 1931, 1876, 1876, 1876,

     2021, 2022, 2019, 2019, 2023, 2006, 2020, 2020, 2020, 1876,
     1876, 1876, 1876, 1947, 1947, 1876, 1947, 2000, 1876, 2002,
     2024, 1931, 1931, 1931, 1876, 1876, 2021, 2022, 2019, 2019,
     2019, 2025, 2026, 2023, 2023, 2023, 2006, 2020, 2017, 2020,
     1876, 1876, 1876, 1876, 1947, 1947, 1876, 1947, 2000, 1876,
     2002, 2024, 1876, 1931, 1931, 1931, 1876, 1876, 2019, 2019,
     2025, 2025, 2025, 2026, 1876, 2026, 2026, 2023, 2019, 2023,
     2006, 1876, 1876, 1947, 1876, 1947, 2000, 1876, 2002, 1931,
     1931, 1876, 1876, 2019, 2019, 2025, 2019, 2025, 2026, 2027,
     2006, 1876, 1947, 1876, 1947, 2000, 1876, 2002, 1931, 1931,

     1876, 2019, 2019, 2019, 2027, 2027, 2027, 2006, 1876, 1876,
     2000, 1876, 2002, 1876, 2019, 2028, 2027, 2027, 2006, 2000,
     1876, 2002, 1876, 2019, 2023, 2019, 2019, 2006, 2000, 1876,
     2002, 2019, 2006, 2000, 1876, 2002, 2006, 2000, 1876, 2002,
     2006, 2000, 1876, 2029, 1876, 1876, 2030, 2002, 2006, 1876,
     2031, 1876, 1876, 2032, 2029, 1876, 1876, 2030, 1876, 2002,
     2031, 1876, 2032, 2002, 2002, 2002, 1876, 2033, 1876, 1876,
     2034, 2033, 1876, 2034, 1876,    0, 1876, 1876, 1876, 1876,
     1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876,
     1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876,

     1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876,
     1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876,
     1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876,
     1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876,
     1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876,
     1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876,
     1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876,
     1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876,
     1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876,
     1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876,

     1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876,
     1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876,
     1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876,
     1876, 1876, 1876, 1876
    } ;

static const flex_int16_t yy_nxt[9447] =
    {   0,
       16,   17,   18,   17,   19,   20,   16,   21,   22,   23,
       24,   25,   26,   27,   26,   28,   26,   29,   30,   31,
       32,   33,   34,   35,   36,   37,   38,   39,   40,   41,
       42,   43,   44,   45,   44,   46,   47,   48,   49,   50,
       51,   44,   52,   53,   54,   55,   44,   56,   44,   44,
       57,   26,   26,   26,   37,   38,   39,   40,   41,   42,
       43,   44,   45,   46,   47,   48,   49,   50,   51,   44,
       52,   53,   54,   55,   44,   56,   44,   44,   57,   16,
       58,   59,   58,   60, 1875,   69,   68,   69,   70,   72,
       73,   72,   72,   73,   72,  108,  334,  242,   74,  242,

      110,   74,  110,  112,  108,  115,   61,   62, 1873,  109,
       63,   70,   64,   71,  472,  472,  237,  113,  109,  121,
      108,  240, 1875,   65,  108,  334,  122,  123, 1873,  108,
      116,  343,  112,  108,  109,   61,   62,  109,  336,   63,
       70,   64,   71,  109,  117,  113,  109,  124, 1856,  108,
      132,   65,   58,   59,   58,   60,  108,  237,  108,  116,
      343,  125,  109,  108,  151,  345,  336,  108, 1862,  344,
      109,  109,  117,  108,  108, 1859,  124,  109,   61,   62,
     1856,  109,   63,  121,   64,  108,  346,  109,  109,  125,
      122,  123,  108,  241,  345,   65,  108,  344,  109,  110,

     1856,  110,  108,  108,  313,  109,  314,   61,   62,  109,
      242,   63,  242,   64,  346,  109,  109,  223,  216,  223,
      217,  241,  315,   65,   16,   17,   75,   17,   19,   20,
       16,   21,   22,   23,   24,   25,   26,   27,   26,   28,
       26,   29,   30,   31,   32,   33,   34,   35,   36,   76,
       77,   78,   79,   80,   81,   82,   83,   84,   83,   85,
       86,   87,   88,   89,   90,   83,   91,   92,   93,   94,
       83,   95,   83,   83,   96,   26,   26,   26,   76,   77,
       78,   79,   80,   81,   82,   83,   84,   85,   86,   87,
       88,   89,   90,   83,   91,   92,   93,   94,   83,   95,

       83,   83,   96,   16,   58,   97,   98,   60,  152,  118,
      118,  118,  119,  115, 1862,  108,   99,  321,  389,  322,
      389,  586,  100,  390, 1859,  155,  126,  393,  126,  109,
      101,  102,  108,  348,  103,  323,  104,  587,  108,  127,
      338,  127,  338,  128,  108,  369,  109,  105,  118,  118,
      118,  119,  109,  115,  158,  130,  393,  109,  339,  101,
      102,  108,  348,  103,  340,  104,  340,  108,  161,  131,
     1856,  250,  370,  369,  109,  105,  106,   59,  106,   60,
      109,  250,  341,  158,  130, 1797,  397,  142,  143,  142,
      349,  870,  399,  342,  107,  144,  161,  131,  145,  148,

      370,  149,   61,   62,  146,  377,   63,  871,   64,  147,
      150,  150,  108,  153,  154,  397,  108,  121,  349,   65,
      108,  399,  342,  150,  122,  123,  109,  202,  158,  250,
      109,   61,   62,  377,  109,   63,  121,   64,  403,  150,
      150,  108,  161,  122,  123,  108,  803,   65,  133,  108,
      133,  150,  157,  250,  109,  158,  202,  158,  109,  357,
      214,  159,  109, 1025,  378, 1025,  134,  403,  160,  161,
      161,  394,  135,  158,  162,  371,  136,  108,  137,  158,
      158,  157,  182,  138,  158,  139,  140,  161,  357,  159,
      372,  109,  378,  161,  161,  141,  160,  161,  359,  394,

      359,  135,  158,  162,  371,  136,  108,  137,  158,  158,
      182,  138,  360,  139,  140,  161,  395,  411,  372,  109,
      436,  161,  161,  141,  163,  158,  168,  158,  187,  158,
      169,  164,  188,  158,  114,  165,  170, 1137,  166,  161,
      225,  161,  189,  161,  395,  226,  411,  161,  167,  436,
      396, 1753,  398,  163,  158,  168,  158,  187,  158,  169,
      164,  188,  158,  165,  170,  183,  166,  161,  227,  161,
      189,  161,  184,  185,  186,  161,  167,  158,  396,  161,
      398,  400,  228,  171,  225,  172,  173,  406,  174,  175,
      229,  161,  410, 1699,  183,  176,  453,  227, 1699,  464,

      184,  185,  186, 1027, 1753, 1027,  158,  161,  230,  400,
      228,  171,  158,  172,  173,  406,  174,  175,  412,  161,
      410,  190,  231,  176,  158,  453,  161,  177,  464, 1699,
      178,  179,  126,  180,  126, 1511,  373,  230,  161,  181,
     1699,  158,  375,  191,  466,  127,  412,  127,  158,  190,
      231,  374,  376,  158,  161,  192,  177,  193,  178,  179,
      158,  180,  161,  194,  198,  373,  161,  181,  195,  413,
      196,  375,  191,  466,  161,  197,  156,  158,  199,  374,
      376,  200,  467,  192,  201,  193, 1027,  250, 1027,  158,
      161,  194,  250,  198,  250,  414,  195,  413,  196,  352,

      353,  352,  161,  197,  156,  250,  199,  354,  355,  200,
      203,  467,  201,  208,  204,  158,  134,  257,  158,  158,
      209,  212,  253,  414,  205,  368,  368,  158,  656,  206,
      207,  213,  161,  161,  417,  210,  256,  418,  368,  203,
      211,  161,  208,  204,  158,  250,  257,  158,  158,  209,
      212,  253,  205,  250,  368,  368,  158,  206,  207,  213,
      161,  161,  417,  210,  256,  418,  368,  419,  211,  161,
      215,  216,  215,  217,  232,  158,  250,  420,  168,  158,
      250,  233,  169,  321,  182,  588,  401,  402,  170,  161,
      238,  237,  238,  161,  468,  419,  218,  219,  237,  108,

      220,  587,  221,  232,  158,  420,  317,  168,  158,  233,
     1690,  169,  182,  222,  401,  402,  170,  161,  239,  656,
     1876,  161, 1876,  468,  250,  218,  219,  208,  108,  220,
      183,  221,  158, 1876,  209, 1876,  250,  184,  234,  186,
      471,  222,  421,  250,  161,  426,  161,  239,  434, 1515,
      250,  235,  245,  246,  247,  248,  208, 1550,  391,  183,
      250,  158,  250,  209,  249,  184,  234,  186,  252,  471,
      421,  253,  161,  426,  161,  392,  434,  254,  249,  235,
      250,  621,  251,  621,  255,  256,  251,  391,  253,  263,
      253,  250,  277,  264,  622,  435,  622,  252,  439,  265,

      253,  121,  256,  392,  256,  254,  250,  249,  122,  123,
      317,  251,  255,  256,  250,  251,  477,  253,  263,  253,
      277,  318,  264,  435,  251,  283,  439,  265,  282,  251,
      256,  404,  256,  250,  251,  284,  405,  258,  319,  251,
      225,  358,  253,  272,  259,  477,  273,  274,  260,  275,
      250,  261,  320,  251,  283,  276,  256,  282,  251,  415,
      404,  262,  251,  284,  250,  405,  258,  319,  251,  358,
      416,  253,  272,  259,  273,  274,  260,  275,  278,  261,
      320,  317,  484,  276,  256,  279,  280,  281,  415,  262,
      251, 1689,  256,  440,  251,  225,  441,  266,  416,  267,

      268,  442,  269,  270,  437,  237,  251,  278,  443,  271,
      251,  484,  438,  279,  280,  281,  285,  446,  481,  251,
      256,  440,  250,  251,  441,  266,  250,  267,  268,  442,
      269,  270, 1706,  437,  286,  251,  443,  271,  251,  251,
      438,  422,  251,  422,  285,  446,  287,  481,  288,  459,
      253,  142,  143,  142,  289,  460, 1720,  461,  290,  144,
      291,  293,  297,  286,  256,  292,  251,  251,  146,  465,
      630,  251,  251,  251,  287,  294,  288,  459,  295,  253,
      250,  296,  289,  460,  423,  461,  290,  250,  291,  444,
      293,  297,  256,  292,  298,  251,  250,  465,  299,  630,

      251,  251,  251,  294,  445,  633,  295,  478,  300,  296,
      303,  250,  423,  301,  302,  253,  250,  304,  444,  305,
      250,  251,  451,  298,  306,  250,  251,  299,  452,  256,
      479,  251,  445,  214,  633,  478,  300,  250,  631,  303,
      631,  301,  302,  632,  253,  407,  304,  305,  253,  251,
      307,  451,  306,  253,  251,  225,  452,  256,  479, 1025,
      308, 1025,  256,  309,  310,  311,  312,  256,  384,  385,
      384,  386,  401,  486,  251,  249,  144,  253,  251,  307,
      447,  335,  253,  448,  277,  146,  408,  409,  308,  249,
      256,  223,  216,  223,  217,  256,  223,  216,  223,  217,

      401,  486,  494,  251,  494,  248, 1796,  251,  447,  335,
      114,  448,  277, 1322,  408,  409,  250,  250,  249,  316,
      316,  324,  316,  316,  316,  316,  325,  316,  316,  316,
      316,  316,  316,  316,  316,  316,  316,  316,  326,  316,
      316,  316,  316,  316,  327,  326,  326,  326,  326,  328,
      326,  329,  326,  326,  326,  330,  326,  326,  331,  326,
      326,  326,  326,  332,  326,  326,  326,  326,  333,  326,
      316,  316,  326,  327,  326,  326,  326,  326,  328,  326,
      329,  326,  326,  330,  326,  326,  331,  326,  326,  326,
      326,  332,  326,  326,  326,  326,  333,  326,  316,  303,

      657,  359,  424,  359,  424,  494,  304,  495,  248,  487,
      251,  317,  425,  251, 1876,  360, 1876,  361,  359,  362,
      359,  469,  225,  337,  225,  470,  393,  250,  303,  657,
      388, 1876,  360, 1876,  361,  304,  362,  487,  251,  150,
      150,  251,  364,  359,  133,  359,  133,  482,  250,  658,
      469,  337,  150,  363,  470,  393, 1876,  360, 1876,  361,
      250,  362,  379,  380,  381,  382,  488,  250,  150,  150,
      363,  364,  366,  449,  383,  148,  482,  149,  658,  661,
      150,  367,  353,  450,  365,  490,  150,  150,  383,  616,
      355,  141, 1876,  427,  488,  363,  431,  432,  610,  150,

      250,  366,  449,  433,  225,  428,  473,  429,  661,  367,
      430,  450,  365,  490,  250,  150,  150,  383,  454,  141,
      455, 1876,  427,  237,  431,  432,  610,  150,  462,  475,
      250,  433,  344,  428,  473,  429,  456,  457,  430,  480,
      458,  398,  463,  476,  483,  396,  611,  454,  485,  455,
      223,  216,  223,  217,  238,  237,  238,  462,  475,  241,
      344,  237,  237,  427,  456,  457,  317,  480,  458,  398,
      463,  476,  483,  396,  611,  428,  485,  429, 1627,  612,
      489, 1628,  239,  505,  491,  251,  317,  241,  492,  589,
      250,  115,  427,  613,  245,  246,  247,  248,  498,  499,

      500,  501,  251,  428,  250,  429,  249,  612,  489,  594,
      502,  239,  505,  491,  251,  629,  250,  492,  589,  251,
      249,  613,  614,  251,  502,  250,  251,  251,  251,  424,
      251,  424,  506,  251,  503,  317,  250,  251,  594,  425,
      251,  251,  509,  629,  251,  251,  251,  251,  251,  249,
      614,  504,  251,  502,  251,  251,  251,  507,  251,  251,
      506,  251,  508,  503,  251,  511,  251,  251,  251, 1512,
      251,  509,  251,  251,  251,  590,  251,  644,  515,  504,
      251,  251,  251,  225,  251,  507,  251,  251,  646,  510,
      508,  251,  251,  512,  511,  251,  251,  251,  251,  530,

      115,  513,  514,  590,  407,  644,  673,  515,  251,  251,
      251,  251,  251,  516,  251,  251,  646,  510,  517,  625,
      251,  512,  251,  251,  518,  251,  251,  530,  251,  513,
      514,  251,  251,  251,  522,  673,  251,  659,  521,  251,
      615,  251,  516,  251,  251,  519,  520,  517,  660,  356,
      251,  251,  518,  584,  251,  584,  523,  251,  251,  251,
      251,  662,  251,  522,  251,  659,  521,  251,  615,  663,
      251,  251,  524,  519,  520,  347,  660,  251,  251,  525,
      115,  251,  585,  251,  523,  251,  251,  251,  251,  662,
      664,  526,  674,  251,  251,  529,  528,  663,  343,  251,

      524,  317,  527,  531,  251,  251,  251,  525,  251,  251,
      665,  585,  609,  422,  251,  422,  251,  251,  664,  532,
      526,  674,  251,  529,  528,  251,  424,  343,  424,  251,
      527,  531,  251,  251,  317,  704,  425,  251,  665,  251,
      609,  250,  534,  535,  705,  706,  251,  532,  250,  338,
      505,  338,  251,  251,  666,  536,  533,  537,  251,  250,
      538,  580,  216,  580,  312,  250,  250,  339,  251,  251,
      534,  671,  535,  647,  251,  647,  382,  251,  251,  505,
      250,  251,  666,  536,  533,  537,  539,  540,  538,  542,
      902,  251,  902,  541,  251,  251,  251,  251,  250,  671,

      544,  903,  543,  352,  353,  352,  251,  251,  251,  251,
      545,  354,  355,  251,  539,  540,  677,  542,  546,  672,
      251,  541,  251,  251,  250,  251,  251,  251,  250,  544,
      543,  251,  251,  251,  251,  251,  251,  547,  251,  545,
      548,  251,  550,  549,  251,  677,  546,  672,  251,  251,
      251,  554,  251,  569,  251,  696,  251,  251,  237,  225,
      251,  251,  251,  251,  251,  547,  551,  251,  548,  552,
      550,  549,  251,  225,  251,  251,  251,  251,  251,  554,
      251,  569,  317,  696,  553,  647,  251,  648,  382,  699,
      555,  702,  251,  556,  551, 1369,  251,  251,  552,  251,

      625,  678,  251,  557,  251,  559,  561,  251,  591, 1798,
      591,  560,  553,  558,  251,  353,  251,  699,  555,  702,
      251,  556,  616,  355,  251,  688,  251,  562,  251,  563,
      678,  251,  557, 1808,  559,  561,  251,  585,  251,  560,
      251,  558,  251,  694,  251,  564,  565,  568,  251,  566,
      567,  570,  251,  251,  688,  251,  562,  604,  563,  356,
      251,  572,  251,  347,  115,  571,  585,  251,  508,  251,
      251,  251,  694,  564,  565,  568,  251,  566,  567,  251,
      570,  695,  251,  251,  667,  604,  573,  668,  251,  251,
      572,  251,  251,  571,  574,  575,  508,  317,  251,  707,

      251,  340,  251,  340,  576, 1627,  251,  251, 1628,  251,
      695,  251,  667,  577,  573,  668,  251,  578,  251,  251,
     1027,  251, 1027,  574,  575,  700,  251,  707,  251,  250,
      608,  251,  579,  576,  251,  250,  686,  251,  686,  591,
      251,  592,  577,  251,  250,  353,  578,  251,  309,  310,
      311,  312,  616,  355,  700,  251,  251,  587,  687,  608,
      249,  579,  580,  216,  581,  312,  250,  321,  585,  588,
      597,  251,  121,  598,  249,  498,  499,  500,  501,  122,
      123,  326,  708,  148,  326,  593,  326,  502,  326,  599,
      709,  326,  710,  326,  150,  150,  713,  585,  326,  597,

      714,  502,  598,  249,  602,  326,  619,  150,  250,  326,
      708,  321,  326,  872,  326,  250,  326,  599,  709,  326,
      710,  326,  250,  150,  150,  713,  326,  250,  714,  871,
      502,  250,  602,  326,  619,  150,  316,  316,  324,  316,
      316,  316,  316,  316,  316,  316,  316,  316,  316,  316,
      316,  316,  316,  316,  316,  603,  316,  316,  316,  316,
      316,  326,  600,  601,  326,  251,  535, 1627,  326,  669,
     1628,  326,  692,  693,  715,  716,  250,  605,  536,  720,
      537,  726,  251,  606,  603,  727,  670,  316,  316,  326,
      600,  601,  326,  225,  251,  535,  326,  920,  669,  326,

      692,  693,  715, 1152,  716,  605,  536,  720,  537,  726,
      251,  606, 1143,  727,  670,  316,  316,  316,  324,  316,
      316,  316,  316,  316,  316,  316,  316,  316,  316,  316,
      316,  316,  316,  316,  316,  250,  316,  316,  316,  316,
      316,  251,  340,  250,  340,  340,  250,  340,  352,  353,
      352,  728,  595,  697,  607,  697,  354,  355,  251,  596,
      339,  148,  717,  341,  717,  250,  250,  316,  316,  121,
      251,  608,  150,  150,  608,  698,  122,  123,  250,  728,
      595,  634,  607,  634,  718,  150,  251,  596,  250,  719,
      359,  359,  359,  359,  641,  316,  641,  635,  711,  250,

      608,  150,  150,  608,  360,  360,  361,  361,  624,  362,
      642,  712,  620,  150,  359,  359,  359,  359,  719,  250,
      499,  721,  722,  723,  725,  723,  636,  711,  360,  360,
      361,  361,  362,  362,  643,  379,  380,  381,  382,  712,
      620,  499,  363,  363,  237,  724,  422,  383,  422,  627,
      721,  722,  964,  725,  636,  384,  385,  384,  386,  628,
      731,  383,  643,  144,  388,  407,  363,  363,  651,  652,
      653,  654,  146,  368,  368,  407,  144,  627,  729,  730,
      383,  675,  689,  732,  689,  146,  368,  628,  731,  701,
      383,  676,  733,  736,  383, 1836,  737,  734,  744,  745,

      746,  738,  368,  368,  690,  735,  407,  729,  730,  739,
      747,  732,  748,  740,  368,  740,  408,  701,  691,  963,
      753,  733,  736,  383,  737,  734,  744,  745,  749,  746,
      738,  750,  751,  735,  407,  741,  752,  739,  754,  747,
      755,  748,  756,  757,  408,  679,  691,  679,  742,  753,
      758,  480,  760,  761,  762,  763,  749,  743,  764,  750,
      751,  765,  225,  225,  752,  225,  754,  680,  755,  769,
      756,  757,  681,  770,  772,  773,  742,  682,  758,  480,
      760,  761,  762,  763,  774,  743,  764,  683,  684,  765,
      250,  685,  766,  669,  775,  250,  767,  712,  769,  237,

      676,  681,  770,  772,  773,  675,  682,  250,  768,  779,
      771,  779,  248,  774,  250,  683,  684,  785,  250,  685,
      250,  766,  669,  775,  767,  712,  786,  250,  776,  777,
      237,  777,  779,  250,  780,  248,  768,  781,  771,  781,
      501,  498,  499,  500,  501,  781,  785,  782,  501,  250,
      790,  788,  787,  502,  792,  786,  250,  776,  789,  250,
      250,  778,  250,  250,  250,  250,  791,  502,  250,  250,
      407,  686,  250,  686,  250,  250,  250,  250,  790,  788,
      787,  250,  792,  250,  797,  801,  803,  789,  869,  793,
      778,  802,  656,  687,  791,  795,  502,  794,  796,  799,

      820,  798,  800,  805,  250,  804,  811, 1025,  821, 1025,
      250,  519,  920,  797,  801,  380,  869,  793,  813,  814,
      802,  250,  250,  795,  817,  794,  796,  799,  820,  798,
      800,  815,  805,  250,  804,  811,  821,  816,  250,  519,
      679,  250,  679,  689,  250,  689,  813,  814,  697,  250,
      697,  250,  817,  250,  250,  634,  818,  634,  819,  250,
      815,  250,  680,  250,  829,  690,  816,  806,  250,  250,
      698,  635,  807,  250,  250,  380,  717,  250,  717,  812,
      250,  823,  808,  809,  818,  822,  810,  819,  827,  250,
      825,  250,  829,  250,  828,  830,  806,  824,  718,  831,

      833,  807,  832,  826,  723,  250,  723,  812,  250,  823,
      808,  809,  250,  822,  810,  250,  834,  827,  835,  825,
      837,  841,  828,  250,  830,  824,  724,  836,  831,  833,
      832,  826,  250,  250,  250,  840,  250,  250,  250,  250,
      250,  250,  250,  250,  838,  834,  839,  835,  837,  841,
      914,  913,  250,  250,  911,  836,  740,  250,  740,  842,
      250,  250,  250,  843,  840,  846,  848,  910,  847,  844,
      854,  632,  838,  250,  849,  839,  250,  845,  741,  862,
      852,  853,  857,  858,  250,  250,  865,  842,  855,  856,
      632,  850,  843,  860,  846,  848,  847,  844,  859,  854,

      851,  123,  849,  317,  317,  845,  583,  862,  852,  853,
      857,  858,  250,  861,  865,  863,  864,  855,  856,  850,
      317,  860,  867,  216,  867,  312,  859,  866,  851,  867,
      216,  868,  312,  309,  310,  311,  312,  874,  591,  591,
      873,  591,  861,  863,  864,  249,  250,  875,  591,  877,
      873,  326,  250,  250,  609,  866,  871,  499,  880,  249,
      499,  915,  121,  915,  382,  874,  871,  585,  585,  122,
      123,  326,  246,  246,  326,  875,  889,  585,  877,  892,
      326,  888,  609,  321,  881,  872,  880,  121,  249,  584,
      216,  584,  217,  326, 1140, 1141,  585,  585,  326,  148,

      326,  876,  250,  326,  895,  889,  585,  882,  892,  888,
      150,  150,  881,  893,  887,  218,  219,  326,  585,  220,
      897,  221,  326,  150,  894,  883,  890,  326,  884,  896,
      244,  912,  473,  895,  115,  882, 1027,  225, 1027,  150,
      150,  893,  887,  759,  218,  219,  326,  585,  220,  897,
      221,  150,  894,  883,  115,  890,  884,  896,  899,  912,
      473,  316,  316,  324,  316,  316,  316,  316,  316,  316,
      316,  316,  316,  316,  316,  316,  316,  316,  316,  316,
      390,  316,  316,  316,  316,  316,  885,  899,  326,  621,
      921,  621,  898,  390, 1876,  922, 1876,  878,  121,  923,

      924,  656,  622,  886,  622,  122,  123, 1876,  359, 1876,
      359,  380,  316,  316,  359,  885,  359,  326,  921,  902,
      898,  902,  360,  922,  380,  878,  625,  923,  360,  924,
      903,  886, 1876,  250, 1876,  904,  645,  905,  927,  900,
      316,  316,  316,  324,  316,  316,  316,  316,  316,  316,
      316,  316,  316,  316,  316,  316,  316,  316,  316,  316,
      891,  316,  316,  316,  316,  316,  927,  900,  121,  704,
      879,  906,  321,  826,  321,  122,  123,  640,  705,  706,
      359,  641,  359,  641,  639,  925,  359,  926,  359,  891,
      930,  638,  316,  316,  360,  901,  928,  642,  624,  879,

      360,  826,  361,  359,  362,  359,  634,  641,  634,  641,
      915,  929,  916,  382,  925,  931,  926,  360,  930,  361,
      316,  362,  635,  642,  901,  928,  932,  637,  908,  933,
      934,  909,  363,  917,  385,  917,  654,  939,  363,  939,
      929,  144,  935,  931,  948,  651,  652,  653,  654,  949,
      146,  936,  937,  144,  932,  363,  908,  383,  933,  934,
      909,  938,  146,  951,  917,  385,  918,  654,  947,  940,
      935,  383,  144,  948,  384,  385,  384,  386,  949,  936,
      937,  146,  144,  941,  686,  950,  686,  952,  689,  938,
      689,  146,  951,  953,  954,  955,  947,  956,  940,  956,

      383,  958,  697,  959,  697,  959,  687,  961,  962,  968,
      690,  941,  679,  950,  679,  952,  965,  704,  965,  957,
      967,  953,  954,  955,  698,  960,  705,  706,  969,  966,
      958,  966,  970,  973,  680,  961,  962,  968,  966,  942,
      971,  625,  974,  975,  943,  976,  123,  972,  979,  967,
      980,  986,  987,  617,  944,  945,  969,  977,  946,  977,
      118,  970,  973,  717,  988,  717,  115,  989,  942,  971,
      974,  990,  975,  943,  976,  972,  991,  979,  980,  978,
      986,  987,  944,  945,  992,  718,  946,  981,  983,  981,
      983,  723,  988,  723,  966,  989,  993,  994,  317,  995,

      990,  996,  317, 1001,  991,  997,  740,  997,  740,  982,
      984, 1002,  992,  724,  999,  998,  999, 1003, 1004, 1005,
     1006,  225, 1007,  985,  993, 1008,  994,  995,  741, 1009,
      996, 1001, 1010, 1012, 1010, 1013, 1000, 1014, 1015, 1002,
     1016, 1017, 1018, 1019, 1020, 1003, 1004,  225, 1005, 1006,
     1007,  985,  225, 1008, 1011, 1024, 1028, 1026, 1009,  237,
      583, 1021, 1012,  250, 1013,  933, 1014, 1015, 1016,  237,
     1017, 1018, 1019, 1020,  777,  237,  777,  250,  246, 1023,
      971,  250, 1030, 1024, 1030, 1028, 1026,  972, 1022, 1021,
      250, 1029, 1031,  250,  933, 1035, 1032, 1035,  248, 1033,

     1035, 1032, 1036,  248, 1039, 1034,  778,  250, 1023,  971,
     1037,  250, 1037,  501, 1040,  972, 1022,  250, 1037, 1029,
     1038,  501, 1041,  250,  250,  250,  250, 1033,  250,  250,
     1048, 1045, 1039, 1034, 1044,  778, 1042,  250,  250,  250,
      250,  250, 1040,  250,  250,  250, 1043, 1046,  250,  250,
     1041,  250, 1047,  250,  250, 1052,  246, 1049, 1048, 1045,
     1051,  250, 1108, 1044,  250, 1042,  939,  250,  939, 1050,
      250, 1053, 1060,  250, 1055, 1043, 1046, 1056, 1059, 1063,
     1061, 1047, 1054,  493, 1052, 1049, 1065, 1062,  250, 1051,
     1108,  244, 1064, 1066, 1067,  250,  250, 1050, 1057, 1053,

     1068, 1060, 1055, 1069, 1070, 1056, 1059, 1071, 1063, 1061,
     1054,  250, 1058,  250, 1065, 1062,  956,  250,  956,  250,
     1064, 1066, 1067,  959,  250,  959, 1073, 1057,  250, 1068,
     1072, 1069, 1077, 1070, 1079, 1071,  250,  250,  957,  250,
     1058,  250, 1074,  250,  250,  960,  977,  250,  977, 1075,
     1076,  237,  981,  250,  981, 1073,  250, 1078, 1072,  225,
     1077, 1080,  250, 1079, 1084,  983,  250,  983,  978,  250,
     1087, 1074,  250, 1081,  982, 1083,  250, 1075, 1085, 1076,
      250,  250,  997,  250,  997,  250, 1078,  984,  250,  250,
     1080, 1086,  998, 1084,  999,  250,  999,  250,  250, 1087,

     1082, 1081, 1092, 1091, 1083,  250, 1085,  250,  250,  250,
      225, 1088, 1093,  250, 1089,  356, 1000, 1130, 1090, 1086,
      118,  347, 1095, 1098,  250, 1094,  115,  250, 1082, 1096,
     1092, 1157, 1091, 1097, 1099, 1102, 1010,  250, 1010, 1088,
     1103, 1093, 1089,  317, 1101, 1130, 1090,  316, 1100,  316,
     1095, 1104, 1098, 1094, 1105,  317,  244, 1096, 1011, 1157,
      321, 1097, 1111, 1099, 1102, 1106,  216, 1106,  312, 1103,
     1114, 1119, 1101,  237, 1876,  591, 1100, 1112, 1110,  591,
     1104, 1112,  317, 1105, 1106,  216, 1107,  312,  584,  216,
     1109,  217,  316, 1110,  316, 1158, 1113, 1110,   68, 1114,

     1119, 1131,  250,  316,  585,  316, 1110,  316,  585,  316,
      316,  321,  316, 1111,  218,  219, 1876,  585,  220, 1133,
      221, 1876, 1116, 1158, 1113, 1128,  359,  148,  359, 1115,
     1131,  473, 1876,  585, 1118, 1120, 1876,  585,  150,  150,
      360, 1121, 1159,  218,  219, 1122,  585,  220, 1133,  221,
     1116,  150, 1876,  115, 1128,  316, 1876,  316, 1153,  473,
     1153,  382, 1118, 1153, 1120, 1154,  382,  150,  150, 1121,
     1876, 1159, 1244, 1122, 1244,  248,  250, 1135, 1876,  150,
      316,  316,  324,  316,  316,  316,  316,  316,  316,  316,
      316,  316,  316,  316,  316,  316,  316,  316,  316, 1123,

      316,  316,  316,  316,  316, 1074, 1135, 1155,  385, 1155,
      654, 1161, 1075, 1160, 1164,  144, 1129, 1163, 1165, 1030,
      250, 1030, 1166, 1172,  146, 1167,  316, 1123,  316, 1031,
     1876,  316,  316, 1032, 1074, 1032, 1876, 1032, 1032, 1161,
     1075, 1876, 1160, 1164, 1129, 1163, 1165, 1876,  121, 1032,
     1124, 1166, 1172, 1167, 1032, 1324, 1325, 1168, 1125,  316,
      316, 1117,  324, 1117,  316,  316,  316,  316,  316,  316,
      316,  316,  316,  316,  316,  316,  316,  316,  316, 1124,
      316,  316,  316,  316,  316, 1168,  316, 1125,  316,  316,
     1876,  316,  121, 1187, 1408, 1187, 1408, 1169, 1876,  122,

      123, 1876,  902, 1170,  902,  902, 1171,  902,  359, 1175,
      359,  316,  316,  903, 1126, 1188,  903, 1127,  904, 1138,
     1142,  904,  360,  905,  361, 1169,  362, 1155,  385, 1156,
      654, 1170,  704, 1176, 1171,  144, 1145, 1876, 1175,  316,
     1177, 1196, 1197, 1126,  146,  997, 1127,  997, 1138, 1230,
     1244, 1230, 1245,  248,  906,  998, 1412,  906, 1412, 1231,
      363,  939, 1176,  939, 1876, 1145, 1132, 1146, 1177, 1146,
     1132, 1132, 1132, 1132, 1132, 1132, 1132, 1132, 1132, 1132,
     1132, 1147, 1132, 1148, 1132, 1149, 1132, 1132, 1132, 1132,
     1132, 1876, 1178, 1173,  651,  652,  653,  654, 1179, 1876,

     1180, 1181,  144, 1182, 1183, 1184,  383, 1174, 1185, 1186,
     1190,  146, 1189, 1199,  956, 1200,  956, 1132, 1132, 1151,
      383, 1178, 1173, 1202,  959, 1876,  959, 1179, 1180, 1876,
     1181, 1182, 1183, 1876, 1184, 1174,  957, 1185, 1186, 1190,
     1189, 1201, 1199, 1876, 1200, 1132,  960, 1876, 1876,  383,
     1132, 1132, 1202, 1132, 1132, 1132, 1132, 1132, 1132, 1132,
     1132, 1132, 1132, 1132, 1132, 1132, 1132, 1132, 1132, 1201,
     1132, 1132, 1132, 1132, 1132, 1191, 1193, 1191, 1193, 1203,
     1117,  317, 1117, 1204, 1205, 1876,  963,  964, 1192, 1194,
     1192, 1194, 1208,  965,  704,  965, 1209, 1192, 1194, 1210,

      704, 1132, 1132,  705,  706, 1211,  966, 1203,  966, 1196,
     1197, 1204, 1198, 1205, 1198,  966, 1206,  977, 1206,  977,
     1208, 1198,  981, 1214,  981, 1209, 1212, 1213, 1210, 1132,
      983, 1215,  983, 1216, 1211, 1217, 1876, 1218, 1207,  978,
     1219, 1220, 1876, 1224,  982,  999, 1221,  999, 1222,  225,
     1223, 1214,  984, 1192, 1194, 1212, 1213, 1225, 1226, 1232,
     1215, 1227, 1216, 1228, 1217, 1218, 1229, 1000, 1233, 1219,
     1220,  966, 1224, 1010, 1221, 1010, 1222, 1198, 1223, 1234,
     1235,  225, 1030, 1238, 1030, 1225, 1239, 1226, 1232, 1227,
     1240, 1228, 1031, 1241, 1229, 1011, 1032, 1233,  237,  237,

     1030, 1032, 1030, 1246,  250, 1246,  501, 1234, 1236, 1235,
     1031,  250, 1238,  250, 1032, 1239,  250,  250, 1240, 1032,
      326, 1246, 1241, 1247,  501,  250, 1187,  250, 1187, 1242,
     1206,  250, 1206, 1876, 1876, 1304, 1251, 1236,  326, 1318,
     1876, 1243,  250, 1250, 1337, 1248, 1337, 1249, 1188,  326,
     1255, 1876, 1207, 1252, 1230,  250, 1230, 1876, 1242, 1302,
      216, 1302,  312, 1304, 1231, 1251, 1338,  326, 1318, 1243,
      250,  250, 1250, 1248,  250, 1249,  250,  250,  250, 1255,
     1254, 1252, 1132, 1132,  250, 1132, 1132, 1132, 1132, 1132,
     1132, 1132, 1132, 1132, 1132, 1132, 1132, 1132, 1132, 1132,

     1132, 1257, 1132, 1132, 1132, 1132, 1132, 1256, 1254,  250,
     1258,  250, 1259,  250, 1260,  250,  250,  250,  250, 1262,
      250,  250,  250,  250,  250,  250,  250,  250, 1876,  250,
     1257,  250,  250, 1132, 1132, 1256,  250,  250, 1258, 1264,
     1259, 1269, 1260, 1261, 1263, 1265, 1876, 1262, 1268,  250,
     1266, 1267, 1270, 1272, 1273, 1876, 1271, 1274,  250, 1276,
     1281, 1132, 1278,  250, 1275, 1279,  250, 1277, 1264, 1280,
     1269, 1261,  250, 1263, 1265,  250, 1282, 1268, 1266, 1267,
      250, 1270, 1272, 1273, 1271, 1287, 1274,  250, 1276, 1281,
     1278, 1284, 1275, 1288, 1279, 1277,  250, 1280,  250,  250,

      250, 1285,  250, 1283,  250, 1282,  250,  250,  250,  250,
      250,  359,  250,  359, 1287,  902, 1286,  902, 1289, 1344,
     1284, 1291, 1288, 1876, 1292,  360,  903,  326, 1296, 1230,
     1285, 1230, 1283, 1313,  591, 1298,  591,  317, 1290, 1231,
      317, 1293, 1295,  591, 1286,  591, 1294, 1289, 1344, 1297,
     1291, 1299, 1300, 1292, 1301, 1876,  326, 1296, 1302,  216,
     1303,  312, 1313,  585, 1298,  326, 1290, 1305, 1307, 1293,
     1295,  326,  585, 1311, 1294,  250, 1310, 1297, 1876, 1299,
     1300, 1876, 1301,  584,  216,  584,  217,  250, 1412,  321,
     1412,  321,  585,  326,  326,  326, 1305, 1307, 1876, 1876,

      326,  585, 1311, 1317, 1310,  148, 1314,  149, 1312,  218,
      219,  121,  585,  220, 1349,  221,  150,  150,  122,  123,
      121, 1316,  326, 1342,  326, 1876,  473,  122,  123,  150,
     1876, 1468, 1317, 1468, 1314,  621, 1312,  621,  218,  219,
     1876,  585,  220, 1349,  221,  150,  150, 1876,  622, 1316,
      622, 1342, 1876, 1333,  473, 1333,  382,  150,  316,  316,
      324,  316,  316,  316,  316,  316,  316,  316,  316,  316,
      316,  316,  316,  316,  316,  316,  316, 1876,  316,  316,
      316,  316,  316,  902,  902,  902,  902, 1308, 1333, 1343,
     1334,  382, 1345, 1351,  903,  903, 1339, 1414, 1339, 1414,

      501, 1142, 1143, 1346, 1347,  359, 1348,  359, 1350,  316,
      316,  359,  359,  359,  359, 1876, 1308, 1343, 1340,  360,
     1345,  361, 1351,  624, 1876,  360,  360,  361,  361,  362,
      362, 1346, 1347, 1876, 1348,  906, 1350,  316,  316,  316,
      324,  316,  316,  316,  316,  316,  316,  316,  316,  316,
      316,  316,  316,  316,  316,  316,  316,  363,  316,  316,
      316,  316,  316,  363,  363, 1309, 1335,  385, 1335,  654,
     1335,  385, 1336,  654,  144, 1355, 1356, 1357,  144, 1358,
     1359, 1360, 1876,  146, 1363, 1361, 1362,  146, 1364,  316,
      316,  963, 1876, 1192, 1309, 1192, 1414, 1876, 1415,  501,

     1876, 1468, 1192, 1468, 1355, 1356, 1357, 1358, 1359, 1876,
     1360, 1352, 1363, 1352, 1361, 1362, 1364,  316, 1306, 1306,
      324, 1306, 1306, 1306, 1306, 1306, 1306, 1306, 1306, 1306,
     1306, 1306, 1306, 1306, 1306, 1306, 1306, 1353, 1306, 1306,
     1306, 1306, 1306, 1365, 1367, 1365, 1187, 1368, 1187, 1191,
     1354, 1191, 1375, 1372, 1876, 1373, 1374, 1387, 1192, 1376,
      963, 1193, 1192, 1193, 1192, 1366, 1353, 1377, 1188, 1306,
     1306, 1192,  964, 1367, 1194, 1368, 1194, 1369, 1354, 1370,
     1375, 1370, 1372, 1194, 1373, 1374, 1387, 1376, 1370, 1455,
      216, 1455,  217, 1876, 1391, 1377, 1380, 1306, 1319, 1327,

     1393, 1327, 1319, 1319, 1319, 1319, 1319, 1319, 1319, 1319,
     1319, 1319, 1319, 1328, 1319, 1329, 1319, 1330, 1319, 1319,
     1319, 1319, 1319, 1391, 1380, 1383, 1206, 1192, 1206, 1393,
     1371,  704, 1371, 1384, 1388, 1378, 1381, 1378, 1381, 1194,
     1196, 1197, 1389, 1198, 1370, 1198,  704, 1401, 1207, 1319,
     1319, 1332, 1198, 1383, 1876, 1196, 1197, 1379, 1198, 1876,
     1198, 1384, 1388, 1455,  216, 1455, 1456, 1198, 1876, 1390,
     1389, 1408,  250, 1408, 1876, 1382, 1401, 1319, 1319, 1319,
     1876, 1319, 1319, 1319, 1319, 1319, 1319, 1319, 1319, 1319,
     1319, 1319, 1319, 1319, 1319, 1319, 1319, 1390, 1319, 1319,

     1319, 1319, 1319, 1382, 1385, 1392, 1385,  237, 1198, 1394,
     1396, 1394, 1396, 1400, 1398, 1402, 1398, 1403, 1876,  225,
     1406,  250, 1407, 1198, 1409,  250, 1386,  237,  250, 1319,
     1319, 1876, 1397, 1392, 1411, 1395, 1399, 1337,  250, 1337,
     1457, 1400, 1474, 1402, 1419, 1876, 1403, 1404,  250, 1406,
     1407, 1434, 1429, 1409,  902, 1410,  902, 1319,  250, 1338,
     1339,  250, 1339, 1411, 1395,  903, 1876, 1876,  250, 1457,
      250, 1474,  250, 1419,  250,  250, 1404, 1876, 1876,  250,
     1434, 1429, 1340, 1418, 1410, 1319, 1319,  250, 1319, 1319,
     1319, 1319, 1319, 1319, 1319, 1319, 1319, 1319, 1319, 1319,

     1319, 1319, 1319, 1319, 1421, 1319, 1319, 1319, 1319, 1319,
     1417, 1418, 1420,  250, 1422, 1423,  250, 1425,  250, 1432,
     1352,  250, 1352,  250,  250,  250,  250, 1365,  250, 1365,
      250,  250, 1421,  250,  250,  250, 1319, 1319, 1417,  250,
     1420, 1424, 1422, 1423, 1426, 1425, 1427, 1432,  250, 1366,
     1430, 1431, 1381,  250, 1381, 1435, 1436, 1437,  250, 1428,
     1433, 1438,  250,  250, 1319,  250,  250, 1378,  250, 1378,
     1424, 1439,  250, 1426, 1440, 1427,  250, 1441,  317, 1430,
     1431, 1447, 1442, 1435, 1475, 1436, 1437, 1428, 1433, 1379,
     1438, 1443, 1385,  250, 1385, 1446,  250,  250, 1448, 1439,

     1449, 1451, 1440, 1444, 1876, 1441, 1459, 1452, 1876, 1447,
     1442, 1876, 1445, 1475, 1386, 1394,  250, 1394, 1462, 1443,
     1396,  250, 1396, 1450, 1446, 1463, 1448, 1464, 1449, 1451,
     1465, 1444, 1398,  250, 1398, 1459, 1452, 1467,  250, 1454,
     1445, 1453, 1397,  121, 1876,  359, 1462,  359, 1876,  121,
      122,  123, 1450, 1463, 1399, 1464,  122,  123, 1465,  360,
     1408, 1466, 1408, 1876, 1408, 1467, 1408, 1454, 1476, 1876,
     1453, 1458, 1458,  324, 1458, 1458, 1458, 1458, 1458, 1458,
     1458, 1458, 1458, 1458, 1458, 1458, 1458, 1458, 1458, 1458,
     1466, 1458, 1458, 1458, 1458, 1458,  621, 1476,  621, 1470,

      385, 1470,  654, 1480, 1477, 1876, 1337,  144, 1337,  622,
     1518,  622, 1518, 1339, 1876, 1339,  146, 1478,  359, 1479,
      359, 1483, 1458, 1458,  359,  359,  359,  359, 1338, 1876,
     1876, 1480,  360, 1477,  361, 1340,  624, 1876,  360,  360,
      361,  361,  362,  362, 1876, 1518, 1478, 1518, 1479, 1483,
     1458,  316,  316,  324,  316,  316,  316,  316,  316,  316,
      316,  316,  316,  316,  316,  316,  316,  316,  316,  316,
      363,  316,  316,  316,  316,  316,  363,  363, 1484, 1470,
      385, 1471,  654, 1485, 1472, 1876, 1486,  144, 1473, 1487,
     1488, 1876, 1489, 1490, 1667, 1461,  146, 1493, 1491, 1492,

     1876, 1668,  316,  316, 1669, 1876, 1484, 1365, 1517, 1365,
     1517,  248, 1485, 1472, 1486, 1876, 1876, 1473, 1487, 1488,
     1489, 1876, 1490, 1461, 1352, 1493, 1352, 1491, 1492, 1366,
      316,  316,  316,  324,  316,  316,  316,  316,  316,  316,
      316,  316,  316,  316,  316,  316,  316,  316,  316,  316,
     1481,  316,  316,  316,  316,  316, 1494, 1369, 1495, 1370,
     1495, 1370, 1876, 1482, 1496, 1497, 1498, 1503, 1370, 1369,
     1500, 1370, 1381, 1370, 1381, 1371,  704, 1371, 1501, 1481,
     1370, 1876,  316,  316, 1494, 1196, 1197, 1378, 1198, 1378,
     1198, 1482, 1496, 1507, 1497, 1498, 1503, 1198, 1500, 1385,

     1502, 1385, 1504, 1505, 1508, 1396, 1501, 1396, 1510, 1379,
      316, 1499, 1394, 1398, 1394, 1398, 1509, 1513, 1514,  237,
      250, 1386, 1507,  250, 1370,  250,  250, 1397, 1502,  250,
     1504, 1505,  250, 1508,  250, 1399, 1370, 1510, 1506, 1499,
      250,  250,  250, 1527, 1509, 1513, 1520, 1514,  250,  250,
     1521, 1523, 1522, 1198,  250,  250, 1524,  250,  250, 1516,
      250, 1525,  250,  250,  250,  250, 1535, 1506, 1526,  250,
      250,  250, 1527, 1549, 1876, 1520,  250, 1528, 1876, 1521,
     1523, 1522, 1530, 1532, 1533, 1524, 1529, 1516, 1531, 1534,
     1525, 1536, 1551, 1539, 1535, 1553, 1537, 1526,  250, 1542,

     1538, 1549, 1541,  250, 1540, 1528, 1544, 1547,  250, 1814,
     1530, 1814, 1532, 1533, 1529, 1545, 1531, 1552, 1534, 1536,
     1554, 1551, 1539, 1553, 1537, 1543, 1546, 1542, 1876, 1538,
     1541, 1564, 1540, 1555, 1544, 1565, 1547, 1548,  216, 1548,
      312, 1566, 1556, 1545, 1556, 1558, 1552, 1558,  382, 1554,
     1876, 1876, 1557, 1567, 1543, 1546, 1559,  143, 1559, 1568,
     1564, 1569, 1555, 1565,  144, 1574, 1560,  385, 1560,  386,
     1566, 1570, 1571,  146,  144, 1572, 1560,  385, 1560, 1561,
     1573, 1567, 1575,  146, 1562, 1576, 1577, 1568, 1578, 1569,
     1579, 1580, 1581, 1563, 1574, 1584, 1876, 1585, 1876, 1570,

     1571, 1586, 1591, 1572, 1582, 1587, 1582, 1587, 1573, 1495,
     1575, 1495, 1589, 1590, 1576, 1577, 1592, 1578, 1579, 1580,
     1369, 1581, 1370, 1584, 1370, 1585, 1583, 1588, 1593, 1586,
     1591, 1370, 1594,  225, 1596, 1597,  250,  250,  237,  237,
      250, 1589, 1590, 1517, 1592, 1517,  248, 1600,  250, 1600,
      501,  250,  250, 1876,  250,  250, 1593,  250,  250, 1595,
     1602, 1594,  250, 1596, 1597, 1598,  250, 1599,  250,  250,
      250, 1623,  250, 1603,  317, 1604, 1876, 1614,  250, 1876,
     1605, 1582,  250, 1582,  250, 1610, 1876, 1370, 1595, 1602,
     1606, 1608, 1609, 1607, 1598, 1611, 1599, 1876, 1613, 1623,

     1622, 1603, 1619, 1583, 1604, 1617, 1614, 1615, 1605, 1876,
     1876, 1612, 1616, 1876, 1610, 1587,  250, 1587, 1606, 1608,
     1609, 1607, 1624, 1876, 1611, 1618, 1613,  250, 1876, 1622,
     1619, 1559, 1876, 1559, 1617, 1615, 1620, 1588, 1620, 1612,
     1616, 1548,  216, 1548,  312, 1556, 1621, 1556,  146, 1558,
     1624, 1558,  382, 1618, 1625, 1557, 1629,  385, 1629,  654,
     1630,  385, 1630, 1636,  144, 1633, 1634, 1638, 1631, 1635,
     1637, 1639, 1640,  146, 1559,  143, 1559, 1632,  384,  385,
      384,  386,  144, 1625, 1641, 1645,  144, 1644, 1646, 1647,
     1649,  146, 1636, 1633, 1634,  146, 1638, 1635, 1637, 1639,

     1642, 1640, 1642, 1582, 1587, 1582, 1587, 1648, 1650, 1651,
     1643,  225, 1653, 1641, 1645, 1644, 1646,  237, 1647, 1649,
      237, 1600,  250, 1600,  501, 1583, 1588,  250,  250,  250,
      250,  317, 1671,  250,  250, 1648,  250, 1650, 1651, 1620,
     1653, 1620, 1876, 1655, 1642,  250, 1642,  250, 1652, 1621,
      250, 1681, 1876, 1654, 1643, 1683, 1656, 1659, 1661, 1662,
     1671, 1663, 1876, 1629,  385, 1629,  654, 1660, 1670, 1657,
     1658,  144, 1655, 1876, 1664, 1678, 1652, 1665, 1876, 1681,
      146, 1654, 1876, 1683, 1656, 1682, 1659, 1661, 1662, 1876,
     1663, 1672, 1673, 1672, 1674, 1660, 1670, 1657, 1658, 1627,

     1684,  250, 1628, 1664, 1678, 1685, 1665, 1630,  656, 1630,
     1675, 1676, 1675, 1677, 1682, 1679, 1686, 1679, 1631, 1687,
     1642, 1642, 1642, 1642, 1632, 1680, 1688, 1632,  237, 1684,
     1643, 1643,  237,  250, 1685, 1694, 1679,  250, 1679,  250,
      250,  250, 1667, 1667, 1876, 1686, 1680, 1713, 1687, 1668,
     1668, 1673, 1669, 1669, 1691, 1688, 1876, 1692, 1708, 1712,
     1693, 1709, 1714, 1694, 1876, 1876, 1876, 1696, 1697, 1876,
     1695, 1876, 1698, 1699, 1700, 1701, 1713, 1703, 1704, 1703,
     1705,  225, 1876, 1691, 1702, 1668, 1692, 1712, 1669, 1693,
     1876, 1714, 1672, 1673, 1672, 1674, 1696, 1697, 1702, 1695,

     1627, 1715, 1716, 1628, 1672, 1673, 1672, 1674, 1675, 1676,
     1675, 1677, 1627, 1717,  237, 1628, 1631, 1718,  651,  652,
      653,  654, 1679, 1719, 1679, 1632,  144, 1702,  250, 1715,
      383, 1716, 1680,  250, 1710,  146, 1710,  250, 1743,  317,
     1876, 1721, 1717, 1746,  383, 1718, 1876, 1711, 1744, 1711,
     1725, 1719, 1725, 1701, 1745, 1722, 1711, 1698, 1699, 1700,
     1701, 1704, 1725, 1724, 1726, 1701, 1876, 1743, 1735, 1702,
     1721, 1736, 1746,  383, 1723, 1737, 1744, 1703, 1704, 1703,
     1705, 1747, 1745, 1702, 1722, 1668,  250,  225, 1669, 1876,
     1673,  250, 1724, 1729, 1730, 1731, 1732, 1708,  250, 1748,

     1709, 1668, 1723, 1737, 1669, 1733, 1738, 1739, 1738, 1740,
     1747, 1673, 1702, 1710, 1708, 1710, 1750, 1709, 1708, 1733,
      237, 1709, 1755, 1741, 1749, 1741, 1711, 1748, 1711, 1772,
     1774, 1741, 1754, 1741, 1742, 1711, 1711, 1757, 1711, 1757,
     1701, 1756, 1742, 1773, 1750, 1711, 1751, 1757, 1733, 1758,
     1701, 1755, 1749, 1759, 1704, 1759, 1732, 1772, 1774,  317,
     1754, 1668,  225, 1876, 1669, 1729, 1730, 1731, 1732, 1756,
      237,  250, 1773, 1668, 1775, 1751, 1669, 1733, 1759, 1704,
     1760, 1732, 1876, 1730, 1776, 1765, 1668, 1704, 1876, 1669,
     1762, 1733, 1766, 1763, 1735, 1767, 1771, 1736, 1768, 1769,

     1768, 1770, 1775, 1704, 1777, 1778, 1735, 1779, 1780, 1736,
     1735, 1673, 1776, 1736, 1738, 1739, 1738, 1740, 1708,  250,
     1733, 1709, 1708, 1876, 1771, 1709, 1672, 1673, 1672, 1674,
     1876, 1795, 1777, 1778, 1627, 1779, 1780, 1628, 1782, 1876,
     1782, 1701, 1782, 1810, 1783, 1701, 1784, 1704, 1784, 1732,
     1784, 1704, 1785, 1732, 1668, 1730,  317, 1669, 1668, 1781,
     1795, 1669, 1762, 1730, 1792, 1763, 1786, 1787, 1786, 1788,
     1762, 1765, 1810, 1763, 1762,  250, 1876, 1763, 1766,  225,
     1830, 1767, 1698, 1699, 1700, 1701, 1793, 1781, 1789, 1730,
     1789, 1790, 1792, 1765, 1702,  250, 1766, 1794, 1791, 1767,

     1766,  225, 1809, 1767, 1768, 1769, 1768, 1770, 1702, 1830,
     1812, 1799, 1735, 1704, 1793, 1736, 1729, 1730, 1731, 1732,
     1735, 1811, 1800, 1736, 1668, 1794, 1791, 1669, 1733, 1801,
     1809, 1801, 1701, 1801,  237, 1802, 1701, 1702, 1812, 1799,
     1821, 1820, 1733, 1730, 1803, 1704, 1803, 1732, 1876, 1811,
     1762, 1800, 1668, 1763, 1839, 1669, 1803, 1704, 1804, 1732,
     1876,  317,  237, 1823, 1668, 1823, 1701, 1669, 1821, 1820,
     1813, 1733, 1786, 1787, 1786, 1788, 1729, 1730, 1731, 1732,
     1762, 1876, 1839, 1763, 1668, 1876, 1730, 1669, 1733, 1789,
     1730, 1789, 1790, 1806,  317, 1835, 1807, 1766, 1813, 1822,

     1767, 1828, 1733, 1815, 1704, 1815, 1732, 1815, 1704, 1816,
     1732, 1668, 1730, 1730, 1669, 1668, 1876, 1876, 1669, 1806,
     1806,  237, 1807, 1807, 1835, 1876,  225, 1822, 1876, 1828,
     1876, 1733, 1876, 1814, 1876, 1814, 1819, 1876,  225, 1730,
     1876, 1876, 1876, 1876, 1876, 1876, 1806, 1876, 1876, 1807,
     1876,  317, 1876, 1829, 1876, 1876, 1876, 1817, 1787, 1817,
     1818, 1834, 1876, 1831, 1819, 1806, 1876, 1876, 1807, 1824,
     1704, 1824, 1705, 1823, 1876, 1823, 1701, 1668, 1833, 1876,
     1669,  317, 1829, 1876, 1876, 1876, 1817, 1787, 1817, 1818,
     1834, 1831, 1876, 1876, 1806, 1704, 1876, 1807, 1832, 1704,

     1832, 1732, 1735,  237, 1837, 1736, 1668, 1833,  225, 1669,
     1876, 1876, 1666, 1824, 1704, 1824, 1825, 1666,  237, 1667,
     1666, 1827, 1666, 1666, 1669,  317, 1668, 1666, 1666, 1669,
     1860, 1876, 1666, 1837, 1666, 1666, 1666, 1703, 1704, 1703,
     1705, 1832, 1704, 1832, 1732, 1668,  237, 1876, 1669, 1668,
     1838, 1876, 1669, 1842, 1843, 1842, 1844, 1876, 1876, 1860,
     1840,  237, 1876, 1666, 1666, 1666, 1876, 1841, 1845, 1846,
     1845, 1847, 1849, 1850, 1849, 1851, 1876, 1876, 1838, 1842,
     1843, 1842, 1844, 1876, 1864, 1876, 1848, 1876, 1840, 1876,
     1876, 1666, 1876, 1876, 1876, 1841, 1852, 1853, 1852, 1854,

     1845, 1846, 1845, 1847, 1857, 1846, 1857, 1847, 1849, 1850,
     1849, 1851,  237, 1864, 1848, 1857, 1846, 1857, 1847, 1852,
     1853, 1852, 1854, 1852, 1853, 1852, 1854, 1852, 1853, 1852,
     1854, 1857, 1846, 1857, 1847, 1857, 1846, 1857, 1847, 1857,
     1846, 1857, 1847, 1866, 1867, 1866, 1868, 1866, 1867, 1866,
     1868, 1876, 1876, 1876, 1865, 1869, 1870, 1869, 1871, 1869,
     1870, 1869, 1871, 1869, 1870, 1869, 1871, 1869, 1870, 1869,
     1871, 1869, 1870, 1869, 1871, 1876, 1876, 1876, 1876, 1876,
     1876, 1876, 1865,   66,   66,   66,   66,   66,   66,   66,
       66,   66,   66,   66,   66,   66,   66,   66,   66,   66,

       66,   67,   67,   67,   67,   67,   67,   67,   67,   67,
       67,   67,   67,   67,   67,   67,   67,   67,   67,  111,
      111, 1876,  111,  111,  111,  111,  111,  111,  111,  111,
      111,  111,  111,  111,  111,  111,  111,  114,  114,  114,
      114,  114,  114,  114,  114,  114,  114,  114,  114,  114,
      114,  114,  114,  114,  114,  120,  120,  120,  120,  120,
      120,  120,  120,  120,  120,  120,  120,  120,  120,  120,
      120,  120,  120,  129, 1876, 1876, 1876, 1876, 1876, 1876,
      129, 1876,  129, 1876,  129,  129,  129,  129,  129,  156,
      156,  156,  156,  156,  224,  224,  224,  224,  224,  224,

      224,  224,  224,  224,  224,  224,  224,  224,  224,  224,
      224,  224,  236,  236,  236,  236,  236,  236,  236,  236,
      236,  236,  236,  236,  236,  236,  236,  236,  236,  236,
      243,  243,  243,  243,  243,  243,  243,  243,  243,  243,
      243,  243,  243,  243,  243,  243,  243,  243,  251, 1876,
     1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876,  251,
      251,  251,  251,  251,  316,  316,  316,  316,  316,  316,
      316,  316,  316,  316,  316,  316,  316,  316,  316,  316,
      316,  316,  111,  111, 1876,  111,  111,  111,  111,  111,
      111,  111,  111,  111,  111,  111,  111,  111,  111,  111,

      114,  114,  114,  114,  114,  114,  114,  114,  114,  114,
      114,  114,  114,  114,  114,  114,  114,  114,  350,  350,
      350,  350,  350,  350,  350,  350,  350,  350,  350,  350,
      350,  350,  350,  350,  350,  350,  120,  120,  120,  120,
      120,  120,  120,  120,  120,  120,  120,  120,  120,  120,
      120,  120,  120,  120,  351,  351,  351,  351,  351,  351,
      351,  351,  351,  351,  351,  351,  351,  351,  351,  351,
      351,  351,  129, 1876, 1876, 1876, 1876, 1876, 1876,  129,
     1876,  129, 1876, 1876,  129,  129,  129,  129,  387,  387,
      387,  387, 1876,  387,  387,  387,  387,  387,  387, 1876,

      387,  387, 1876, 1876,  387,  387,  156,  156,  156,  156,
      156,  474,  474,  474,  474,  474,  474,  474,  474,  474,
      474,  474,  474,  474,  474,  474,  474,  474,  474,  224,
      224,  224,  224,  224,  224,  224,  224,  224,  224,  224,
      224,  224,  224,  224,  224,  224,  224,  236,  236,  236,
      236,  236,  236,  236,  236,  236,  236,  236,  236,  236,
      236,  236,  236,  236,  236,  243,  243,  243,  243,  243,
      243,  243,  243,  243,  243,  243,  243,  243,  243,  243,
      243,  243,  243,  496,  496,  496,  496,  496,  496,  496,
      496,  496,  496,  496,  496,  496,  496,  496,  496,  496,

      496,  497,  497,  497,  497,  497,  497,  497,  497,  497,
      497,  497,  497,  497,  497,  497,  497,  497,  497,  582,
      582,  582,  582,  582,  582,  582,  582,  582,  582,  582,
      582,  582,  582,  582,  582,  582,  582,  316,  316,  316,
      316,  316,  316,  316,  316,  316,  316,  316,  316,  316,
      316,  316,  316,  316,  316,  326,  326,  326,  326,  326,
      326,  326,  326,  326,  326,  326,  326,  326,  326,  326,
      326,  326,  326,  111,  111, 1876,  111,  111,  111,  111,
      111,  111,  111,  111,  111,  111,  111,  111,  111,  111,
      111,  114,  114,  114,  114,  114,  114,  114,  114,  114,

      114,  114,  114,  114,  114,  114,  114,  114,  114,  350,
      350,  350,  350,  350,  350,  350,  350,  350,  350,  350,
      350,  350,  350,  350,  350,  350,  350,  351,  351,  351,
      351,  351,  351,  351,  351,  351,  351,  351,  351,  351,
      351,  351,  351,  351,  351,  618,  618,  618,  618,  618,
      618,  618,  618,  618,  618,  618,  618,  618,  618,  618,
      618,  618,  618,  120,  120,  120,  120,  120,  120,  120,
      120,  120,  120,  120,  120,  120,  120,  120,  120,  120,
      120,  623, 1876, 1876, 1876, 1876, 1876, 1876,  623, 1876,
      623, 1876, 1876,  623,  623,  623,  623,  129, 1876, 1876,

     1876, 1876, 1876, 1876, 1876,  129, 1876,  129, 1876,  129,
      129,  129,  129,  129,  626,  626,  626,  626,  649,  649,
      649,  649,  649,  649,  649,  649,  649,  649,  649,  649,
      649,  649,  649,  649,  649,  649,  650,  650,  650,  650,
      650,  650,  650,  650,  650,  650,  650,  650,  650,  650,
      650,  650,  650,  650,  655,  655,  655,  655,  655,  655,
      655,  655,  655,  655,  655,  655,  655,  655,  655,  655,
      655,  655,  387,  387,  387,  387, 1876,  387,  387,  387,
      387,  387,  387, 1876,  387,  387, 1876, 1876,  387,  387,
      156,  156,  156,  156,  156,  703,  703,  703,  703,  703,

      703,  703,  703,  703,  703,  703,  703,  703,  703,  703,
      703,  703,  703,  472, 1876, 1876, 1876, 1876, 1876, 1876,
     1876,  472,  472,  474,  474,  474,  474,  474,  474,  474,
      474,  474,  474,  474,  474,  474,  474,  474,  474,  474,
      474,  224,  224,  224,  224,  224,  224,  224,  224,  224,
      224,  224,  224,  224,  224,  224,  224,  224,  224,  236,
      236,  236,  236,  236,  236,  236,  236,  236,  236,  236,
      236,  236,  236,  236,  236,  236,  236,  243,  243,  243,
      243,  243,  243,  243,  243,  243,  243,  243,  243,  243,
      243,  243,  243,  243,  243,  496,  496,  496,  496,  496,

      496,  496,  496,  496,  496,  496,  496,  496,  496,  496,
      496,  496,  496,  497,  497,  497,  497,  497,  497,  497,
      497,  497,  497,  497,  497,  497,  497,  497,  497,  497,
      497,  783,  783,  783,  783,  783,  783,  783,  783,  783,
      783,  783,  783,  783,  783,  783,  783,  783,  783,  784,
      784,  784,  784,  784,  784,  784,  784,  784,  784,  784,
      784,  784,  784,  784,  784,  784,  784,  251, 1876, 1876,
     1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876,  251,  251,
      251,  251,  251,  582,  582,  582,  582,  582,  582,  582,
      582,  582,  582,  582,  582,  582,  582,  582,  582,  582,

      582,  316,  316,  316,  316,  316,  316,  316,  316,  316,
      316,  316,  316,  316,  316,  316,  316,  316,  316,  326,
      326,  326,  326,  326,  326,  326,  326,  326,  326,  326,
      326,  326,  326,  326,  326,  326,  326,  111,  111, 1876,
      111,  111,  111,  111,  111,  111,  111,  111,  111,  111,
      111,  111,  111,  111,  111,  114,  114,  114,  114,  114,
      114,  114,  114,  114,  114,  114,  114,  114,  114,  114,
      114,  114,  114,  351,  351,  351,  351,  351,  351,  351,
      351,  351,  351,  351,  351,  351,  351,  351,  351,  351,
      351,  120,  120,  120,  120,  120,  120,  120,  120,  120,

      120,  120,  120,  120,  120,  120,  120,  120,  120,  618,
      618,  618,  618,  618,  618,  618,  618,  618,  618,  618,
      618,  618,  618,  618,  618,  618,  618,  623, 1876, 1876,
     1876, 1876, 1876, 1876,  623, 1876,  623, 1876, 1876,  623,
      623,  623,  623,  907, 1876, 1876, 1876, 1876, 1876, 1876,
     1876,  907, 1876, 1876, 1876,  907,  907,  907,  907,  907,
      129, 1876, 1876, 1876, 1876, 1876, 1876, 1876,  129, 1876,
      129, 1876,  129,  129,  129,  129,  129,  649,  649,  649,
      649,  649,  649,  649,  649,  649,  649,  649,  649,  649,
      649,  649,  649,  649,  649,  650,  650,  650,  650,  650,

      650,  650,  650,  650,  650,  650,  650,  650,  650,  650,
      650,  650,  650,  919,  919,  919,  919,  919,  919,  919,
      919,  919,  919,  919,  919,  919,  919,  919,  919,  919,
      919,  655,  655,  655,  655,  655,  655,  655,  655,  655,
      655,  655,  655,  655,  655,  655,  655,  655,  655,  156,
      156,  156,  156,  156,  703,  703,  703,  703,  703,  703,
      703,  703,  703,  703,  703,  703,  703,  703,  703,  703,
      703,  703,  704,  704,  704,  704,  704,  704, 1876,  704,
      704,  704,  704,  704,  704,  704,  704,  704,  704,  704,
      705,  705, 1876,  705,  705,  705,  705,  705,  705,  705,

      705,  705,  705,  705,  705,  705,  705,  705,  224,  224,
      224,  224,  224,  224,  224,  224,  224,  224,  224,  224,
      224,  224,  224,  224,  224,  224,  236,  236,  236,  236,
      236,  236,  236,  236,  236,  236,  236,  236,  236,  236,
      236,  236,  236,  236,  783,  783,  783,  783,  783,  783,
      783,  783,  783,  783,  783,  783,  783,  783,  783,  783,
      783,  783,  784,  784,  784,  784,  784,  784,  784,  784,
      784,  784,  784,  784,  784,  784,  784,  784,  784,  784,
      316,  316,  316,  316,  316,  316,  316,  316,  316,  316,
      316,  316,  316,  316,  316,  316,  316,  316,  326,  326,

      326,  326,  326,  326,  326,  326,  326,  326,  326,  326,
      326,  326,  326,  326,  326,  326, 1132, 1132, 1876, 1132,
     1132, 1132, 1132, 1132, 1132, 1132, 1132, 1132, 1132, 1132,
     1132, 1132, 1132, 1132,  111,  111, 1876,  111,  111,  111,
      111,  111,  111,  111,  111,  111,  111,  111,  111,  111,
      111,  111, 1134, 1134, 1876, 1134, 1134, 1134, 1134, 1134,
     1134, 1134, 1134, 1134, 1134, 1134, 1134, 1134, 1134, 1134,
      114,  114,  114,  114,  114,  114,  114,  114,  114,  114,
      114,  114,  114,  114,  114,  114,  114,  114, 1136, 1136,
     1136, 1136, 1136, 1136, 1136, 1136, 1136, 1136, 1136, 1136,

     1136, 1136, 1136, 1136, 1136, 1136,  120,  120,  120,  120,
      120,  120,  120,  120,  120,  120,  120,  120,  120,  120,
      120,  120,  120,  120, 1139, 1139, 1139, 1139, 1139, 1139,
     1139, 1139, 1139, 1139, 1139, 1139, 1139, 1139, 1139, 1139,
     1139, 1139,  623, 1876, 1876, 1876, 1876, 1876,  623, 1876,
     1876, 1876,  623, 1876,  623,  623,  623,  623,  623, 1144,
     1144, 1144, 1144,  907, 1876, 1876, 1876, 1876, 1876, 1876,
     1876,  907, 1876, 1876, 1876,  907,  907,  907,  907,  907,
      129, 1876, 1876, 1876, 1876, 1876, 1876, 1876,  129, 1876,
      129, 1876,  129,  129,  129,  129,  129, 1150, 1150, 1876,

     1150, 1150, 1150, 1150, 1150, 1150, 1150, 1150, 1150, 1150,
     1150, 1150, 1150, 1150, 1150,  919,  919,  919,  919,  919,
      919,  919,  919,  919,  919,  919,  919,  919,  919,  919,
      919,  919,  919, 1162, 1162, 1876, 1162, 1162, 1162, 1162,
     1162, 1162, 1162, 1162, 1162, 1162, 1162, 1162, 1162, 1162,
     1162,  704,  704,  704,  704,  704,  704, 1876,  704,  704,
      704,  704,  704,  704,  704,  704,  704,  704,  704,  705,
      705, 1876,  705,  705,  705,  705,  705,  705,  705,  705,
      705,  705,  705,  705,  705,  705,  705,  703,  703,  703,
      703,  703,  703,  703,  703,  703,  703,  703,  703,  703,

      703,  703,  703,  703,  703, 1195, 1195, 1195, 1195, 1195,
     1195, 1195, 1195, 1195, 1195, 1195, 1195, 1195, 1195, 1195,
     1195, 1195, 1195,  224,  224,  224,  224,  224,  224,  224,
      224,  224,  224,  224,  224,  224,  224,  224,  224,  224,
      224, 1237, 1237, 1237, 1237, 1237, 1237, 1237, 1237, 1237,
     1237, 1237, 1237, 1237, 1237, 1237, 1237, 1237, 1237,  236,
      236,  236,  236,  236,  236,  236,  236,  236,  236,  236,
      236,  236,  236,  236,  236,  236,  236, 1253, 1253, 1253,
     1253, 1253, 1253, 1253, 1253, 1253, 1253, 1253, 1253, 1253,
     1253, 1253, 1253, 1253, 1253,  316,  316,  316,  316,  316,

      316,  316,  316,  316,  316,  316,  316,  316,  316,  316,
      316,  316,  316, 1306, 1306, 1306, 1306, 1306, 1306, 1306,
     1306, 1306, 1306, 1306, 1306, 1306, 1306, 1306, 1306, 1306,
     1306,  326,  326,  326,  326,  326,  326,  326,  326,  326,
      326,  326,  326,  326,  326,  326,  326,  326,  326, 1315,
     1315, 1315, 1315, 1315, 1315, 1315, 1315, 1315, 1315, 1315,
     1315, 1315, 1315, 1315, 1315, 1315, 1315, 1319, 1319, 1876,
     1319, 1319, 1319, 1319, 1319, 1319, 1319, 1319, 1319, 1319,
     1319, 1319, 1319, 1319, 1319, 1320, 1320, 1876, 1320, 1320,
     1320, 1320, 1320, 1320, 1320, 1320, 1320, 1320, 1320, 1320,

     1320, 1320, 1320,  111,  111, 1876,  111,  111,  111,  111,
      111,  111,  111,  111,  111,  111,  111,  111,  111,  111,
      111, 1321, 1321, 1321, 1321, 1321, 1321, 1321, 1321, 1321,
     1321, 1321, 1321, 1321, 1321, 1321, 1321, 1321, 1321,  114,
      114,  114,  114,  114,  114,  114,  114,  114,  114,  114,
      114,  114,  114,  114,  114,  114,  114, 1323, 1323, 1323,
     1323, 1323, 1323, 1323, 1323, 1323, 1323, 1323, 1323, 1323,
     1323, 1323, 1323, 1323, 1323,  120,  120,  120,  120,  120,
      120,  120,  120,  120,  120,  120,  120,  120,  120,  120,
      120,  120,  120, 1326, 1876, 1876, 1876, 1876, 1876, 1326,

     1876, 1876, 1876, 1876, 1876, 1326, 1326, 1326, 1326, 1326,
     1331, 1331, 1876, 1331, 1331, 1331, 1331, 1331, 1331, 1331,
     1331, 1331, 1331, 1331, 1331, 1331, 1331, 1331,  623, 1876,
     1876, 1876, 1876, 1876, 1876,  623, 1876,  623, 1876, 1876,
      623,  623,  623,  623,  129, 1876, 1876, 1876, 1876, 1876,
     1876, 1876,  129, 1876,  129, 1876,  129,  129,  129,  129,
      129,  626,  626,  626,  626, 1341, 1341, 1876, 1341, 1341,
     1341, 1341, 1341, 1341, 1341, 1341, 1341, 1341, 1341, 1341,
     1341, 1341, 1341,  704,  704,  704,  704,  704,  704, 1876,
      704,  704,  704,  704,  704,  704,  704,  704,  704,  704,

      704,  705,  705, 1876,  705,  705,  705,  705,  705,  705,
      705,  705,  705,  705,  705,  705,  705,  705,  705, 1196,
     1196, 1876, 1196, 1196, 1196, 1196, 1196, 1196, 1196, 1196,
     1196, 1196, 1196, 1196, 1196, 1196, 1196, 1195, 1195, 1195,
     1195, 1195, 1195, 1195, 1195, 1195, 1195, 1195, 1195, 1195,
     1195, 1195, 1195, 1195, 1195,  224,  224,  224,  224,  224,
      224,  224,  224,  224,  224,  224,  224,  224,  224,  224,
      224,  224,  224, 1405, 1405, 1405, 1405, 1405, 1405, 1405,
     1405, 1405, 1405, 1405, 1405, 1405, 1405, 1405, 1405, 1405,
     1405,  236,  236,  236,  236,  236,  236,  236,  236,  236,

      236,  236,  236,  236,  236,  236,  236,  236,  236, 1413,
     1876, 1413, 1876, 1876, 1876, 1876, 1413, 1876, 1876, 1413,
     1413, 1413, 1413, 1413, 1413, 1416, 1416, 1416, 1416, 1416,
     1416, 1416, 1416, 1416, 1416, 1416, 1416, 1416, 1416, 1416,
     1416, 1416, 1416, 1458, 1458, 1458, 1458, 1458, 1458, 1458,
     1458, 1458, 1458, 1458, 1458, 1458, 1458, 1458, 1458, 1458,
     1458,  316,  316,  316,  316,  316,  316,  316,  316,  316,
      316,  316,  316,  316,  316,  316,  316,  316,  316, 1460,
     1460, 1460, 1460, 1460, 1460, 1460, 1460, 1460, 1460, 1460,
     1460, 1460, 1460, 1460, 1460, 1460, 1460,  326,  326,  326,

      326,  326,  326,  326,  326,  326,  326,  326,  326,  326,
      326,  326,  326,  326,  326,  111,  111, 1876,  111,  111,
      111,  111,  111,  111,  111,  111,  111,  111,  111,  111,
      111,  111,  111,  114,  114,  114,  114,  114,  114,  114,
      114,  114,  114,  114,  114,  114,  114,  114,  114,  114,
      114,  120,  120,  120,  120,  120,  120,  120,  120,  120,
      120,  120,  120,  120,  120,  120,  120,  120,  120, 1326,
     1876, 1876, 1876, 1876, 1876, 1326, 1876, 1876, 1876, 1876,
     1876, 1326, 1326, 1326, 1326, 1326, 1469, 1876, 1469, 1876,
     1876, 1876, 1876, 1469, 1876, 1876, 1469, 1469, 1469, 1469,

     1469, 1469, 1519, 1876, 1519, 1876, 1876, 1876, 1876, 1519,
     1876, 1876, 1519, 1519, 1519, 1519, 1519, 1519,  474,  474,
      474,  474,  474,  474,  474,  474,  474,  474,  474,  474,
      474,  474,  474,  474,  474,  474, 1601, 1601, 1601, 1601,
     1601, 1626, 1626, 1876, 1626, 1626, 1626, 1626, 1626, 1626,
     1626, 1626, 1626, 1626, 1626, 1626, 1626, 1626, 1626,  655,
      655,  655,  655,  655,  655,  655,  655,  655,  655,  655,
      655,  655,  655,  655,  655,  655,  655, 1666, 1666, 1666,
     1666, 1666, 1666, 1666, 1666, 1666, 1666, 1666, 1666, 1666,
     1666, 1666, 1666, 1666, 1666, 1707, 1707, 1707, 1707, 1707,

     1707, 1707, 1707, 1707, 1707, 1707, 1707, 1707, 1707, 1707,
     1707, 1707, 1707, 1727, 1727, 1727, 1727, 1727, 1727, 1727,
     1727, 1727, 1727, 1727, 1727, 1727, 1727, 1727, 1727, 1727,
     1727, 1728, 1728, 1728, 1728, 1728, 1728, 1728, 1728, 1728,
     1728, 1728, 1728, 1728, 1728, 1728, 1728, 1728, 1728, 1734,
     1734, 1734, 1734, 1734, 1734, 1734, 1734, 1734, 1734, 1734,
     1734, 1734, 1734, 1734, 1734, 1734, 1734, 1752, 1752, 1752,
     1752, 1752, 1752, 1752, 1752, 1752, 1752, 1752, 1752, 1752,
     1752, 1752, 1752, 1752, 1752, 1761, 1761, 1761, 1761, 1761,
     1761, 1761, 1761, 1761, 1761, 1761, 1761, 1761, 1761, 1761,

     1761, 1761, 1761, 1764, 1764, 1764, 1764, 1764, 1764, 1764,
     1764, 1764, 1764, 1764, 1764, 1764, 1764, 1764, 1764, 1764,
     1764, 1805, 1805, 1805, 1805, 1805, 1805, 1805, 1805, 1805,
     1805, 1805, 1805, 1805, 1805, 1805, 1805, 1805, 1805, 1826,
     1826, 1826, 1826, 1826, 1826, 1826, 1826, 1826, 1826, 1826,
     1826, 1826, 1826, 1826, 1826, 1826, 1826, 1855, 1855, 1855,
     1855, 1855, 1855, 1855, 1855, 1855, 1855, 1855, 1855, 1855,
     1855, 1855, 1855, 1855, 1855, 1858, 1858, 1858, 1858, 1858,
     1858, 1858, 1858, 1858, 1858, 1858, 1858, 1858, 1858, 1858,
     1858, 1858, 1858, 1861, 1861, 1861, 1861, 1861, 1861, 1861,

     1861, 1861, 1861, 1861, 1861, 1861, 1861, 1861, 1861, 1861,
     1861, 1863, 1863, 1863, 1863, 1863, 1863, 1863, 1863, 1863,
     1863, 1863, 1863, 1863, 1863, 1863, 1863, 1863, 1863, 1872,
     1872, 1872, 1872, 1872, 1872, 1872, 1872, 1872, 1872, 1872,
     1872, 1872, 1872, 1872, 1872, 1872, 1872, 1874, 1874, 1874,
     1874, 1874, 1874, 1874, 1874, 1874, 1874, 1874, 1874, 1874,
     1874, 1874, 1874, 1874, 1874,   15, 1876, 1876, 1876, 1876,
     1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876,
     1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876,
     1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876,

     1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876,
     1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876,
     1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876,
     1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876,
     1876, 1876, 1876, 1876, 1876, 1876
    } ;

static const flex_int16_t yy_chk[9447] =
    {   0,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        2,    2,    2,    2, 1874,    8,    8,    8,    8,    9,
        9,    9,   10,   10,   10,   16,  102,   72,    9,   72,

       17,   10,   17,   19,   21,   20,    2,    2, 1872,   16,
        2,    8,    2,    8, 1897, 1897,   70,   19,   21,   24,
       22,   70, 1871,    2,   16,  102,   24,   24, 1868,   17,
       20,  108,   19,   21,   22,    2,    2,   16,  104,    2,
        8,    2,    8,   17,   20,   19,   21,   24, 1863,   22,
       27,    2,    6,    6,    6,    6,   26,   71,   17,   20,
      108,   24,   22,   28,   32,  112,  104,   27, 1861,  109,
       26,   17,   20,   33,   32, 1858,   24,   28,    6,    6,
     1855,   27,    6,  120,    6,   26,  113,   33,   32,   24,
      120,  120,   28,   71,  112,    6,   27,  109,   26,  110,

     1854,  110,   33,   32,   98,   28,   98,    6,    6,   27,
      242,    6,  242,    6,  113,   33,   32,   59,   59,   59,
       59,   71,   98,    6,   11,   11,   11,   11,   11,   11,
       11,   11,   11,   11,   11,   11,   11,   11,   11,   11,
       11,   11,   11,   11,   11,   11,   11,   11,   11,   11,
       11,   11,   11,   11,   11,   11,   11,   11,   11,   11,
       11,   11,   11,   11,   11,   11,   11,   11,   11,   11,
       11,   11,   11,   11,   11,   11,   11,   11,   11,   11,
       11,   11,   11,   11,   11,   11,   11,   11,   11,   11,
       11,   11,   11,   11,   11,   11,   11,   11,   11,   11,

       11,   11,   11,   11,   12,   12,   12,   12,   34,   23,
       23,   23,   23,  116, 1851,   34,   12,  100,  150,  100,
      150,  314,   12,  150, 1847,   36,   25,  158,   25,   34,
       12,   12,   36,  116,   12,  100,   12,  314,   23,   25,
      106,   25,  106,   25,   34,  135,   36,   12,  118,  118,
      118,  118,   23,  117,   44,   25,  158,   34,  106,   12,
       12,   36,  116,   12,  107,   12,  107,   23,   44,   25,
     1844, 1800,  136,  135,   36,   12,   14,   14,   14,   14,
       23, 1799,  107,   44,   25, 1778,  162,   30,   30,   30,
      117,  586,  164,  107,   14,   30,   44,   25,   30,   31,

      136,   31,   14,   14,   30,  140,   14,  586,   14,   30,
       31,   31,   31,   35,   35,  162,   30,  122,  117,   14,
       35,  164,  107,   31,  122,  122,   31,   52,   52,  803,
       30,   14,   14,  140,   35,   14,  124,   14,  167,   31,
       31,   31,   52,  124,  124,   30,  803,   14,   29,   35,
       29,   31,   37, 1754,   31,   37,   52,   52,   30,  124,
       57,   37,   35,  770,  141,  770,   29,  167,   37,   37,
       52,  159,   29,   43,   38,  137,   29,   29,   29,   38,
       57,   37,   43,   29,   37,   29,   29,   43,  124,   37,
      137,   29,  141,   38,   57,   29,   37,   37,  359,  159,

      359,   29,   43,   38,  137,   29,   29,   29,   38,   57,
       43,   29,  359,   29,   29,   43,  160,  172,  137,   29,
      189,   38,   57,   29,   39,   46,   40,   40,   46,   39,
       40,   39,   47,   47,  899,   39,   40,  899,   39,   46,
       60,   40,   47,   39,  160,   60,  172,   47,   39,  189,
      161, 1752,  163,   39,   46,   40,   40,   46,   39,   40,
       39,   47,   47,   39,   40,   45,   39,   46,   60,   40,
       47,   39,   45,   45,   45,   47,   39,   41,  161,   45,
      163,  165,   60,   41, 1404,   41,   41,  169,   41,   41,
       61,   41,  171, 1728,   45,   41,  201,   60, 1727,  207,

       45,   45,   45,  772, 1721,  772,   41,   45,   61,  165,
       60,   41,   48,   41,   41,  169,   41,   41,  173,   41,
      171,   48,   61,   41,   42,  201,   48,   42,  207, 1702,
       42,   42,  126,   42,  126, 1404,  138,   61,   42,   42,
     1701,   48,  139,   49,  209,  126,  173,  126,   49,   48,
       61,  138,  139,   42,   48,   49,   42,   49,   42,   42,
       50,   42,   49,   49,   51,  138,   42,   42,   50,  174,
       50,  139,   49,  209,   50,   50,   51,   49,   51,  138,
      139,   51,  210,   49,   51,   49,  889,  889,  889,   50,
       49,   49, 1696,   51,   77,  175,   50,  174,   50,  121,

      121,  121,   50,   50,   51, 1693,   51,  121,  121,   51,
       53,  210,   51,   54,   53,   53,  134,   77,   54,   56,
       54,   56,   77,  175,   53,  134,  134,   55, 1677,   53,
       53,   56,   54,   56,  177,   55,   77,  178,  134,   53,
       55,   55,   54,   53,   53, 1665,   77,   54,   56,   54,
       56,   77,   53, 1664,  134,  134,   55,   53,   53,   56,
       54,   56,  177,   55,   77,  178,  134,  179,   55,   55,
       58,   58,   58,   58,   62,   63, 1659,  180,   62,   62,
     1658,   63,   62,  315,   63,  315,  166,  166,   62,   63,
       69,   69,   69,   62,  211,  179,   58,   58, 1410,   58,

       58,  315,   58,   62,   63,  180, 1459,   62,   62,   63,
     1653,   62,   63,   58,  166,  166,   62,   63,   69, 1632,
      127,   62,  127,  211, 1625,   58,   58,   65,   58,   58,
       64,   58,   65,  127,   65,  127, 1617,   64,   64,   64,
      213,   58,  181,   76,   64,  184,   65,   69,  187, 1410,
     1616,   65,   75,   75,   75,   75,   65, 1459,  157,   64,
       83,   65,   79,   65,   75,   64,   64,   64,   76,  213,
      181,   76,   64,  184,   65,  157,  187,   76,   75,   65,
     1614,  360,   82,  360,   76,   76,   82,  157,   83,   79,
       79, 1612,   82,   79,  360,  188,  360,   76,  191,   79,

       76,  125,   83,  157,   79,   76, 1608,   75,  125,  125,
       99,   82,   76,   76,   78,   82,  219,   83,   79,   79,
       82,   99,   79,  188,   85,   86,  191,   79,   85,   86,
       83,  168,   79, 1605,   85,   86,  168,   78,   99,   81,
     1652,  125,   78,   81,   78,  219,   81,   81,   78,   81,
       84,   78,   99,   85,   86,   81,   78,   85,   86,  176,
      168,   78,   85,   86, 1604,  168,   78,   99,   81,  125,
      176,   78,   81,   78,   81,   81,   78,   81,   84,   78,
       99, 1670,  229,   81,   78,   84,   84,   84,  176,   78,
       80, 1652,   84,  192,   80,  226,  193,   80,  176,   80,

       80,  194,   80,   80,  190, 1691,   87,   84,  195,   80,
       87,  229,  190,   84,   84,   84,   87,  197,  226,   80,
       84,  192,   89,   80,  193,   80, 1542,   80,   80,  194,
       80,   80, 1670,  190,   88,   87,  195,   80,   88,   87,
      190,  182,   88,  182,   87,  197,   88,  226,   88,  203,
       89,  142,  142,  142,   88,  204, 1691,  205,   89,  142,
       89,   90,   91,   88,   89,   89,   91,   88,  142,  208,
      367,   88,   91,   90,   88,   90,   88,  203,   90,   89,
     1541,   90,   88,  204,  182,  205,   89,   93,   89,  196,
       90,   91,   89,   89,   92,   91, 1537,  208,   92,  367,

       91,   90,   92,   90,  196,  369,   90,  220,   92,   90,
       93, 1536,  182,   92,   92,   93, 1535,   93,  196,   94,
       95,   94,  200,   92,   94,   96,   94,   92,  200,   93,
      221,   92,  196,   96,  369,  220,   92, 1533,  368,   93,
      368,   92,   92,  368,   93,  170,   93,   94,   95,   94,
       95,  200,   94,   96,   94, 1777,  200,   93,  221, 1015,
       95, 1015,   95,   97,   97,   97,   97,   96,  144,  144,
      144,  144,  231,  231,  103,   97,  144,   95,  103,   95,
      198,  103,   96,  198,  103,  144,  170,  170,   95,   97,
       95,  216,  216,  216,  216,   96,  223,  223,  223,  223,

      231,  231,  245,  103,  245,  245, 1777,  103,  198,  103,
     1135,  198,  103, 1135,  170,  170, 1526, 1521,   97,  101,
      101,  101,  101,  101,  101,  101,  101,  101,  101,  101,
      101,  101,  101,  101,  101,  101,  101,  101,  101,  101,
      101,  101,  101,  101,  101,  101,  101,  101,  101,  101,
      101,  101,  101,  101,  101,  101,  101,  101,  101,  101,
      101,  101,  101,  101,  101,  101,  101,  101,  101,  101,
      101,  101,  101,  101,  101,  101,  101,  101,  101,  101,
      101,  101,  101,  101,  101,  101,  101,  101,  101,  101,
      101,  101,  101,  101,  101,  101,  101,  101,  101,  105,

      391,  129,  183,  129,  183,  247,  105,  247,  247,  232,
      105, 1458,  183,  105,  129,  129,  129,  129,  130,  129,
      130,  212, 1456,  105,  227,  212,  183, 1451,  105,  391,
      148,  130,  130,  130,  130,  105,  130,  232,  105,  148,
      148,  105,  130,  131,  133,  131,  133,  227, 1450,  392,
      212,  105,  148,  129,  212,  183,  131,  131,  131,  131,
     1448,  131,  143,  143,  143,  143,  233, 1445,  148,  148,
      130,  130,  133,  199,  143,  149,  227,  149,  392,  395,
      148,  133,  351,  199,  131,  235,  149,  149,  143,  351,
      351,  133,  215,  185,  233,  131,  186,  186,  343,  149,

     1442,  133,  199,  186,  228,  185,  215,  185,  395,  133,
      185,  199,  131,  235, 1439,  149,  149,  143,  202,  133,
      202,  215,  185,  239,  186,  186,  343,  149,  206,  218,
     1438,  186,  222,  185,  215,  185,  202,  202,  185,  222,
      202,  230,  206,  218,  228,  206,  344,  202,  230,  202,
      225,  225,  225,  225,  238,  238,  238,  206,  218,  239,
      222,  240,  241,  234,  202,  202,  319,  222,  202,  230,
      206,  218,  228,  206,  344,  234,  230,  234, 1557,  345,
      234, 1557,  238,  253,  240,  253,  325,  239,  241,  319,
     1437,  348,  234,  346,  246,  246,  246,  246,  250,  250,

      250,  250,  253,  234, 1431,  234,  246,  345,  234,  325,
      250,  238,  253,  240,  253,  366, 1425,  241,  319,  251,
      246,  346,  348,  254,  250, 1421,  251,  251,  251,  424,
      253,  424,  254,  251,  252,  320, 1416,  252,  325,  424,
      254,  255,  257,  366,  252,  252,  252,  256,  251,  246,
      348,  252,  254,  250,  251,  251,  251,  255,  255,  257,
      254,  251,  256,  252,  256,  259,  252,  259,  254, 1406,
      255,  257,  252,  252,  252,  320,  256,  376,  262,  252,
      258,  260,  274, 1405,  259,  255,  255,  257,  378,  258,
      256,  261,  256,  260,  259,  262,  259,  258,  260,  274,

      349,  261,  261,  320,  265,  376,  405,  262,  261,  258,
      260,  274,  259,  263,  264,  263,  378,  258,  263, 1332,
      261,  260,  266,  262,  264,  258,  260,  274,  265,  261,
      261,  264,  263,  267,  267,  405,  261,  393,  266,  266,
      349,  268,  263,  264,  263,  265,  265,  263,  394, 1325,
      267,  266,  264,  313,  269,  313,  268,  265,  268,  264,
      263,  396,  267,  267,  270,  393,  266,  266,  349,  397,
      268,  269,  269,  265,  265, 1322,  394,  273,  267,  270,
     1321,  270,  313,  269,  268,  271,  268,  272,  275,  396,
      398,  271,  406,  270,  273,  273,  272,  397,  342,  269,

      269, 1306,  271,  275,  272,  275,  273,  270,  276,  270,
      399,  313,  342,  277,  271,  277,  272,  275,  398,  276,
      271,  406,  273,  273,  272,  276,  278,  342,  278,  279,
      271,  275,  272,  275, 1305,  425,  278,  276,  399,  277,
      342, 1301,  279,  280,  425,  425,  279,  276, 1296,  338,
      278,  338,  278,  276,  400,  280,  277,  280,  279, 1295,
      280,  309,  309,  309,  309, 1294, 1292,  338,  277,  278,
      279,  403,  280,  379,  279,  379,  379,  281,  282,  278,
     1277,  278,  400,  280,  277,  280,  281,  281,  280,  282,
      902,  283,  902,  281,  281,  282,  284,  278, 1264,  403,

      284,  902,  283,  352,  352,  352,  281,  282,  283,  285,
      285,  352,  352,  284,  281,  281,  409,  282,  285,  404,
      283,  281,  281,  282, 1256,  284,  285,  286, 1253,  284,
      283,  287,  288,  289,  292,  300,  283,  286,  285,  285,
      287,  284,  289,  288,  286,  409,  285,  404,  287,  288,
      289,  292,  300,  300,  285,  418,  286,  290, 1779, 1237,
      287,  288,  289,  292,  300,  286,  290,  291,  287,  291,
      289,  288,  286, 1236,  290,  293,  287,  288,  289,  292,
      300,  300, 1791,  418,  291,  381,  290,  381,  381,  420,
      293,  423,  293,  293,  290, 1196,  291,  294,  291,  295,

     1151,  410,  290,  294,  293,  295,  296,  296,  321, 1779,
      321,  295,  291,  294,  294,  354,  295,  420,  293,  423,
      293,  293,  354,  354,  296,  413,  294,  297,  295,  297,
      410,  298,  294, 1791,  295,  296,  296,  321,  299,  295,
      334,  294,  294,  416,  295,  297,  297,  299,  298,  297,
      298,  301,  296,  301,  413,  299,  297,  334,  297, 1141,
      298,  302,  302, 1137, 1136,  301,  321,  299,  301,  334,
      301,  303,  416,  297,  297,  299,  298,  297,  298,  302,
      301,  417,  301,  299,  401,  334,  303,  401,  303,  305,
      302,  302,  304,  301,  304,  305,  301, 1114,  301,  426,

      303,  340,  306,  340,  306, 1626,  305,  302, 1626,  304,
      417,  307,  401,  307,  303,  401,  303,  307,  305,  306,
     1017,  304, 1017,  304,  305,  421,  308,  426,  307, 1104,
      340,  306,  308,  306,  305, 1103,  412,  304,  412,  322,
      307,  322,  307,  308, 1090,  616,  307,  306,  310,  310,
      310,  310,  616,  616,  421,  308,  307,  322,  412,  340,
      310,  308,  311,  311,  311,  311, 1087,  323,  322,  323,
      328,  308,  357,  329,  310,  324,  324,  324,  324,  357,
      357,  328,  427,  323,  329,  323,  330,  324,  328,  330,
      428,  329,  429,  330,  323,  323,  431,  322,  332,  328,

      432,  324,  329,  310,  332,  332,  357,  323, 1072,  328,
      427,  587,  329,  587,  330, 1068,  328,  330,  428,  329,
      429,  330, 1060,  323,  323,  431,  332, 1059,  432,  587,
      324, 1052,  332,  332,  357,  323,  326,  326,  326,  326,
      326,  326,  326,  326,  326,  326,  326,  326,  326,  326,
      326,  326,  326,  326,  326,  333,  326,  326,  326,  326,
      326,  331,  331,  331,  333,  335,  336, 1628,  331,  402,
     1628,  333,  415,  415,  433,  434, 1047,  335,  336,  437,
      336,  442,  335,  336,  333,  443,  402,  326,  326,  331,
      331,  331,  333, 1023,  335,  336,  331,  919,  402,  333,

      415,  415,  433,  912,  434,  335,  336,  437,  336,  442,
      335,  336,  906,  443,  402,  326,  327,  327,  327,  327,
      327,  327,  327,  327,  327,  327,  327,  327,  327,  327,
      327,  327,  327,  327,  327,  862,  327,  327,  327,  327,
      327,  337,  339,  859,  339,  341,  855,  341,  353,  353,
      353,  444,  327,  419,  337,  419,  353,  353,  337,  327,
      339,  341,  435,  341,  435,  849,  837,  327,  327,  358,
      337,  339,  341,  341,  341,  419,  358,  358,  835,  444,
      327,  370,  337,  370,  435,  341,  337,  327,  820,  436,
      361,  362,  361,  362,  375,  327,  375,  370,  430,  814,

      339,  341,  341,  341,  361,  362,  361,  362,  361,  362,
      375,  430,  358,  341,  364,  365,  364,  365,  436,  792,
      784,  438,  439,  440,  441,  440,  370,  430,  364,  365,
      364,  365,  364,  365,  375,  380,  380,  380,  380,  430,
      358,  783,  361,  362, 1831,  440,  422,  380,  422,  364,
      438,  439,  705,  441,  370,  384,  384,  384,  384,  365,
      448,  380,  375,  384,  388,  407,  364,  365,  385,  385,
      385,  385,  384,  388,  388,  408,  385,  364,  445,  447,
      385,  407,  414,  449,  414,  385,  388,  365,  448,  422,
      380,  408,  450,  452,  385, 1831,  453,  451,  456,  457,

      458,  454,  388,  388,  414,  451,  407,  445,  447,  454,
      459,  449,  460,  455,  388,  455,  408,  422,  414,  704,
      465,  450,  452,  385,  453,  451,  456,  457,  461,  458,
      454,  462,  463,  451,  407,  455,  464,  454,  466,  459,
      467,  460,  469,  470,  408,  411,  414,  411,  455,  465,
      471,  473,  475,  476,  477,  478,  461,  455,  479,  462,
      463,  480,  482,  481,  464,  483,  466,  411,  467,  484,
      469,  470,  411,  485,  487,  488,  455,  411,  471,  473,
      475,  476,  477,  478,  489,  455,  479,  411,  411,  480,
      503,  411,  481,  486,  490,  504,  482,  489,  484,  491,

      676,  411,  485,  487,  488,  675,  411,  508,  483,  494,
      486,  494,  494,  489,  506,  411,  411,  503,  505,  411,
      510,  481,  486,  490,  482,  489,  504,  507,  491,  492,
      492,  492,  495,  509,  495,  495,  483,  498,  486,  498,
      498,  499,  499,  499,  499,  500,  503,  500,  500,  511,
      508,  506,  505,  499,  510,  504,  512,  491,  507,  514,
      513,  492,  517,  533,  518,  515,  509,  499,  516,  519,
      519,  523,  523,  523,  534,  524,  521,  526,  508,  506,
      505,  520,  510,  529,  514,  517,  519,  507,  585,  511,
      492,  518,  655,  523,  509,  513,  499,  512,  513,  515,

      533,  514,  516,  521,  527,  520,  524, 1025,  534, 1025,
      528,  519,  654,  514,  517,  650,  585,  511,  526,  526,
      518,  531,  541,  513,  529,  512,  513,  515,  533,  514,
      516,  527,  521,  532,  520,  524,  534,  528,  536,  519,
      522,  522,  522,  525,  525,  525,  526,  526,  530,  530,
      530,  535,  529,  540,  537,  629,  531,  629,  532,  545,
      527,  539,  522,  538,  541,  525,  528,  522,  544,  542,
      530,  629,  522,  551,  546,  649,  543,  543,  543,  525,
      556,  536,  522,  522,  531,  535,  522,  532,  539,  547,
      538,  549,  541,  550,  540,  542,  522,  537,  543,  544,

      546,  522,  545,  538,  548,  548,  548,  525,  552,  536,
      522,  522,  555,  535,  522,  553,  547,  539,  549,  538,
      551,  556,  540,  557,  542,  537,  548,  550,  544,  546,
      545,  538,  558,  559,  561,  555,  560,  564,  565,  566,
      562,  570,  569,  574,  552,  547,  553,  549,  551,  556,
      646,  644,  578,  572,  636,  550,  563,  563,  563,  557,
      571,  567,  568,  558,  555,  560,  562,  633,  561,  559,
      566,  632,  552,  575,  562,  553,  577,  559,  563,  574,
      564,  565,  569,  570,  573,  579,  578,  557,  567,  568,
      631,  563,  558,  572,  560,  562,  561,  559,  571,  566,

      563,  618,  562,  589,  590,  559,  582,  574,  564,  565,
      569,  570,  576,  573,  578,  575,  577,  567,  568,  563,
      594,  572,  580,  580,  580,  580,  571,  579,  563,  581,
      581,  581,  581,  583,  583,  583,  583,  589,  588,  591,
      588,  591,  573,  575,  577,  583,  554,  590,  592,  594,
      592,  597,  607,  604,  608,  579,  588,  502,  597,  583,
      501,  647,  617,  647,  647,  589,  592,  588,  591,  617,
      617,  598,  497,  496,  603,  590,  604,  592,  594,  607,
      597,  603,  608,  593,  598,  593,  597,  901,  583,  584,
      584,  584,  584,  599,  901,  901,  588,  591,  602,  593,

      598,  593,  605,  603,  611,  604,  592,  599,  607,  603,
      593,  593,  598,  609,  602,  584,  584,  600,  584,  584,
      613,  584,  599,  593,  610,  600,  605,  602,  600,  612,
      493,  643,  584,  611,  615,  599, 1027,  474, 1027,  593,
      593,  609,  602,  472,  584,  584,  600,  584,  584,  613,
      584,  593,  610,  600,  614,  605,  600,  612,  615,  643,
      584,  595,  595,  595,  595,  595,  595,  595,  595,  595,
      595,  595,  595,  595,  595,  595,  595,  595,  595,  595,
      390,  595,  595,  595,  595,  595,  601,  615,  601,  621,
      657,  621,  614,  389,  622,  658,  622,  595,  619,  659,

      660,  386,  621,  601,  621,  619,  619,  622,  625,  622,
      625,  383,  595,  595,  626,  601,  626,  601,  657,  623,
      614,  623,  625,  658,  382,  595,  625,  659,  626,  660,
      623,  601,  623,  606,  623,  623,  377,  623,  663,  619,
      595,  596,  596,  596,  596,  596,  596,  596,  596,  596,
      596,  596,  596,  596,  596,  596,  596,  596,  596,  596,
      606,  596,  596,  596,  596,  596,  663,  619,  620,  703,
      596,  623, 1110,  606, 1110,  620,  620,  374,  703,  703,
      624,  630,  624,  630,  373,  661,  627,  662,  627,  606,
      667,  372,  596,  596,  624,  620,  665,  630,  624,  596,

      627,  606,  627,  628,  627,  628,  634,  641,  634,  641,
      648,  666,  648,  648,  661,  668,  662,  628,  667,  628,
      596,  628,  634,  641,  620,  665,  669,  371,  627,  670,
      671,  628,  624,  651,  651,  651,  651,  678,  627,  678,
      666,  651,  672,  668,  682,  652,  652,  652,  652,  683,
      651,  673,  674,  652,  669,  628,  627,  652,  670,  671,
      628,  677,  652,  685,  653,  653,  653,  653,  681,  678,
      672,  652,  653,  682,  656,  656,  656,  656,  683,  673,
      674,  653,  656,  678,  686,  684,  686,  688,  689,  677,
      689,  656,  685,  691,  692,  694,  681,  695,  678,  695,

      652,  696,  697,  699,  697,  699,  686,  700,  701,  708,
      689,  678,  679,  684,  679,  688,  706,  706,  706,  695,
      707,  691,  692,  694,  697,  699,  706,  706,  709,  706,
      696,  706,  710,  712,  679,  700,  701,  708,  706,  679,
      711,  363,  713,  714,  679,  715,  356,  711,  719,  707,
      720,  726,  728,  355,  679,  679,  709,  716,  679,  716,
      350,  710,  712,  717,  729,  717,  347,  730,  679,  711,
      713,  731,  714,  679,  715,  711,  732,  719,  720,  716,
      726,  728,  679,  679,  733,  717,  679,  721,  722,  721,
      722,  723,  729,  723,  706,  730,  734,  735,  318,  736,

      731,  737,  316,  743,  732,  738,  740,  738,  740,  721,
      722,  744,  733,  723,  742,  738,  742,  745,  746,  748,
      749,  766,  750,  722,  734,  752,  735,  736,  740,  753,
      737,  743,  755,  756,  755,  757,  742,  758,  760,  744,
      761,  762,  763,  764,  765,  745,  746,  767,  748,  749,
      750,  722,  768,  752,  755,  769,  773,  771,  753,  776,
      312,  766,  756,  785,  757,  771,  758,  760,  761,  778,
      762,  763,  764,  765,  777,  777,  777,  786,  249,  768,
      774,  787,  775,  769,  775,  773,  771,  774,  767,  766,
      791,  774,  775,  795,  771,  779,  775,  779,  779,  776,

      780,  775,  780,  780,  785,  778,  777,  790,  768,  774,
      781,  788,  781,  781,  786,  774,  767,  789,  782,  774,
      782,  782,  787,  796,  793,  794,  797,  776,  799,  798,
      795,  791,  785,  778,  790,  777,  788,  800,  802,  801,
      809,  806,  786,  804,  807,  812,  789,  793,  811,  810,
      787,  813,  794,  808,  815,  799,  248,  796,  795,  791,
      798,  819,  869,  790,  822,  788,  805,  805,  805,  797,
      817,  800,  807,  821,  802,  789,  793,  804,  806,  810,
      808,  794,  801,  244,  799,  796,  812,  809,  823,  798,
      869,  243,  811,  813,  815,  824,  827,  797,  805,  800,

      817,  807,  802,  819,  821,  804,  806,  822,  810,  808,
      801,  829,  805,  825,  812,  809,  816,  816,  816,  826,
      811,  813,  815,  818,  818,  818,  824,  805,  828,  817,
      823,  819,  827,  821,  829,  822,  831,  838,  816,  832,
      805,  839,  825,  841,  836,  818,  830,  830,  830,  825,
      826,  236,  833,  833,  833,  824,  840,  828,  823,  224,
      827,  831,  846,  829,  838,  834,  834,  834,  830,  842,
      841,  825,  843,  832,  833,  836,  844,  825,  839,  826,
      845,  847,  848,  848,  848,  851,  828,  834,  852,  853,
      831,  840,  848,  838,  850,  850,  850,  854,  856,  841,

      834,  832,  846,  845,  836,  858,  839,  860,  861,  857,
      217,  842,  847,  864,  843,  123,  850,  893,  844,  840,
      119,  115,  852,  856,  865,  851,  114,  866,  834,  853,
      846,  921,  845,  854,  857,  861,  863,  863,  863,  842,
      864,  847,  843,  875,  860,  893,  844,  881,  858,  881,
      852,  865,  856,  851,  866,  874,   74,  853,  863,  921,
      871,  854,  871,  857,  861,  867,  867,  867,  867,  864,
      875,  881,  860,   67,   15,  872,  858,  872,  871,  873,
      865,  873,  877,  866,  868,  868,  868,  868,  870,  870,
      870,  870,  880,  872,  880,  922,  874,  873,    7,  875,

      881,  894,  890,  883,  872,  883,  870,  882,  873,  882,
      884,  876,  884,  876,  870,  870,    0,  870,  870,  896,
      870,    0,  877,  922,  874,  890,  907,  876,  907,  876,
      894,  870,    0,  872,  880,  882,    0,  873,  876,  876,
      907,  883,  923,  870,  870,  884,  870,  870,  896,  870,
      877,  876,    0,  898,  890,  885,    0,  885,  915,  870,
      915,  915,  880,  916,  882,  916,  916,  876,  876,  883,
        0,  923, 1035,  884, 1035, 1035,  891,  898,    0,  876,
      878,  878,  878,  878,  878,  878,  878,  878,  878,  878,
      878,  878,  878,  878,  878,  878,  878,  878,  878,  885,

      878,  878,  878,  878,  878,  891,  898,  917,  917,  917,
      917,  925,  891,  924,  928,  917,  891,  927,  930,  892,
      892,  892,  931,  938,  917,  932,  886,  885,  886,  892,
        0,  878,  878,  892,  891, 1032,    0, 1032,  892,  925,
      891,    0,  924,  928,  891,  927,  930,    0, 1138, 1032,
      886,  931,  938,  932, 1032, 1138, 1138,  933,  886,  878,
      879,  879,  879,  879,  879,  879,  879,  879,  879,  879,
      879,  879,  879,  879,  879,  879,  879,  879,  879,  886,
      879,  879,  879,  879,  879,  933,  887,  886,  887,  888,
        0,  888,  900,  954, 1240,  954, 1240,  935,    0,  900,

      900,    0,  904,  936,  904,  905,  937,  905,  908,  940,
      908,  879,  879,  904,  887,  954,  905,  888,  904,  900,
      904,  905,  908,  905,  908,  935,  908,  918,  918,  918,
      918,  936, 1195,  941,  937,  918,  908,    0,  940,  879,
      942, 1195, 1195,  887,  918,  997,  888,  997,  900, 1014,
     1036, 1014, 1036, 1036,  904,  997, 1244,  905, 1244, 1014,
      908,  939,  941,  939,    0,  908,  909,  909,  942,  909,
      909,  909,  909,  909,  909,  909,  909,  909,  909,  909,
      909,  909,  909,  909,  909,  909,  909,  909,  909,  909,
      909,    0,  943,  939,  920,  920,  920,  920,  944,    0,

      945,  946,  920,  949,  950,  951,  920,  939,  952,  953,
      961,  920,  955,  967,  956,  968,  956,  909,  909,  909,
      920,  943,  939,  971,  959,    0,  959,  944,  945,    0,
      946,  949,  950,    0,  951,  939,  956,  952,  953,  961,
      955,  970,  967,    0,  968,  909,  959,    0,    0,  920,
      926,  926,  971,  926,  926,  926,  926,  926,  926,  926,
      926,  926,  926,  926,  926,  926,  926,  926,  926,  970,
      926,  926,  926,  926,  926,  963,  964,  963,  964,  972,
     1117, 1117, 1117,  973,  974,    0,  963,  964,  963,  964,
      963,  964,  976,  965,  965,  965,  979,  963,  964,  980,

      966,  926,  926,  965,  965,  985,  965,  972,  965,  966,
      966,  973,  966,  974,  966,  965,  975,  977,  975,  977,
      976,  966,  981,  988,  981,  979,  986,  987,  980,  926,
      983,  989,  983,  991,  985,  992,    0,  994,  975,  977,
      995,  996,    0, 1004,  981,  999, 1001,  999, 1002, 1021,
     1003,  988,  983,  963,  964,  986,  987, 1005, 1006, 1016,
      989, 1007,  991, 1008,  992,  994, 1009,  999, 1018,  995,
      996,  965, 1004, 1010, 1001, 1010, 1002,  966, 1003, 1019,
     1021, 1022, 1020, 1024, 1020, 1005, 1026, 1006, 1016, 1007,
     1028, 1008, 1020, 1029, 1009, 1010, 1020, 1018, 1033, 1034,

     1030, 1020, 1030, 1037, 1039, 1037, 1037, 1019, 1022, 1021,
     1030, 1040, 1024, 1042, 1030, 1026, 1041, 1043, 1028, 1030,
     1120, 1038, 1029, 1038, 1038, 1046, 1066, 1066, 1066, 1033,
     1078, 1078, 1078,    0,    0, 1108, 1042, 1022, 1127, 1130,
        0, 1034, 1045, 1041, 1157, 1039, 1157, 1040, 1066, 1120,
     1046,    0, 1078, 1043, 1105, 1105, 1105,    0, 1033, 1106,
     1106, 1106, 1106, 1108, 1105, 1042, 1157, 1127, 1130, 1034,
     1050, 1048, 1041, 1039, 1049, 1040, 1051, 1053, 1055, 1046,
     1045, 1043, 1044, 1044, 1044, 1044, 1044, 1044, 1044, 1044,
     1044, 1044, 1044, 1044, 1044, 1044, 1044, 1044, 1044, 1044,

     1044, 1049, 1044, 1044, 1044, 1044, 1044, 1048, 1045, 1054,
     1050, 1057, 1051, 1062, 1053, 1058, 1061, 1056, 1064, 1055,
     1067, 1063, 1069, 1065, 1073, 1075, 1076, 1079,    0, 1070,
     1049, 1071, 1074, 1044, 1044, 1048, 1077, 1080, 1050, 1057,
     1051, 1064, 1053, 1054, 1056, 1058,    0, 1055, 1063, 1081,
     1061, 1062, 1065, 1069, 1070,    0, 1067, 1071, 1086, 1074,
     1080, 1044, 1076, 1083, 1073, 1077, 1088, 1075, 1057, 1079,
     1064, 1054, 1082, 1056, 1058, 1084, 1081, 1063, 1061, 1062,
     1085, 1065, 1069, 1070, 1067, 1086, 1071, 1089, 1074, 1080,
     1076, 1083, 1073, 1088, 1077, 1075, 1091, 1079, 1092, 1093,

     1094, 1084, 1097, 1082, 1095, 1081, 1096, 1098, 1099, 1101,
     1100, 1146, 1102, 1146, 1086, 1144, 1085, 1144, 1089, 1163,
     1083, 1092, 1088,    0, 1093, 1146, 1144, 1124, 1097, 1230,
     1084, 1230, 1082, 1124, 1111, 1099, 1111, 1116, 1091, 1230,
     1113, 1094, 1096, 1112, 1085, 1112, 1095, 1089, 1163, 1098,
     1092, 1100, 1101, 1093, 1102,    0, 1124, 1097, 1107, 1107,
     1107, 1107, 1124, 1111, 1099, 1121, 1091, 1113, 1116, 1094,
     1096, 1122, 1112, 1122, 1095, 1129, 1121, 1098,    0, 1100,
     1101,    0, 1102, 1109, 1109, 1109, 1109, 1128, 1245, 1115,
     1245, 1115, 1111, 1123, 1121, 1125, 1113, 1116,    0,    0,

     1122, 1112, 1122, 1129, 1121, 1115, 1125, 1115, 1123, 1109,
     1109, 1139, 1109, 1109, 1169, 1109, 1115, 1115, 1139, 1139,
     1140, 1128, 1123, 1160, 1125,    0, 1109, 1140, 1140, 1115,
        0, 1333, 1129, 1333, 1125, 1147, 1123, 1147, 1109, 1109,
        0, 1109, 1109, 1169, 1109, 1115, 1115,    0, 1147, 1128,
     1147, 1160,    0, 1153, 1109, 1153, 1153, 1115, 1118, 1118,
     1118, 1118, 1118, 1118, 1118, 1118, 1118, 1118, 1118, 1118,
     1118, 1118, 1118, 1118, 1118, 1118, 1118,    0, 1118, 1118,
     1118, 1118, 1118, 1142, 1143, 1142, 1143, 1118, 1154, 1161,
     1154, 1154, 1164, 1171, 1142, 1143, 1158, 1246, 1158, 1246,

     1246, 1142, 1143, 1166, 1167, 1148, 1168, 1148, 1170, 1118,
     1118, 1149, 1150, 1149, 1150,    0, 1118, 1161, 1158, 1148,
     1164, 1148, 1171, 1148,    0, 1149, 1150, 1149, 1150, 1149,
     1150, 1166, 1167,    0, 1168, 1142, 1170, 1118, 1119, 1119,
     1119, 1119, 1119, 1119, 1119, 1119, 1119, 1119, 1119, 1119,
     1119, 1119, 1119, 1119, 1119, 1119, 1119, 1148, 1119, 1119,
     1119, 1119, 1119, 1149, 1150, 1119, 1155, 1155, 1155, 1155,
     1156, 1156, 1156, 1156, 1155, 1173, 1174, 1176, 1156, 1179,
     1180, 1181,    0, 1155, 1184, 1182, 1183, 1156, 1185, 1119,
     1119, 1192,    0, 1192, 1119, 1192, 1247,    0, 1247, 1247,

        0, 1334, 1192, 1334, 1173, 1174, 1176, 1179, 1180,    0,
     1181, 1172, 1184, 1172, 1182, 1183, 1185, 1119, 1126, 1126,
     1126, 1126, 1126, 1126, 1126, 1126, 1126, 1126, 1126, 1126,
     1126, 1126, 1126, 1126, 1126, 1126, 1126, 1172, 1126, 1126,
     1126, 1126, 1126, 1186, 1189, 1186, 1187, 1190, 1187, 1191,
     1172, 1191, 1202, 1199,    0, 1200, 1201, 1214, 1192, 1204,
     1191, 1193, 1191, 1193, 1191, 1186, 1172, 1205, 1187, 1126,
     1126, 1191, 1193, 1189, 1193, 1190, 1193, 1194, 1172, 1194,
     1202, 1194, 1199, 1193, 1200, 1201, 1214, 1204, 1194, 1302,
     1302, 1302, 1302,    0, 1218, 1205, 1209, 1126, 1145, 1145,

     1221, 1145, 1145, 1145, 1145, 1145, 1145, 1145, 1145, 1145,
     1145, 1145, 1145, 1145, 1145, 1145, 1145, 1145, 1145, 1145,
     1145, 1145, 1145, 1218, 1209, 1211, 1206, 1191, 1206, 1221,
     1197, 1197, 1197, 1212, 1215, 1208, 1210, 1208, 1210, 1193,
     1197, 1197, 1216, 1197, 1194, 1197, 1198, 1232, 1206, 1145,
     1145, 1145, 1197, 1211,    0, 1198, 1198, 1208, 1198,    0,
     1198, 1212, 1215, 1303, 1303, 1303, 1303, 1198,    0, 1217,
     1216, 1316, 1316, 1316,    0, 1210, 1232, 1145, 1159, 1159,
        0, 1159, 1159, 1159, 1159, 1159, 1159, 1159, 1159, 1159,
     1159, 1159, 1159, 1159, 1159, 1159, 1159, 1217, 1159, 1159,

     1159, 1159, 1159, 1210, 1213, 1219, 1213, 1243, 1197, 1225,
     1226, 1225, 1226, 1228, 1227, 1233, 1227, 1234,    0, 1235,
     1238, 1254, 1239, 1198, 1241, 1265, 1213, 1242, 1271, 1159,
     1159,    0, 1226, 1219, 1243, 1225, 1227, 1248, 1248, 1248,
     1304, 1228, 1343, 1233, 1254,    0, 1234, 1235, 1252, 1238,
     1239, 1271, 1265, 1241, 1326, 1242, 1326, 1159, 1257, 1248,
     1249, 1249, 1249, 1243, 1225, 1326,    0,    0, 1251, 1304,
     1255, 1343, 1259, 1254, 1258, 1261, 1235,    0,    0, 1268,
     1271, 1265, 1249, 1252, 1242, 1250, 1250, 1250, 1250, 1250,
     1250, 1250, 1250, 1250, 1250, 1250, 1250, 1250, 1250, 1250,

     1250, 1250, 1250, 1250, 1257, 1250, 1250, 1250, 1250, 1250,
     1251, 1252, 1255, 1262, 1258, 1259, 1260, 1261, 1272, 1268,
     1263, 1263, 1263, 1266, 1267, 1273, 1269, 1270, 1270, 1270,
     1274, 1276, 1257, 1278, 1275, 1279, 1250, 1250, 1251, 1287,
     1255, 1260, 1258, 1259, 1262, 1261, 1263, 1268, 1281, 1270,
     1266, 1267, 1282, 1282, 1282, 1272, 1273, 1274, 1288, 1263,
     1269, 1275, 1283, 1289, 1250, 1284, 1291, 1280, 1280, 1280,
     1260, 1276, 1286, 1262, 1278, 1263, 1293, 1279, 1307, 1266,
     1267, 1287, 1281, 1272, 1344, 1273, 1274, 1263, 1269, 1280,
     1275, 1282, 1285, 1285, 1285, 1286, 1290, 1300, 1288, 1276,

     1289, 1291, 1278, 1283,    0, 1279, 1307, 1293,    0, 1287,
     1281,    0, 1284, 1344, 1285, 1297, 1297, 1297, 1311, 1282,
     1298, 1298, 1298, 1290, 1286, 1312, 1288, 1313, 1289, 1291,
     1314, 1283, 1299, 1299, 1299, 1307, 1293, 1318, 1317, 1300,
     1284, 1297, 1298, 1323,    0, 1327, 1311, 1327,    0, 1324,
     1323, 1323, 1290, 1312, 1299, 1313, 1324, 1324, 1314, 1327,
     1402, 1317, 1402,    0, 1408, 1318, 1408, 1300, 1345,    0,
     1297, 1308, 1308, 1308, 1308, 1308, 1308, 1308, 1308, 1308,
     1308, 1308, 1308, 1308, 1308, 1308, 1308, 1308, 1308, 1308,
     1317, 1308, 1308, 1308, 1308, 1308, 1328, 1345, 1328, 1335,

     1335, 1335, 1335, 1351, 1347,    0, 1337, 1335, 1337, 1328,
     1414, 1328, 1414, 1339,    0, 1339, 1335, 1348, 1329, 1349,
     1329, 1353, 1308, 1308, 1330, 1331, 1330, 1331, 1337,    0,
        0, 1351, 1329, 1347, 1329, 1339, 1329,    0, 1330, 1331,
     1330, 1331, 1330, 1331,    0, 1415, 1348, 1415, 1349, 1353,
     1308, 1309, 1309, 1309, 1309, 1309, 1309, 1309, 1309, 1309,
     1309, 1309, 1309, 1309, 1309, 1309, 1309, 1309, 1309, 1309,
     1329, 1309, 1309, 1309, 1309, 1309, 1330, 1331, 1354, 1336,
     1336, 1336, 1336, 1356, 1342,    0, 1357, 1336, 1342, 1358,
     1359,    0, 1360, 1361, 1621, 1309, 1336, 1367, 1363, 1364,

        0, 1621, 1309, 1309, 1621,    0, 1354, 1365, 1412, 1365,
     1412, 1412, 1356, 1342, 1357,    0,    0, 1342, 1358, 1359,
     1360,    0, 1361, 1309, 1352, 1367, 1352, 1363, 1364, 1365,
     1309, 1310, 1310, 1310, 1310, 1310, 1310, 1310, 1310, 1310,
     1310, 1310, 1310, 1310, 1310, 1310, 1310, 1310, 1310, 1310,
     1352, 1310, 1310, 1310, 1310, 1310, 1368, 1370, 1369, 1370,
     1369, 1370,    0, 1352, 1372, 1376, 1377, 1388, 1370, 1369,
     1382, 1369, 1381, 1369, 1381, 1371, 1371, 1371, 1383, 1352,
     1369,    0, 1310, 1310, 1368, 1371, 1371, 1378, 1371, 1378,
     1371, 1352, 1372, 1395, 1376, 1377, 1388, 1371, 1382, 1385,

     1387, 1385, 1390, 1393, 1400, 1396, 1383, 1396, 1403, 1378,
     1310, 1381, 1394, 1398, 1394, 1398, 1401, 1407, 1409, 1411,
     1424, 1385, 1395, 1417, 1370, 1418, 1419, 1396, 1387, 1420,
     1390, 1393, 1434, 1400, 1422, 1398, 1369, 1403, 1394, 1381,
     1426, 1423, 1428, 1424, 1401, 1407, 1417, 1409, 1429, 1427,
     1417, 1419, 1418, 1371, 1436, 1435, 1420, 1432, 1430, 1411,
     1444, 1422, 1433, 1441, 1443, 1446, 1434, 1394, 1423, 1440,
     1449, 1452, 1424, 1457,    0, 1417, 1454, 1426,    0, 1417,
     1419, 1418, 1428, 1430, 1432, 1420, 1427, 1411, 1429, 1433,
     1422, 1435, 1461, 1441, 1434, 1464, 1436, 1423, 1447, 1446,

     1440, 1457, 1444, 1453, 1443, 1426, 1449, 1454, 1466, 1801,
     1428, 1801, 1430, 1432, 1427, 1452, 1429, 1463, 1433, 1435,
     1465, 1461, 1441, 1464, 1436, 1447, 1453, 1446,    0, 1440,
     1444, 1472, 1443, 1466, 1449, 1474, 1454, 1455, 1455, 1455,
     1455, 1475, 1467, 1452, 1467, 1468, 1463, 1468, 1468, 1465,
        0,    0, 1467, 1476, 1447, 1453, 1469, 1469, 1469, 1477,
     1472, 1479, 1466, 1474, 1469, 1484, 1470, 1470, 1470, 1470,
     1475, 1480, 1481, 1469, 1470, 1482, 1471, 1471, 1471, 1471,
     1483, 1476, 1485, 1470, 1471, 1486, 1487, 1477, 1489, 1479,
     1490, 1492, 1497, 1471, 1484, 1499,    0, 1500,    0, 1480,

     1481, 1503, 1507, 1482, 1498, 1504, 1498, 1504, 1483, 1495,
     1485, 1495, 1505, 1506, 1486, 1487, 1508, 1489, 1490, 1492,
     1495, 1497, 1495, 1499, 1495, 1500, 1498, 1504, 1509, 1503,
     1507, 1495, 1510, 1511, 1512, 1514, 1520, 1522, 1515, 1516,
     1524, 1505, 1506, 1517, 1508, 1517, 1517, 1518, 1523, 1518,
     1518, 1527, 1538,    0, 1525, 1528, 1509, 1529, 1530, 1511,
     1520, 1510, 1534, 1512, 1514, 1515, 1547, 1516, 1531, 1532,
     1540, 1551, 1543, 1522, 1550, 1523,    0, 1538, 1545,    0,
     1524, 1539, 1539, 1539, 1546, 1530,    0, 1495, 1511, 1520,
     1525, 1528, 1529, 1527, 1515, 1531, 1516,    0, 1534, 1551,

     1550, 1522, 1547, 1539, 1523, 1545, 1538, 1540, 1524,    0,
        0, 1532, 1543,    0, 1530, 1544, 1544, 1544, 1525, 1528,
     1529, 1527, 1552,    0, 1531, 1546, 1534, 1555,    0, 1550,
     1547, 1559,    0, 1559, 1545, 1540, 1549, 1544, 1549, 1532,
     1543, 1548, 1548, 1548, 1548, 1556, 1549, 1556, 1559, 1558,
     1552, 1558, 1558, 1546, 1555, 1556, 1560, 1560, 1560, 1560,
     1561, 1561, 1561, 1569, 1560, 1564, 1565, 1572, 1561, 1568,
     1571, 1573, 1574, 1560, 1563, 1563, 1563, 1561, 1562, 1562,
     1562, 1562, 1563, 1555, 1575, 1580, 1562, 1577, 1584, 1585,
     1591, 1563, 1569, 1564, 1565, 1562, 1572, 1568, 1571, 1573,

     1576, 1574, 1576, 1582, 1587, 1582, 1587, 1590, 1592, 1594,
     1576, 1595, 1596, 1575, 1580, 1577, 1584, 1598, 1585, 1591,
     1599, 1600, 1602, 1600, 1600, 1582, 1587, 1603, 1606, 1609,
     1607, 1622, 1624, 1610, 1613, 1590, 1615, 1592, 1594, 1620,
     1596, 1620,    0, 1599, 1611, 1611, 1611, 1618, 1595, 1620,
     1619, 1637,    0, 1598, 1611, 1639, 1602, 1607, 1610, 1613,
     1624, 1615,    0, 1629, 1629, 1629, 1629, 1609, 1622, 1603,
     1606, 1629, 1599,    0, 1618, 1633, 1595, 1619,    0, 1637,
     1629, 1598,    0, 1639, 1602, 1638, 1607, 1610, 1613,    0,
     1615, 1627, 1627, 1627, 1627, 1609, 1622, 1603, 1606, 1627,

     1640, 1660, 1627, 1618, 1633, 1645, 1619, 1630, 1630, 1630,
     1631, 1631, 1631, 1631, 1638, 1634, 1646, 1634, 1631, 1647,
     1641, 1642, 1641, 1642, 1630, 1634, 1648, 1631, 1654, 1640,
     1641, 1642, 1655, 1656, 1645, 1660, 1657, 1657, 1657, 1661,
     1662, 1663, 1666, 1669,    0, 1646, 1657, 1682, 1647, 1666,
     1669, 1674, 1666, 1669, 1654, 1648,    0, 1655, 1674, 1681,
     1656, 1674, 1683, 1660,    0,    0,    0, 1662, 1663,    0,
     1661,    0, 1667, 1667, 1667, 1667, 1682, 1668, 1668, 1668,
     1668, 1689,    0, 1654, 1667, 1668, 1655, 1681, 1668, 1656,
        0, 1683, 1672, 1672, 1672, 1672, 1662, 1663, 1667, 1661,

     1672, 1684, 1686, 1672, 1673, 1673, 1673, 1673, 1675, 1675,
     1675, 1675, 1673, 1687, 1692, 1673, 1675, 1689, 1676, 1676,
     1676, 1676, 1679, 1690, 1679, 1675, 1676, 1667, 1694, 1684,
     1676, 1686, 1679, 1695, 1680, 1676, 1680, 1697, 1712, 1706,
        0, 1692, 1687, 1715, 1676, 1689,    0, 1680, 1713, 1680,
     1698, 1690, 1698, 1698, 1714, 1694, 1680, 1699, 1699, 1699,
     1699, 1705, 1700, 1697, 1700, 1700,    0, 1712, 1705, 1699,
     1692, 1705, 1715, 1676, 1695, 1706, 1713, 1703, 1703, 1703,
     1703, 1716, 1714, 1699, 1694, 1703, 1722, 1718, 1703,    0,
     1707, 1723, 1697, 1704, 1704, 1704, 1704, 1707, 1724, 1717,

     1707, 1704, 1695, 1706, 1704, 1704, 1708, 1708, 1708, 1708,
     1716, 1709, 1699, 1710, 1708, 1710, 1719, 1708, 1709, 1704,
     1720, 1709, 1723, 1711, 1718, 1711, 1710, 1717, 1710, 1743,
     1746, 1741, 1722, 1741, 1711, 1710, 1711, 1725, 1711, 1725,
     1725, 1724, 1741, 1744, 1719, 1711, 1720, 1726, 1704, 1726,
     1726, 1723, 1718, 1729, 1729, 1729, 1729, 1743, 1746, 1737,
     1722, 1729, 1749,    0, 1729, 1730, 1730, 1730, 1730, 1724,
     1751, 1755, 1744, 1730, 1747, 1720, 1730, 1730, 1731, 1731,
     1731, 1731,    0, 1732, 1748, 1733, 1731, 1734,    0, 1731,
     1732, 1730, 1733, 1732, 1734, 1733, 1737, 1734, 1735, 1735,

     1735, 1735, 1747, 1736, 1749, 1750, 1735, 1751, 1755, 1735,
     1736, 1740, 1748, 1736, 1738, 1738, 1738, 1738, 1740, 1756,
     1730, 1740, 1738,    0, 1737, 1738, 1739, 1739, 1739, 1739,
        0, 1776, 1749, 1750, 1739, 1751, 1755, 1739, 1757,    0,
     1757, 1757, 1758, 1794, 1758, 1758, 1759, 1759, 1759, 1759,
     1760, 1760, 1760, 1760, 1759, 1761, 1771, 1759, 1760, 1756,
     1776, 1760, 1761, 1763, 1773, 1761, 1762, 1762, 1762, 1762,
     1763, 1764, 1794, 1763, 1762, 1780,    0, 1762, 1764, 1796,
     1821, 1764, 1765, 1765, 1765, 1765, 1774, 1756, 1766, 1766,
     1766, 1766, 1773, 1767, 1765, 1781, 1766, 1775, 1771, 1766,

     1767, 1811, 1792, 1767, 1768, 1768, 1768, 1768, 1765, 1821,
     1797, 1780, 1768, 1770, 1774, 1768, 1769, 1769, 1769, 1769,
     1770, 1796, 1781, 1770, 1769, 1775, 1771, 1769, 1769, 1782,
     1792, 1782, 1782, 1783, 1798, 1783, 1783, 1765, 1797, 1780,
     1812, 1811, 1769, 1788, 1784, 1784, 1784, 1784,    0, 1796,
     1788, 1781, 1784, 1788, 1835, 1784, 1785, 1785, 1785, 1785,
        0, 1819, 1813, 1814, 1785, 1814, 1814, 1785, 1812, 1811,
     1798, 1769, 1786, 1786, 1786, 1786, 1787, 1787, 1787, 1787,
     1786,    0, 1835, 1786, 1787,    0, 1790, 1787, 1787, 1789,
     1789, 1789, 1789, 1790, 1808, 1830, 1790, 1789, 1798, 1813,

     1789, 1819, 1787, 1803, 1803, 1803, 1803, 1804, 1804, 1804,
     1804, 1803, 1805, 1807, 1803, 1804,    0,    0, 1804, 1805,
     1807, 1822, 1805, 1807, 1830,    0, 1820, 1813,    0, 1819,
        0, 1787, 1802, 1802, 1802, 1802, 1808, 1802, 1829, 1818,
     1802,    0, 1802, 1802, 1802,    0, 1818, 1802, 1802, 1818,
        0, 1828, 1802, 1820, 1802, 1802, 1802, 1806, 1806, 1806,
     1806, 1829,    0, 1822, 1808, 1806,    0,    0, 1806, 1815,
     1815, 1815, 1815, 1823,    0, 1823, 1823, 1815, 1828,    0,
     1815, 1833, 1820, 1802, 1802, 1802, 1817, 1817, 1817, 1817,
     1829, 1822,    0,    0, 1817, 1825,    0, 1817, 1824, 1824,

     1824, 1824, 1825, 1848, 1833, 1825, 1824, 1828, 1834, 1824,
        0, 1802, 1816, 1816, 1816, 1816, 1816, 1816, 1836, 1826,
     1816, 1816, 1816, 1816, 1816, 1837, 1826, 1816, 1816, 1826,
     1848,    0, 1816, 1833, 1816, 1816, 1816, 1827, 1827, 1827,
     1827, 1832, 1832, 1832, 1832, 1827, 1840,    0, 1827, 1832,
     1834,    0, 1832, 1838, 1838, 1838, 1838,    0,    0, 1848,
     1836, 1860,    0, 1816, 1816, 1816,    0, 1837, 1839, 1839,
     1839, 1839, 1841, 1841, 1841, 1841,    0,    0, 1834, 1842,
     1842, 1842, 1842,    0, 1860,    0, 1840,    0, 1836,    0,
        0, 1816,    0,    0,    0, 1837, 1843, 1843, 1843, 1843,

     1845, 1845, 1845, 1845, 1846, 1846, 1846, 1846, 1849, 1849,
     1849, 1849, 1864, 1860, 1840, 1850, 1850, 1850, 1850, 1852,
     1852, 1852, 1852, 1853, 1853, 1853, 1853, 1856, 1856, 1856,
     1856, 1857, 1857, 1857, 1857, 1859, 1859, 1859, 1859, 1862,
     1862, 1862, 1862, 1865, 1865, 1865, 1865, 1866, 1866, 1866,
     1866,    0,    0,    0, 1864, 1867, 1867, 1867, 1867, 1869,
     1869, 1869, 1869, 1870, 1870, 1870, 1870, 1873, 1873, 1873,
     1873, 1875, 1875, 1875, 1875,    0,    0,    0,    0,    0,
        0,    0, 1864, 1877, 1877, 1877, 1877, 1877, 1877, 1877,
     1877, 1877, 1877, 1877, 1877, 1877, 1877, 1877, 1877, 1877,

     1877, 1878, 1878, 1878, 1878, 1878, 1878, 1878, 1878, 1878,
     1878, 1878, 1878, 1878, 1878, 1878, 1878, 1878, 1878, 1879,
     1879,    0, 1879, 1879, 1879, 1879, 1879, 1879, 1879, 1879,
     1879, 1879, 1879, 1879, 1879, 1879, 1879, 1880, 1880, 1880,
     1880, 1880, 1880, 1880, 1880, 1880, 1880, 1880, 1880, 1880,
     1880, 1880, 1880, 1880, 1880, 1881, 1881, 1881, 1881, 1881,
     1881, 1881, 1881, 1881, 1881, 1881, 1881, 1881, 1881, 1881,
     1881, 1881, 1881, 1882,    0,    0,    0,    0,    0,    0,
     1882,    0, 1882,    0, 1882, 1882, 1882, 1882, 1882, 1883,
     1883, 1883, 1883, 1883, 1884, 1884, 1884, 1884, 1884, 1884,

     1884, 1884, 1884, 1884, 1884, 1884, 1884, 1884, 1884, 1884,
     1884, 1884, 1885, 1885, 1885, 1885, 1885, 1885, 1885, 1885,
     1885, 1885, 1885, 1885, 1885, 1885, 1885, 1885, 1885, 1885,
     1886, 1886, 1886, 1886, 1886, 1886, 1886, 1886, 1886, 1886,
     1886, 1886, 1886, 1886, 1886, 1886, 1886, 1886, 1887,    0,
        0,    0,    0,    0,    0,    0,    0,    0,    0, 1887,
     1887, 1887, 1887, 1887, 1888, 1888, 1888, 1888, 1888, 1888,
     1888, 1888, 1888, 1888, 1888, 1888, 1888, 1888, 1888, 1888,
     1888, 1888, 1889, 1889,    0, 1889, 1889, 1889, 1889, 1889,
     1889, 1889, 1889, 1889, 1889, 1889, 1889, 1889, 1889, 1889,

     1890, 1890, 1890, 1890, 1890, 1890, 1890, 1890, 1890, 1890,
     1890, 1890, 1890, 1890, 1890, 1890, 1890, 1890, 1891, 1891,
     1891, 1891, 1891, 1891, 1891, 1891, 1891, 1891, 1891, 1891,
     1891, 1891, 1891, 1891, 1891, 1891, 1892, 1892, 1892, 1892,
     1892, 1892, 1892, 1892, 1892, 1892, 1892, 1892, 1892, 1892,
     1892, 1892, 1892, 1892, 1893, 1893, 1893, 1893, 1893, 1893,
     1893, 1893, 1893, 1893, 1893, 1893, 1893, 1893, 1893, 1893,
     1893, 1893, 1894,    0,    0,    0,    0,    0,    0, 1894,
        0, 1894,    0,    0, 1894, 1894, 1894, 1894, 1895, 1895,
     1895, 1895,    0, 1895, 1895, 1895, 1895, 1895, 1895,    0,

     1895, 1895,    0,    0, 1895, 1895, 1896, 1896, 1896, 1896,
     1896, 1898, 1898, 1898, 1898, 1898, 1898, 1898, 1898, 1898,
     1898, 1898, 1898, 1898, 1898, 1898, 1898, 1898, 1898, 1899,
     1899, 1899, 1899, 1899, 1899, 1899, 1899, 1899, 1899, 1899,
     1899, 1899, 1899, 1899, 1899, 1899, 1899, 1900, 1900, 1900,
     1900, 1900, 1900, 1900, 1900, 1900, 1900, 1900, 1900, 1900,
     1900, 1900, 1900, 1900, 1900, 1901, 1901, 1901, 1901, 1901,
     1901, 1901, 1901, 1901, 1901, 1901, 1901, 1901, 1901, 1901,
     1901, 1901, 1901, 1902, 1902, 1902, 1902, 1902, 1902, 1902,
     1902, 1902, 1902, 1902, 1902, 1902, 1902, 1902, 1902, 1902,

     1902, 1903, 1903, 1903, 1903, 1903, 1903, 1903, 1903, 1903,
     1903, 1903, 1903, 1903, 1903, 1903, 1903, 1903, 1903, 1904,
     1904, 1904, 1904, 1904, 1904, 1904, 1904, 1904, 1904, 1904,
     1904, 1904, 1904, 1904, 1904, 1904, 1904, 1905, 1905, 1905,
     1905, 1905, 1905, 1905, 1905, 1905, 1905, 1905, 1905, 1905,
     1905, 1905, 1905, 1905, 1905, 1906, 1906, 1906, 1906, 1906,
     1906, 1906, 1906, 1906, 1906, 1906, 1906, 1906, 1906, 1906,
     1906, 1906, 1906, 1907, 1907,    0, 1907, 1907, 1907, 1907,
     1907, 1907, 1907, 1907, 1907, 1907, 1907, 1907, 1907, 1907,
     1907, 1908, 1908, 1908, 1908, 1908, 1908, 1908, 1908, 1908,

     1908, 1908, 1908, 1908, 1908, 1908, 1908, 1908, 1908, 1909,
     1909, 1909, 1909, 1909, 1909, 1909, 1909, 1909, 1909, 1909,
     1909, 1909, 1909, 1909, 1909, 1909, 1909, 1910, 1910, 1910,
     1910, 1910, 1910, 1910, 1910, 1910, 1910, 1910, 1910, 1910,
     1910, 1910, 1910, 1910, 1910, 1911, 1911, 1911, 1911, 1911,
     1911, 1911, 1911, 1911, 1911, 1911, 1911, 1911, 1911, 1911,
     1911, 1911, 1911, 1912, 1912, 1912, 1912, 1912, 1912, 1912,
     1912, 1912, 1912, 1912, 1912, 1912, 1912, 1912, 1912, 1912,
     1912, 1913,    0,    0,    0,    0,    0,    0, 1913,    0,
     1913,    0,    0, 1913, 1913, 1913, 1913, 1914,    0,    0,

        0,    0,    0,    0,    0, 1914,    0, 1914,    0, 1914,
     1914, 1914, 1914, 1914, 1915, 1915, 1915, 1915, 1916, 1916,
     1916, 1916, 1916, 1916, 1916, 1916, 1916, 1916, 1916, 1916,
     1916, 1916, 1916, 1916, 1916, 1916, 1917, 1917, 1917, 1917,
     1917, 1917, 1917, 1917, 1917, 1917, 1917, 1917, 1917, 1917,
     1917, 1917, 1917, 1917, 1918, 1918, 1918, 1918, 1918, 1918,
     1918, 1918, 1918, 1918, 1918, 1918, 1918, 1918, 1918, 1918,
     1918, 1918, 1919, 1919, 1919, 1919,    0, 1919, 1919, 1919,
     1919, 1919, 1919,    0, 1919, 1919,    0,    0, 1919, 1919,
     1920, 1920, 1920, 1920, 1920, 1921, 1921, 1921, 1921, 1921,

     1921, 1921, 1921, 1921, 1921, 1921, 1921, 1921, 1921, 1921,
     1921, 1921, 1921, 1922,    0,    0,    0,    0,    0,    0,
        0, 1922, 1922, 1923, 1923, 1923, 1923, 1923, 1923, 1923,
     1923, 1923, 1923, 1923, 1923, 1923, 1923, 1923, 1923, 1923,
     1923, 1924, 1924, 1924, 1924, 1924, 1924, 1924, 1924, 1924,
     1924, 1924, 1924, 1924, 1924, 1924, 1924, 1924, 1924, 1925,
     1925, 1925, 1925, 1925, 1925, 1925, 1925, 1925, 1925, 1925,
     1925, 1925, 1925, 1925, 1925, 1925, 1925, 1926, 1926, 1926,
     1926, 1926, 1926, 1926, 1926, 1926, 1926, 1926, 1926, 1926,
     1926, 1926, 1926, 1926, 1926, 1927, 1927, 1927, 1927, 1927,

     1927, 1927, 1927, 1927, 1927, 1927, 1927, 1927, 1927, 1927,
     1927, 1927, 1927, 1928, 1928, 1928, 1928, 1928, 1928, 1928,
     1928, 1928, 1928, 1928, 1928, 1928, 1928, 1928, 1928, 1928,
     1928, 1929, 1929, 1929, 1929, 1929, 1929, 1929, 1929, 1929,
     1929, 1929, 1929, 1929, 1929, 1929, 1929, 1929, 1929, 1930,
     1930, 1930, 1930, 1930, 1930, 1930, 1930, 1930, 1930, 1930,
     1930, 1930, 1930, 1930, 1930, 1930, 1930, 1931,    0,    0,
        0,    0,    0,    0,    0,    0,    0,    0, 1931, 1931,
     1931, 1931, 1931, 1932, 1932, 1932, 1932, 1932, 1932, 1932,
     1932, 1932, 1932, 1932, 1932, 1932, 1932, 1932, 1932, 1932,

     1932, 1933, 1933, 1933, 1933, 1933, 1933, 1933, 1933, 1933,
     1933, 1933, 1933, 1933, 1933, 1933, 1933, 1933, 1933, 1934,
     1934, 1934, 1934, 1934, 1934, 1934, 1934, 1934, 1934, 1934,
     1934, 1934, 1934, 1934, 1934, 1934, 1934, 1935, 1935,    0,
     1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935, 1935,
     1935, 1935, 1935, 1935, 1935, 1936, 1936, 1936, 1936, 1936,
     1936, 1936, 1936, 1936, 1936, 1936, 1936, 1936, 1936, 1936,
     1936, 1936, 1936, 1937, 1937, 1937, 1937, 1937, 1937, 1937,
     1937, 1937, 1937, 1937, 1937, 1937, 1937, 1937, 1937, 1937,
     1937, 1938, 1938, 1938, 1938, 1938, 1938, 1938, 1938, 1938,

     1938, 1938, 1938, 1938, 1938, 1938, 1938, 1938, 1938, 1939,
     1939, 1939, 1939, 1939, 1939, 1939, 1939, 1939, 1939, 1939,
     1939, 1939, 1939, 1939, 1939, 1939, 1939, 1940,    0,    0,
        0,    0,    0,    0, 1940,    0, 1940,    0,    0, 1940,
     1940, 1940, 1940, 1941,    0,    0,    0,    0,    0,    0,
        0, 1941,    0,    0,    0, 1941, 1941, 1941, 1941, 1941,
     1942,    0,    0,    0,    0,    0,    0,    0, 1942,    0,
     1942,    0, 1942, 1942, 1942, 1942, 1942, 1943, 1943, 1943,
     1943, 1943, 1943, 1943, 1943, 1943, 1943, 1943, 1943, 1943,
     1943, 1943, 1943, 1943, 1943, 1944, 1944, 1944, 1944, 1944,

     1944, 1944, 1944, 1944, 1944, 1944, 1944, 1944, 1944, 1944,
     1944, 1944, 1944, 1945, 1945, 1945, 1945, 1945, 1945, 1945,
     1945, 1945, 1945, 1945, 1945, 1945, 1945, 1945, 1945, 1945,
     1945, 1946, 1946, 1946, 1946, 1946, 1946, 1946, 1946, 1946,
     1946, 1946, 1946, 1946, 1946, 1946, 1946, 1946, 1946, 1947,
     1947, 1947, 1947, 1947, 1948, 1948, 1948, 1948, 1948, 1948,
     1948, 1948, 1948, 1948, 1948, 1948, 1948, 1948, 1948, 1948,
     1948, 1948, 1949, 1949, 1949, 1949, 1949, 1949,    0, 1949,
     1949, 1949, 1949, 1949, 1949, 1949, 1949, 1949, 1949, 1949,
     1950, 1950,    0, 1950, 1950, 1950, 1950, 1950, 1950, 1950,

     1950, 1950, 1950, 1950, 1950, 1950, 1950, 1950, 1951, 1951,
     1951, 1951, 1951, 1951, 1951, 1951, 1951, 1951, 1951, 1951,
     1951, 1951, 1951, 1951, 1951, 1951, 1952, 1952, 1952, 1952,
     1952, 1952, 1952, 1952, 1952, 1952, 1952, 1952, 1952, 1952,
     1952, 1952, 1952, 1952, 1953, 1953, 1953, 1953, 1953, 1953,
     1953, 1953, 1953, 1953, 1953, 1953, 1953, 1953, 1953, 1953,
     1953, 1953, 1954, 1954, 1954, 1954, 1954, 1954, 1954, 1954,
     1954, 1954, 1954, 1954, 1954, 1954, 1954, 1954, 1954, 1954,
     1955, 1955, 1955, 1955, 1955, 1955, 1955, 1955, 1955, 1955,
     1955, 1955, 1955, 1955, 1955, 1955, 1955, 1955, 1956, 1956,

     1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956, 1956,
     1956, 1956, 1956, 1956, 1956, 1956, 1957, 1957,    0, 1957,
     1957, 1957, 1957, 1957, 1957, 1957, 1957, 1957, 1957, 1957,
     1957, 1957, 1957, 1957, 1958, 1958,    0, 1958, 1958, 1958,
     1958, 1958, 1958, 1958, 1958, 1958, 1958, 1958, 1958, 1958,
     1958, 1958, 1959, 1959,    0, 1959, 1959, 1959, 1959, 1959,
     1959, 1959, 1959, 1959, 1959, 1959, 1959, 1959, 1959, 1959,
     1960, 1960, 1960, 1960, 1960, 1960, 1960, 1960, 1960, 1960,
     1960, 1960, 1960, 1960, 1960, 1960, 1960, 1960, 1961, 1961,
     1961, 1961, 1961, 1961, 1961, 1961, 1961, 1961, 1961, 1961,

     1961, 1961, 1961, 1961, 1961, 1961, 1962, 1962, 1962, 1962,
     1962, 1962, 1962, 1962, 1962, 1962, 1962, 1962, 1962, 1962,
     1962, 1962, 1962, 1962, 1963, 1963, 1963, 1963, 1963, 1963,
     1963, 1963, 1963, 1963, 1963, 1963, 1963, 1963, 1963, 1963,
     1963, 1963, 1964,    0,    0,    0,    0,    0, 1964,    0,
        0,    0, 1964,    0, 1964, 1964, 1964, 1964, 1964, 1965,
     1965, 1965, 1965, 1966,    0,    0,    0,    0,    0,    0,
        0, 1966,    0,    0,    0, 1966, 1966, 1966, 1966, 1966,
     1967,    0,    0,    0,    0,    0,    0,    0, 1967,    0,
     1967,    0, 1967, 1967, 1967, 1967, 1967, 1968, 1968,    0,

     1968, 1968, 1968, 1968, 1968, 1968, 1968, 1968, 1968, 1968,
     1968, 1968, 1968, 1968, 1968, 1969, 1969, 1969, 1969, 1969,
     1969, 1969, 1969, 1969, 1969, 1969, 1969, 1969, 1969, 1969,
     1969, 1969, 1969, 1970, 1970,    0, 1970, 1970, 1970, 1970,
     1970, 1970, 1970, 1970, 1970, 1970, 1970, 1970, 1970, 1970,
     1970, 1971, 1971, 1971, 1971, 1971, 1971,    0, 1971, 1971,
     1971, 1971, 1971, 1971, 1971, 1971, 1971, 1971, 1971, 1972,
     1972,    0, 1972, 1972, 1972, 1972, 1972, 1972, 1972, 1972,
     1972, 1972, 1972, 1972, 1972, 1972, 1972, 1973, 1973, 1973,
     1973, 1973, 1973, 1973, 1973, 1973, 1973, 1973, 1973, 1973,

     1973, 1973, 1973, 1973, 1973, 1974, 1974, 1974, 1974, 1974,
     1974, 1974, 1974, 1974, 1974, 1974, 1974, 1974, 1974, 1974,
     1974, 1974, 1974, 1975, 1975, 1975, 1975, 1975, 1975, 1975,
     1975, 1975, 1975, 1975, 1975, 1975, 1975, 1975, 1975, 1975,
     1975, 1976, 1976, 1976, 1976, 1976, 1976, 1976, 1976, 1976,
     1976, 1976, 1976, 1976, 1976, 1976, 1976, 1976, 1976, 1977,
     1977, 1977, 1977, 1977, 1977, 1977, 1977, 1977, 1977, 1977,
     1977, 1977, 1977, 1977, 1977, 1977, 1977, 1978, 1978, 1978,
     1978, 1978, 1978, 1978, 1978, 1978, 1978, 1978, 1978, 1978,
     1978, 1978, 1978, 1978, 1978, 1979, 1979, 1979, 1979, 1979,

     1979, 1979, 1979, 1979, 1979, 1979, 1979, 1979, 1979, 1979,
     1979, 1979, 1979, 1980, 1980, 1980, 1980, 1980, 1980, 1980,
     1980, 1980, 1980, 1980, 1980, 1980, 1980, 1980, 1980, 1980,
     1980, 1981, 1981, 1981, 1981, 1981, 1981, 1981, 1981, 1981,
     1981, 1981, 1981, 1981, 1981, 1981, 1981, 1981, 1981, 1982,
     1982, 1982, 1982, 1982, 1982, 1982, 1982, 1982, 1982, 1982,
     1982, 1982, 1982, 1982, 1982, 1982, 1982, 1983, 1983,    0,
     1983, 1983, 1983, 1983, 1983, 1983, 1983, 1983, 1983, 1983,
     1983, 1983, 1983, 1983, 1983, 1984, 1984,    0, 1984, 1984,
     1984, 1984, 1984, 1984, 1984, 1984, 1984, 1984, 1984, 1984,

     1984, 1984, 1984, 1985, 1985,    0, 1985, 1985, 1985, 1985,
     1985, 1985, 1985, 1985, 1985, 1985, 1985, 1985, 1985, 1985,
     1985, 1986, 1986, 1986, 1986, 1986, 1986, 1986, 1986, 1986,
     1986, 1986, 1986, 1986, 1986, 1986, 1986, 1986, 1986, 1987,
     1987, 1987, 1987, 1987, 1987, 1987, 1987, 1987, 1987, 1987,
     1987, 1987, 1987, 1987, 1987, 1987, 1987, 1988, 1988, 1988,
     1988, 1988, 1988, 1988, 1988, 1988, 1988, 1988, 1988, 1988,
     1988, 1988, 1988, 1988, 1988, 1989, 1989, 1989, 1989, 1989,
     1989, 1989, 1989, 1989, 1989, 1989, 1989, 1989, 1989, 1989,
     1989, 1989, 1989, 1990,    0,    0,    0,    0,    0, 1990,

        0,    0,    0,    0,    0, 1990, 1990, 1990, 1990, 1990,
     1991, 1991,    0, 1991, 1991, 1991, 1991, 1991, 1991, 1991,
     1991, 1991, 1991, 1991, 1991, 1991, 1991, 1991, 1992,    0,
        0,    0,    0,    0,    0, 1992,    0, 1992,    0,    0,
     1992, 1992, 1992, 1992, 1993,    0,    0,    0,    0,    0,
        0,    0, 1993,    0, 1993,    0, 1993, 1993, 1993, 1993,
     1993, 1994, 1994, 1994, 1994, 1995, 1995,    0, 1995, 1995,
     1995, 1995, 1995, 1995, 1995, 1995, 1995, 1995, 1995, 1995,
     1995, 1995, 1995, 1996, 1996, 1996, 1996, 1996, 1996,    0,
     1996, 1996, 1996, 1996, 1996, 1996, 1996, 1996, 1996, 1996,

     1996, 1997, 1997,    0, 1997, 1997, 1997, 1997, 1997, 1997,
     1997, 1997, 1997, 1997, 1997, 1997, 1997, 1997, 1997, 1998,
     1998,    0, 1998, 1998, 1998, 1998, 1998, 1998, 1998, 1998,
     1998, 1998, 1998, 1998, 1998, 1998, 1998, 1999, 1999, 1999,
     1999, 1999, 1999, 1999, 1999, 1999, 1999, 1999, 1999, 1999,
     1999, 1999, 1999, 1999, 1999, 2000, 2000, 2000, 2000, 2000,
     2000, 2000, 2000, 2000, 2000, 2000, 2000, 2000, 2000, 2000,
     2000, 2000, 2000, 2001, 2001, 2001, 2001, 2001, 2001, 2001,
     2001, 2001, 2001, 2001, 2001, 2001, 2001, 2001, 2001, 2001,
     2001, 2002, 2002, 2002, 2002, 2002, 2002, 2002, 2002, 2002,

     2002, 2002, 2002, 2002, 2002, 2002, 2002, 2002, 2002, 2003,
        0, 2003,    0,    0,    0,    0, 2003,    0,    0, 2003,
     2003, 2003, 2003, 2003, 2003, 2004, 2004, 2004, 2004, 2004,
     2004, 2004, 2004, 2004, 2004, 2004, 2004, 2004, 2004, 2004,
     2004, 2004, 2004, 2005, 2005, 2005, 2005, 2005, 2005, 2005,
     2005, 2005, 2005, 2005, 2005, 2005, 2005, 2005, 2005, 2005,
     2005, 2006, 2006, 2006, 2006, 2006, 2006, 2006, 2006, 2006,
     2006, 2006, 2006, 2006, 2006, 2006, 2006, 2006, 2006, 2007,
     2007, 2007, 2007, 2007, 2007, 2007, 2007, 2007, 2007, 2007,
     2007, 2007, 2007, 2007, 2007, 2007, 2007, 2008, 2008, 2008,

     2008, 2008, 2008, 2008, 2008, 2008, 2008, 2008, 2008, 2008,
     2008, 2008, 2008, 2008, 2008, 2009, 2009,    0, 2009, 2009,
     2009, 2009, 2009, 2009, 2009, 2009, 2009, 2009, 2009, 2009,
     2009, 2009, 2009, 2010, 2010, 2010, 2010, 2010, 2010, 2010,
     2010, 2010, 2010, 2010, 2010, 2010, 2010, 2010, 2010, 2010,
     2010, 2011, 2011, 2011, 2011, 2011, 2011, 2011, 2011, 2011,
     2011, 2011, 2011, 2011, 2011, 2011, 2011, 2011, 2011, 2012,
        0,    0,    0,    0,    0, 2012,    0,    0,    0,    0,
        0, 2012, 2012, 2012, 2012, 2012, 2013,    0, 2013,    0,
        0,    0,    0, 2013,    0,    0, 2013, 2013, 2013, 2013,

     2013, 2013, 2014,    0, 2014,    0,    0,    0,    0, 2014,
        0,    0, 2014, 2014, 2014, 2014, 2014, 2014, 2015, 2015,
     2015, 2015, 2015, 2015, 2015, 2015, 2015, 2015, 2015, 2015,
     2015, 2015, 2015, 2015, 2015, 2015, 2016, 2016, 2016, 2016,
     2016, 2017, 2017,    0, 2017, 2017, 2017, 2017, 2017, 2017,
     2017, 2017, 2017, 2017, 2017, 2017, 2017, 2017, 2017, 2018,
     2018, 2018, 2018, 2018, 2018, 2018, 2018, 2018, 2018, 2018,
     2018, 2018, 2018, 2018, 2018, 2018, 2018, 2019, 2019, 2019,
     2019, 2019, 2019, 2019, 2019, 2019, 2019, 2019, 2019, 2019,
     2019, 2019, 2019, 2019, 2019, 2020, 2020, 2020, 2020, 2020,

     2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020,
     2020, 2020, 2020, 2021, 2021, 2021, 2021, 2021, 2021, 2021,
     2021, 2021, 2021, 2021, 2021, 2021, 2021, 2021, 2021, 2021,
     2021, 2022, 2022, 2022, 2022, 2022, 2022, 2022, 2022, 2022,
     2022, 2022, 2022, 2022, 2022, 2022, 2022, 2022, 2022, 2023,
     2023, 2023, 2023, 2023, 2023, 2023, 2023, 2023, 2023, 2023,
     2023, 2023, 2023, 2023, 2023, 2023, 2023, 2024, 2024, 2024,
     2024, 2024, 2024, 2024, 2024, 2024, 2024, 2024, 2024, 2024,
     2024, 2024, 2024, 2024, 2024, 2025, 2025, 2025, 2025, 2025,
     2025, 2025, 2025, 2025, 2025, 2025, 2025, 2025, 2025, 2025,

     2025, 2025, 2025, 2026, 2026, 2026, 2026, 2026, 2026, 2026,
     2026, 2026, 2026, 2026, 2026, 2026, 2026, 2026, 2026, 2026,
     2026, 2027, 2027, 2027, 2027, 2027, 2027, 2027, 2027, 2027,
     2027, 2027, 2027, 2027, 2027, 2027, 2027, 2027, 2027, 2028,
     2028, 2028, 2028, 2028, 2028, 2028, 2028, 2028, 2028, 2028,
     2028, 2028, 2028, 2028, 2028, 2028, 2028, 2029, 2029, 2029,
     2029, 2029, 2029, 2029, 2029, 2029, 2029, 2029, 2029, 2029,
     2029, 2029, 2029, 2029, 2029, 2030, 2030, 2030, 2030, 2030,
     2030, 2030, 2030, 2030, 2030, 2030, 2030, 2030, 2030, 2030,
     2030, 2030, 2030, 2031, 2031, 2031, 2031, 2031, 2031, 2031,

     2031, 2031, 2031, 2031, 2031, 2031, 2031, 2031, 2031, 2031,
     2031, 2032, 2032, 2032, 2032, 2032, 2032, 2032, 2032, 2032,
     2032, 2032, 2032, 2032, 2032, 2032, 2032, 2032, 2032, 2033,
     2033, 2033, 2033, 2033, 2033, 2033, 2033, 2033, 2033, 2033,
     2033, 2033, 2033, 2033, 2033, 2033, 2033, 2034, 2034, 2034,
     2034, 2034, 2034, 2034, 2034, 2034, 2034, 2034, 2034, 2034,
     2034, 2034, 2034, 2034, 2034, 1876, 1876, 1876, 1876, 1876,
     1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876,
     1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876,
     1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876,

     1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876,
     1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876,
     1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876,
     1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876, 1876,
     1876, 1876, 1876, 1876, 1876, 1876
    } ;

extern int yy_flex_debug;
int yy_flex_debug = 0;

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
*yy_cp = (yy_hold_char); /* undo effects of setting up yytext */ \
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
char *yytext;
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






#line 45 "fortran.lex"
#include <math.h>
#include <stdlib.h>
#include <string.h>
extern FILE * yyin;
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

#define PRINT_LINE_NUM()    // { fprintf(stderr,"== Parsing l.%4d...\n", line_num_input); }
#define INCREMENT_LINE_NUM() { line_num_input+=count_newlines(fortran_text) ; PRINT_LINE_NUM(); }
#define YY_USER_ACTION       { if (increment_nbtokens !=0) token_since_endofstmt++; increment_nbtokens = 1; if (token_since_endofstmt>=1) lastwasendofstmt=0; /*printf("VALLIJSDFLSD = %d %d %s \n",lastwasendofstmt,token_since_endofstmt,fortran_text); */ if (firstpass) { strcpy(linebuf1, linebuf2); strncpy(linebuf2, fortran_text,80);} \
                               else {my_position_before=setposcur();/*printf("muposition = %d\n",my_position_before);*/ECHO;} }
#define YY_BREAK {/*printf("VALL = %d %d\n",lastwasendofstmt,token_since_endofstmt);*/if (token_since_endofstmt>=1) lastwasendofstmt=0; break;}

void out_of_donottreat(void);

#line 3701 "fortran.yy.c"
#line 3702 "fortran.yy.c"

#define INITIAL 0
#define parameter 1
#define character 2
#define donottreat 3
#define includestate 4
#define fortran77style 5
#define fortran90style 6

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

static int yy_init_globals ( void );

/* Accessor methods to globals.
   These are made visible to non-reentrant scanners for convenience. */

int yylex_destroy ( void );

int yyget_debug ( void );

void yyset_debug ( int debug_flag  );

YY_EXTRA_TYPE yyget_extra ( void );

void yyset_extra ( YY_EXTRA_TYPE user_defined  );

FILE *yyget_in ( void );

void yyset_in  ( FILE * _in_str  );

FILE *yyget_out ( void );

void yyset_out  ( FILE * _out_str  );

			int yyget_leng ( void );

char *yyget_text ( void );

int yyget_lineno ( void );

void yyset_lineno ( int _line_number  );

/* Macros after this point can all be overridden by user definitions in
 * section 1.
 */

#ifndef YY_SKIP_YYWRAP
#ifdef __cplusplus
extern "C" int yywrap ( void );
#else
extern int yywrap ( void );
#endif
#endif

#ifndef YY_NO_UNPUT
    
    static void yyunput ( int c, char *buf_ptr  );
    
#endif

#ifndef yytext_ptr
static void yy_flex_strncpy ( char *, const char *, int );
#endif

#ifdef YY_NEED_STRLEN
static int yy_flex_strlen ( const char * );
#endif

#ifndef YY_NO_INPUT
#ifdef __cplusplus
static int yyinput ( void );
#else
static int input ( void );
#endif

#endif

/* Amount of stuff to slurp up with each read. */
#ifndef YY_READ_BUF_SIZE
#ifdef __ia64__
/* On IA-64, the buffer size is 16k, not 8k */
#define YY_READ_BUF_SIZE 16384
#else
#define YY_READ_BUF_SIZE 8192
#endif /* __ia64__ */
#endif

/* Copy whatever the last rule matched to the standard output. */
#ifndef ECHO
/* This used to be an fputs(), but since the string might contain NUL's,
 * we now use fwrite().
 */
#define ECHO do { if (fwrite( yytext, (size_t) yyleng, 1, yyout )) {} } while (0)
#endif

/* Gets input and stuffs it into "buf".  number of characters read, or YY_NULL,
 * is returned in "result".
 */
#ifndef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
	if ( YY_CURRENT_BUFFER_LVALUE->yy_is_interactive ) \
		{ \
		int c = '*'; \
		int n; \
		for ( n = 0; n < max_size && \
			     (c = getc( yyin )) != EOF && c != '\n'; ++n ) \
			buf[n] = (char) c; \
		if ( c == '\n' ) \
			buf[n++] = (char) c; \
		if ( c == EOF && ferror( yyin ) ) \
			YY_FATAL_ERROR( "input in flex scanner failed" ); \
		result = n; \
		} \
	else \
		{ \
		errno=0; \
		while ( (result = (int) fread(buf, 1, (yy_size_t) max_size, yyin)) == 0 && ferror(yyin)) \
			{ \
			if( errno != EINTR) \
				{ \
				YY_FATAL_ERROR( "input in flex scanner failed" ); \
				break; \
				} \
			errno=0; \
			clearerr(yyin); \
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

extern int yylex (void);

#define YY_DECL int yylex (void)
#endif /* !YY_DECL */

/* Code executed at the beginning of each rule, after yytext and yyleng
 * have been set up.
 */
#ifndef YY_USER_ACTION
#define YY_USER_ACTION
#endif

/* Code executed at the end of each rule. */
#ifndef YY_BREAK
#define YY_BREAK /*LINTED*/break;
#endif

#define YY_RULE_SETUP \
	if ( yyleng > 0 ) \
		YY_CURRENT_BUFFER_LVALUE->yy_at_bol = \
				(yytext[yyleng - 1] == '\n'); \
	YY_USER_ACTION

/** The main scanner function which does all the work.
 */
YY_DECL
{
	yy_state_type yy_current_state;
	char *yy_cp, *yy_bp;
	int yy_act;
    
	if ( !(yy_init) )
		{
		(yy_init) = 1;

#ifdef YY_USER_INIT
		YY_USER_INIT;
#endif

        /* Create the reject buffer large enough to save one state per allowed character. */
        if ( ! (yy_state_buf) )
            (yy_state_buf) = (yy_state_type *)yyalloc(YY_STATE_BUF_SIZE  );
            if ( ! (yy_state_buf) )
                YY_FATAL_ERROR( "out of dynamic memory in yylex()" );

		if ( ! (yy_start) )
			(yy_start) = 1;	/* first start state */

		if ( ! yyin )
			yyin = stdin;

		if ( ! yyout )
			yyout = stdout;

		if ( ! YY_CURRENT_BUFFER ) {
			yyensure_buffer_stack ();
			YY_CURRENT_BUFFER_LVALUE =
				yy_create_buffer( yyin, YY_BUF_SIZE );
		}

		yy_load_buffer_state(  );
		}

	{
#line 100 "fortran.lex"

#line 102 "fortran.lex"
  if (infixed) BEGIN(fortran77style) ;
  if (infree)  BEGIN(fortran90style) ;

#line 3940 "fortran.yy.c"

	while ( /*CONSTCOND*/1 )		/* loops until end-of-file is reached */
		{
		yy_cp = (yy_c_buf_p);

		/* Support of yytext. */
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
			YY_CHAR yy_c = yy_ec[YY_SC_TO_UI(*yy_cp)] ;
			while ( yy_chk[yy_base[yy_current_state] + yy_c] != yy_current_state )
				{
				yy_current_state = (int) yy_def[yy_current_state];
				if ( yy_current_state >= 1877 )
					yy_c = yy_meta[yy_c];
				}
			yy_current_state = yy_nxt[yy_base[yy_current_state] + yy_c];
			*(yy_state_ptr)++ = yy_current_state;
			++yy_cp;
			}
		while ( yy_base[yy_current_state] != 9366 );

yy_find_action:
		yy_current_state = *--(yy_state_ptr);
		(yy_lp) = yy_accept[yy_current_state];
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
{ pos_cur_decl = setposcur()-5; return TOK_TYPEPAR; }
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
*yy_cp = (yy_hold_char); /* undo effects of setting up yytext */
YY_LINENO_REWIND_TO(yy_bp + 2);
(yy_c_buf_p) = yy_cp = yy_bp + 2;
YY_DO_BEFORE_ACTION; /* set up yytext again */
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
*yy_cp = (yy_hold_char); /* undo effects of setting up yytext */
(yy_c_buf_p) = yy_cp = yy_bp + 3;
YY_DO_BEFORE_ACTION; /* set up yytext again */
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
*yy_cp = (yy_hold_char); /* undo effects of setting up yytext */
(yy_c_buf_p) = yy_cp = yy_bp + 4;
YY_DO_BEFORE_ACTION; /* set up yytext again */
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
*yy_cp = (yy_hold_char); /* undo effects of setting up yytext */
(yy_c_buf_p) = yy_cp = yy_bp + 4;
YY_DO_BEFORE_ACTION; /* set up yytext again */
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
*yy_cp = (yy_hold_char); /* undo effects of setting up yytext */
(yy_c_buf_p) = yy_cp = yy_bp + 3;
YY_DO_BEFORE_ACTION; /* set up yytext again */
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
*yy_cp = (yy_hold_char); /* undo effects of setting up yytext */
(yy_c_buf_p) = yy_cp = yy_bp + 6;
YY_DO_BEFORE_ACTION; /* set up yytext again */
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
{ BEGIN(donottreat); }
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
{strcpy(yylval.na,fortran_text); removenewline(yylval.na);
                            return TOK_NAME; }
	YY_BREAK
case 150:
YY_RULE_SETUP
#line 333 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_NAME; }
	YY_BREAK
case 151:
YY_RULE_SETUP
#line 334 "fortran.lex"
{strcpy(yylval.na,fortran_text); return TOK_CSTREAL; }
	YY_BREAK
case 152:
/* rule 152 can match eol */
*yy_cp = (yy_hold_char); /* undo effects of setting up yytext */
YY_LINENO_REWIND_TO(yy_cp - 1);
(yy_c_buf_p) = yy_cp -= 1;
YY_DO_BEFORE_ACTION; /* set up yytext again */
YY_RULE_SETUP
#line 335 "fortran.lex"
{  // REAL1
                              strcpy(yylval.na,fortran_text); return TOK_CSTREAL; }
	YY_BREAK
case 153:
YY_RULE_SETUP
#line 337 "fortran.lex"
{  // REAL2
                              strcpy(yylval.na,fortran_text); return TOK_CSTREAL; }
	YY_BREAK
case 154:
YY_RULE_SETUP
#line 339 "fortran.lex"
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
case 155:
YY_RULE_SETUP
#line 353 "fortran.lex"
{}
	YY_BREAK
case 156:
YY_RULE_SETUP
#line 354 "fortran.lex"
{}
	YY_BREAK
case 157:
*yy_cp = (yy_hold_char); /* undo effects of setting up yytext */
(yy_c_buf_p) = yy_cp = yy_bp + 1;
YY_DO_BEFORE_ACTION; /* set up yytext again */
YY_RULE_SETUP
#line 355 "fortran.lex"
{
                            in_complex_literal = -1;
                            return (int) *fortran_text;
                            }
	YY_BREAK
case 158:
YY_RULE_SETUP
#line 359 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return (int) *fortran_text; }
	YY_BREAK
case 159:
YY_RULE_SETUP
#line 360 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return (int) *fortran_text; }
	YY_BREAK
case 160:
YY_RULE_SETUP
#line 361 "fortran.lex"
{ lastwasendofstmt=1; token_since_endofstmt = 0; return TOK_SEMICOLON; }
	YY_BREAK
case 161:
YY_RULE_SETUP
#line 362 "fortran.lex"
{ if (in_complex_literal==-1) {return TOK_COMMACOMPLEX; in_complex_literal=0;} else; return (int) *fortran_text; }
	YY_BREAK
case 162:
YY_RULE_SETUP
#line 363 "fortran.lex"
{ return (int) *fortran_text; }
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
/* rule 165 can match eol */
YY_RULE_SETUP
#line 366 "fortran.lex"
{ INCREMENT_LINE_NUM() ; lastwasendofstmt=1; token_since_endofstmt = 0; increment_nbtokens = 0; return '\n'; }
	YY_BREAK
case 166:
YY_RULE_SETUP
#line 367 "fortran.lex"
{increment_nbtokens = 0;}
	YY_BREAK
case 167:
/* rule 167 can match eol */
YY_RULE_SETUP
#line 368 "fortran.lex"
{
                              return TOK_LABEL_FORMAT; }
	YY_BREAK
case 168:
/* rule 168 can match eol */
YY_RULE_SETUP
#line 370 "fortran.lex"
{return TOK_LABEL_FORMAT; }
	YY_BREAK
case 169:
/* rule 169 can match eol */
YY_RULE_SETUP
#line 371 "fortran.lex"
{ INCREMENT_LINE_NUM() ; newlinef90=1; }
	YY_BREAK
case 170:
/* rule 170 can match eol */
YY_RULE_SETUP
#line 372 "fortran.lex"
{ INCREMENT_LINE_NUM() ;}
	YY_BREAK
case 171:
/* rule 171 can match eol */
YY_RULE_SETUP
#line 374 "fortran.lex"
{INCREMENT_LINE_NUM() ; BEGIN(donottreat); }
	YY_BREAK
case 172:
/* rule 172 can match eol */
YY_RULE_SETUP
#line 375 "fortran.lex"
{out_of_donottreat(); return '\n'; }
	YY_BREAK
case 173:
/* rule 173 can match eol */
YY_RULE_SETUP
#line 376 "fortran.lex"
{INCREMENT_LINE_NUM() ; }
	YY_BREAK
case 174:
/* rule 174 can match eol */
YY_RULE_SETUP
#line 377 "fortran.lex"
{INCREMENT_LINE_NUM() ; increment_nbtokens = 0;}
	YY_BREAK
case 175:
/* rule 175 can match eol */
YY_RULE_SETUP
#line 378 "fortran.lex"
{INCREMENT_LINE_NUM() ; increment_nbtokens = 0;}
	YY_BREAK
case 176:
YY_RULE_SETUP
#line 379 "fortran.lex"
{increment_nbtokens = 0;}
	YY_BREAK
case YY_STATE_EOF(INITIAL):
case YY_STATE_EOF(parameter):
case YY_STATE_EOF(character):
case YY_STATE_EOF(donottreat):
case YY_STATE_EOF(includestate):
case YY_STATE_EOF(fortran77style):
case YY_STATE_EOF(fortran90style):
#line 380 "fortran.lex"
{endoffile = 1; yyterminate();}
	YY_BREAK
case 177:
YY_RULE_SETUP
#line 381 "fortran.lex"
ECHO;
	YY_BREAK
#line 5060 "fortran.yy.c"

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
			 * just pointed yyin at a new source and called
			 * yylex().  If so, then we have to assure
			 * consistency between YY_CURRENT_BUFFER and our
			 * globals.  Here is the right place to do so, because
			 * this is the first action (other than possibly a
			 * back-up) that will match for the new input source.
			 */
			(yy_n_chars) = YY_CURRENT_BUFFER_LVALUE->yy_n_chars;
			YY_CURRENT_BUFFER_LVALUE->yy_input_file = yyin;
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

				if ( yywrap(  ) )
					{
					/* Note: because we've taken care in
					 * yy_get_next_buffer() to have set up
					 * yytext, we can now set up
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
	} /* end of user's declarations */
} /* end of yylex */

/* yy_get_next_buffer - try to read in a new buffer
 *
 * Returns a code representing an action:
 *	EOB_ACT_LAST_MATCH -
 *	EOB_ACT_CONTINUE_SCAN - continue scanning from current position
 *	EOB_ACT_END_OF_FILE - end of file
 */
static int yy_get_next_buffer (void)
{
    	char *dest = YY_CURRENT_BUFFER_LVALUE->yy_ch_buf;
	char *source = (yytext_ptr);
	int number_to_move, i;
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
	number_to_move = (int) ((yy_c_buf_p) - (yytext_ptr) - 1);

	for ( i = 0; i < number_to_move; ++i )
		*(dest++) = *(source++);

	if ( YY_CURRENT_BUFFER_LVALUE->yy_buffer_status == YY_BUFFER_EOF_PENDING )
		/* don't do the read, it's not guaranteed to return an EOF,
		 * just force an EOF
		 */
		YY_CURRENT_BUFFER_LVALUE->yy_n_chars = (yy_n_chars) = 0;

	else
		{
			int num_to_read =
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
			yyrestart( yyin  );
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

	if (((yy_n_chars) + number_to_move) > YY_CURRENT_BUFFER_LVALUE->yy_buf_size) {
		/* Extend the array by 50%, plus the number we really need. */
		int new_size = (yy_n_chars) + number_to_move + ((yy_n_chars) >> 1);
		YY_CURRENT_BUFFER_LVALUE->yy_ch_buf = (char *) yyrealloc(
			(void *) YY_CURRENT_BUFFER_LVALUE->yy_ch_buf, (yy_size_t) new_size  );
		if ( ! YY_CURRENT_BUFFER_LVALUE->yy_ch_buf )
			YY_FATAL_ERROR( "out of dynamic memory in yy_get_next_buffer()" );
		/* "- 2" to take care of EOB's */
		YY_CURRENT_BUFFER_LVALUE->yy_buf_size = (int) (new_size - 2);
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
	yy_state_type yy_current_state;
	char *yy_cp;
    
	yy_current_state = (yy_start);
	yy_current_state += YY_AT_BOL();

	(yy_state_ptr) = (yy_state_buf);
	*(yy_state_ptr)++ = yy_current_state;

	for ( yy_cp = (yytext_ptr) + YY_MORE_ADJ; yy_cp < (yy_c_buf_p); ++yy_cp )
		{
		YY_CHAR yy_c = (*yy_cp ? yy_ec[YY_SC_TO_UI(*yy_cp)] : 1);
		while ( yy_chk[yy_base[yy_current_state] + yy_c] != yy_current_state )
			{
			yy_current_state = (int) yy_def[yy_current_state];
			if ( yy_current_state >= 1877 )
				yy_c = yy_meta[yy_c];
			}
		yy_current_state = yy_nxt[yy_base[yy_current_state] + yy_c];
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
	int yy_is_jam;
    
	YY_CHAR yy_c = 1;
	while ( yy_chk[yy_base[yy_current_state] + yy_c] != yy_current_state )
		{
		yy_current_state = (int) yy_def[yy_current_state];
		if ( yy_current_state >= 1877 )
			yy_c = yy_meta[yy_c];
		}
	yy_current_state = yy_nxt[yy_base[yy_current_state] + yy_c];
	yy_is_jam = (yy_current_state == 1876);
	if ( ! yy_is_jam )
		*(yy_state_ptr)++ = yy_current_state;

		return yy_is_jam ? 0 : yy_current_state;
}

#ifndef YY_NO_UNPUT

    static void yyunput (int c, char * yy_bp )
{
	char *yy_cp;
    
    yy_cp = (yy_c_buf_p);

	/* undo effects of setting up yytext */
	*yy_cp = (yy_hold_char);

	if ( yy_cp < YY_CURRENT_BUFFER_LVALUE->yy_ch_buf + 2 )
		{ /* need to shift things up to make room */
		/* +2 for EOB chars. */
		int number_to_move = (yy_n_chars) + 2;
		char *dest = &YY_CURRENT_BUFFER_LVALUE->yy_ch_buf[
					YY_CURRENT_BUFFER_LVALUE->yy_buf_size + 2];
		char *source =
				&YY_CURRENT_BUFFER_LVALUE->yy_ch_buf[number_to_move];

		while ( source > YY_CURRENT_BUFFER_LVALUE->yy_ch_buf )
			*--dest = *--source;

		yy_cp += (int) (dest - source);
		yy_bp += (int) (dest - source);
		YY_CURRENT_BUFFER_LVALUE->yy_n_chars =
			(yy_n_chars) = (int) YY_CURRENT_BUFFER_LVALUE->yy_buf_size;

		if ( yy_cp < YY_CURRENT_BUFFER_LVALUE->yy_ch_buf + 2 )
			YY_FATAL_ERROR( "flex scanner push-back overflow" );
		}

	*--yy_cp = (char) c;

	(yytext_ptr) = yy_bp;
	(yy_hold_char) = *yy_cp;
	(yy_c_buf_p) = yy_cp;
}

#endif

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
			int offset = (int) ((yy_c_buf_p) - (yytext_ptr));
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
					yyrestart( yyin );

					/*FALLTHROUGH*/

				case EOB_ACT_END_OF_FILE:
					{
					if ( yywrap(  ) )
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
	*(yy_c_buf_p) = '\0';	/* preserve yytext */
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
    void yyrestart  (FILE * input_file )
{
    
	if ( ! YY_CURRENT_BUFFER ){
        yyensure_buffer_stack ();
		YY_CURRENT_BUFFER_LVALUE =
            yy_create_buffer( yyin, YY_BUF_SIZE );
	}

	yy_init_buffer( YY_CURRENT_BUFFER, input_file );
	yy_load_buffer_state(  );
}

/** Switch to a different input buffer.
 * @param new_buffer The new input buffer.
 * 
 */
    void yy_switch_to_buffer  (YY_BUFFER_STATE  new_buffer )
{
    
	/* TODO. We should be able to replace this entire function body
	 * with
	 *		yypop_buffer_state();
	 *		yypush_buffer_state(new_buffer);
     */
	yyensure_buffer_stack ();
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
	yy_load_buffer_state(  );

	/* We don't actually know whether we did this switch during
	 * EOF (yywrap()) processing, but the only time this flag
	 * is looked at is after yywrap() is called, so it's safe
	 * to go ahead and always set it.
	 */
	(yy_did_buffer_switch_on_eof) = 1;
}

static void yy_load_buffer_state  (void)
{
    	(yy_n_chars) = YY_CURRENT_BUFFER_LVALUE->yy_n_chars;
	(yytext_ptr) = (yy_c_buf_p) = YY_CURRENT_BUFFER_LVALUE->yy_buf_pos;
	yyin = YY_CURRENT_BUFFER_LVALUE->yy_input_file;
	(yy_hold_char) = *(yy_c_buf_p);
}

/** Allocate and initialize an input buffer state.
 * @param file A readable stream.
 * @param size The character buffer size in bytes. When in doubt, use @c YY_BUF_SIZE.
 * 
 * @return the allocated buffer state.
 */
    YY_BUFFER_STATE yy_create_buffer  (FILE * file, int  size )
{
	YY_BUFFER_STATE b;
    
	b = (YY_BUFFER_STATE) yyalloc( sizeof( struct yy_buffer_state )  );
	if ( ! b )
		YY_FATAL_ERROR( "out of dynamic memory in yy_create_buffer()" );

	b->yy_buf_size = size;

	/* yy_ch_buf has to be 2 characters longer than the size given because
	 * we need to put in 2 end-of-buffer characters.
	 */
	b->yy_ch_buf = (char *) yyalloc( (yy_size_t) (b->yy_buf_size + 2)  );
	if ( ! b->yy_ch_buf )
		YY_FATAL_ERROR( "out of dynamic memory in yy_create_buffer()" );

	b->yy_is_our_buffer = 1;

	yy_init_buffer( b, file );

	return b;
}

/** Destroy the buffer.
 * @param b a buffer created with yy_create_buffer()
 * 
 */
    void yy_delete_buffer (YY_BUFFER_STATE  b )
{
    
	if ( ! b )
		return;

	if ( b == YY_CURRENT_BUFFER ) /* Not sure if we should pop here. */
		YY_CURRENT_BUFFER_LVALUE = (YY_BUFFER_STATE) 0;

	if ( b->yy_is_our_buffer )
		yyfree( (void *) b->yy_ch_buf  );

	yyfree( (void *) b  );
}

/* Initializes or reinitializes a buffer.
 * This function is sometimes called more than once on the same buffer,
 * such as during a yyrestart() or at EOF.
 */
    static void yy_init_buffer  (YY_BUFFER_STATE  b, FILE * file )

{
	int oerrno = errno;
    
	yy_flush_buffer( b );

	b->yy_input_file = file;
	b->yy_fill_buffer = 1;

    /* If b is the current buffer, then yy_init_buffer was _probably_
     * called from yyrestart() or through yy_get_next_buffer.
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
    void yy_flush_buffer (YY_BUFFER_STATE  b )
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
		yy_load_buffer_state(  );
}

/** Pushes the new state onto the stack. The new state becomes
 *  the current state. This function will allocate the stack
 *  if necessary.
 *  @param new_buffer The new state.
 *  
 */
void yypush_buffer_state (YY_BUFFER_STATE new_buffer )
{
    	if (new_buffer == NULL)
		return;

	yyensure_buffer_stack();

	/* This block is copied from yy_switch_to_buffer. */
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

	/* copied from yy_switch_to_buffer. */
	yy_load_buffer_state(  );
	(yy_did_buffer_switch_on_eof) = 1;
}

/** Removes and deletes the top of the stack, if present.
 *  The next element becomes the new top.
 *  
 */
void yypop_buffer_state (void)
{
    	if (!YY_CURRENT_BUFFER)
		return;

	yy_delete_buffer(YY_CURRENT_BUFFER );
	YY_CURRENT_BUFFER_LVALUE = NULL;
	if ((yy_buffer_stack_top) > 0)
		--(yy_buffer_stack_top);

	if (YY_CURRENT_BUFFER) {
		yy_load_buffer_state(  );
		(yy_did_buffer_switch_on_eof) = 1;
	}
}

/* Allocates the stack if it does not exist.
 *  Guarantees space for at least one push.
 */
static void yyensure_buffer_stack (void)
{
	yy_size_t num_to_alloc;
    
	if (!(yy_buffer_stack)) {

		/* First allocation is just for 2 elements, since we don't know if this
		 * scanner will even need a stack. We use 2 instead of 1 to avoid an
		 * immediate realloc on the next call.
         */
      num_to_alloc = 1; /* After all that talk, this was set to 1 anyways... */
		(yy_buffer_stack) = (struct yy_buffer_state**)yyalloc
								(num_to_alloc * sizeof(struct yy_buffer_state*)
								);
		if ( ! (yy_buffer_stack) )
			YY_FATAL_ERROR( "out of dynamic memory in yyensure_buffer_stack()" );

		memset((yy_buffer_stack), 0, num_to_alloc * sizeof(struct yy_buffer_state*));

		(yy_buffer_stack_max) = num_to_alloc;
		(yy_buffer_stack_top) = 0;
		return;
	}

	if ((yy_buffer_stack_top) >= ((yy_buffer_stack_max)) - 1){

		/* Increase the buffer to prepare for a possible push. */
		yy_size_t grow_size = 8 /* arbitrary grow size */;

		num_to_alloc = (yy_buffer_stack_max) + grow_size;
		(yy_buffer_stack) = (struct yy_buffer_state**)yyrealloc
								((yy_buffer_stack),
								num_to_alloc * sizeof(struct yy_buffer_state*)
								);
		if ( ! (yy_buffer_stack) )
			YY_FATAL_ERROR( "out of dynamic memory in yyensure_buffer_stack()" );

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
YY_BUFFER_STATE yy_scan_buffer  (char * base, yy_size_t  size )
{
	YY_BUFFER_STATE b;
    
	if ( size < 2 ||
	     base[size-2] != YY_END_OF_BUFFER_CHAR ||
	     base[size-1] != YY_END_OF_BUFFER_CHAR )
		/* They forgot to leave room for the EOB's. */
		return NULL;

	b = (YY_BUFFER_STATE) yyalloc( sizeof( struct yy_buffer_state )  );
	if ( ! b )
		YY_FATAL_ERROR( "out of dynamic memory in yy_scan_buffer()" );

	b->yy_buf_size = (int) (size - 2);	/* "- 2" to take care of EOB's */
	b->yy_buf_pos = b->yy_ch_buf = base;
	b->yy_is_our_buffer = 0;
	b->yy_input_file = NULL;
	b->yy_n_chars = b->yy_buf_size;
	b->yy_is_interactive = 0;
	b->yy_at_bol = 1;
	b->yy_fill_buffer = 0;
	b->yy_buffer_status = YY_BUFFER_NEW;

	yy_switch_to_buffer( b  );

	return b;
}

/** Setup the input buffer state to scan a string. The next call to yylex() will
 * scan from a @e copy of @a str.
 * @param yystr a NUL-terminated string to scan
 * 
 * @return the newly allocated buffer state object.
 * @note If you want to scan bytes that may contain NUL values, then use
 *       yy_scan_bytes() instead.
 */
YY_BUFFER_STATE yy_scan_string (const char * yystr )
{
    
	return yy_scan_bytes( yystr, (int) strlen(yystr) );
}

/** Setup the input buffer state to scan the given bytes. The next call to yylex() will
 * scan from a @e copy of @a bytes.
 * @param yybytes the byte buffer to scan
 * @param _yybytes_len the number of bytes in the buffer pointed to by @a bytes.
 * 
 * @return the newly allocated buffer state object.
 */
YY_BUFFER_STATE yy_scan_bytes  (const char * yybytes, int  _yybytes_len )
{
	YY_BUFFER_STATE b;
	char *buf;
	yy_size_t n;
	int i;
    
	/* Get memory for full buffer, including space for trailing EOB's. */
	n = (yy_size_t) (_yybytes_len + 2);
	buf = (char *) yyalloc( n  );
	if ( ! buf )
		YY_FATAL_ERROR( "out of dynamic memory in yy_scan_bytes()" );

	for ( i = 0; i < _yybytes_len; ++i )
		buf[i] = yybytes[i];

	buf[_yybytes_len] = buf[_yybytes_len+1] = YY_END_OF_BUFFER_CHAR;

	b = yy_scan_buffer( buf, n );
	if ( ! b )
		YY_FATAL_ERROR( "bad buffer in yy_scan_bytes()" );

	/* It's okay to grow etc. this buffer, and we should throw it
	 * away when we're done.
	 */
	b->yy_is_our_buffer = 1;

	return b;
}

#ifndef YY_EXIT_FAILURE
#define YY_EXIT_FAILURE 2
#endif

static void yynoreturn yy_fatal_error (const char* msg )
{
			fprintf( stderr, "%s\n", msg );
	exit( YY_EXIT_FAILURE );
}

/* Redefine yyless() so it works in section 3 code. */

#undef yyless
#define yyless(n) \
	do \
		{ \
		/* Undo effects of setting up yytext. */ \
        int yyless_macro_arg = (n); \
        YY_LESS_LINENO(yyless_macro_arg);\
		yytext[yyleng] = (yy_hold_char); \
		(yy_c_buf_p) = yytext + yyless_macro_arg; \
		(yy_hold_char) = *(yy_c_buf_p); \
		*(yy_c_buf_p) = '\0'; \
		yyleng = yyless_macro_arg; \
		} \
	while ( 0 )

/* Accessor  methods (get/set functions) to struct members. */

/** Get the current line number.
 * 
 */
int yyget_lineno  (void)
{
    
    return yylineno;
}

/** Get the input stream.
 * 
 */
FILE *yyget_in  (void)
{
        return yyin;
}

/** Get the output stream.
 * 
 */
FILE *yyget_out  (void)
{
        return yyout;
}

/** Get the length of the current token.
 * 
 */
int yyget_leng  (void)
{
        return yyleng;
}

/** Get the current token.
 * 
 */

char *yyget_text  (void)
{
        return yytext;
}

/** Set the current line number.
 * @param _line_number line number
 * 
 */
void yyset_lineno (int  _line_number )
{
    
    yylineno = _line_number;
}

/** Set the input stream. This does not discard the current
 * input buffer.
 * @param _in_str A readable stream.
 * 
 * @see yy_switch_to_buffer
 */
void yyset_in (FILE *  _in_str )
{
        yyin = _in_str ;
}

void yyset_out (FILE *  _out_str )
{
        yyout = _out_str ;
}

int yyget_debug  (void)
{
        return yy_flex_debug;
}

void yyset_debug (int  _bdebug )
{
        yy_flex_debug = _bdebug ;
}

static int yy_init_globals (void)
{
        /* Initialization is the same as for the non-reentrant scanner.
     * This function is called from yylex_destroy(), so don't allocate here.
     */

    (yy_buffer_stack) = NULL;
    (yy_buffer_stack_top) = 0;
    (yy_buffer_stack_max) = 0;
    (yy_c_buf_p) = NULL;
    (yy_init) = 0;
    (yy_start) = 0;

    (yy_state_buf) = 0;
    (yy_state_ptr) = 0;
    (yy_full_match) = 0;
    (yy_lp) = 0;

/* Defined in main.c */
#ifdef YY_STDINIT
    yyin = stdin;
    yyout = stdout;
#else
    yyin = NULL;
    yyout = NULL;
#endif

    /* For future reference: Set errno on error, since we are called by
     * yylex_init()
     */
    return 0;
}

/* yylex_destroy is for both reentrant and non-reentrant scanners. */
int yylex_destroy  (void)
{
    
    /* Pop the buffer stack, destroying each element. */
	while(YY_CURRENT_BUFFER){
		yy_delete_buffer( YY_CURRENT_BUFFER  );
		YY_CURRENT_BUFFER_LVALUE = NULL;
		yypop_buffer_state();
	}

	/* Destroy the stack itself. */
	yyfree((yy_buffer_stack) );
	(yy_buffer_stack) = NULL;

    yyfree ( (yy_state_buf) );
    (yy_state_buf)  = NULL;

    /* Reset the globals. This is important in a non-reentrant scanner so the next time
     * yylex() is called, initialization will occur. */
    yy_init_globals( );

    return 0;
}

/*
 * Internal utility routines.
 */

#ifndef yytext_ptr
static void yy_flex_strncpy (char* s1, const char * s2, int n )
{
		
	int i;
	for ( i = 0; i < n; ++i )
		s1[i] = s2[i];
}
#endif

#ifdef YY_NEED_STRLEN
static int yy_flex_strlen (const char * s )
{
	int n;
	for ( n = 0; s[n]; ++n )
		;

	return n;
}
#endif

void *yyalloc (yy_size_t  size )
{
			return malloc(size);
}

void *yyrealloc  (void * ptr, yy_size_t  size )
{
		
	/* The cast to (char *) in the following accommodates both
	 * implementations that use char* generic pointers, and those
	 * that use void* generic pointers.  It works with the latter
	 * because both ANSI C and C++ allow castless assignment from
	 * any pointer type to void*, and deal with argument conversions
	 * as though doing an assignment.
	 */
	return realloc(ptr, size);
}

void yyfree (void * ptr )
{
			free( (char *) ptr );	/* see yyrealloc() for (char *) cast */
}

#define YYTABLES_NAME "yytables"

#line 381 "fortran.lex"


void out_of_donottreat ( void )
{
    BEGIN(INITIAL);
    if (infixed) BEGIN(fortran77style) ;
    if (infree)  BEGIN(fortran90style) ;
    INCREMENT_LINE_NUM() ;
}

