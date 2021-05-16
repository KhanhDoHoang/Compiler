/****************************************************
 * Filename: token.h
 * Token declarations necessary for the scanner implementation
 * CST8152, Assignment #2
 * Version: 1.3
 * Date: 1st Jan 2021
 * Created by: Svillen Ranev - Paulo Sousa - Abdulah
 * The file is complete and SHOULD NOT be modified.
*************************************************************/

#ifndef TOKEN_H_
#define TOKEN_H_

/*#pragma warning(1:4001) *//*to enforce C89 type comments  - to make //comments an warning */

/*#pragma warning(error:4001)*//* to enforce C89 comments - to make // comments an error */

/* Constants */
#define VID_LEN 8   /* variable identifier length */
#define ERR_LEN 20  /* error message length */
#define INL_LEN 5   /* maximum number of digits for IL */

/* Token codes: Numbers from -1 to 18 (old: 0 to 19) */
#define ERR_T     -1 /* Error token */
#define SEOF_T    0  /* Source end-of-file token */
#define AVID_T    1  /* Arithmetic Variable identifier token */
#define SVID_T    2  /* String Variable identifier token: Ending in $ (old: @) */
#define FPL_T     3  /* Floating point literal token */
#define INL_T     4  /* Integer literal token */
#define STR_T     5  /* String literal token */
#define SCC_OP_T  6  /* String concatenation operator token: $$ (old: ## or >>) */
#define ASS_OP_T  7  /* Assignment operator token */
#define ART_OP_T  8  /* Arithmetic operator token */
#define REL_OP_T  9  /* Relational operator token */ 
#define LOG_OP_T 10  /* Logical operator token */
#define LPR_T    11  /* Left parenthesis token */
#define RPR_T    12  /* Right parenthesis token */
#define LBR_T    13  /* Left brace token */
#define RBR_T    14  /* Right brace token */
#define KW_T     15  /* Keyword token */
#define COM_T    16  /* Comma token */
#define EOS_T    17  /* End of statement (semicolon) */
#define RTE_T    18  /* Run-time error token */

/* Operators token attributes */
typedef enum ArithmeticOperators { ADD, SUB, MUL, DIV } AriOperator;
typedef enum RelationalOperators { EQ, NE, GT, LT } RelOperator;
typedef enum LogicalOperators { AND, OR, NOT } LogOperator;
typedef enum SourceEndOfFile { SEOF_0, SEOF_EOF } EofOperator;

/* Data structures for declaring the token and its attributes */
typedef union TokenAttribute {
	int codeType;						/* integer attributes accessor */
	AriOperator arithmeticOperator;		/* arithmetic operator attribute code */
	RelOperator relationalOperator;		/* relational operator attribute code */
	LogOperator logicalOperator;		/* logical operator attribute code */
	EofOperator seofType;				/* source-end-of-file attribute code */
	int intValue;						/* integer literal attribute (value) */
	int keywordIndex;					/* keyword index in the keyword table */
	short contentString;				/* sring literal offset from the beginning of the string literal buffer (str_LTBL->cb_head) */
	float floatValue;					/* floating-point literal attribute (value) */
	char vidLexeme[VID_LEN + 1];		/* variable identifier token attribute */
	char errLexeme[ERR_LEN + 1];		/* error token attribite */
} TokenAtt;

/* Should be used if no symbol table is implemented */
typedef struct vidTokenAttibutes {
	unsigned char flags;			/* Flags information */
	union {
		short intValue;				/* Integer value */
		float floatValue;			/* Float value */
		void* stringContent;		/* String value */
	} values;
} VidAtt;

/* Token declaration */
typedef struct Token {
	int code;				/* token code */
	TokenAtt attribute;		/* token attribute */
	VidAtt avidAttribute;	/* not used in this scanner implementation - for further use */
} Token;

#endif
