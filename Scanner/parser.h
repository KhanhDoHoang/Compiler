/*************************************************************
* COMPILERS COURSE - Algonquin College
* Code version: Winter, 2021
*************************************************************
* File name: parser.h
* Compiler: MS Visual Studio 2019
* Author: Hoang Do, Jenni Jenni
* Course: CST 8152 – Compilers, Lab Section: [014]
* Assignment: A3.
* Due Date: April 17, 2021
* Purpose: This file is the main header for Parser (.h)
* Function list: 1. startParser()
*				 2. matchToken(),
*				 3. syncErrorHandler(),
*				 4. printError(),
*				 5. program(),
*				 6. optionalStatements(),
*				 7. inputStatement(),
*				 8. statements(),
*				 9. statement(),
*				 10. statementsPrime(),
*				 11. assignmentStatement(),
*				 12. assignmentExpression(),
*				 13. arithmeticExpression(),
*				 14. unaryArithmeticExpression(),
*				 15. additiveArithmeticExpression(),
*				 16. additiveArithmeticExpressionPrime(),
*				 17. multiplicativeArithmeticExpression(),
*				 18. multiplicativeArithmeticExpressionPrime(),
*				 19. primaryArithmeticExpression(),
*				 20. stringExpression(),
*				 21. stringExpressionPrime(),
*				 22. primaryStringExpression(),
*				 23. preCondition(),
*				 24. selectionStatement(),
*				 25. conditionalExpression(),
*				 26. logicalORExpression(),
*				 27. logicalORExpressionPrime(),
*				 28. logicalANDExpression(),
*				 29. logicalANDExpressionPrime(),
*				 30. logicalNOTExpression(),
*				 31. relationalExpression(),
*				 32. relationArithmeticExpression(),
*				 33. primaryARelationalExpressionPrime(),
*				 34. primaryARelationalExpression(),
*				 35. relationStringExpression(),
*				 36. primarySRelationalExpressionPrime(),
*			     37. primarySRelationalExpression(),
*				 38. iterationStatement(),
*				 39. variableList(),
*				 40. varIdentifier(),
*				 41. variableListPrime(),
*				 42. outputStatement(),
*				 43. outputList().
*************************************************************/

/* Inclusion section */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include "token.h"
#include "buffer.h"

/* Global vars */
static Token lookahead; /* To look for the next token*/
int syntaxErrorNumber = 0; /* Run time error counter */
extern bStructure* stringLiteralTable; /* String literal table */
extern int line; /* Current line number of the source code*/
extern Token tokenizer(); /* Scanner function to get next token */
extern char* keywordTable[]; /* Kw lookup table*/

#define	NO_ATTR	-1

/* TODO_102: Create ALL constants for keywords (sequence given in table.h) */
/* Constants */
#define MAIN 	0
#define IF		1
#define THEN	2
#define	ELSE	3
#define WHILE	4
#define DO		5
#define READ	6
#define WRITE	7
#define TRUE	8
#define FALSE	9

/* Operator constant */
#define AND 0
#define OR  1
#define NOT 2

#define EQ 0
#define NE 1
#define GT 2
#define LT 3

#define ADD 0
#define SUB 1
#define MUL 2
#define DIV 3

#define SCC_OP_T 6


/* Function definitions */
void startParser(void);
void matchToken(int, int);
void syncErrorHandler(int);
void printError();

/* PLatypus functions */
void program(void);
void optionalStatements(void);

void statements(void); /* with "s"*/
void statement(void); /* no "s"*/
void statementsPrime(void);

/* Statement */
void assignmentStatement(void);
void selectionStatement(void); /* Selection statement (if) */
void iterationStatement(void); /* Looop statement (whike) */
void preCondition(void); /* Pre condition (true, false) */
void inputStatement(void); /* READ */
void outputStatement(void); /* WRITE */

/* Assignment statement */
void assignmentExpression(void);


/* Input statement */
/* Variable list */
void variableList(void);
void variableListPrime(void);
/* Var ident*/
void variableIdentifier(void);

/* Output statement */
void outputList(void);
//void optionalVariableList(void);

/* EXPRESSION */
/* Mode */
/* Assignment Expression */
void arithmeticExpression(void);

void unaryArithmeticExpression(void);
void additiveArithmeticExpression(void);
void additiveArithmeticExpressionPrime(void);

void multiplicativeArithmeticExpression(void);
void multiplicativeArithmeticExpressionPrime(void);

void primaryArithmeticExpression(void);

/*========*/
/* STRING EXPRESSION */
void stringExpression(void);
void stringExpressionPrime(void);
void primaryStringExpression(void);

/* Condition for OR, AND and NOT*/
void conditionalExpression(void);

void logicalORExpression(void);
void logicalORExpressionPrime(void);

void logicalANDExpression(void);
void logicalANDExpressionPrime(void);
/*
 * <logical NOT expression>  .NOT. <relational expression> | <relational expression>
 */
void logicalNOTExpression(void);
/* RELATIONAL EXPRESSION */
void relationalExpression(void);

void relationArithmeticExpression(void);
void primaryARelationalExpression(void); /*Arithmetic*/
void primaryARelationalExpressionPrime(void); /*Arithmetic prime */

void relationStringExpression(void);
void primarySRelationalExpression(void); /* String */
void primarySRelationalExpressionPrime(void); /* String prime */
