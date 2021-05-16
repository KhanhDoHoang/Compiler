/*************************************************************
* COMPILERS COURSE - Algonquin College
* Code version: Winter, 2021
*************************************************************
* File name: parser.c
* Compiler: MS Visual Studio 2019
* Author: Hoang Do, Jenni Jenni
* Course: CST 8152 – Compilers, Lab Section: 014
* Assignment: A3.
* Date: April 17, 2021
* Purpose: Holds all functions that are declared in parser.h
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

#include "parser.h"

/*************************************************************
 *Purpose: This function begin the parsing
* Author: Hoang Do
* Versions: 1.2
* Called Functions: malar_next_token(), program(), match(), gen_incode()
* Parameters: void
* Return value: N/A
* Algorithm: start the program 
* Process \Parser
 ************************************************************/
void startParser(void) {
	lookahead = tokenizer();
	program();
	matchToken(SEOF_T, NO_ATTR);
	printf("%s\n", "PLATY: Source file parsed");
}

/**************************************************************
* Purpose: To match two tokens: the current input token (lookahead)
*          and the token required by the parser.
* Author: Hoang Do
* Versions: 3.2
* Called Functions: syncErrorHandler(), printError(), tokenizer()
* Parameters: int tokenCode, int tokenAttribute
* Return value: returns to the program
* Algorithm: 1. to check if the match is successful or not
*            2. to parse the token code and check if token is SEOF_T or not
*			 3. to check the lookahead token is ERR_T or not
*****************************************************************/
void matchToken(int tokenCode, int tokenAttribute) {
	int matchFlag = 1;
	switch (lookahead.code) {
	case KW_T:
	case REL_OP_T:
	case ART_OP_T:
	case LOG_OP_T:
		if (lookahead.attribute.codeType != tokenAttribute)
			matchFlag = 0;
	default:
		if (lookahead.code != tokenCode)
			matchFlag = 0;
	}
	if (matchFlag && lookahead.code == SEOF_T)
		return;
	if (matchFlag) {
		lookahead = tokenizer();
		if (lookahead.code == ERR_T) {
			printError();
			lookahead = tokenizer();
			++syntaxErrorNumber;
		}
	}
	else
		syncErrorHandler(tokenCode);
}

/*
* Purpose: To implements a simple panic mode error recovery.
* Author: Hoang Do
* Versions: 1.9
* Called Functions: printError(), tokenizer(), exit()
* Parameters: int syncTokenCode
* Return value: return to the program
* Algorithm: 1. count the error and printing
*            2. advance the input token
*		     3. check if the function reach the end of file
*    		 4. find the matching token
*/
void syncErrorHandler(int syncTokenCode) {
	printError();
	++syntaxErrorNumber;

	while (lookahead.code != syncTokenCode) {
		/* Check for SEOF_T*/
		/* Get the next token */
		if (lookahead.code == syncTokenCode) {
			lookahead = tokenizer();
			return;
		}
		if (lookahead.code == SEOF_T) {
			exit(syntaxErrorNumber);
		}
		lookahead = tokenizer();

	}
	/* Found the token and not SEOF_T advanced the code and return */
	if (lookahead.code != SEOF_T)
		lookahead = tokenizer();
	return;

}

/*************************************************************
 * Program statement
 * BNF: <program> -> MAIN { <opt_statements> }
 * FIRST(<program>) = {KW_T (MAIN)}.
 *
 * Author: Hoang Do
 * Versions: 1.1
 * Called Functions: matchToken(), optionalStatements()
 * Parameters: void
 * Return value: N/A
 * Algorithm: 1. match the tokens
 *            2. print message
 ************************************************************/
void program(void) {
	matchToken(KW_T, MAIN);
	matchToken(LBR_T, NO_ATTR);
	optionalStatements();
	matchToken(RBR_T, NO_ATTR);
	printf("%s\n", "PLATY: Program parsed");
}

/*************************************************************
 * Purpose: To print the error messages
 * Author: Hoang Do
 * Versions: 2.1
 * Called Functions: printf()
 * Parameters: N/A
 * Return value: N/A
 * Algorithm: look for different situations, print error messages
 ************************************************************/
void printError() {
	Token t = lookahead;
	printf("PLATY: Syntax error:  Line:%3d\n", line);
	printf("*****  Token code:%3d Attribute: ", t.code);
	switch (t.code) {
	case  ERR_T: /* ERR_T     0   Error token */
		printf("%s\n", t.attribute.errLexeme);
		break;
	case  SEOF_T: /*SEOF_T    1   Source end-of-file token */
		printf("SEOF_T\t\t%d\t\n", t.attribute.seofType);
		break;
	case  AVID_T: /* AVID_T    2   Arithmetic Variable identifier token */
	case  SVID_T:/* SVID_T    3  String Variable identifier token */
		printf("%s\n", t.attribute.vidLexeme);
		break;
	case  FPL_T: /* FPL_T     4  Floating point literal token */
		printf("%5.1f\n", t.attribute.floatValue);
		break;
	case INL_T: /* INL_T      5   Integer literal token */
		printf("%d\n", t.attribute.codeType);
		break;
	case STR_T:/* STR_T     6   String literal token */
		bSetMarkOffset(stringLiteralTable, t.attribute.contentString);
		printf("%s\n", bGetContent(stringLiteralTable,
			(short)t.attribute.codeType));		
		break;
	case SCC_OP_T: /* 7   String concatenation operator token */
		printf("NA\n");
		break;
	case  ASS_OP_T:/* ASS_OP_T  8   Assignment operator token */
		printf("NA\n");
		break;
	case  ART_OP_T:/* ART_OP_T  9   Arithmetic operator token */
		printf("%d\n", t.attribute.codeType);
		break;
	case  REL_OP_T: /*REL_OP_T  10   Relational operator token */
		printf("%d\n", t.attribute.codeType);
		break;
	case  LOG_OP_T:/*LOG_OP_T 11  Logical operator token */
		printf("%d\n", t.attribute.codeType);
		break;
	case  LPR_T: /*LPR_T    12  Left parenthesis token */
		printf("NA\n");
		break;
	case  RPR_T: /*RPR_T    13  Right parenthesis token */
		printf("NA\n");
		break;
	case LBR_T: /*    14   Left brace token */
		printf("NA\n");
		break;
	case RBR_T: /*    15  Right brace token */
		printf("NA\n");
		break;
	case KW_T: /*     16   Keyword token */
		printf("%s\n", keywordTable[t.attribute.codeType]);
		break;
	case COM_T: /* 17   Comma token */
		printf("NA\n");
		break;
	case EOS_T: /*    18  End of statement *(semi - colon) */
		printf("NA\n");
		break;
	case RTE_T:
		printf("PLATY: Scanner error: invalid token code: %d\n", t.code);
		break;
	default:
		printf("PLATY: Scanner error: invalid token code: %d\n", t.code);
	}/*end switch*/
}

/*************************************************************
 * Optional statement
 * BNF: <opt_statements> -> { <statements> | epsilon }
 * FIRST(<opt_statements>) = {SVID_T, AVID_T, KW_T(IF), KW_T(WHILE),
 *						      KW_T(READ), KW_T(WRITE), epsilon}.
 * Author: Hoang Do
 * Versions: 2.4
 * Called Functions: statements()
 * Parameters: N/A
 * Return value: N/A
 * Algorithm: 1. looking for the tokens
 *			 2. chack for some keyword
 ************************************************************/
void optionalStatements(void) {
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T:
		statements();
		break;
	case KW_T:
		if (lookahead.attribute.codeType == IF
			|| lookahead.attribute.codeType == WHILE
			|| lookahead.attribute.codeType == READ
			|| lookahead.attribute.codeType == WRITE) {
			statements();
			break;
		}
	default:
		break;
	}
	printf("%s\n", "PLATY: Optional statements parsed");

}

/*************************************************************
 * Statements
 * BNF: <statements> -> { <statement><statementsPrime> }
 * FIRST(<statements>) = {SVID_T, AVID_T, KW_T(IF), KW_T(WHILE),
 *						  KW_T(READ), KW_T(WRITE)}.
 * Author: Hoang Do
 * Versions: 1.2
 * Called Functions: statement(), statementsPrime()
 * Parameters: N/A
 * Return value: N/A
 * Algorithm: loop the statement() and statementsPrime() function
 ************************************************************/
void statements(void) {
	statement();
	statementsPrime();
}


/*************************************************************
 * Statement
 * BNF: <statement> -> { <assignment statement>
 *						 | <selection statement>
 *						 | <iteration statement>
 *						 | <input statement>
 *						 | <output statement> }
 * FIRST(<statement>) = {AVID_T, SVID_T, KW_T(IF), KW_T(WHILE),
 *						 KW_T(READ), KW_T(WRITE)}.
 * Author: Hoang Do
 * Versions: 3.1
 * Called Functions: assignmentStatement(), selectionStatement(), iterationStatement(),
 *                   inputStatement(), outputStatement(), printError()
 * Parameters: N/A
 * Return value: N/A
 * Algorithm: go through the statement FIRST Set to find tokens
 ************************************************************/
void statement(void) {
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T:
		assignmentStatement();
		break;

	case KW_T:
		if (lookahead.attribute.codeType == IF) {
			selectionStatement();
		}
		else if (lookahead.attribute.codeType == WHILE) {
			iterationStatement();
		}
		else if (lookahead.attribute.codeType == READ) {
			inputStatement();
		}
		else if (lookahead.attribute.codeType == WRITE) {
			outputStatement();
		}
		break;
	default:
		printError();
		break;
	}
	printf("%s\n", "PLATY: Statement parsed");
}

/*************************************************************
 * statementsPrime
 * BNF: <statementsPrime> -> { <statement><statementsPrime> | epsilon }
 * FIRST(<statementsPrime>) = {SVID_T, AVID_T, KW_T(IF),
 *							   KW_T(WHILE), KW_T(READ),
 *							   KW_T(WRITE), epsilon}.
 * Author: Hoang Do
 * Versions: 2.2
 * Called Functions: statement(), statementsPrime(), statements()
 * Parameters: N/A
 * Return value: N/A
 * Algorithm: loop the statement() and statementsPrime() function
*************************************************************/
void statementsPrime() {
	switch (lookahead.code)
	{
	case AVID_T:
	case SVID_T:
		statement();
		statementsPrime();
		printf("%s\n", "PLATY: Statements parsed");

		break;
	case KW_T:
		if (lookahead.attribute.codeType == IF
			|| lookahead.attribute.codeType == WHILE
			|| lookahead.attribute.codeType == READ
			|| lookahead.attribute.codeType == WRITE) {
			statements();
			statementsPrime();
		}
		break;
	}
}
/*************************************************************
 * Assignment statement
 * BNF: <assignment statement> -> { <assignment expression> }
 * FIRST(<assignment statement>) = {AVID_T, SVID_T}.
 * Author: Hoang Do
 * Versions: 1.5
 * Called Functions: assignmentExpression(), matchToken()
 * Parameters: N/A
 * Return value: N/A
 * Algorithm: go through the assignment_statement FIRST Set
*************************************************************/
void assignmentStatement(void) {
	assignmentExpression();
	matchToken(EOS_T, NO_ATTR);
	printf("%s\n", "PLATY: Assignment statement parsed");
}
/*************************************************************
 * Assignment expression
 * BNF: <assignment expression> -> { AVID_T | SVID_T }
 * FIRST(<assignment expression>) = {AVID_T, SVID_T}.
 * Author: Hoang Do
 * Versions: 4.2
 * Called Functions: assignment_expression(), match(), string_expression(), gen_incode(), syn_printe()
 * Parameters: N/A
 * Return value: N/A
 * Algorithm: go through the assignment_statement FIRST Set
*************************************************************/
void assignmentExpression(void) {
	switch (lookahead.code)
	{
	case AVID_T: 
		matchToken(AVID_T, NO_ATTR);
		matchToken(ASS_OP_T, NO_ATTR);
		arithmeticExpression();
		printf("%s\n", "PLATY: Assignment expression parsed");
		break;
	case SVID_T:
		matchToken(SVID_T, NO_ATTR);
		matchToken(ASS_OP_T, NO_ATTR);
		stringExpression();
		printf("%s\n", "PLATY: Assignment expression parsed");
		break;
	default:
		printError();
		break;
	}
}
/*************************************************************
 * Arithmetic expression
 * BNF: <arithmetic expression> -> { <unary arithmetic expression>
 *								    | <additive arithmetic expression> }
 * FIRST(<arithmetic expression>) = {-, +, AVID_T, FPL_T, INL_T,
 *									 LBR_T}.
 * Author: Hoang Do
 * Versions: 3.6
 * Called Functions: printError(), unaryArithmeticExpression(), additiveArithmeticExpression()
 * Parameters: N/A
 * Return value: N/A
 * Algorithm: go through the arithmetic_expression FIRST Set
*************************************************************/
void arithmeticExpression(void) {
	switch (lookahead.code)
	{
	case ART_OP_T:
		/* The atr should be MULT and DIV*/
		switch (lookahead.attribute.arithmeticOperator)
		{
		case ADD:
		case SUB:
			unaryArithmeticExpression();
			break;
		default:
			printError();
			break;
		}
		printf("%s\n", "PLATY: Arithemtic expression parsed");
		break;
	case AVID_T:
	case FPL_T:
	case INL_T:
	case LPR_T: /* ) */
		additiveArithmeticExpression();
		printf("%s\n", "PLATY: Arithmetic expression parsed");
		break;
	default:
		printError();
		break;
	}
}
/*************************************************************
 * Unary arithmetic expression
 * BNF: <unary arithmetic expression> -> { -<primary arithmetic expression>
 *										   | +<primary arithmetic expression> }
 * FIRST(<unary arithmetic expression>) = {-, +}.
 * Author: Hoang Do
 * Versions: 5.3
 * Called Functions: printError(), matchToken(), primaryArithmeticExpression()
 * Parameters: N/A
 * Return value: N/A
 * Algorithm: go through the unary_arithmetic_expression FIRST Set
 ************************************************************/
void unaryArithmeticExpression(void) {
	switch (lookahead.code)
	{
	case ART_OP_T:
		if (lookahead.attribute.arithmeticOperator == SUB
			|| lookahead.attribute.arithmeticOperator == ADD) {
			matchToken(ART_OP_T, lookahead.attribute.arithmeticOperator);
			primaryArithmeticExpression();
		}
		else
			printError();
		break;
	}
	//printf("%s\n", "=====Test================");
}
/*************************************************************
 * Additive arithmetic expression
 * BNF: <additive arithmetic expression> -> { <multiplicative arithmetic expression><additive arithmetic expressionPrime> }
 * FIRST(<additive arithmetic expression>) = {AVID_T, FPL_T, INL_T, LBR_T}.
 * Author: Hoang Do
 * Versions: 3.2
 * Called Functions: multiplicativeArithmeticExpression(), additiveArithmeticExpressionPrime()
 * Parameters: N/A
 * Return value: N/A
 * Algorithm: go through the additive_arithmetic_expression FIRST Set
 ************************************************************/
void additiveArithmeticExpression(void) {
	multiplicativeArithmeticExpression();
	additiveArithmeticExpressionPrime();
}
/*************************************************************
 * Additive arithmetic expressionPrime
 * BNF: <additive arithmetic expressionPrime> -> { +<multiplicative arithmetic expression><additive arithmetic expressionPrime>
 *												   | -<multiplicative arithmetic expression><additive arithmetic expressionPrime>
 *												   | epsilon }
 * FIRST(<additive arithmetic expressionPrime>) = {+, -, epsilon}.
 * 
 * Author: Hoang Do
 * Versions: 5.9
 * Called Functions: matchToken(), multiplicativeArithmeticExpression(), additiveArithmeticExpressionPrime(), gen_incode()
 * Parameters: N/A
 * Return value: N/A
 * Algorithm: go through the additiveArithmeticExpressionPrime FIRST Set
 ************************************************************/
void additiveArithmeticExpressionPrime(void) {
	switch (lookahead.code) {
	case ART_OP_T:
		/* the attribute should be PLUS and MINUS */
		switch (lookahead.attribute.arithmeticOperator) {
		case ADD:
			matchToken(ART_OP_T, ADD);
			multiplicativeArithmeticExpression();
			additiveArithmeticExpressionPrime();
			printf("%s\n", "PLATY: Additive arithmetic expression parsed");
			break;

		case SUB:
			matchToken(ART_OP_T, SUB);
			multiplicativeArithmeticExpression();
			additiveArithmeticExpressionPrime();
			printf("%s\n", "PLATY: Additive arithmetic expression parsed");
			break;

		default:
			printError();
			break;
		}
	}
}
/*************************************************************
 * Multiplicative arithmetic expression
 * BNF: <multiplicative arithmetic expression> -> { <primary arithmetic expression><multiplicative arithmetic expressionPrime> }
 * FIRST(<multiplicative arithmetic expression>) = {AVID_T, FPL_T, INL_T, LBR_T}.
 * 
 * Author: Hoang Do
 * Versions: 1.6
 * Called Functions: primaryArithmeticExpression(), multiplicativeArithmeticExpressionPrime()
 * Parameters: N/A
 * Return value: N/A
 * Algorithm: go through the multiplicative_arithmetic_expression FIRST Set
 ************************************************************/
void multiplicativeArithmeticExpression(void) {
	primaryArithmeticExpression();
	multiplicativeArithmeticExpressionPrime();
}
/*************************************************************
 * multiplicative arithmetic expressionPrime
 * BNF: <multiplicative arithmetic expressionPrime> -> { *<primary arithmetic expression><multiplicative arithmetic expressionPrime>
 *														| /<primary arithmetic expression><multiplicative arithmetic expressionPrime>
 *														| epsilon }
 * FIRST(<multiplicative arithmetic expressionPrime>) = {*, /, epsilon}.
 * Author: Hoang Do
 * Versions: 4.6
 * Called Functions: matchToken(), primaryArithmeticExpression(), multiplicativeArithmeticExpressionPrime(), gen_incode()
 * Parameters: N/A
 * Return value: N/A
 * Algorithm: go through the multiplicativeArithmeticExpressionPrime FIRST Set
 ************************************************************/
void multiplicativeArithmeticExpressionPrime(void) {
	if (lookahead.code == ART_OP_T) {
		if (lookahead.attribute.arithmeticOperator == MUL
			|| lookahead.attribute.arithmeticOperator == DIV) {
			matchToken(ART_OP_T, lookahead.attribute.arithmeticOperator);
			primaryArithmeticExpression();
			multiplicativeArithmeticExpressionPrime();
			printf("%s\n", "PLATY: Multiplicative arithmetic expression parsed");
		}
	}

	switch (lookahead.code) {
	case ART_OP_T:
		/* the attribute should be MULT and DIV */
		switch (lookahead.attribute.arithmeticOperator) {
		case MUL:
			matchToken(ART_OP_T, MUL);
			primaryArithmeticExpression();
			multiplicativeArithmeticExpressionPrime();
			//printf("%s\n", "PLATY: Multiplicative arithmetic expression parsed");
			break;

		case DIV:
			matchToken(ART_OP_T, DIV);
			primaryArithmeticExpression();
			multiplicativeArithmeticExpressionPrime();
			break;
		default:
			break;
		}
	default: break;
	}
	printf("%s\n", "PLATY: Multiplicative arithmetic expression parsed");

}
/*************************************************************
 * Primary arithmetic expression
 * BNF: <primary arithmetic expression> -> { AVID_T | FPL_T | INL_T | (<arithmetic expression>) }
 * FIRST(<primary arithmetic expression>) = {AVID_T, FPL_T, INL_T, LBR_T}.
 * 
 * Author: Hoang Do
 * Versions: 3.5
 * Called Functions: printError(), matchToken(), arithmeticExpression
 * Parameters: N/A
 * Return value: N/A
 * Algorithm: go through the primaryArithmeticExpression FIRST Set
 ************************************************************/
void primaryArithmeticExpression(void) {
	switch (lookahead.code) {
	case AVID_T:
	case FPL_T:
	case INL_T:
		matchToken(lookahead.code, NO_ATTR);
		printf("%s\n", "PLATY: Primary arithmetic expression parsed");
		break;

	case LPR_T:
		matchToken(LPR_T, NO_ATTR);
		arithmeticExpression();
		matchToken(RPR_T, NO_ATTR);
		printf("%s\n", "PLATY: Primary arithmetic expression parsed");
		break;
	default:
		printError();
		break;
	}
}
/*************************************************************
 * String expression
 * BNF: <string expression> -> { <primary string expression><string expressionPrime> }
 * FIRST(<string expression>) = {SVID_T, STR_T}.
 * 
 * Author: Hoang Do
 * Versions: 1.3
 * Called Functions: primaryStringExpression(), stringExpressionPrime()
 * Parameters: N/A
 * Return value: N/A
 * Algorithm: go through the stringExpression FIRST Set
 ************************************************************/
void stringExpression(void) {
	primaryStringExpression();
	stringExpressionPrime();
	printf("%s\n", "PLATY: String expression parsed");
}
/*************************************************************
 * Primary string expression
 * BNF: <primary string expression> -> { SVID_T | STR_T }
 * FIRST(<primary string expression>) = {SVID_T, STR_T}.
 * 
 * Author: Hoang Do
 * Versions: 3.4
 * Called Functions: matchToken(), printError()
 * Parameters: N/A
 * Return value: N/A
 * Algorithm: go through the primaryStringExpression FIRST Set
 ************************************************************/
void primaryStringExpression(void) {
	switch (lookahead.code) {
	case SVID_T:
	case STR_T:
		matchToken(lookahead.code, NO_ATTR);
		printf("%s\n", "PLATY: Primary string expression parsed");
		break;
	default:
		printError();
		break;
	}
}
/*************************************************************
 * String expressionPrime
 * BNF: <string expressionPrime> -> { ++<primary string expression><string expressionPrime>
 *									  | epsilon }
 * FIRST(<string expressionPrime>) = {++, epsilon}.
 * 
 * Author: Hoang Do
 * Versions: 4.5
 * Called Functions: primaryStringExpression(), stringExpressionPrime(), matchToken()
 * Parameters: N/A
 * Return value: N/A
 * Algorithm: go through the stringExpressionPrime FIRST Set
 ************************************************************/
void stringExpressionPrime(void) {
	switch (lookahead.code) {
	case SCC_OP_T:
		matchToken(SCC_OP_T, NO_ATTR);
		primaryStringExpression();
		stringExpressionPrime();
		break;
	default:
		break;
	}
}
/*************************************************************
 * pre-condition
 * BNF: <pre-condition> -> { TRUE | FALSE }
 * FIRST(<pre-condition>) = {KW_T(TRUE), KW_T(FALSE)}.
 * 

 ************************************************************/
void preCondition(void) {
	switch (lookahead.code) {
	case KW_T:
		if (lookahead.attribute.codeType == TRUE)
			matchToken(KW_T, TRUE);
		else if (lookahead.attribute.codeType == FALSE)
			matchToken(KW_T, FALSE);
		else
			printError();
		break;
	default:
		printError();
		break;
	}
	printf("%s\n", "PLATY: Pre-condition parsed");

}
/*************************************************************
 * Selection statement
 * BNF: <selection statement> -> { IF <pre-condition>(<conditional expression>)
 *								   THEN { <opt_statements> }
 *  							   ELSE { <opt_statements> }; }
 * FIRST(<selection statement>) = {KW_T(IF)}.
 * 
 * Author: Hoang Do
 * Versions: 4.5
 * Called Functions: matchToken(), preCondition(), conditionalExpression(), optionalStatements()
 * Parameters: N/A
 * Return value: N/A
 * Algorithm: go through the selectionStatement FIRST Set
 ************************************************************/
void selectionStatement(void) {
	matchToken(KW_T, IF);
	/*matchToken(KW_T, TRUE);*/
	preCondition();
	matchToken(LPR_T, NO_ATTR);
	conditionalExpression();
	matchToken(RPR_T, NO_ATTR);

	matchToken(KW_T, THEN);
	matchToken(LBR_T, NO_ATTR);
	optionalStatements();
	matchToken(RBR_T, NO_ATTR);

	matchToken(KW_T, ELSE);
	matchToken(LBR_T, NO_ATTR);
	optionalStatements();
	matchToken(RBR_T, NO_ATTR);
	matchToken(EOS_T, NO_ATTR);
	printf("%s\n", "PLATY: Selection statement parsed");
}
/*************************************************************
 * Conditional expression
 * BNF: <conditional expression> -> { <logical OR expression> }
 * FIRST(<conditional expression>) = {AVID_T, FPL_T, INL_T, SVID_T, STR_T}.
 * 
 * Author: Hoang Do
 * Versions: 1.3
 * Called Functions: logicalORExpression()
 * Parameters: N/A
 * Return value: N/A
 * Algorithm: go through the conditionalExpression FIRST Set
 ************************************************************/
void conditionalExpression(void) {
	logicalORExpression();
	printf("%s\n", "PLATY: Conditional expression parsed");
}
/*************************************************************
 * Logical OR expression
 * BNF: <logical OR expression> -> { <logical AND expression><logical OR expressionPrime> }
 * FIRST(<logical OR expression>) = {AVID_T, FPL_T, INL_T, SVID_T, STR_T}.
 * 
 * Author: Hoang Do
 * Versions: 3.4
 * Called Functions: logicalANDExpression(), logicalORExpressionPrime()s
 * Parameters: N/A
 * Return value: N/A
 * Algorithm: go through the logicalORExpression FIRST Set
 ************************************************************/
void logicalORExpression(void)
{
	logicalANDExpression();
	logicalORExpressionPrime();
	printf("%s\n", "PLATY: Logical Or expression parsed");
}
/*************************************************************
 * Logical OR expressionPrime
 * BNF: <logical OR expressionPrime> -> { .OR. <logical AND expression><logical OR expressionPrime> }
 * FIRST(<logical OR expressionPrime>) = {.OR., epsilon}.
 * 
 * Author: Hoang Do
 * Versions: 3.7
 * Called Functions: logicalANDExpression(), logicalORExpressionPrime(), matchToken()
 * Parameters: N/A
 * Return value: N/A
 * Algorithm: go through the logicalORExpressionPrime FIRST Set
 ************************************************************/
void logicalORExpressionPrime(void)
{
	switch (lookahead.code) {
	case LOG_OP_T:
		/* the attribute logical operator should be OR */
		switch (lookahead.attribute.logicalOperator) {
		case OR:
			matchToken(LOG_OP_T, OR);
			logicalANDExpression();
			logicalORExpressionPrime();
			break;

		default:
			break;
		}
		printf("%s\n", "PLATY: Logical OR expression parsed");
	default:
		break;
	}
}
/*************************************************************
 * Logical AND expression
 * BNF: <logical AND expression> -> { <logical NOT expression><logical AND expressionPrime> }
 * FIRST(<logical AND expression>) = {AVID_T, FPL_T, INL_T, SVID_T, STR_T}.
 * 
 * Author: Hoang Do
 * Versions: 1.4
 * Called Functions: logicalNOTExpression(), logicalANDExpressionPrime()
 * Parameters: N/A
 * Return value: N/A
 * Algorithm: go through the logicalANDExpression FIRST Set
 ************************************************************/
void logicalANDExpression(void)
{
	logicalNOTExpression();
	logicalANDExpressionPrime();
}
/*************************************************************
 * Logical AND expressionPrime
 * BNF: <logical AND expressionPrime> -> { .AND. <logical NOT expression><logical AND expressionPrime>
 *										   | epsilon}
 * FIRST(<logical AND expressionPrime>) = {.AND., epsilon}.
 * Author: Hoang Do
 * Versions: 5.1
 * Called Functions: matchToken(),logicalNOTExpression(), logicalANDExpressionPrime()
 * Parameters: N/A
 * Return value: N/A
 * Algorithm: go through the logicalANDExpressionPrime FIRST Set
 ************************************************************/
void logicalANDExpressionPrime(void)
{
	switch (lookahead.code) {
	case LOG_OP_T:
		/* the attribute logical operator should be AND */
		switch (lookahead.attribute.logicalOperator) {
		case AND:
			matchToken(LOG_OP_T, AND);
			logicalNOTExpression();
			logicalANDExpressionPrime();
			printf("%s\n", "PLATY: Logical AND expression parsed");
			break;

		default:
			break;
		}

	default:
		printf("%s\n", "PLATY: Logical And expression parsed");
		break;
	}
}
/*************************************************************
 * Logical NOT expression
 * BNF: <logical NOT expression> -> { .NOT. <relational expression>
 *									  | <relational expression> }
 * FIRST(<logical NOT expression>) = {.NOT. AVID_T, FPL_T, INL_T, SVID_T, STR_T}.
 * Author: Hoang Do
 * Versions: 3.1
 * Called Functions: matchToken(),relationalExpression(), printError()
 * Parameters: N/A
 * Return value: N/A
 * Algorithm: go through the logicalNOTExpression FIRST Set
 ************************************************************/
void logicalNOTExpression(void)
{
	switch (lookahead.code) {
	case LOG_OP_T:
		/* the attribute logical operator should be AND */
		switch (lookahead.attribute.logicalOperator) {
		case NOT:
			matchToken(LOG_OP_T, NOT);
			relationalExpression();
			//printf("%s\n", "PLATY: Logical NOT expression parsed");
			break;
			
		default:
			printError();
			break;
		}
	case AVID_T:
	case FPL_T:
	case INL_T:
	case SVID_T:
	case STR_T:
		relationalExpression();
		break;
	default:
		printError();
		break;
	}
	printf("%s\n", "PLATY: Logical Not expression parsed");

}
/*************************************************************
 * Relational expression
 * BNF: <relational expression> -> { <relational a_expression>
 *									 | <relational s_expression> }
 * FIRST(<relational expression>) = {AVID_T, FPL_T, INL_T, SVID_T, STR_T}.
 * 
 * Author: Hoang Do
 * Versions: 2.3
 * Called Functions: relationArithmeticExpression(),relationStringExpression(), printError()
 * Parameters: N/A
 * Return value: N/A
 * Algorithm: go through the relationalExpression FIRST Set
 ************************************************************/
void relationalExpression(void)
{
	switch (lookahead.code)
	{
	case AVID_T:
	case FPL_T:
	case INL_T:
		relationArithmeticExpression();
		break;

	case SVID_T:
	case STR_T:
		relationStringExpression();
		break;

	default:
		printError();
		break;
	}
	printf("%s\n", "PLATY: Relational expression parsed");

}
//=============================== ARITHMETIC ===============================//

/*************************************************************
 * Relational a_expression
 * BNF: <relational a_expression> -> { <primary a_relational expression>
 *									   <primary a_relational expressionPrime> }
 * FIRST(<relational a_expression>) = {AVID_T, FPL_T, INL_T}.
 * 
 * Author: Hoang Do
 * Versions: 1.2
 * Called Functions: primaryARelationalExpression(),primaryARelationalExpressionPrime()
 * Parameters: N/A
 * Return value: N/A
 * Algorithm: go through the relationArithmeticExpression FIRST Set
 ************************************************************/
void relationArithmeticExpression(void)
{
	primaryARelationalExpression();
	primaryARelationalExpressionPrime();
	printf("%s\n", "PLATY: Relational arithmetic operator parsed");
}
/*************************************************************
 * Primary a_relational expression
 * BNF: <primary a_relational expression> -> { AVID_T | FPL_T | INL_T }
 * FIRST(<primary a_relational expression>) = {AVID_T, FPL_T, INL_T}.
 * Author: Hoang Do
 * Versions: 4.5
 * Called Functions: matchToken(),printError()
 * Parameters: N/A
 * Return value: N/A
 * Algorithm: go through the primaryARelationalExpression FIRST Set
 ************************************************************/
void primaryARelationalExpression(void)
{
	switch (lookahead.code)
	{
	case AVID_T:
	case FPL_T:
	case INL_T:
		matchToken(lookahead.code, NO_ATTR);
		break;
	default:
		printError();
		break;
	}
	printf("%s\n", "PLATY: Primary relational arithmetic expression parsed");
}
/*************************************************************
 * Relational a_relational expressionPrime
 * BNF: <relational a_relational expressionPrime> -> { == <primary a_relational expression>
												  | != <primary a_relational expression>
												  | > <primary a_relational expression>
												  | < <primary a_relational expression> }
 * FIRST(<relational a_relational expressionPrime>) = {==, !=, >, <}
 * 
 * Author: Hoang Do
 * Versions: 4.6
 * Called Functions: matchToken(), primaryARelationalExpression(), printError()
 * Parameters: N/A
 * Return value: N/A
 * Algorithm: go through the primaryARelationalExpressionPrime FIRST Set
 ************************************************************/
void primaryARelationalExpressionPrime(void)
{
	switch (lookahead.code) {
	case REL_OP_T:
		switch (lookahead.attribute.relationalOperator) {
		case EQ:
		case NE:
		case GT:
		case LT:
			matchToken(REL_OP_T, lookahead.attribute.relationalOperator);
			primaryARelationalExpression();
			break;

		default:
			printError();
			break;
		}
	default:
		break;
	}
}
//=============================== STRING ===============================//

/*************************************************************
 * Relational string expression
 * BNF: <relational s_expression> -> { <primary s_relational expression>
 *									   <primary s_relational expressionPrime> }
 * FIRST(<relational s_expression>) = {SVID_T, STR_T}.
 * Author: Hoang Do
 * Versions: 1.4
 * Called Functions: primarySRelationalExpression(), primarySRelationalExpressionPrime()
 * Parameters: N/A
 * Return value: N/A
 * Algorithm: go through the relationStringExpression FIRST Set
 ************************************************************/
void relationStringExpression(void)
{
	primarySRelationalExpression();
	primarySRelationalExpressionPrime();
	printf("%s\n", "PLATY: Relational string operator parsed");
}
/*************************************************************
 * Primary s_relational expression
 * BNF: <relational s_expression> -> { <primary string expression> }
 * FIRST(<relational s_expression>) = {SVID_T, STR_T}.
 * Author: Hoang Do
 * Versions: 3.2
 * Called Functions: primaryStringExpression(), printError()
 * Parameters: N/A
 * Return value: N/A
 * Algorithm: go through the primarySRelationalExpression FIRST Set
 ************************************************************/
void primarySRelationalExpression(void)
{
	switch (lookahead.code) {
	case SVID_T:
	case STR_T:
		primaryStringExpression();
		printf("%s\n", "PLATY: Primary relational string expression parsed");
		break;

	default:
		printError();
		break;
	}
}
/*************************************************************
 * relational string expressionPrime
 * BNF: <relational s_expressionPrime> -> { ==<primary s_relational expression>
 *											| != <primary s_relational expression>
 *											| > <primary s_relational expression>
 *											| < <primary s_relational expression> }
 * FIRST(<relational s_expressionPrime>) = {==, !=, >, <}.
 * 
 * Author: Hoang Do
 * Versions: 3.4
 * Called Functions: primarySRelationalExpression(), matchToken(), printError()
 * Parameters: N/A
 * Return value: N/A
 * Algorithm: go through the primarySRelationalExpressionPrime FIRST Set
 ************************************************************/
void primarySRelationalExpressionPrime(void)
{
	switch (lookahead.code) {
	case REL_OP_T:
		switch (lookahead.attribute.relationalOperator) {
		case EQ:
		case NE:
		case GT:
		case LT:
			matchToken(REL_OP_T, lookahead.attribute.relationalOperator);
			primarySRelationalExpression();
			break;

		default:
			printError();
			break;
		}

	default:
		break;
	}
}
/*************************************************************
 * Iteration statement
 * BNF: <iteration statement> -> { WHILE<pre-condition> (<conditional expression>)
 *								   DO {<statements>} }
 * FIRST(<iteration statement>) = {KW_T(WHILE)}.
 * Author: Hoang Do
 * Versions: 3.5
 * Called Functions: preCondition(), conditionalExpression(), matchToken(), statements(), gen_incode()
 * Parameters: N/A
 * Return value: N/A
 * Algorithm: go through the iterationStatement FIRST Set
 ************************************************************/
void iterationStatement(void) {
	matchToken(KW_T, WHILE);
	/*matchToken(KW_T, TRUE);*/
	preCondition();
	matchToken(LPR_T, NO_ATTR);
	conditionalExpression();
	matchToken(RPR_T, NO_ATTR);

	matchToken(KW_T, DO);
	matchToken(LBR_T, NO_ATTR);
	statements();
	matchToken(RBR_T, NO_ATTR);
	matchToken(EOS_T, NO_ATTR);
	printf("%s\n", "PLATY: Iteration statement parsed");
}

/*************************************************************
 * Input statement
 * BNF: <input statement> -> READ { <variable list> }
 * FIRST(<input statement>) = {KW_T(READ)}.
 * 
 * Author: Hoang Do
 * Versions: 1.5
 * Called Functions: matchToken(), variableList()
 * Parameters: N/A
 * Return value: N/A
 * Algorithm: go through the inputStatement FIRST Set
 ************************************************************/
void inputStatement(void) {
	matchToken(KW_T, READ);
	matchToken(LPR_T, NO_ATTR);
	variableList();
	matchToken(RPR_T, NO_ATTR);
	matchToken(EOS_T, NO_ATTR);
	printf("%s\n", "PLATY: Input statement parsed");
}

/*************************************************************
 * Variable list
 * BNF: <variable list> -> { <variable identifier><variable listPrime> }
 * FIRST(<variable list>) = {AVID_T, SVID_T}.
 * Author: Hoang Do
 * Versions: 4.1
 * Called Functions: variableIdentifier(), variableListPrime()
 * Parameters: N/A
 * Return value: N/A
 * Algorithm: go through the variableList FIRST Set
 ************************************************************/
void variableList(void) {
	variableIdentifier();
	variableListPrime();
	printf("%s\n", "PLATY: Variable list parsed");
}
/*************************************************************
 * Variable identifier
 * BNF: <variable identifier> -> { AVID_T, SVID_T }
 * FIRST(<variable identifier>) = {AVID_T, SVID_T}.
 * 
 * Author: Hoang Do
 * Versions: 3.4
 * Called Functions: matchToken(), printError()
 * Parameters: N/A
 * Return value: N/A
 * Algorithm: go through the variableIdentifier FIRST Set
 ************************************************************/
void variableIdentifier(void) {
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T:
		matchToken(lookahead.code, NO_ATTR);
		printf("%s\n", "PLATY: Variable identifier parsed");
		break;

	default:
		printError();
		break;
	}
}
/*************************************************************
 * Variable listPrime
 * BNF: <variable listPrime> -> { ,<variable identifier><variable listPrime> | epsilon }
 * FIRST(<variable listPrime>) = {COM_T, epsilon}.
 * 
 * Author: Hoang Do
 * Versions: 4.5
 * Called Functions: variableIdentifier(), variableListPrime(), matchToken()
 * Parameters: N/A
 * Return value: N/A
 * Algorithm: go through the variableListPrime FIRST Set
 ************************************************************/
void variableListPrime(void) {
	if (lookahead.code == COM_T) {
		matchToken(COM_T, NO_ATTR);
		variableIdentifier();
		variableListPrime();
	}
}
/*************************************************************
 * Output statement
 * BNF: <output statement> -> OUTPUT { (<output list>) }
 * FIRST(<output statement>) = {KW_T(OUTPUT)}.
 * 
 * Author: Hoang Do
 * Versions: 1.7
 * Called Functions: outputList(), matchToken()
 * Parameters: N/A
 * Return value: N/A
 * Algorithm: go through the outputStatement FIRST Set
 ************************************************************/
void outputStatement(void) {
	matchToken(KW_T, WRITE);
	matchToken(LPR_T, NO_ATTR);
	outputList();
	matchToken(RPR_T, NO_ATTR);
	matchToken(EOS_T, NO_ATTR);
	printf("%s\n", "PLATY: Output statement parsed");
}
/*************************************************************
 * Output list
 * BNF: <output_list> -> { <opt_variable list> | STR_T }
 * FIRST(<output_list>) = {STR_T, epsilon, AVID_T, SVID_T}.
 * 
 * Author: Hoang Do
 * Versions: 3.3
 * Called Functions: variableList(), matchToken()
 * Parameters: N/A
 * Return value: N/A
 * Algorithm: go through the outputList FIRST Set
 ************************************************************/
void outputList(void) {
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T:
		variableList();
		break;

	case STR_T:
		matchToken(STR_T, NO_ATTR);
		printf("%s\n", "PLATY: Output variable list parsed");
		break;

	default:
		printf("%s\n", "PLATY: Output list empty parsed");
		break;
	}
}
