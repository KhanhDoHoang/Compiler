/*************************************************************
 * File name: scanner.c
 * Compiler: MS Visual Studio 2019
 * Author: Do749_Jenni742_s14
 * Course: CST 8152, Compilers, Lab Section: 014
 * Assignment: 2
 * Date: March 20th, 2021
 * Professor: Abdullah Kadri, Paulo Sousa
 * Purpose: - Implementing a Lexical Analyzer (Scanner).
 *			- This file contains some declarations and definitions
 *			  necessary for the scanner.
 * Function list: startScanner(), tokenizer(), nextState(),
 *				  nextClass(), funcAVID(), funcSVID(), funcIL(),
 *				  funcFPL(), funcSL(), funcErr(), isKeyword().
  ************************************************************/


  /* The #define _CRT_SECURE_NO_WARNINGS should be used in MS Visual Studio projects
   * to suppress the warnings about using "unsafe" functions like fopen()
   * and standard sting library functions defined in string.h.
   * The define does not have any effect in Borland compiler projects.
   */
#define _CRT_SECURE_NO_WARNINGS

#include <stdio.h>   /* standard input / output */
#include <ctype.h>   /* conversion functions */
#include <stdlib.h>  /* standard library functions and constants */
#include <string.h>  /* string functions */
#include <limits.h>  /* integer types constants */
#include <float.h>   /* floating-point types constants */

   /*#define NDEBUG        to suppress assert() call */
#include <assert.h>  /* assert() prototype */

/* project header files */
#include "buffer.h"
#include "token.h"
#include "table.h"

#define DEBUG  /* for conditional processing */
#undef  DEBUG

/* Global objects - variables */
/* This buffer is used as a repository for string literals.
   It is defined in platy_st.c */
extern bPointer stringLiteralTable;		/* String literal table */
int line;								/* current line number of the source code */
extern int errorNumber;					/* defined in platy_st.c - run-time error number */

static char debugMode = 0;				/* optional for debugging */

/* Local(file) global objects - variables */
static bPointer lexemeBuffer;			/* pointer to temporary lexeme buffer */
static bPointer sourceBuffer;			/* pointer to input source buffer */
/* No other global variable declarations/definitiond are allowed */

/* scanner.c static(local) function  prototypes */
static int nextClass(char c);			/* character class function */
static int nextState(int, char);		/* state machine function */
static int isKeyword(char* kw_lexeme);	/* keywords lookup function */

/*************************************************************
* Function name: startScanner.
* Purpose: Performs the initialization of the scanner input
*		   buffer and some other scanner components.
* Author: Hoang Do, Jenni Jenni
* Versions: 1.4
* Called functions: bIsEmpty(), bRewind(), bClean().
* Parameters: bPointer psc_buf.
* Return value: EXIT_FAILURER(1), EXIT_SUCCESS(0).
* Algorithm: 1. Calls the bIsEmpty() function to check if the
*				passed buffer is empty. If it is, return 1.
*			 2. Calls bRewind and bClean in case the buffer has
*				been read previously.
*			 3. Then, return 0 as the value of EXIT_SUCCESS at the end.
 ************************************************************/
/*************************************************************
 * Intitializes scanner
 *		This function initializes the scanner using defensive programming.
 ************************************************************/
int startScanner(bPointer psc_buf) {
	if (bIsEmpty(psc_buf))
		return EXIT_FAILURE; /*1*/
	/* in case the buffer has been read previously  */
	bRewind(psc_buf);
	bClean(stringLiteralTable);
	line = 1;
	sourceBuffer = psc_buf;
	return EXIT_SUCCESS; /*0*/
}

/*************************************************************
* Function name: tokenizer.
* Purpose: Performs the token recognition.
* Author: Hoang Do
* Versions: 1.4
* Called functions: bGetCh(), isspace(), strcpy(), bRetract(),
*					 bSetMarkOffset(), bGetAddChOffset(), bRestore(),
*					 bGetChOffset(), bAddCh(), bCreate(), bFinish(),
*					 bFree(), nextState(), bGetContent().
* Parameters: void.
* Return value: Token currentToken.
* Algorithm: 1. Checks for the current token code by using while loop.
*			  2.
 ************************************************************/
/*************************************************************
 * Process Token
 *		Main function of buffer, responsible to classify a char (or sequence
 *		of chars). In the first part, a specific sequence is detected (reading
 *		from buffer). In the second part, a pattern (defined by Regular Expression)
 *		is recognized and the appropriate function is called (related to final states
 *		in the Transition Diagram).
 ************************************************************/
Token tokenizer(void) {
	Token currentToken = { 0 }; /* token to return after pattern recognition. Set all structure members to 0 */
	unsigned char c;		/* input symbol */
	int state = 0;			/* initial state of the FSM */
	short lexStart;			/* start offset of a lexeme in the input char buffer (array) */
	short lexEnd;			/* end offset of a lexeme in the input char buffer (array)*/
	int lexLength;			/* token length */
	int i;					/* counter */
	/* DECLARE MY LOCAL VAR TO AVOID CALLING BGETCH TWICE*/
	unsigned char newc;		/* new char avoid calling bgetch twice */
	unsigned char newc2;	/* new char avoid calling bgetch multi times*/
	unsigned char newc3;
	unsigned char newc4;

	while (1) { /* endless loop broken by token returns it will generate a warning */
		/*GET THE NEXT SYMBOL FROM THE INPUT BUFFER*/
		c = bGetCh(sourceBuffer);

		/* ------------------------------------------------------------------------
			Part 1: Implementation of token driven scanner.
			Every token is possessed by its own dedicated code
			-----------------------------------------------------------------------
		*/

		switch (c) {
			/*White space characters*/
		case ' ': case '\t': case '\v': case '\f': case '\r':
			continue;

			/*New line*/
		case '\n':
			line++;
			continue;

		/* Checking comments and errs*/
		case '%':
			newc = bGetCh(sourceBuffer);
			//printf("I am the comment! \n");
			/* If it is a comment*/
			if (newc == '%') {
				/* Ignore all characters until \n or SEOF*/
				while (newc != '\n') {
					/*Get the next characters in the line*/
					newc = bGetCh(sourceBuffer);

					/*if go to the end of file*/
					if (newc == CHARSEOF255 || newc == CHARSEOF0) {
						/*Retract the SEOF/EOF*/
						bRetract(sourceBuffer);
						/*Break the while loop*/
						break;
					}
				}
				/*Count the line*/
				if (newc == '\n')
					line++;
				/*Move to the next line*/
				continue;
			}

			/*If it is not a comment*/
			if (newc != '%') {
				currentToken.code = ERR_T;
				currentToken.attribute.errLexeme[0] = '%';
				bRetract(sourceBuffer);
				return currentToken;
			}
		

		/*Error cases*/
		case RT_FAIL_2:
			errorNumber = TRUE;
			currentToken.code = RTE_T;
			strcpy(currentToken.attribute.errLexeme, "RUN TIME ERROR: ");
			return currentToken;

		/*Seperators*/
		case '{':
			currentToken.code = LBR_T;
			return currentToken;
		case '}':
			currentToken.code = RBR_T;
			return currentToken;
		case ';':
			currentToken.code = EOS_T;
			return currentToken;
		case ',':
			currentToken.code = COM_T;
			return currentToken;
		case '(':
			currentToken.code = LPR_T;
			return currentToken;
		case ')':
			currentToken.code = RPR_T;
			return currentToken;
		
		/*EOF*/
		case CHARSEOF255:
			currentToken.attribute.seofType = SEOF_EOF;
			currentToken.code = SEOF_T;
			return currentToken;

		case CHARSEOF0:
			currentToken.attribute.seofType = SEOF_0;
			currentToken.code = SEOF_T;
			return currentToken;

		/*Arithmethic Operator characters - String Concatenation */
		case '+':
			newc = bGetCh(sourceBuffer);
			if (newc == '+'){
				currentToken.code = SCC_OP_T;
				return currentToken;
			}
			bRetract(sourceBuffer);
			currentToken.code = ART_OP_T;
			currentToken.attribute.arithmeticOperator = ADD;
			return currentToken;
		case '-':
			currentToken.code = ART_OP_T;
			currentToken.attribute.arithmeticOperator = SUB;
			return currentToken;
		case '*':
			currentToken.code = ART_OP_T;
			currentToken.attribute.arithmeticOperator = MUL;
			return currentToken;
		case '/':
			currentToken.code = ART_OP_T;
			currentToken.attribute.arithmeticOperator = DIV;
			return currentToken;

		/*Relational Operator / Assignment characters*/
		case '>':
			currentToken.code = REL_OP_T;
			currentToken.attribute.relationalOperator = GT;
			return currentToken;
		case '<':
			/*if not same or less than token*/
			currentToken.code = REL_OP_T;
			currentToken.attribute.relationalOperator = LT;
			return currentToken;
		case '=':
			newc = bGetCh(sourceBuffer);
			/* if '=' or '==' sign, set tokens */
			if (newc == '=') {
				currentToken.code = REL_OP_T;
				currentToken.attribute.codeType = EQ;
				return currentToken;
			}
			bRetract(sourceBuffer);
			currentToken.code = ASS_OP_T;
			return currentToken;
		
		/*Logical operator*/
		case '.':
			/* Set the current mark*/
			bSetMarkOffset(sourceBuffer, bGetChOffset(sourceBuffer));
			newc = bGetCh(sourceBuffer);//Be A, O, N
			newc2 = bGetCh(sourceBuffer); //Be N, R, O
			newc3 = bGetCh(sourceBuffer); // Be D, ., T
			newc4 = bGetCh(sourceBuffer); //Be ., .
			//printf("I am logical operator! \n");

			if (newc == CHARSEOF0 || newc == CHARSEOF255) {
				bRetract(sourceBuffer);
				currentToken.code = ERR_T;
				currentToken.attribute.errLexeme[0] = '.';
				currentToken.attribute.errLexeme[1] = '\0';

				return currentToken;
			}

			/*.AND. logical operator*/
			if (newc == 'A' && newc2 == 'N' && newc3 == 'D' && newc4 == '.') {
				currentToken.code = LOG_OP_T;
				currentToken.attribute.logicalOperator = AND;
				return currentToken;
			}
			/*.NOT. logical operator*/
			if (newc == 'N' && newc2 == 'O' && newc3 == 'T' && newc4 == '.') {
				currentToken.code = LOG_OP_T;
				currentToken.attribute.logicalOperator = NOT;
				return currentToken;
			}
			/*OR just need 3 b_getc call, call one retract*/
			/*bRetract(sourceBuffer);*/
			/*.OR. logical operator*/
			if (newc == 'O' && newc2 == 'R' && newc3 == '.') {
				currentToken.code = LOG_OP_T;
				currentToken.attribute.logicalOperator = OR;
				return currentToken;
			}			

			/*If not, retract three times and return an error*/
			/*for (i = 0; i < 3; i++) {
				bRetract(sourceBuffer);
			}*/
			
			bRestore(sourceBuffer);
			currentToken.code = ERR_T;
			currentToken.attribute.errLexeme[0] = '.';
			currentToken.attribute.errLexeme[1] = '\0';
			//printf("Hello====== %c\n", bGetCh(sourceBuffer));

			//strncpy(currentToken.attribute.errLexeme, ".", strlen(lexemeBuffer));
			return currentToken;

		case '!':
			newc = bGetCh(sourceBuffer);
			if (newc == '=') {
				currentToken.code = REL_OP_T;
				currentToken.attribute.relationalOperator = NE;
				return currentToken;
			}
			

			bRetract(sourceBuffer);
			
			currentToken.code = ERR_T;
			currentToken.attribute.errLexeme[0] = '!';
			currentToken.attribute.errLexeme[1] = newc;
			currentToken.attribute.errLexeme[2] = '\0';
			
			return currentToken;
			
			/* ------------------------------------------------------------------------
				Part 2: Implementation of Finite State Machine (DFA)
						   or Transition Table driven Scanner
						   Note: Part 2 must follow Part 1 to catch the illegal symbols
				-----------------------------------------------------------------------
			*/

		/*Transition driven scanner implementation inside default */
		default: /* general case */
			state = nextState(state, c);
			lexStart = bSetMarkOffset(sourceBuffer, bGetChOffset(sourceBuffer) - 1);

			//printf("c: %c\n", c);

			/*Check if run time error happened*/
			if (lexStart == RT_FAIL_1) {
				errorNumber = TRUE;
				strcpy(currentToken.attribute.errLexeme, "RUN TIME ERROR: ");
				currentToken.code = RTE_T;
				return currentToken;
			}

			/*FSM0. Start with state = 0 and input c. Can be Letter, Digit or String opening double quote*/
			/*FSM1. Get the next sate from the transition table*/
			/*FSM2. Use as_table to get the type of state (NOAS, ASWR, ASNR)
			If it is NOAS, get the next character and repeat FSM1*/
			while (stateType[state] == NOAS) {
				/* Continue reading = bGetChar(sourceBuffer) and going to the next state*/
				c = bGetCh(sourceBuffer);
				state = nextState(state, c);

			}
			if (stateType[state] == ASWR) {
				bRetract(sourceBuffer);
			}
			/*Since it reached an accepting state after getting an extra character,
			retract the buffer*/
			lexEnd = bGetChOffset(sourceBuffer);
			lexLength = lexEnd - lexStart;
			lexemeBuffer = bCreate(DEFAULT_SIZE, FIX_INC_FACTOR, 'f');
			if (lexemeBuffer == NULL) {
				errorNumber = 1;
				strcpy(currentToken.attribute.errLexeme, "RUN TIME ERROR: ");
				currentToken.code = RTE_T;
				return currentToken;
			}
			bRestore(sourceBuffer);
			for (i = lexStart; i < lexEnd; i++) {
				c = bGetCh(sourceBuffer);
				if (bAddCh(lexemeBuffer, c) == NULL) {
					currentToken.code = RTE_T;
					strcpy(currentToken.attribute.errLexeme, "RUN TIME ERROR: ");
					errorNumber = TRUE;
					return currentToken;
				}
			}
			/* Call the retract function from buffer*/
			//bRetract(sourceBuffer);

			//printf("c: %c\n", c);

			//state = nextState(state, c);//bRestore(sourceBuffer);
	
			/*FSM3. If the state is an accepting state, token is found, leave the machine and call a function*/
			/*For ASWR state, retract function is called first*/
			

			//printf("c: %c\n", c);

			/*4.setlexend at this time you have finished the reading of current lexeme*/
			/*5.cal the lenglenght (use lexStart and lexEnd*/
			/*6. Create a temp buffer(leeme buffer - allocate using lexLength*/
			/*7. Test the bufer (call EXIT in problems)*/
			/*Check null -> failure */

			//printf("c: %c\n", c);

			
			/* Reset*/
			/*8.Time to call bRestore to adjust cGetOffset*/

			//printf("c: %c\n", c);

			/*Copy the lexeme betwween lexstart and lexend from the input buffer to the lexeme buffer
			using b_getc() and b_addc()*/
			

			//printf("c: %c\n", c);

			/*Add SEOF to the lexeme buffer end*/
			bAddCh(lexemeBuffer, CHARSEOF0);

			//printf("c: %c\n", c);

			/*9.Copy from source buffer to lexemeBuffer and finalize it with \0*/
			/*bFinish(lexemeBuffer, '\0');*/
			/*Create the lexemeBuffer*/
			currentToken = (*finalStateTable[state])(bGetContent(lexemeBuffer, 0));

			//newc = bGetCh(sourceBuffer);

			//if (newc == CHARSEOF255 || newc == CHARSEOF0) {
			//	/*Retract the SEOF/EOF*/
			//	bRetract(sourceBuffer);
			//	/*Break the while loop*/
			//	break;
			//}

			bFree(lexemeBuffer);//free the buffer

			//newc = bGetCh(sourceBuffer);
			/*if go to the end of file*/

			return currentToken;
		} 

	} 

} 



/*************************************************************
* Function name: nextState.
* Purpose: Gets the next state.
* Author: Hoang Do, Jenni Jenni
* Versions: 1.0
* Called functions: nextClass(), assert().
* Parameters: int state, char c.
* Return value: operation status.
* Algorithm: 1. Calls nextClass() function to identify the
*				 input character's "class".
*			  2. Assigns the value of variable next to be equal
*				 to the value in the transition table.
*			  3. Calls the assert function to determine whether
*				 the value of next is an illegal state or not.
*			  4. Checks if next is an illegal state then exit.
*			  5. Returns the value of variable next at the end.
 ************************************************************/
int nextState(int state, char c) {
	/*************************************************************
	 * Get Next State
		The assert(int test) macro can be used to add run-time diagnostic to programs
		and to "defend" from producing unexpected results.
		- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		(*) assert() is a macro that expands to an if statement;
		if test evaluates to false (zero) , assert aborts the program
		(by calling abort()) and sends the following message on stderr:
		(*) Assertion failed: test, file filename, line linenum.
		The filename and linenum listed in the message are the source file name
		and line number where the assert macro appears.
		- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		If you place the #define NDEBUG directive ("no debugging")
		in the source code before the #include <assert.h> directive,
		the effect is to comment out the assert statement.
		- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		The other way to include diagnostics in a program is to use
		conditional preprocessing as shown bellow. It allows the programmer
		to send more details describing the run-time problem.
		Once the program is tested thoroughly #define DEBUG is commented out
		or #undef DEBUF is used - see the top of the file.
	 ************************************************************/

	int col;
	int next;
	col = nextClass(c);
	next = transitionTable[state][col];
#ifdef DEBUG
	printf("Input symbol: %c Row: %d Column: %d Next: %d \n", c, state, col, next);
#endif
	assert(next != IS);
#ifdef DEBUG
	if (next == IS) {
		printf("Scanner Error: Illegal state:\n");
		printf("Input symbol: %c Row: %d Column: %d\n", c, state, col);
		exit(1);
	}
#endif
	return next;
}

/*************************************************************
* Function name: nextClass.
* Purpose: Returns a column number from the transition table
*		   based on the input character.
* Author: Hoang Do, Jenni Jenni
* Versions: 1.0
* Called functions: isalpha(), isdigit().
* Parameters: char c.
* Return value: variable val(0/1/2/3/4/5/6).
* Algorithm: 1. Takes the character input.
*			 2. Identifies the character whether it is a letter,
*				digit, float number, SVID, string delimitator,
*				end of symbol(EOF), or other character.
*			 3. Return the variable val based on the identified
*				"class".
 ************************************************************/
int nextClass(char c) {
	/*************************************************************
	 * Get Next Token Class
	* Create a function to return the column number in the transition table:
	* Considering an input char c, you can identify the "class".
	* For instance, a letter should return the column for letters, etc.
	************************************************************/
	int val = -1;
	/* TODO_207: Adjust the logic to return next column in TT */
	/*				[A-z](0),	[0-9](1),	.(2),	$(3),	'(4),	SEOF(5),	other(6) */
	/*Checking all the alphabet*/
	if (isalpha(c)) {
		val = 0;
	}
	else {
		switch (c) {
		/*Check numbers*/
		case '0':
		case '1':
		case '2':
		case '3':
		case '4':
		case '5':
		case '6':
		case '7':
		case '8':
		case '9':
			val = 1;
			break;
		/*Check the floating point*/
		case '.':
			val = 2;
			break;
		/*Check the fourth state*/
		case '$':
			val = 3;
			break;
		/* Check literal string*/
		case '\'':
			val = 4;
			break;
		case CHARSEOF0: 
		case CHARSEOF255:
			val = 5;
			break;
		default:
			val = 6;
			break;
		}
	}
	//printf("Hello im here10========================\n");

	return val;
}

/*************************************************************
* Function name: funcAVID.
* Purpose: Accepts function for the arithmetic variable
*			identifier and keywords.
* Author: Hoang Do
* Versions: 1.17.2
* Called functions: isKeyword(), strlen(), strncpy().
* Parameters: char lexeme.
* Return value: Token currentToken.
* Algorithm: 1. Checks if the lexeme is a keyword.
*			  2. If it is, return the corresponded token attribute.
*			  3. Sets the AVID token.
*			  4. Checks if the lexeme is longer than VID_LEN.
 ************************************************************/
Token funcAVID(char lexeme[]) {
	/*************************************************************
	 * Acceptance State Function AVID
	 *		In this function, the pattern for AVID must be recognized.
	 *		Since keywords obey the same pattern, is required to test if
	 *		the current lexeme matches with KW from language.
	 *	- Remember to respect the limit defined for lexemes (VID_LEN) and
	 *	  set the lexeme to the corresponding attribute (vidLexeme).
	 *    Remember to end each token with the \0.
	 *  - Suggestion: Use "strncpy" function.
	 ************************************************************/
	Token currentToken = { 0 };
	int length = strlen(lexeme);
	int i;
	//printf("Hello im here11========================\n");
	/*Check if lexeme is a keyword*/

	if (isKeyword(lexeme) != RT_FAIL_1) {
		currentToken.code = KW_T;
		currentToken.attribute.keywordIndex = isKeyword(lexeme);
		return currentToken;
	}
	/*If the lexeme is not a keyword, check for AVID*/
	/*Set a AVID TOKEN*/
	currentToken.code = AVID_T;

	/* If the length is longer than VID_LENGTH*/
	if (strlen(lexeme) > VID_LEN) {
		/* To store the char into var string arr*/
		for (i = 0; i < VID_LEN; i++) {
			currentToken.attribute.vidLexeme[i] = lexeme[i];
		}
		/*Add the '\0' at the end of the buffer*/
		currentToken.attribute.vidLexeme[VID_LEN] = CHARSEOF0;
		strncpy(currentToken.attribute.vidLexeme, lexeme, length);
		return currentToken;
	}

	/*If not, store the lexeme to vid_lex attribute*/
	strncpy(currentToken.attribute.vidLexeme, lexeme, length);

	return currentToken;
}

//char temp1 = bGetCh(sourceBuffer);
//char temp2 = bGetCh(sourceBuffer);
//
//if (temp2 == CHARSEOF0 || temp2 == CHARSEOF255) {
//	//bRetract(sourceBuffer);
//	currentToken.attribute.vidLexeme[0] = temp1;
//	//currentToken.attribute.vidLexeme[VID_LEN] = temp1;
//	strncpy(currentToken.attribute.vidLexeme, temp1, length);

//	return currentToken;
//}

/*************************************************************
 * Function name: funcSVID.
 * Purpose: Accepts functionfor the string variable identifier.
 * Author: Hoang Do
 * Versions: 1.17.4
 * Called functions: isKeyword(), strlen(), strcspn(), strncpy().
 * Parameters: char lexeme.
 * Return value: Token currentToken.
 * Algorithm: 1. Checks if the lexeme is a keyword.
 *			   2. If it is, return the corresponded token attribute.
 *			   3. Sets the SVID token.
 *			   4. Checks if the lexeme is longer than VID_LEN.
  ************************************************************/
Token funcSVID(char lexeme[]) {
	/*************************************************************
	 * Acceptance State Function SVID
	 *		In this function, the pattern for SVID must be recognized.
	 *	- Remember to respect the limit defined for lexemes (VID_LEN),
	 *	  also observing the delimiters. Remember to end also with \0.
	 *	  set the lexeme to the corresponding attribute (vidLexeme).
	 *  - Suggestion: Use "strncpy" function.
	 ************************************************************/
	Token currentToken = { 0 };
	int length = strlen(lexeme);
	int i;
	/*SVID must start and end with $*/
	/* To check if the lexeme is a keyword*/
		/* 1. Test if the lexeme is a keyword (use the isKeyword function)*/
		/* 2. If it is, set the code to KW_T*/
		/* 3. At the same time, the attribute codType must be the index of this keyword*/
	/* 4. Else, adjust the code to AVID_T*/
	if (isKeyword(lexeme) != RT_FAIL_1) {
		currentToken.code = KW_T;
		currentToken.attribute.keywordIndex = isKeyword(lexeme);
		return currentToken;
	}
	/* 5, Copy this content to "vidLexeme" and end it with \0*/
	currentToken.code = SVID_T;

	/* If the length is longer than VID_LENGTH*/
	if (length > VID_LEN) {
		currentToken.attribute.vidLexeme[0] = '$';

		/*need check*/
		/* to store the char into var string arr*/
		for (i = 0; i < VID_LEN - 2; i++) {
			currentToken.attribute.vidLexeme[i + 1] = lexeme[i];
		}
		currentToken.attribute.vidLexeme[VID_LEN - 1] = '$';
		currentToken.attribute.vidLexeme[VID_LEN] = CHARSEOF0;
		return currentToken;

	}
	
	strncpy(currentToken.attribute.vidLexeme, lexeme, length);
	//currentToken.attribute.errLexeme[length] = '\0';
	
	return currentToken;
}

/*
* Purpose: to accept function for the integer literal(IL) - decimal constant (DIL)
* Author: Hoang Do
* Versions:
* Called Functions: strlen(), strcpy(), atolh()
* Parameters: char lexeme[]
* Return value: Token currentToken
* Algorithm: 1. check if the range of lexeme is 2-byte integer
2. check if the length of character is valid or not
3. set token token and attribute in different situation
*/
Token funcIL(char lexeme[]) {
	Token currentToken = { 0 };
	/*************************************************************
	 * Acceptance State Function IL
	 *		Function responsible to identify IL (integer literals).
	 * - It is necessary respect the limit (ex: 2-byte integer in C).
	 * - In the case of larger lexemes, error shoul be returned.
	 * - Only first ERR_LEN characters are accepted and eventually,
	 *   additional three dots (...) should be put in the output.
	 ************************************************************/
	long num = atol(lexeme);
	int i;

	//printf("Hello im here14========================\n");

	/* convers a dec const to a dec int val*/
	/* It is necessary respect the limit (ex: 2-byte integer in C).*/
	if (num > SHRT_MAX || num < SHRT_MIN) {
		/*Set token to error*/
		currentToken.code = ERR_T;
		if (strlen(lexeme) > ERR_LEN) {
			for (i = 0; i < ERR_LEN - 3; i++)
				currentToken.attribute.errLexeme[i] = lexeme[i];
			/*Add 3 dots and SEOF to the end*/
			strcat(currentToken.attribute.errLexeme, "...");
		}
		/*If the error is shorter*/
		else {
			strncpy(currentToken.attribute.errLexeme, lexeme, strlen(lexeme));
		}
		return currentToken;
	}

	/*Set the token*/
	currentToken.code = INL_T;
	currentToken.attribute.intValue = (short)num;

	return currentToken;
}

/*
* Purpose: accept function for the floating-point literal (FPL)
* Author: Hoang Do
* Versions:
* Called Functions: strlen(), strcpy()
* Parameters: char lexeme
* Return value: Token currentToken
* Algorithm: 1. check the range of float data type
2. check if the length of character is valid or not
3. set token token and attribute in different situation
*/
Token funcFPL(char lexeme[]) {
	Token currentToken = { 0 };
	/*************************************************************
	 *   Acceptance State Function FPL
	 *	 Function responsible to identify FPL (float point literals).
	 * - It is necessary respect the limit (ex: 4-byte integer in C).
	 * - In the case of larger lexemes, error shoul be returned.
	 * - Only first ERR_LEN characters are accepted and eventually,
	 *   additional three dots (...) should be put in the output.
	 ************************************************************/
	//float num = strtof(lexeme, NULL);
	float fpl = atof(lexeme);
	int length = strlen(lexeme);
	int i = 0;
	//printf("Hello im here16========================\n");

	/* the val must in the same range as the 4byte float*/
	if (fpl > FLT_MAX || (fpl < FLT_MIN && fpl != 0)) {
		/*Set token to error*/
		currentToken.code = ERR_T;
		/*If ther error lexeme islonger*/
		if (strlen(lexeme) > ERR_LEN) {
			for (i = 0; i < ERR_LEN - 3; i++)
				currentToken.attribute.errLexeme[i] = lexeme[i];
			/*Add 3 dots and SEOF to the end*/
			strcat(currentToken.attribute.errLexeme, "...");
		}
		/*If the error is shorter*/
		else
			strncpy(currentToken.attribute.errLexeme, lexeme, length);

		return currentToken;
	}

	currentToken.code = FPL_T;
	//printf("Hello im here18========================\n");

	currentToken.attribute.floatValue = (float)fpl;
	return currentToken;

}

/*************************************************************
 * Function name: funcSL.
 * Purpose: Accepts function for the string literal.
 * Author: Hoang Do, Jenni Jenni
 * Versions:
 * Called functions: strlen(), bGetAddChOffset(), bAddCh(),
 *					  bGetCh().
 * Parameters: char lexeme.
 * Return value: Token currentToken.
 * Algorithm: 1. Sets the token code.
 *			   2. Includes the literals by calling bGetAddChOffset().
 *			   3. Go through the lexeme and store it in the String
 *				  Literal Table.
  ************************************************************/
Token funcSL(char lexeme[]) {
	Token currentToken = { 0 };
	/*************************************************************
	 * Acceptance State Function SL
	 *	 Function responsible to identify SL (string literals).
	 *   The lexeme must be stored in the String Literal Table
	 *   (stringLiteralTable). You need to include the literals in
	 *   this structure, using offsets. Remember to include \0 to
	 *   separate the lexemes. Remember also to incremente the line.
	 ************************************************************/
	unsigned int i;
	currentToken.code = STR_T;
	
	currentToken.attribute.contentString = bGetAddChOffset(stringLiteralTable);
	//printf("Hello im here19========================\n");

	/* go through the string and store it*/
	for (i = 0; i < strlen(lexeme); i++) {
		/*Skip the ' opening and closing*/
		if (lexeme[i] != '\'')
			bAddCh(stringLiteralTable, lexeme[i]);
	
		/*Add the line counter if the lexeme has an line terminator*/
		if (lexeme[i] == '\n')
			line++;
	}
	/*Add '\0' at the end of the string buffer*/
	bAddCh(stringLiteralTable, CHARSEOF0);

	//printf("Hello im here20========================\n");

	return currentToken;
}

/*************************************************************
  * Function name: funcErr.
  * Purpose: Accepts function for the ERROR token.
  * Author: Hoang Do, Jenni Jenni
  * Versions:
  * Called functions: strlen(), strncpy().
  * Parameters: char lexeme.
  * Return value: Token currentToken.
  * Algorithm: 1. Sets the token code.
  *			   2. Checks if the length of the character is valid
  *				  or not.
  *			   3. Sets the token attribute in different situations.
   ************************************************************/

Token funcErr(char lexeme[]) {
	Token currentToken = { 0 };
	/*************************************************************
	 * Acceptance State Function Error
	 *		Function responsible to deal with ERR token.
	 * - This function uses the errLexeme, respecting the limit given
	 *   by ERR_LEN. If necessary, use three dots (...) to use the
	 *   limit defined. The error lexeme contains line terminators,
	 *   so remember to increment line.
	 ************************************************************/
	int length = strlen(lexeme);
	int i;

	currentToken.code = ERR_T;
	/* ERROR is longer than ERR_LEN , ERR_LEN - 3*/
	if (length > ERR_LEN) {
		for (i = 0; i < ERR_LEN - 3; i++)
			currentToken.attribute.errLexeme[i] = lexeme[i];
		/*Add 3 dots and SEOF to the end*/
		strcat(currentToken.attribute.errLexeme, "...");

	}
	else {
		/* ERROR lexeme is not longer than err_len char*/
		strncpy(currentToken.attribute.errLexeme, lexeme, length);
	}

	/*Check if the lexeme have a line terminator or not. If yes, add one to the line counter*/
	for (i = 0; i < length; i++) {
		if (lexeme[i] == '\n')
			line++;
	}
	//printf("Hello im here22========================\n");

	return currentToken;
}

/*************************************************************
 * Function name: isKeyword.
 * Purpose: Checks if one specific lexeme is a keyword.
 * Author: Hoang Do, Jenni Jenni
 * Versions:
 * Called functions: strcmp().
 * Parameters: char lexeme.
 * Return value: variable i
 * Algorithm: 1. Checks if lexeme is NULL.
 *			   2. If it is, return -1 as the value of i.
 *			   3. Compares the lexeme to the keyword in the
 *				  keyword table.
  ************************************************************/

int isKeyword(char kw_lexeme[]) {
	int i = -1;
	/*************************************************************
	 * This function checks if one specific lexeme is a keyword.
	 * - Tip: Remember to use the keywordTable to check the keywords.
	 ************************************************************/
	/*to check if lexeme is NULL*/
	//printf("Hello im here23 keyword========================\n");

	/*Loop through the kw_table[]*/
	for (i = 0; i < KWT_SIZE; i++) {
		if (strcmp(kw_lexeme, keywordTable[i]) == 0)
			return i;
	}

	//printf("Hello im here24========================\n");

	return -1;
}


/*
TODO_222: (If necessary): HERE YOU WRITE YOUR ADDITIONAL FUNCTIONS (IF ANY).
*/
