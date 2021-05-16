/*  File name: testScanner.c
 *  Purpose:This is the main program for Assignment #2 - Scanner
 *  CST8152 - Compilers
 *  Version: 1.21
 *  Authors: Svillen Ranev - Paulo Sousa
 *  Date: 24th September 2020
 */


 /* The #define _CRT_SECURE_NO_WARNINGS should be used in MS Visual Studio projects
  * to suppress the warnings about using "unsafe" functions like fopen()
  * and standard sting library functions defined in string.h.
  * The define does not have any effect in other compiler projects.
  */
//#define _CRT_SECURE_NO_WARNINGS
//
//#include <stdio.h>
//#include <stdlib.h>
//#include <string.h>
//#include <stdarg.h>
//
//#include "buffer.h"
//#include "token.h"
//
//  /* constant definitions */
//#define INIT_SIZE 200		/* initial buffer capacity */
//#define INIT_INCREMENT 15			/* increment factor */ 
//
//#define STR_INIT_SIZE 100	/* initial string literal table capacity */
//#define STR_INCREMENT  50	/* initial string literal table capacity inc */
//
///*check for ANSI C compliancy */
//#define ANSI_C 0
//#if defined(__STDC__)
//#undef ANSI_C
//#define ANSI_C 1
//#endif
//
///* Global objects - variables */
//bPointer stringLiteralTable;	/* this buffer implements String Literal Table */
//							/* it is used as a repository for string literals */
//int errorNumber;			/* run-time error number = 0 by default (ANSI) */
//
///*external objects */
//extern int line; /* source code line numbers - defined in scanner.c */
//extern int startScanner(bStructure* sc_buf);
//extern Token tokenizer(void);
//
///*function declarations */
//void printScannerError(char* fmt, ...);
//void displayScanner(bStructure* ptrBuffer);
//long getScannerFilesize(char* fname);
//void printToken(Token t);
//
///*************************************************************
// *  main function takes a PLATYPUS source file as
// *  an argument at the command line.
// *  usage: scanner source_file_name"
//************************************************************/
//
//int mainScanner(int argc, char** argv) {
//
//	bPointer sourceBuffer;	/* pointer to input (source) buffer */
//	FILE* fileHandler;		/* input file handle */
//	Token currentToken;		/* token produced by the scanner */
//	int loadSize = 0;		/* the size of the file loaded in the buffer */
//	int isAnsiC = !ANSI_C;	/* ANSI C compliancy flag */
//
///* Check if the compiler option is set to compile ANSI C */
///* __DATE__, __TIME__, __LINE__, __FILE__, __STDC__ are predefined preprocessor macros*/
//	if (isAnsiC) {
//		printScannerError("Date: %s  Time: %s", __DATE__, __TIME__);
//		printScannerError("ERROR: Compiler is not ANSI C compliant!\n");
//		exit(1);
//	}
//
//	/*check for correct arrguments - source file name */
//	if (argc <= 1) {
//		/* __DATE__, __TIME__, __LINE__, __FILE__ are predefined preprocessor macros*/
//		printScannerError("Date: %s  Time: %s", __DATE__, __TIME__);
//		printScannerError("Runtime error at line %d in file %s", __LINE__, __FILE__);
//		printScannerError("%s%s%s", argv[0], ": ", "Missing source file name.");
//		printScannerError("%s%s%s", "Usage: ", "scanner", "  source_file_name");
//		exit(1);
//	}
//
//	/* create a source code input buffer - multiplicative mode */
//	sourceBuffer = bCreate(INIT_SIZE, INIT_INCREMENT, 'm');
//	if (sourceBuffer == NULL) {
//		printScannerError("%s%s%s", argv[0], ": ", "Could not create source buffer");
//		exit(1);
//	}
//
//	/* open source file */
//	if ((fileHandler = fopen(argv[1], "r")) == NULL) {
//		printScannerError("%s%s%s%s", argv[0], ": ", "Cannot open file: ", argv[1]);
//		exit(1);
//	}
//
//	/* load source file into input buffer  */
//	printf("Reading file %s ....Please wait\n", argv[1]);
//	loadSize = bLoad(sourceBuffer, fileHandler);
//	if (loadSize == RT_FAIL_1)
//		printScannerError("%s%s%s", argv[0], ": ", "Error in loading buffer.");
//
//	/* close source file */
//	fclose(fileHandler);
//	/* find the size of the file */
//	if (loadSize == LOAD_FAIL) {
//		printf("The input file %s %s\n", argv[1], "is not completely loaded.");
//		printf("Input file size: %ld\n", getScannerFilesize(argv[1]));
//	}
//
//	/* compact and display the source buffer */
//	/* add SEOF to input program buffer*/
//	if (bFinish(sourceBuffer, '\0')) {/* '\0' */ /* EOF */
//		displayScanner(sourceBuffer);
//	}
//
//	/* create string Literal Table */
//	stringLiteralTable = bCreate(INIT_SIZE, INIT_INCREMENT, 'a');
//	if (stringLiteralTable == NULL) {
//		printScannerError("%s%s%s", argv[0], ": ", "Could not create string literals buffer");
//		exit(1);
//	}
//
//	/* Testbed for the scanner */
//	/* add SEOF to input program buffer*/
//	/* Initialize scanner input buffer */
//	if (startScanner(sourceBuffer)) {
//		;
//		printScannerError("%s%s%s", argv[0], ": ", "Empty program buffer - scanning canceled");
//		exit(1);
//	}
//
//	printf("\nScanning source file...\n\n");
//	printf("Token\t\tAttribute\n");
//	printf("----------------------------------\n");
//	do {
//		currentToken = tokenizer();
//		printToken(currentToken);
//	} while (currentToken.code != SEOF_T);
//
//	/* print String Literal Table if not empty */
//	if (bGetAddChOffset(stringLiteralTable)) bDisplay(stringLiteralTable, 1);
//	bFree(sourceBuffer);
//	bFree(stringLiteralTable);
//	sourceBuffer = stringLiteralTable = NULL;
//
//	/* Ass2 evaluation only */
//	if (argv[2] != NULL && *argv[2] == 'l')
//		printf("The number of lines is: %d\n", line);
//
//	return (0);
//}
//
///*************************************************************
// *  Error printing function with variable number of arguments
//************************************************************/
//
//void printScannerError(char* fmt, ...) {
//	va_list ap;
//	va_start(ap, fmt);
//	(void)vfprintf(stderr, fmt, ap);
//	va_end(ap);
//	/* Move to new line */
//	if (strchr(fmt, '\n') == NULL)
//		fprintf(stderr, "\n");
//}
//
///*************************************************************
// * The function displays buffer contents
// ************************************************************/
//
//void displayScanner(bStructure* ptrBuffer) {
//	printf("\nPrinting buffer parameters:\n\n");
//	printf("The capacity of the buffer is:  %d\n", bGetSize(ptrBuffer));
//	printf("The current size of the buffer is:  %d\n", bGetAddChOffset(ptrBuffer));
//	printf("\nPrinting buffer contents:\n\n");
//	bRewind(ptrBuffer);
//	bDisplay(ptrBuffer, 1);
//
//}
//
///*************************************************************
// * The function gets size of scanner file
// ************************************************************/
//
//long getScannerFilesize(char* fname) {
//	FILE* fileInput;
//	long fileLength;
//	fileInput = fopen(fname, "r");
//	if (fileInput == NULL) {
//		printScannerError("%s%s", "Cannot open file: ", fname);
//		return 0L;
//	}
//	fseek(fileInput, 0L, SEEK_END);
//	fileLength = ftell(fileInput);
//	fclose(fileInput);
//	return fileLength;
//}

/*************************************************************
 * The function prints the token returned by the scanner
 ************************************************************/

//void printToken(Token t) {
//	extern char* keywordTable[]; /* link to keyword table in */
//	switch (t.code) {
//	case RTE_T:
//		printf("RTE_T\t\t%s", t.attribute.errLexeme);
//		/* Call here run-time error handling component */
//		if (errorNumber) {
//			printf("%d", errorNumber);
//			exit(errorNumber);
//		}printf("\n");
//		break;
//	case ERR_T:
//		printf("ERR_T\t\t%s\n", t.attribute.errLexeme);
//		break;
//	case SEOF_T:
//		printf("SEOF_T\t\t%d\t\n", t.attribute.seofType);
//		break;
//	case AVID_T:
//		printf("AVID_T\t\t%s\n", t.attribute.vidLexeme);
//		break;
//	case SVID_T:
//		printf("SVID_T\t\t%s\n", t.attribute.vidLexeme);
//		break;
//	case FPL_T:
//		printf("FPL_T\t\t%f\n", t.attribute.floatValue);
//		break;
//	case INL_T:
//		printf("INL_T\t\t%d\n", t.attribute.codeType);
//		break;
//	case STR_T:
//		printf("STR_T\t\t%d\t ", (short)t.attribute.codeType);
//		printf("%s\n", bGetContent(stringLiteralTable,
//			(short)t.attribute.codeType));
//		break;
//	case SCC_OP_T:
//		printf("SCC_OP_T\n");
//		break;
//	case ASS_OP_T:
//		printf("ASS_OP_T\n");
//		break;
//	case ART_OP_T:
//		printf("ART_OP_T\t%d\n", t.attribute.codeType);
//		break;
//	case REL_OP_T:
//		printf("REL_OP_T\t%d\n", t.attribute.codeType);
//		break;
//	case LOG_OP_T:
//		printf("LOG_OP_T\t%d\n", t.attribute.codeType);
//		break;
//	case LPR_T:
//		printf("LPR_T\n");
//		break;
//	case RPR_T:
//		printf("RPR_T\n");
//		break;
//	case LBR_T:
//		printf("LBR_T\n");
//		break;
//	case RBR_T:
//		printf("RBR_T\n");
//		break;
//	case KW_T:
//		printf("KW_T\t\t%s\n", keywordTable[t.attribute.codeType]);
//		break;
//	case COM_T:
//		printf("COM_T\n");
//		break;
//	case EOS_T:
//		printf("EOS_T\n");
//		break;
//	default:
//		printf("Scanner error: invalid token code: %d\n", t.code);
//	}
//}
