
/**************************************************************************************************************************
* File name: evalBuffer.c
* Compiler: MS Visual Studio 2019
* Author: HoangDo_040971749, JenniJenni_040961742
* Course: CST 8152 – Compilers, Lab Section: 014
* Assignment: 1
* Date: Feb 5th, 2021
* Professor: Abdullah Kadri, Paulo Sousa
* Purpose: This file contains all functions that will run the Buffer.
* Function list: mainBuffer(), bErrorPrint(), displayBuffer(), getFileSize(), isNumber()
**************************************************************************************************************************/



 /*
  * The #define _CRT_SECURE_NO_WARNINGS should be used in MS Visual Studio projects
  * to suppress the warnings about using "unsafe" functions like fopen()
  * and standard sting library functions defined in string.h.
  * The define directive does not have any effect on other compiler projects (gcc, Borland).
  */


#define _CRT_SECURE_NO_WARNINGS
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <ctype.h>

#include "buffer.h"

  /* Check for ANSI C compliancy */
#define ANSI_C 0
#if defined(__STDC__)
#undef ANSI_C
#define ANSI_C 1
#endif

/*
 *  Declaration of an error printing function with
 *  variable number of arguments
 */
void bErrorPrint(char* fmt, ...);
void displayBuffer(bStructure* ptr_Buffer);
long getFileSize(char* fname);
int isNumber(const char* ns);

/*************************************************************
* Main function
*************************************************************/

int mainBuffer(int argc, char** argv) {

	bPointer bufferPointer;		/* pointer to Buffer structure */
	FILE* fileHandler;			/* input file handle */
	int loadSize = 0;			/*the size of the file loaded in the buffer */
	int isAnsiC = !ANSI_C;		/* ANSI C flag */
	char symbol;				/*symbol read from input file */
	short size = 0, increment = 0, wrongNumber = 0;

	/* Check if the compiler option is set to compile ANSI C */
	/* __DATE__, __TIME__, __LINE__, __FILE__, __STDC__ are predefined preprocessor macros*/
	if (isAnsiC) {
		bErrorPrint("Date: %s  Time: %s", __DATE__, __TIME__);
		bErrorPrint("ERROR: Compiler is not ANSI C compliant!\n");
		exit(1);
	}

	/* missing file name or/and mode parameter */
	if (argc <= 2) {
		bErrorPrint("\nDate: %s  Time: %s", __DATE__, __TIME__);
		bErrorPrint("\nRuntime error at line %d in file %s\n", __LINE__, __FILE__);
		bErrorPrint("%s\b\b\b\b%s%s", argv[0], ": ", "Missing parameters.");
		bErrorPrint("Usage: platybt source_file_name mode");
		exit(1);
	}

	/* create source input buffer */
	switch (*argv[2]) {
	case 'f': case 'a': case 'm': break;
	default:
		bErrorPrint("%s%s%s", argv[0], ": ", "Wrong mode parameter.");
		exit(1);
	}

	/* read additional parameters, if any */
	if (argc == 5) {
		if (isNumber(argv[3]))size = (short)atoi(argv[3]); else wrongNumber = 1;
		if (isNumber(argv[4]))increment = (short)atoi(argv[4]); else wrongNumber = 1;
		if (wrongNumber) {
			bErrorPrint("\nDate: %s  Time: %s", __DATE__, __TIME__);
			bErrorPrint("\nRuntime error at line %d in file %s\n", __LINE__, __FILE__);
			bErrorPrint("%s\b\b\b\b%s%s", argv[0], ": ", "Missing or wrong number parameters.");
			bErrorPrint("Usage: platybt source_file_name mode number number");
			exit(1);
		}
	}

	/* create buffer */
	bufferPointer = bCreate(size, (char)increment, *argv[2]);

	if (bufferPointer == NULL) {
		bErrorPrint("%s%s%s", argv[0], ": ", "Cannot allocate buffer.");
		bErrorPrint("Filename: %s %s %s %s\n", argv[1], argv[2], argv[3], argv[4]);
		exit(1);
	}

	/* open source file */
	if ((fileHandler = fopen(argv[1], "r")) == NULL) {
		bErrorPrint("%s%s%s%s", argv[0], ": ", "Cannot open file: ", argv[1]);
		exit(1);
	}

	/* load source file into input buffer  */
	printf("Reading file %s ....Please wait\n", argv[1]);
	loadSize = bLoad(bufferPointer, fileHandler);

	if (loadSize == RT_FAIL_1)
		bErrorPrint("%s%s%s", argv[0], ": ", "Error in loading buffer.");

	/* if the input file has not been completely loaded, find the file size and print the last symbol loaded */
	if (loadSize == LOAD_FAIL) {
		printf("The input file %s %s\n", argv[1], "has not been completely loaded.");
		symbol = (char)fgetc(fileHandler);
		printf("Last character read from the input file is: %c %d\n", symbol, symbol);
		printf("Input file size: %ld\n", getFileSize(argv[1]));
	}

	/* close source file */
	fclose(fileHandler);

	/* display the contents of buffer */
	displayBuffer(bufferPointer);

	/*
	 * Finishes the buffer: add end of file character (EOF) to the buffer
	 * display again
	 */
	if (!bFinish(bufferPointer, EOF)) {
		bErrorPrint("%s%s%s", argv[0], ": ", "Error in compacting buffer.");
	}
	displayBuffer(bufferPointer);

	if (argc == 6 && *argv[5] == 't') {
		bErrorPrint("%s%s%s", argv[0], ": ", "Adding a symbol.");
		if (!bAddCh(bufferPointer, 'A')) {
			bErrorPrint("%s%s%s", argv[0], ": ", "Error adding a symbol.");
		}
		bErrorPrint("%s%s%s", argv[0], ": ", "Compacting buffer.");
		if (!bFinish(bufferPointer, 'B')) {
			bErrorPrint("%s%s%s", argv[0], ": ", "Error in compacting buffer.");
		}
	}

	/* free the dynamic memory used by the buffer */
	bFree(bufferPointer);
	bufferPointer = NULL;

	/*return success */
	return (0);
}

/*************************************************************
* error printing function with variable number of arguments
*************************************************************/

void bErrorPrint(char* fmt, ...) {

	/* Initialize variable list */
	va_list ap;
	va_start(ap, fmt);

	(void)vfprintf(stderr, fmt, ap);
	va_end(ap);

	/* Move to new line */
	if (strchr(fmt, '\n') == NULL)
		fprintf(stderr, "\n");
}

/*************************************************************
* Print function
*************************************************************/

void displayBuffer(bStructure* ptr_Buffer) {

	printf("\nPrinting buffer parameters:\n\n");
	printf("The capacity of the buffer is:  %d\n",
		bGetSize(ptr_Buffer));
	printf("The current size of the buffer is:  %d\n",
		bGetAddChOffset(ptr_Buffer));
	printf("The operational mode of the buffer is:   %d\n",
		bGetMode(ptr_Buffer));
	///Fixed here
	printf("The increment factor of the buffer is:  %zu\n",
		bGetIncrement(ptr_Buffer));
	printf("The first symbol in the buffer is:  %c\n",
		bGetAddChOffset(ptr_Buffer) ? *bGetContent(ptr_Buffer, 0) : ' ');
	printf("The value of the flags field is: %04hX\n",
		bGetFlags(ptr_Buffer));
	printf("\nPrinting buffer contents:\n\n");
	bRewind(ptr_Buffer);
	if (!bDisplay(ptr_Buffer, 1))
		printf("empty buffer\n");

}

/*************************************************************
* Get buffer size
*************************************************************/

long getFileSize(char* fname) {
	FILE* input;
	long flength;
	input = fopen(fname, "r");
	if (input == NULL) {
		bErrorPrint("%s%s", "Cannot open file: ", fname);
		return 0;
	}
	fseek(input, 0L, SEEK_END);
	flength = ftell(input);
	fclose(input);
	return flength;
}

/*************************************************************
 * Tests for decimal-digit character string
 * returns nonzero if ns is a number; 0 otherwise
*************************************************************/

int isNumber(const char* ns) {
	char c; int i = 0;
	if (ns == NULL) return 0;
	while ((c = ns[i++]) == 0) {
		if (!isdigit(c)) return 0;
	}
	return 1;
}
