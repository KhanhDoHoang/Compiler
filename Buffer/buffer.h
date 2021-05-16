/**************************************************************************************************************************
* File name: buffer.c
* Compiler: MS Visual Studio 2019
* Author: HoangDo_040971749, JenniJenni_040961742
* Course: CST 8152 – Compilers, Lab Section: 014
* Assignment: 1
* Date: Feb 5th, 2021
* Professor: Abdullah Kadri, Paulo Sousa
* Purpose: This file contains all functions that will be used for The Buffer(A1).
* Function list: bCreate(), bAddCh(), bClean(), bFree(), bIsFull(), bGetAddChOffset(), bGetSize(), bGetMode(),
*				 bGetMarkOffset(), bSetMarkOffset(), bFinish(), bDisplay(), bLoad(), bIsEmpty(), bGetCh(), bRewind(),
*				 bRetract(), bRestore(), bGetChOffset(), bGetIncrement(), bGetContent(), bufferAddCPosition(), bGetFlags().
**************************************************************************************************************************/

#ifndef BUFFER_H_
#define BUFFER_H_

/* standard header files */
#include <stdio.h>  /* standard input/output */
#include <malloc.h> /* for dynamic memory allocation*/
#include <limits.h> /* implementation-defined data type ranges and limits */

/* constant definitions */
#ifndef TRUE 
#define TRUE 1
#endif

#ifndef FALSE 
#define FALSE 0
#endif

#define RT_FAIL_1 (-1)			/*fail return value*/
#define RT_FAIL_2 (-2)			/*fail return value*/
#define LOAD_FAIL (-2)			/*load fail error*/

#define DEFAULT_SIZE (200)        /*200*/
#define DEFAULT_INCREMENT (15)    /*15*/

/* You should add your own constant definitions here */
#define MAX_SIZE SHRT_MAX-1   /* maximum capacity*/ 

/* Buffer Modes */
#define FIXMODE 0         /*Fixed OP mode*/
#define ADDMODE 1         /*Additive op mode*/
#define MULMODE -1         /*Multiplicative op mode*/

/* Add your bit-masks constant definitions here */
#define DEFAULT_FLAGS 0x3FFF 	/* 0011.1111 1111.1111 */
#define SET_EOB 0x8000			/* 1000 0000 0000 0000 	/* set end of buffer flag */
#define RESET_EOB 0x7FFF		/* 0111 1111 1111 1111		/* reset end-of-buffer flag*/
#define CHECK_EOB 0x8000		        /* this is checking 1st bit*/

/*The rest of the bits are resetting eob_flagor r_flagreserved
for further use and
must be set by default to1.When setting or
they must not be changed by the bitwise operation manipulating bit 0and */
#define SET_R_FLAG 0x4000			/* 0100 0000 0000 0000 /* realloc flag set value  10*/
#define RESET_R_FLAG 0xBFFF			/* 1011 1111 1111 1111 */
#define CHECK_R_FLAG 0x4000 		    /* this is checking 2nd bit*/

/* Constants used in buffer */
#define MAXINCREMENT 100 		/*Arcoding to the max value in MULT mode*/
#define MAX_VALUE SHRT_MAX-1 			/* maximum allowed positive value*/
#define RT_INC_FAIL 0x100		/*256 /* If a run-time error is possible, thefunction shouldreturna special value*/

#define FIX_INC_FACTOR 0        /* fixed increment factor constant */
#define MIN_INC_FACTOR 1        /* minimum additive increment factor constant */
#define MAX_INC_FACTOR 255        /* max additive increment factor constant */
/*#define MAX_MUL_INC_FACTOR 0x64  /* maximum multiplicative increment factor constant */

/* user data type declarations */
typedef struct Buffer {
	char* content;         /* pointer to the beginning of character array (character buffer) */
	short size;            /* current dynamic memory size (in bytes) allocated to  buffer */
	char  increment;       /* character array increment factor */
	char  mode;            /* operational mode indicator*/
	short addCOffset;      /* the offset (in chars) to the add-character location */
	short getCOffset;      /* the offset (in chars) to the get-character location */
	short markOffset;      /* the offset (in chars) to the mark location */
	unsigned short flags;  /* contains character array reallocation and end-of-buffer flag */
} bStructure, * bPointer;

/* Function declarations */
bPointer bCreate(short size, char increment, char mode);
bPointer bAddCh(bPointer const pBuffer, char ch);
int bClean(bPointer const pBuffer);
int bFree(bPointer const pBuffer);
int bIsFull(bPointer const pBuffer);
short bGetAddChOffset(bPointer const pBuffer);
short bGetSize(bPointer const pBuffer);
int bGetMode(bPointer const pBuffer);
short bGetMarkOffset(bPointer const pBuffer);
short bSetMarkOffset(bPointer const pBuffer, short mark);
bPointer bFinish(bPointer const pBuffer, char ch);
int bDisplay(bPointer const pBuffer, char nl);
int bLoad(bPointer const pBuffer, FILE* const fi);
int bIsEmpty(bPointer const pBuffer);
char bGetCh(bPointer const pBuffer);
int bRewind(bPointer const pBuffer);
int bRewind(bPointer const pBuffer);
bPointer bRetract(bPointer const pBuffer);
short bRestore(bPointer const pBuffer);
short bGetChOffset(bPointer const pBuffer);
size_t bGetIncrement(bPointer const pBuffer);
char* bGetContent(bPointer const pBuffer, short pos);
short bufferAddCPosition(bPointer const pBuffer);
unsigned short bGetFlags(bPointer const pBuffer);

#endif
