/**************************************************************************************************************************
* File name: main.c
* Compiler: MS Visual Studio 2019
* Author: Do749_Jenni742_s14
* Course: CST 8152 – Compilers, Lab Section: 014
* Assignment: 1
* Date: Feb 6th, 2021
* Professor: Abdullah Kadri, Paulo Sousa
* Purpose: This file is the main program for assignment 1.
* Function list: bCreate(), bAddCh(), bClean(), bFree(), bIsFull(), bGetAddChOffset(), bGetSize(), bGetMode(),
*				 bGetMarkOffset(), bSetMarkOffset(), bFinish(), bDisplay(), bLoad(), bIsEmpty(), bGetCh(), bRewind(),
*				 bRetract(), bRestore(), bGetChOffset(), bGetIncrement(), bGetContent(), bufferAddCPosition(), bGetFlags().
**************************************************************************************************************************/

#define _CRT_SECURE_NO_WARNINGS
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <ctype.h>

// Function Prototype.
int mainBuffer(int, char**);
int mainScanner(int, char**);
int mainParser(int, char**);

/*****************************************************************
* Function name: main.
* Purpose: Runs the mainBuffer function for assignment 1.
* Author: Hoang Do, Jenni Jenni
* Versions: 1.2
* Called functions: mainBuffer().
* Parameters: int argc, char** argv.
* Return value: 0 for successful execution.
*****************************************************************/
int main(int argc, char** argv) {
	//mainBuffer(argc, argv);
	//mainScanner(argc, argv);
	mainParser(argc, argv);

	return 0;
}