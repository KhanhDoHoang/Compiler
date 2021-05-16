/*************************************************************
* COMPILERS COURSE - Algonquin College
* Code version: Fall, 2020
* This Code is EXCLUSIVE for professors (must not be shared)
* Author: Svillen Ranev - Paulo Sousa - Abdulah.
*************************************************************
* File name: buffer.c
* Compiler: MS Visual Studio 2019
* Author: Hoang Do
* Course: CST 8152 – Compilers, Lab Section: [011, 012, 013, 014]
* Assignment: A1.
* Date: Jan 01 2021
* Professor: Paulo Sousa / Abdulah
* Purpose: This file is the main code for Buffer (A1)
* Called functions: mainBuffer()
* Return value: Status
* Algorithm: -
*************************************************************/

#define _CRT_SECURE_NO_WARNINGS 
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <ctype.h>

int mainParser(int argc, char** argv);

int main(int argc, char** argv) {
	char debug = 0;
	int i;
	if (debug) {
		for (i = 0; i < argc; ++i) {
			fprintf(stderr, "argv[%d] = %s\n", i, argv[0]);
		}
	}


	return mainParser(argc, argv);
}