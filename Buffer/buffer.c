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


#include "buffer.h"

/**************************************************************************************************************************
* Function name: bCreate.
* Purpose: Creates a new buffer in memory (on the program heap).
* Author: Hoang Do, Jenni Jenni
* Versions: 1.1.1
* Called functions: calloc(), malloc(), free().
* Parameters: short size, char increment, char mode.
* Return value: b, NULL.
* Algorithm: 1. Allocates the memory for one buffer.
*			 2. Allocates the memory for one dynamic character buffer.
*			 3. Sets the buffer operational mode indicator mode.
**************************************************************************************************************************/
bPointer bCreate(short size, char increment, char mode) {
	bPointer b = NULL;
	char tempMode;
	/* check the range of the size */
	if (size < 0 || size > MAX_SIZE) { //Max value is -1
		return NULL;
	}
	if (size == 0) {
		size = DEFAULT_SIZE; //200
		increment = DEFAULT_INCREMENT; //15
	}
	/* set the buffer operational mode indicator mode */
	switch (mode)
	{
	case 'f':
		tempMode = FIXMODE;
		increment = 0;
		break;
	case 'm':
		tempMode = MULMODE;
		break;
	case 'a':
		tempMode = ADDMODE;
		break;
	default:
		return NULL;
	}
	b = (bPointer)calloc(1, sizeof(bStructure));
	if (!b) {
		return NULL;
	}
	b->content = (char*)malloc(size);
	if (!b->content) {
		bFree(b);
		return NULL;
	}

	/*---Update the remaining variables---*/
	b->mode = tempMode;
	b->size = size;
	b->increment = increment;
	b->flags = DEFAULT_FLAGS; /* 0011.1111 1111.1111 */
	return b;
}

 
/**************************************************************************************************************************
* Function name: bAddCh.
* Purpose: Resets R_FLAG to 0 and adds the character symbol to the character array of the given buffer pointed by pBD.
* Author: Hoang Do
* Versions: 1.1.1
* Called functions: realloc().
* Parameters: bPointer const pBuffer, char ch.
* Return value: pBuffer, NULL.
* Algorithm: 1. Checks whether the buffer is operational and not full, then add the character to the buffer.
*			 2. Checks whether the buffer is full, if yes resize the buffer.
*			 3. Checks the operational mode, function returns different value operational mode indicator mode.
**************************************************************************************************************************/
bPointer bAddCh(bPointer const pBuffer, char ch) {
	short available_space, new_inc, new_capacity = 0;
	/* variables needed to check the memory location when reacllocation */

	char* newContent;

	/* validation of the function arguments*/
	if (!pBuffer) {
		return NULL;
	}
	/*Reset r_flag bit to 0*/
	pBuffer->flags &= RESET_R_FLAG;
	/* 0011.1111 1111.1111 */
	/* 1011 1111 1111 1111  ==>  0011.1111.1111.1111 :3FFF*/

	if (pBuffer->addCOffset == MAX_SIZE) {
		return NULL;
	}

	/*Using bIsFull*/
	if (bIsFull(pBuffer)) {

		if (pBuffer->mode == FIXMODE) {
			return NULL;
		}
		if (pBuffer->mode == ADDMODE) {
			/* If the result is positive, add increment*/
			new_capacity = pBuffer->size + bGetIncrement(pBuffer);

			//printf("Here I am the addCh and this is new size, old one and increment: %d   %d   %d\n", new_capacity, pBuffer->size, pBuffer->increment);

			if (new_capacity < 0 || new_capacity > MAX_VALUE) {
				return NULL;
			}
			/* If the result is equal to max positive value, assigns -1 to the new capacity */
			else if (new_capacity == SHRT_MAX) {
				new_capacity = MAX_VALUE;
			}
			pBuffer->size = new_capacity;
		}
		if (pBuffer->mode == MULMODE) {
			/*new_capacity = pBuffer->size;*/
			if (pBuffer->size == MAX_VALUE) {
				return NULL;
			}
			available_space = MAX_SIZE - pBuffer->size;
			new_inc = (short int)(available_space * ((pBuffer->increment) / 100.0));   
			new_capacity = pBuffer->size + new_inc;

			if (new_capacity < 0 || new_capacity > MAX_SIZE) {
				return NULL;
			}
			/* Check if the new capacity has incremented successfully */
			if (new_inc == 0) {
				if (new_capacity < MAX_VALUE) {
					new_capacity = MAX_VALUE;
				}
				else {
					return NULL;
				}
			}
			else {
				pBuffer->size = new_capacity;
			}
		}
		newContent = (char*)realloc(pBuffer->content, new_capacity);
		if (!newContent) {
			return NULL;
		}
		if (newContent != pBuffer->content) {
			//fprintf(stderr, "Flag realocated on size = %d to new size = %d\n", pBuffer->size, new_capacity);
			pBuffer->flags |= SET_R_FLAG;
			/*0011.1111.1111.1111 & 0100 0000 0000 0000*/
								  /*===>0111.1111.1111.1111 : 7FFF*/
		}
		pBuffer->content = newContent;
		pBuffer->size = (short)new_capacity;
	}
	
	/* char stored */
	pBuffer->content[pBuffer->addCOffset++] = ch;

	return pBuffer;
}

/**************************************************************************************************************************
* Function name: bClean.
* Purpose: Retains the currently memory space allocated to the buffer, but re-initializes all appropiate data members of
*		   the given Buffer.
* Author: Hoang Do
* Versions: 1.17.2
* Called functions: none.
* Parameters: bPointer const pBuffer.
* Return value: 1(TRUE), -1(R_FAIL).
* Algorithm: Resets everything in the buffer.
**************************************************************************************************************************/
int bClean(bPointer const pBuffer) {
	if (!pBuffer) {
		return RT_FAIL_1;
	}

	/* Re-initialize the data members of buffer */
	pBuffer->addCOffset = 0; /* Offset reset*/
	pBuffer->getCOffset = 0;
	pBuffer->markOffset = 0;
	pBuffer->flags = DEFAULT_FLAGS;
	

	return 0;
}

/**************************************************************************************************************************
* Function name: bFree.
* Purpose: De-allocates (frees) the memory occupied by the character buffer and the Buffer.
* Author: Hoang Do
* Versions: 1.2
* Called functions: free().
* Parameters: bPointer const pBuffer.
* Return value: none.
* Algorithm: De-allocates the memory occupied by the character buffer.
**************************************************************************************************************************/
int bFree(bPointer const pBuffer) {
	if (!pBuffer) {
		return RT_FAIL_1;
	}
	/* De-allocation the memory*/
		free(pBuffer->content);
		free(pBuffer);
	
	return 0;
}

/**************************************************************************************************************************
* Function name: bIsFull.
* Purpose: Returns 1 or 0 depends on the buffer's size.
* Author: Hoang Do, Jenni Jenni
* Versions: 1.17.2
* Called functions: none.
* Parameters: bPointer const pBuffer.
* Return value: -1(RT_FAIL_1), 0(FALSE), 1(TRUE).
* Algorithm: 1. Checks whether the character buffer is full or not.
*			 2. Checks whether the run-time is possible or not.
**************************************************************************************************************************/
int bIsFull(bPointer const pBuffer) {
	/* If run-time error, returns -1 */
	if (!pBuffer) {
		return RT_FAIL_1;
	}

	/*Check the char buffer is full yet*/
	if (pBuffer->addCOffset == bGetSize(pBuffer)) {
		return TRUE;
	}
	else {
		return FALSE;
	}
}

/**************************************************************************************************************************
* Function name: bGetAddChOffset.
* Purpose: Checks whether the Buffer is full or not.
* Author: Jenni Jenni
* Versions: 1.17.2
* Called functions: none.
* Parameters: bPointer const pBuffer.
* Return value: -1(RT_FAIL_1), 0(FALSE), 1(TRUE).
* Algorithm: 1. Checks whether the character buffer is full or not.
*			 2. Checks whether the run-time is possible or not.
**************************************************************************************************************************/
short bGetAddChOffset(bPointer const pBuffer) {
	if (!pBuffer) {
		return RT_FAIL_1;
	}

	/* The function returns the current addCOffset*/
	return pBuffer->addCOffset;

}

/**************************************************************************************************************************
* Function name: bGetSize.
* Purpose: Finds the current size of the character buffer.
* Author: Jenni Jenni
* Versions: 1.17.2
* Called functions: none.
* Parameters: bPointer const pBuffer.
* Return value: -1(RT_FAIL_1), pBuffer size.
* Algorithm: 1. check the buffer size
**************************************************************************************************************************/
short bGetSize(bPointer const pBuffer) {
	if (!pBuffer) {
		return RT_FAIL_1;
	}

	/* The function returns the current size*/
	return pBuffer->size;
}

/************************************************************
* Function name: bGetMode.
* Purpose: Returns the value of mode to the calling function.
* Author: Jenni Jenni
* Versions: 1.17.2
* Called functions: none.
* Parameters: bPointer const pBuffer.
* Return value: -2(RT_FAIL_2), pBuffer mode.
* Algorithm: 1. check the buffer mode
**************************************************************/
int bGetMode(bPointer const pBuffer) {
	if (!pBuffer) {
		return RT_FAIL_2; /* Run time error*/
	}

	/* The function returns the current mode*/
	return pBuffer->mode;
}

/************************************************************
* Function name: bGetMarkOffset.
* Purpose: Returns the value of markOffset to the calling function.
* Author: Jenni Jenni
* Versions: 1.17.2
* Called functions: none.
* Parameters: bPointer const pBuffer.
* Return value: -1(RT_FAIL_1), pBuffer markOffset.
* Algorithm: Checks whether the run-time is possible to happen or not.
**************************************************************/
short bGetMarkOffset(bPointer const pBuffer) {
	if (!pBuffer) {
		return RT_FAIL_1;
	}

	/* The function returns the current markOffset*/
	return pBuffer->markOffset;
}

/************************************************************
* Function name: bSetMarkOffset.
* Purpose: Sets variable mark to markOffset.
* Author: Hoang Do, Jenni Jenni
* Versions: 1.17.3
* Called functions: none.
* Parameters: bPointer const pBuffer, short mark.
* Return value: -1(RT_FAIL_1), short mark.
* Algorithm: 1. Checks whether the run-time error is possible to happen or not.
*			 2. Checks whether the mark is within 0 to addCOffset value.
*			 3. Sets the value of mark to be the value of markOffset.
**************************************************************/
short bSetMarkOffset(bPointer const pBuffer, short mark) {
	/* Run-time err*/
	if (!pBuffer) {
		return RT_FAIL_1;
	}

	/* Check if rhe mark must be witin the current limit of the buffer
	 * 0 to addCOffset inclusive
	 */
	if (mark < 0 || mark > pBuffer->addCOffset) {
		return RT_FAIL_1;
	}
	pBuffer->markOffset = mark;
	return pBuffer->markOffset;
}

/**************************************************************************************************************************
* Function name: bFinish.
* Purpose: For all operational mode, this function shrinks the buffer to new_capacity.
* Author: Hoang Do, Jenni Jenni
* Versions:
* Called functions: realloc().
* Parameters: bPointer const pBuffer, char ch.
* Return value: NULL.
* Algorithm: 1. Defines and adjusts the memory of the new capacity.
*			 2. Updates all necessary members of the buffer.
*			 3. Add the symbol to the end of the buffer.
*			 4. Sets the buffer R_Flag appropriately.
**************************************************************************************************************************/
bPointer bFinish(bPointer const pBuffer, char ch) {
	short new_capacity = 0;
	/* variables needed to check the memory location when reacllocation */
	char* newContent;
	char* oldContent;

	if (!pBuffer) {
		return NULL;
	}
	pBuffer->flags &= RESET_R_FLAG;
	//printf("here I am in the bFinish: 1st flag: %d\n", pBuffer->flags);
	/* 1011.1111.1111.1111 BFFF */
	new_capacity = pBuffer->addCOffset + 1;

	/* underflow, new size will return null*/
	if (new_capacity <= 0) {
		return NULL;
	}
	oldContent = pBuffer->content;
	/* Use realloc to adjust new size * sizeof(char) */
	newContent = (char*)realloc(pBuffer->content, new_capacity);

	/* Fails */
	if (!newContent) {
		return NULL;
	}

	if (newContent != pBuffer->content) {
		pBuffer->flags |= SET_R_FLAG;
		/*0011.1111.1111.1111 & 0100 0000 0000 0000*/
							  /*===>0111.1111.1111.1111 : 7FFF*/
	}
	/*Update others*/
	pBuffer->content = newContent;
	pBuffer->size = new_capacity;

	//printf("here I am in the bFinish: 1st flag: %d\n", pBuffer->flags);
	/* 1011 1111 1111 1111 BFFF */
	pBuffer->content[pBuffer->addCOffset++] = ch;
	
	return pBuffer;
}

/**************************************************************************************************************************
* Function name: bDisplay.
* Purpose: Prints the content of the buffer (in content field).
* Author:
* Versions:
* Called functions: realloc(), bgetCh(), bIsEmpty().
* Parameters: bPointer const pBuffer, char nl.
* Return value: NULL.
* Algorithm: 1. Checks whether the buffer is empty or not.
*			 2. Prints the content of the buffer character by character.
*			 3. Detects the end of the buffer content.
*			 4. Prints a new line character.
**************************************************************************************************************************/
int bDisplay(bPointer const pBuffer, char nl) {
	/* catch the character counter */
	short count = 0;
	char ch;
	/* How to detect end of buffer */
	/* Run-time err --> RT_FAIL_1*/
	if (!pBuffer || !pBuffer->content) {
		return RT_FAIL_1;
	}
	/* Check if the buffer empty*/
	if (bIsEmpty(pBuffer) == TRUE) {
		return 0;
	}

	/*1011.1111.1111.1111 (1)
	0011.1111.1111.1111 (2)
	*/
	
	/*printf("Hello i m in display \n");*/
	ch = bGetCh(pBuffer);
	while (!(pBuffer->flags & CHECK_EOB)) {
		printf("%c", ch);
		count++;
		ch = bGetCh(pBuffer);
	}

	if (nl != 0) {
		printf("\n");
	}

	/*pBuffer->flags &= RESET_R_FLAG;*/

	return count;
}

/************************************************************
* Function name: bLoad.
* Purpose: Loads / reads an open input file specified by fi into a buffer specified by pBuffer.
* Author: Hoang Do, Jenni Jenni
* Versions:
* Called functions: bAddCh(), fgetc(), ungetc().
* Parameters: bPointer const pBuffer, FILE* const fi.
* Return value: -1(RT_FAIL_1), int count.
* Algorithm: 1. Reads an open input file into a buffer.
*			 2. Reads a character one at a time and adds the character to the buffer.**************************************************************/
int bLoad(bPointer const pBuffer, FILE* const fi) {
	char ch;
	int count = 0;
	/* If run-time error, returns -1 */
	if (!pBuffer || !fi) {
		return RT_FAIL_1;
	}

	/* Read one character at a time and bAddCh() to add the char to buffer*/
	ch = (char)fgetc(fi);
	while (!feof(fi)) {
		/*cant be put in the buffer*/
		if (!bAddCh(pBuffer, ch)) {
			ungetc(ch, fi);
			return LOAD_FAIL;
		}
		ch = (char)fgetc(fi);
		count++;
	}

	return count;
}

/************************************************************
* Function name: bIsEmpty.
* Purpose: Returns 0 or 1 depends on the value of variable addCOffset.
* Author: Jenni Jenni
* Versions: 1.17.2
* Called functions: none.
* Parameters: bPointer const pBuffer.
* Return value: -1(RT_FAIL_1), 1, 0.
* Algorithm: 1. check the value of addCOffset 
*            2. check the run-time error
**************************************************************/
int bIsEmpty(bPointer const pBuffer) {
	if (!pBuffer) {
		return RT_FAIL_1;
	}

	if (pBuffer->addCOffset == 0) {
		return 1;
	}

	return 0;
}


/************************************************************
* Function name: bGetCh.
* Purpose: Reads the buffer.
* Author: Hoang Do
* Versions:
* Called functions: none.
* Parameters: bPointer const pBuffer.
* Return value: -1(RT_FAIL_1), 0, char ch.
* Algorithm: 1. checks the argument for validity (possible run-time error).
             2. check if getcoffset and addcoffset are equal
			 3. returns the character located at getcoffset
**************************************************************/
char bGetCh(bPointer const pBuffer) {
	char ch;
	
	if (!pBuffer) {
		return RT_FAIL_1;
	}
	/*pBuffer->flags &= RESET_R_FLAG;*/
	/*1011 1111 1111 1111 &  0011.1111 1111.1111 -->0011.1111.1111.1111*/

	/*TODO: when trying to get the char, CHECK if it's the last*/
	/*To adjust eob flag*/

	if (pBuffer->getCOffset == pBuffer->addCOffset) {
		pBuffer->flags |= SET_EOB;
		/*0011.1111.1111.1111 | 1000 0000 0000 0000 --> 1011.1111.1111.1111*/
		return 0;
	}
	else {
		pBuffer->flags &= RESET_EOB;
		/*1000 0000 0000 0000 : set_eob*/
		/*0011.1111.1111.1111 & 0111 1111 1111 1111 :reset eob --> 0011.1111.1111.1111*/
	}

	ch = pBuffer->content[pBuffer->getCOffset++];


	return ch;
}


/************************************************************
* Function name: bRewind.
* Purpose: Sets getCOffset and markCOffset to 0 to be able to re-read the buffer.
* Author: Jenni Jenni.
* Versions: 1.17.2
* Called functions: none.
* Parameters: bPointer const pBuffer.
* Return value: -1(RT_FAIL_1), 0.
* Algorithm: 1. Checks whether the run-time error is possible to happen or not.
*			 2. Sets both getCOffset and markCOffset to 0.
**************************************************************/
int bRewind(bPointer const pBuffer) {
	if (!pBuffer) {
		return RT_FAIL_1;
	}
	pBuffer->getCOffset = 0;
	pBuffer->markOffset = 0;
	return 0;
}


/************************************************************
* Function name: bRetract.
* Purpose: Decrements getCOffset by 1 and returns the buffer.
* Author: Hoang Do
* Versions: 1.17.2
* Called functions: none.
* Parameters: bPointer const pBuffer.
* Return value: NULL, pBuffer getCOffset.
* Algorithm: 1. Decrements the value of getCOffset by 1.
*			 2. Returns he buffer after decrementation.
**************************************************************/
bPointer bRetract(bPointer const pBuffer) {
	if (!pBuffer) {
		return NULL;
	}
	else if (pBuffer->getCOffset > 0) {
		pBuffer->getCOffset -= 1;
	}
	return pBuffer;
}

/************************************************************
* Function name: bRestore.
* Purpose: Sets getCOffset to the value of the current markOffset.
* Author: Hoang Do, Jenni Jenni
* Versions: 1.17.2
* Called functions: none.
* Parameters: bPointer const pBuffer.
* Return value: -1(RT_FAIL_1), pBuffer getCOffset.
* Algorithm: 1. set the value of getcoffset to current markcoffset
             2. check the run-time error
**************************************************************/
short bRestore(bPointer const pBuffer) {
	if (!pBuffer || pBuffer->markOffset < 0 || pBuffer->markOffset > pBuffer->size) {
		return RT_FAIL_1;
	}
	pBuffer->getCOffset = pBuffer->markOffset;

	return pBuffer->getCOffset;
}

/************************************************************
* Function name: bGetChOffset.
* Purpose: Returns getCOffset to the calling function.
* Author: Jenni Jenni
* Versions: 1.1.0
* Called functions: none.
* Parameters: bPointer const pBuffer.
* Return value: -1(RT_FAIL_1), pBuffer getCOffset.
* Algorithm: 1. returns getcoffset to the calling function
*            2. check the run-time error
**************************************************************/
short bGetChOffset(bPointer const pBuffer) {
	if (!pBuffer) {
		return RT_FAIL_1;
	}

	return pBuffer->getCOffset;
}


/************************************************************
* Function name: bGetIncrement.
* Purpose: Returns the non-negative value of increment to the calling function.
* Author: Hoang Do
* Versions: 1.17.2
* Called functions: none.
* Parameters: bPointer const pBuffer.
* Return value: 0x100(RT_INC_FAIL), pBuffer increment.
* Algorithm: 1. return the value of increment
             2. check the run-time error is possible or not
**************************************************************/
size_t bGetIncrement(bPointer const pBuffer) {
	if (!pBuffer) {
		return RT_INC_FAIL; //0x100
	}
	return (size_t)((unsigned char)pBuffer->increment);
}


/************************************************************
* Function name: bGetContent.
* Purpose: Returns a pointer to the location of the character buffer.
* Author: Hoang Do
* Versions: 1.17.2
* Called functions: none.
* Parameters: bPointer const pBuffer.
* Return value: NULL, (pBuffer content) + chPosition.
* Algorithm: Checks whether the run-time error is possible to happen or not.
**************************************************************/
char* bGetContent(bPointer const pBuffer, short chPosition) {
	if (!pBuffer) {
		return NULL;
	}
	/*
	 *The function returns a pointer to the location of the character buffer
	 indicated by pos that is the distance
	 (measured in chars) from the beginning of the character array(content)
	 */
	if (chPosition < 0 || chPosition >= pBuffer->addCOffset) {
		return NULL;
	}
	return pBuffer->content + chPosition;
}


/************************************************************
* Function name: bufferAddCPosition.
* Purpose: Returns getCOffset to the calling function.
* Author: Jenni Jenni
* Versions: 1.1.0
* Called functions: none.
* Parameters: bStructure* const pBuffer.
* Return value: -1(RT_FAIL_1), pBuffer getCOffset.
* Algorithm: Checks whether the run-time error is possible to happen or not.**************************************************************/
short bufferAddCPosition(bStructure* const pBuffer) {
	if (!pBuffer) {
		return RT_FAIL_1;
	}

	return pBuffer->getCOffset;
}


/************************************************************
* Function name: bGetFlags.
* Purpose: Returns the flag field from the buffer.
* Author: Hoang Do
* History/Versions: Jan 29, 2021 / Version 1.0
* Called functions: none.
* Parameters: bPointer const.
* Return value: unsigned short.
* Algorithm: 1. returns flags to the calling function
             2. check the run-time error
**************************************************************/
#define FLAGS_
#undef FLAGS_
#ifndef FLAGS_
unsigned short bGetFlags(bPointer const pBuffer) {
	if (!pBuffer) {
		return (unsigned short)RT_FAIL_1;
	}

	return (unsigned short)pBuffer->flags;
}
#else
#define bGetFlags (!pBuffer) ? (RT_FAIL_1) : (unsigned int)pBuffer->flags //Ternary operator /*/TODO: if the flag exits // TODO248: Macro definition for flags*/
#endif

