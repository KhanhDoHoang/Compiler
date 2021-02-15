# Compiler

### Buffers are often used when developing compilers because of their efficiency
### The buffer implementation is based on two associated data structures: 

* 1)Buffer Entity(or Buffer Handle) and,  *
* 2)Array of characters(the actual character buffer). Both structures areto be created “on demand” at runtime, thatis, they are to be allocated dynamically. *
* The Buffer Descriptor or Buffer Handle -the names suggest the purpose of this buffer control data structure. *
* Itcontains all the necessary information about the array ofcharacters, includinga pointerto the beginning of the character array location in memory, the current size, the next character entry position, the increment factor, the operational mode and some additional parameters *

### It is necessary to generate a binary that can accept the followingparameters:input file, mode, sizeand increment.
### The standard testsare composed by the following parameters in the execution:

* buffer.exe a1e.pls f 0 0 *
* buffer.exe a1r.pls f 0 0 *
* buffer.exe a1r.pls a 0 0 *
* buffer.exe a1r.pls m 0 0 *


### These executions must generate filesin the output. 
### For instance:
#### buffer.exea1e.pls f 0 0 > a11e.seva
*This is especially important because the final comparison is done by files(not only by visual checking), for instance,comparing the following outputs:*
* fc /b a11e.sout* 
* a11e.sevaSo *
*It is required to be sure that files are beinggeneratedok and the outputs can be checked using binary comparison*
