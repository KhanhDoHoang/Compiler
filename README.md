# Compiler

### Buffersare often used when developingcompilers because of their efficiency (see page 111 of your textbook). 
### The buffer implementation is based on two associated data structures: 

* 1)Buffer Entity(or Buffer Handle) and,  *
* 2)Array of characters(the actual character buffer). Both structures areto be created “on demand” at runtime, thatis, they are to be allocated dynamically. *
* The Buffer Descriptor or Buffer Handle -the names suggest the purpose of this buffer control data structure. *
* Itcontains all the necessary information about the array ofcharacters, includinga pointerto the beginning of the character array location in memory, the current size, the next character entry position, the increment factor, the operational mode and some additional parameters *

It is necessary to generate a binary that can accept the followingparameters:input file, mode, sizeand increment.
The standard testsare composed by the following parameters in the execution:
•buffer.exe a1e.pls f 0 0
•buffer.exe a1r.pls f 0 0
•buffer.exe a1r.pls a 0 0
•buffer.exe a1r.pls m 0 0
