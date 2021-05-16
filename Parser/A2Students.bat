:: SCRIPT A2 - CST8152 - W21
::
:: COMPILERS COURSE - README ----------------------------------------------------------------------
::
:: THE FIRST SESSION CONTAINS THE CONSTANTS THAT YOU *NEED* TO CHANGE ACCORDING TO YOUR PROJECT
::
:: PRJPATH: PATH OF YOUR PROJECT
:: FILPATH: WHERE ARE THE FILES TO COPY TO PROJECT FOLDER (OK IF ALREADY IN DEBUG FOLDER)
:: DEBPATH: DEBUG FOLDER - MOST OF TIME (USING x86 ARCHITECTURE IS GIVEN BY %PRJPATH%\Debug)
:: INPPATH: WHERE ARE THE INPUT (.pls) FILES - IN MY CASE I HAVE COPYED TO SAME DEBUG FOLDER
:: OUTPATH: WHERE ARE THE OUTPUT (SEE OUTNAME - I AM USING THE SAME DEBUG FOLDER)
:: PRJNAME: NAME OF THE BINARY FILE CREATED BY THE PROJECT
:: BINNAME: NAME OF THE DEFAULT BINARY NAME (IN MY CASE, I AM CALLING "scanner.exe")
:: CMPNAME: NAME OF THE OS TOOL USED TO COMPARE FILES - IN MY CASE (Windows 64), "fc.exe"
:: OUTNAME: NAME OF THE OUTPUT FILE THAT WILL INDICATE IF THE FILES ARE MATCHING
:: SUFINPU: SUFIX NAME FOR INPUT FILES (OUR ".pls" FILES)
:: SUFOUTP: SUFIX NAME FOR GENERATED OUTPUTS (COMMONLY THE ".seva" = SELF-EVALUATION FILES)
:: SUFOUTS: SUFIX NAME FOR STANDARD OUTPUTS (OUR ".sout" FILES).
::
:: END OF README SESSION

@echo off
set PRJPATH=C:\Users\katie\source\repos\"CST8215_Assignment 2"
set FILPATH=.
set DEBPATH=%PRJPATH%\Debug
set INPPATH=%DEBPATH%
set OUTPATH=%DEBPATH%
set PRJNAME="CST8215_Assignment 2.exe"
set BINNAME="CST8215_Assignment 2.exe"
set CMPNAME=fc.exe
set OUTNAME=output.txt
set SUFINPU=.pls
set SUFOUTP=.sout
set SUFOUTS=.sout
cls

::
:: A2 SCRIPT - MENU -------------------------------------------------------------------------------
::

:menu
echo:
cls
echo ---------------------------------------------------------------------
echo - Main Menu (A2 - W21) ----------------------------------------------
echo ---------------------------------------------------------------------
echo 1: Help - Show configuration
echo 2: Clean environment
echo 3: Execute all
echo 4: Run standard tests
echo 5: Run aditional tests
echo 6: Show output
echo 0: Exit
echo:
set /p option="Menu options (select one): " 

if "%option%" == "1" goto con
if "%option%" == "2" goto cle
if "%option%" == "3" goto all
if "%option%" == "4" goto bas
if "%option%" == "5" goto adv
if "%option%" == "6" goto out
if "%option%" == "0" goto exi

::
:: A2 SCRIPT - SHOW CONF --------------------------------------------------------------------------
::

:con
cls
echo - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
echo -  SHOW CONFIGURATION                                               -
echo - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
echo MENU OPTIONS:
echo 1: Help (this option): Show details and configuration.
echo 2: Copy files to project: From FILPATH to PRJPATH.
echo 3: Clean environment: Remove outputs, BINNAME, OUTNAME.
echo 4: List files: Show outputs (SUFOUTP), BINNAME, OUTNAME.
echo 5: Execute all: Standard and additional tests.
echo 6: Run standard tests: Required for working program.
echo 7: Run additional tests: Required for complete marks.
echo 8: Show output: Show OUTNAME.
echo 9: Copy files from project: From PRJPATH to FILPATH.
echo 0: Exit.
echo:
echo CURRENT CONFIGURATION:
echo PRJPATH = %PRJPATH%
echo FILPATH = %FILPATH%
echo DEBPATH = %DEBPATH%
echo INPPATH = %INPPATH%
echo OUTPATH = %OUTPATH%
echo PRJNAME = %PRJNAME%
echo BINNAME = %BINNAME%
echo CMPNAME = %CMPNAME%
echo OUTNAME = %OUTNAME%
echo SUFINPU = %SUFINPU%
echo SUFOUTP = %SUFOUTP%
echo SUFOUTS = %SUFOUTS%
echo:
echo NOTE: Change variables to adjust for your configuration.
echo:
pause
goto menu

::
:: A2 SCRIPT - CLEAN FILES (AFTER REBUILD SOLUTION)  ----------------------------------------------
::

:cle
cls
echo - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
echo - PREPARE FOR EXECUTION                                             -
echo - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
echo "cd %DEBPATH%"
	cd %DEBPATH%
echo "del *%SUFOUTP%"
	del *%SUFOUTP%
echo "del %BINNAME%"
	del %BINNAME%
echo "copy %PRJNAME% %BINNAME%"
	copy %PRJNAME% %BINNAME%
echo "del %PRJNAME%"
	del %PRJNAME%
echo "del %OUTNAME%"
	del %OUTNAME%
echo:
pause
goto menu

::
:: A2 SCRIPT - SHOW CONTENT OF OUTPUT  ------------------------------------------------------------
::

:out
cls
echo - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
echo - SHOW CONTENT OF OUTPUT FILE                                       -
echo - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
echo "type %OUTNAME%"
	type %OUTNAME%
echo:
pause
goto menu

::
:: A2 SCRIPT - EXECUTE ALL TESTS ------------------------------------------------------------------
::

:all
cls
echo - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
echo - EXECUTE ALL TESTS                                                 -
echo - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
echo "cd %DEBPATH%"
	cd %DEBPATH%
echo "echo > %OUTNAME%"
	echo > %OUTNAME%
echo "%BINNAME% a201empty%SUFINPU% > a201empty%SUFOUTP%"
	%BINNAME% a201empty%SUFINPU% > a201empty%SUFOUTP%
echo "%CMPNAME% /B a201empty%SUFOUTS% a201empty%SUFOUTP% >> %OUTNAME%"
	%CMPNAME% /B a201empty%SUFOUTS% a201empty%SUFOUTP% >> %OUTNAME%
echo "%BINNAME% a202r%SUFINPU% > a202r%SUFOUTP%"
	%BINNAME% a202r%SUFINPU% > a202r%SUFOUTP%
echo "%CMPNAME% /B a202r%SUFOUTS% a202r%SUFOUTP% >> %OUTNAME%"
	%CMPNAME% /B a202r%SUFOUTS% a202r%SUFOUTP% >> %OUTNAME%
echo "%BINNAME% a203w%SUFINPU% > a203w%SUFOUTP%"
	%BINNAME% a203w%SUFINPU% > a203w%SUFOUTP%
echo "%CMPNAME% /B a203w%SUFOUTS% a203w%SUFOUTP% >> %OUTNAME%"
	%CMPNAME% /B a203w%SUFOUTS% a203w%SUFOUTP% >> %OUTNAME%
echo "%BINNAME% a204error%SUFINPU% > a204error%SUFOUTP%"
	%BINNAME% a204error%SUFINPU% > a204error%SUFOUTP%
echo "%CMPNAME% /B a204error%SUFOUTS% a204error%SUFOUTP% >> %OUTNAME%"
	%CMPNAME% /B a204error%SUFOUTS% a204error%SUFOUTP% >> %OUTNAME%
	
echo "%BINNAME% a205line%SUFINPU% > a205line%SUFOUTP%"
	%BINNAME% a205line%SUFINPU% > a205line%SUFOUTP%
echo "%CMPNAME% /B a205line%SUFOUTS% a205line%SUFOUTP% >> %OUTNAME%"
	%CMPNAME% /B a205line%SUFOUTS% a205line%SUFOUTP% >> %OUTNAME%
echo "%BINNAME% a206coml%SUFINPU% > a206coml%SUFOUTP%"
	%BINNAME% a206coml%SUFINPU% > a206coml%SUFOUTP%
echo "%CMPNAME% /B a206coml%SUFOUTS% a206coml%SUFOUTP% >> %OUTNAME%"
	%CMPNAME% /B a206coml%SUFOUTS% a206coml%SUFOUTP% >> %OUTNAME%
echo "%BINNAME% a207strl%SUFINPU% > a207strl%SUFOUTP%"
	%BINNAME% a207strl%SUFINPU% > a207strl%SUFOUTP%
echo "%CMPNAME% /B a207strl%SUFOUTS% a207strl%SUFOUTP% >> %OUTNAME%"
	%CMPNAME% /B a207strl%SUFOUTS% a207strl%SUFOUTP% >> %OUTNAME%
echo "%BINNAME% a208nl%SUFINPU% > a208nl%SUFOUTP%"
	%BINNAME% a208nl%SUFINPU% > a208nl%SUFOUTP%
echo "%CMPNAME% /B a208nl%SUFOUTS% a208nl%SUFOUTP% >> %OUTNAME%"
	%CMPNAME% /B a208nl%SUFOUTS% a208nl%SUFOUTP% >> %OUTNAME%
echo "%BINNAME% a209esl%SUFINPU% > a209esl%SUFOUTP%"
	%BINNAME% a209esl%SUFINPU% > a209esl%SUFOUTP%
echo "%CMPNAME% /B a209esl%SUFOUTS% a209esl%SUFOUTP% >> %OUTNAME%"
	%CMPNAME% /B a209esl%SUFOUTS% a209esl%SUFOUTP% >> %OUTNAME%
echo "%BINNAME% a210lint%SUFINPU% > a210lint%SUFOUTP%"
	%BINNAME% a210lint%SUFINPU% > a210lint%SUFOUTP%
echo "%CMPNAME% /B a210lint%SUFOUTS% a210lint%SUFOUTP% >> %OUTNAME%"
	%CMPNAME% /B a210lint%SUFOUTS% a210lint%SUFOUTP% >> %OUTNAME%
echo "%BINNAME% a211comro%SUFINPU% > a211comro%SUFOUTP%"
	%BINNAME% a211comro%SUFINPU% > a211comro%SUFOUTP%
echo "%CMPNAME% /B a211comro%SUFOUTS% a211comro%SUFOUTP% >> %OUTNAME%"
	%CMPNAME% /B a211comro%SUFOUTS% a211comro%SUFOUTP% >> %OUTNAME%
echo "%BINNAME% a212t7%SUFINPU% > a212t7%SUFOUTP%"
	%BINNAME% a212t7%SUFINPU% > a212t7%SUFOUTP%
echo "%CMPNAME% /B a212t7%SUFOUTS% a212t7%SUFOUTP% >> %OUTNAME%"
	%CMPNAME% /B a212t7%SUFOUTS% a212t7%SUFOUTP% >> %OUTNAME%
echo "%BINNAME% a213dot%SUFINPU% > a213dot%SUFOUTP%"
	%BINNAME% a213dot%SUFINPU% > a213dot%SUFOUTP%
echo "%CMPNAME% /B a213dot%SUFOUTS% a213dot%SUFOUTP% >> %OUTNAME%"
	%CMPNAME% /B a213dot%SUFOUTS% a213dot%SUFOUTP% >> %OUTNAME%
echo "%BINNAME% a214com%SUFINPU% > a214com%SUFOUTP%"
	%BINNAME% a214com%SUFINPU% > a214com%SUFOUTP%
echo "%CMPNAME% /B a214com%SUFOUTS% a214com%SUFOUTP% >> %OUTNAME%"
	%CMPNAME% /B a214com%SUFOUTS% a214com%SUFOUTP% >> %OUTNAME%
echo "%BINNAME% a215dAE%SUFINPU% > a215dAE%SUFOUTP%"
	%BINNAME% a215dAE%SUFINPU% > a215dAE%SUFOUTP%
echo "%CMPNAME% /B a215dAE%SUFOUTS% a215dAE%SUFOUTP% >> %OUTNAME%"
	%CMPNAME% /B a215dAE%SUFOUTS% a215dAE%SUFOUTP% >> %OUTNAME%
echo "%BINNAME% a216dnl%SUFINPU% > a216dnl%SUFOUTP%"
	%BINNAME% a216dnl%SUFINPU% > a216dnl%SUFOUTP%
echo "%CMPNAME% /B a216dnl%SUFOUTS% a216dnl%SUFOUTP% >> %OUTNAME%"
	%CMPNAME% /B a216dnl%SUFOUTS% a216dnl%SUFOUTP% >> %OUTNAME%
echo "%BINNAME% a217err%SUFINPU% > a217err%SUFOUTP%"
	%BINNAME% a217err%SUFINPU% > a217err%SUFOUTP%
echo "%CMPNAME% /B a217err%SUFOUTS% a217err%SUFOUTP% >> %OUTNAME%"
	%CMPNAME% /B a217err%SUFOUTS% a217err%SUFOUTP% >> %OUTNAME%
echo "%BINNAME% a218kw%SUFINPU% > a218kw%SUFOUTP%"
	%BINNAME% a218kw%SUFINPU% > a218kw%SUFOUTP%
echo "%CMPNAME% /B a218kw%SUFOUTS% a218kw%SUFOUTP% >> %OUTNAME%"
	%CMPNAME% /B a218kw%SUFOUTS% a218kw%SUFOUTP% >> %OUTNAME%
echo "%BINNAME% a219test%SUFINPU% > a219test%SUFOUTP%"
	%BINNAME% a219test%SUFINPU% > a219test%SUFOUTP%
echo "%CMPNAME% /B a219test%SUFOUTS% a219test%SUFOUTP% >> %OUTNAME%"
	%CMPNAME% /B a219test%SUFOUTS% a219test%SUFOUTP% >> %OUTNAME%
echo "%BINNAME% a220test%SUFINPU% > a220test%SUFOUTP%"
	%BINNAME% a220test%SUFINPU% > a220test%SUFOUTP%
echo "%CMPNAME% /B a220test%SUFOUTS% a220test%SUFOUTP% >> %OUTNAME%"
	%CMPNAME% /B a220test%SUFOUTS% a220test%SUFOUTP% >> %OUTNAME%
echo:
pause
goto menu

::
:: A2 SCRIPT - EXECUTE ONLY STANDARD TESTS  -------------------------------------------------------
::

:bas
cls
echo - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
echo - EXECUTE STANDARD TESTS                                            -
echo - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
echo "cd %DEBPATH%"
	cd %DEBPATH%
echo "echo > %OUTNAME%"
	echo > %OUTNAME%
echo "%BINNAME% a201empty%SUFINPU% > a201empty%SUFOUTP%"
	%BINNAME% a201empty%SUFINPU% > a201empty%SUFOUTP%
echo "%CMPNAME% /B a201empty%SUFOUTS% a201empty%SUFOUTP% >> %OUTNAME%"
	%CMPNAME% /B a201empty%SUFOUTS% a201empty%SUFOUTP% >> %OUTNAME%
echo "%BINNAME% a202r%SUFINPU% > a202r%SUFOUTP%"
	%BINNAME% a202r%SUFINPU% > a202r%SUFOUTP%
echo "%CMPNAME% /B a202r%SUFOUTS% a202r%SUFOUTP% >> %OUTNAME%"
	%CMPNAME% /B a202r%SUFOUTS% a202r%SUFOUTP% >> %OUTNAME%
echo "%BINNAME% a203w%SUFINPU% > a203w%SUFOUTP%"
	%BINNAME% a203w%SUFINPU% > a203w%SUFOUTP%
echo "%CMPNAME% /B a203w%SUFOUTS% a203w%SUFOUTP% >> %OUTNAME%"
	%CMPNAME% /B a203w%SUFOUTS% a203w%SUFOUTP% >> %OUTNAME%
echo "%BINNAME% a204error%SUFINPU% > a204error%SUFOUTP%"
	%BINNAME% a204error%SUFINPU% > a204error%SUFOUTP%
echo "%CMPNAME% /B a204error%SUFOUTS% a204error%SUFOUTP% >> %OUTNAME%"
	%CMPNAME% /B a204error%SUFOUTS% a204error%SUFOUTP% >> %OUTNAME%
echo:
pause
goto menu

::
:: A2 SCRIPT - EXECUTE ADDITIONAL TESTS  ----------------------------------------------------------
::

:adv
cls
echo - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
echo - EXECUTE ADDITIONAL TESTS                                          -
echo - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
echo "cd %DEBPATH%"
	cd %DEBPATH%
echo "echo > %OUTNAME%"
	echo > %OUTNAME%
echo "%BINNAME% a205line%SUFINPU% > a205line%SUFOUTP%"
	%BINNAME% a205line%SUFINPU% > a205line%SUFOUTP%
echo "%CMPNAME% /B a205line%SUFOUTS% a205line%SUFOUTP% >> %OUTNAME%"
	%CMPNAME% /B a205line%SUFOUTS% a205line%SUFOUTP% >> %OUTNAME%
echo "%BINNAME% a206coml%SUFINPU% > a206coml%SUFOUTP%"
	%BINNAME% a206coml%SUFINPU% > a206coml%SUFOUTP%
echo "%CMPNAME% /B a206coml%SUFOUTS% a206coml%SUFOUTP% >> %OUTNAME%"
	%CMPNAME% /B a206coml%SUFOUTS% a206coml%SUFOUTP% >> %OUTNAME%
echo "%BINNAME% a207strl%SUFINPU% > a207strl%SUFOUTP%"
	%BINNAME% a207strl%SUFINPU% > a207strl%SUFOUTP%
echo "%CMPNAME% /B a207strl%SUFOUTS% a207strl%SUFOUTP% >> %OUTNAME%"
	%CMPNAME% /B a207strl%SUFOUTS% a207strl%SUFOUTP% >> %OUTNAME%
echo "%BINNAME% a208nl%SUFINPU% > a208nl%SUFOUTP%"
	%BINNAME% a208nl%SUFINPU% > a208nl%SUFOUTP%
echo "%CMPNAME% /B a208nl%SUFOUTS% a208nl%SUFOUTP% >> %OUTNAME%"
	%CMPNAME% /B a208nl%SUFOUTS% a208nl%SUFOUTP% >> %OUTNAME%
echo "%BINNAME% a209esl%SUFINPU% > a209esl%SUFOUTP%"
	%BINNAME% a209esl%SUFINPU% > a209esl%SUFOUTP%
echo "%CMPNAME% /B a209esl%SUFOUTS% a209esl%SUFOUTP% >> %OUTNAME%"
	%CMPNAME% /B a209esl%SUFOUTS% a209esl%SUFOUTP% >> %OUTNAME%
echo "%BINNAME% a210lint%SUFINPU% > a210lint%SUFOUTP%"
	%BINNAME% a210lint%SUFINPU% > a210lint%SUFOUTP%
echo "%CMPNAME% /B a210lint%SUFOUTS% a210lint%SUFOUTP% >> %OUTNAME%"
	%CMPNAME% /B a210lint%SUFOUTS% a210lint%SUFOUTP% >> %OUTNAME%
echo "%BINNAME% a211comro%SUFINPU% > a211comro%SUFOUTP%"
	%BINNAME% a211comro%SUFINPU% > a211comro%SUFOUTP%
echo "%CMPNAME% /B a211comro%SUFOUTS% a211comro%SUFOUTP% >> %OUTNAME%"
	%CMPNAME% /B a211comro%SUFOUTS% a211comro%SUFOUTP% >> %OUTNAME%
echo "%BINNAME% a212t7%SUFINPU% > a212t7%SUFOUTP%"
	%BINNAME% a212t7%SUFINPU% > a212t7%SUFOUTP%
echo "%CMPNAME% /B a212t7%SUFOUTS% a212t7%SUFOUTP% >> %OUTNAME%"
	%CMPNAME% /B a212t7%SUFOUTS% a212t7%SUFOUTP% >> %OUTNAME%
echo "%BINNAME% a213dot%SUFINPU% > a213dot%SUFOUTP%"
	%BINNAME% a213dot%SUFINPU% > a213dot%SUFOUTP%
echo "%CMPNAME% /B a213dot%SUFOUTS% a213dot%SUFOUTP% >> %OUTNAME%"
	%CMPNAME% /B a213dot%SUFOUTS% a213dot%SUFOUTP% >> %OUTNAME%
echo "%BINNAME% a214com%SUFINPU% > a214com%SUFOUTP%"
	%BINNAME% a214com%SUFINPU% > a214com%SUFOUTP%
echo "%CMPNAME% /B a214com%SUFOUTS% a214com%SUFOUTP% >> %OUTNAME%"
	%CMPNAME% /B a214com%SUFOUTS% a214com%SUFOUTP% >> %OUTNAME%
echo "%BINNAME% a215dAE%SUFINPU% > a215dAE%SUFOUTP%"
	%BINNAME% a215dAE%SUFINPU% > a215dAE%SUFOUTP%
echo "%CMPNAME% /B a215dAE%SUFOUTS% a215dAE%SUFOUTP% >> %OUTNAME%"
	%CMPNAME% /B a215dAE%SUFOUTS% a215dAE%SUFOUTP% >> %OUTNAME%
echo "%BINNAME% a216dnl%SUFINPU% > a216dnl%SUFOUTP%"
	%BINNAME% a216dnl%SUFINPU% > a216dnl%SUFOUTP%
echo "%CMPNAME% /B a216dnl%SUFOUTS% a216dnl%SUFOUTP% >> %OUTNAME%"
	%CMPNAME% /B a216dnl%SUFOUTS% a216dnl%SUFOUTP% >> %OUTNAME%
echo "%BINNAME% a217err%SUFINPU% > a217err%SUFOUTP%"
	%BINNAME% a217err%SUFINPU% > a217err%SUFOUTP%
echo "%CMPNAME% /B a217err%SUFOUTS% a217err%SUFOUTP% >> %OUTNAME%"
	%CMPNAME% /B a217err%SUFOUTS% a217err%SUFOUTP% >> %OUTNAME%
echo "%BINNAME% a218kw%SUFINPU% > a218kw%SUFOUTP%"
	%BINNAME% a218kw%SUFINPU% > a218kw%SUFOUTP%
echo "%CMPNAME% /B a218kw%SUFOUTS% a218kw%SUFOUTP% >> %OUTNAME%"
	%CMPNAME% /B a218kw%SUFOUTS% a218kw%SUFOUTP% >> %OUTNAME%
echo "%BINNAME% a219test%SUFINPU% > a219test%SUFOUTP%"
	%BINNAME% a219test%SUFINPU% > a219test%SUFOUTP%
echo "%CMPNAME% /B a219test%SUFOUTS% a219test%SUFOUTP% >> %OUTNAME%"
	%CMPNAME% /B a219test%SUFOUTS% a219test%SUFOUTP% >> %OUTNAME%
echo "%BINNAME% a220test%SUFINPU% > a220test%SUFOUTP%"
	%BINNAME% a220test%SUFINPU% > a220test%SUFOUTP%
echo "%CMPNAME% /B a220test%SUFOUTS% a220test%SUFOUTP% >> %OUTNAME%"
	%CMPNAME% /B a220test%SUFOUTS% a220test%SUFOUTP% >> %OUTNAME%
echo:
pause
goto menu

::
:: A2 SCRIPT - TERMINATE EXECUTION  ---------------------------------------------------------------
::

:exi
echo - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
echo - TERMINATE EXECUTION                                               -
echo - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
set PRJPATH = ""
set FILPATH = ""
set DEBPATH = ""
set INPPATH = ""
set OUTPATH = ""
set PRJNAME = ""
set BINNAME = ""
set CMPNAME = ""
set OUTNAME = ""
set SUFOUTP = ""
set SUFOUTS = ""
echo ---------------------------------------------------------------------
echo - Batch End (A2 - W21) ----------------------------------------------
echo ---------------------------------------------------------------------
