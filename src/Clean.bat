::********************************************************************************
::* BANCO REPEAT CHECKER                                                         *
::* -----------------------------------------------------------------------------*
::* Application to make some fun with past draw results of Loto-Quénbec BANCO    *
::* lottery draw results.                                                        *
::* May be executed prior build, commmit, backup, etc. to remove unused files.   *
::* Written by Denis Bisson, Drummondville, Québec, 2021-09-09.                  *
::* -----------------------------------------------------------------------------*
::* Originally written by Denis Bisson, Drummondville, Québec, Canada            *
::*   https://github.com/denis-bisson/                                           *
::*   2021-09-09                                                                 *
::* -----------------------------------------------------------------------------*
::* You should not remove these comments.                                        *
::********************************************************************************
::*
@ECHO off
ECHO "Cleaning files..."
RD /Q /S __recovery
RD /Q /S __history
DEL /Q /S *.dcu
DEL /Q /S *.local
DEL /Q /S *.res
DEL /Q /S *.map
DEL /Q /S *.drc
DEL /Q /S *.stat
DEL /Q /S *.local
DEL /Q /S *.~dsk
DEL /Q /S *.identcache
DEL /Q /S *.dsk
DEL BancoRepeatChecker.exe
ECHO "Clean is done!"