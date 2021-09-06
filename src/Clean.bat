::********************************************************************************
::* DprojFilter                                                                  *
::* -----------------------------------------------------------------------------*
::* Batch file to clean the project directory.                                   *
::* May be executed prior build, commmit, backup, etc. to remove unused files.   *
::* Written by Denis Bisson, Drummondville, Québec, 2021-08-27.                  *
::* -----------------------------------------------------------------------------*
::* Used in the project DprojFilter                                              *
::* Originally and mainly written by Thomas Mueller                              *
::*   https://osdn.net/projects/dprojfilter                                      *
::* This little adaptation written by Denis Bisson, Drummondville, Québec, Canada*
::*   https://github.com/denis-bisson/DprojFilter                                *
::*   2021-08-27                                                                 *
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
DEL DprojFilter.exe
ECHO "Clean is done!"