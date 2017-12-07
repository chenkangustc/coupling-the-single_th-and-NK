dnl This program is free software; you can redistribute it and/or modify
dnl it under the terms of the GNU General Public License as published by
dnl the Free Software Foundation; either version 2, or (at your option)
dnl any later version.
dnl
dnl This program is distributed in the hope that it will be useful,
dnl but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl GNU General Public License for more details.
dnl
dnl You should have received a copy of the GNU General Public License
dnl along with this program; if not, write to the Free Software
dnl Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
dnl 02111-1307, USA.
dnl
dnl As a special exception, the Free Software Foundation gives unlimited
dnl permission to copy, distribute and modify the configure scripts that
dnl are the output of Autoconf.  You need not follow the terms of the GNU
dnl General Public License when using or distributing such scripts, even
dnl though portions of the text of Autoconf appear in them.  The GNU
dnl General Public License (GPL) does govern all other use of the material
dnl that constitutes the Autoconf program.
dnl
dnl Certain portions of the Autoconf source text are designed to be copied
dnl (in certain cases, depending on the input) into the output of
dnl Autoconf.  We call these the "data" portions.  The rest of the Autoconf
dnl source text consists of comments plus executable code that decides which
dnl of the data portions to output in any given case.  We call these
dnl comments and executable code the "non-data" portions.  Autoconf never
dnl copies any of the non-data portions into its output.
dnl
dnl This special exception to the GPL applies to versions of Autoconf
dnl released by the Free Software Foundation.  When you make and
dnl distribute a modified version of Autoconf, you may extend this special
dnl exception to the GPL to apply to your modified version as well, *unless*
dnl your modified version has the potential to copy into its output some
dnl of the text that was the non-data portion of the version that you started
dnl with.  (In other words, unless your change moves or copies text from
dnl the non-data portions to the data portions.)  If your modification has
dnl such potential, you must delete any notice of this special exception
dnl to the GPL from your modified version.
dnl
dnl Copyright Toby White <tow21@cam.ac.uk>  2004-2006     


# TW_FC_CHECK_EOL()
# -------------------
#
# This macro checks what kind of line ending the Fortran compiler
# writes out, and expects to read in. (Checking reading explicitly really doesnt work)


AC_DEFUN([TW_FC_CHECK_EOL], [dnl
AC_REQUIRE([AC_PROG_FC])
AC_LANG_PUSH([Fortran])
FCFLAGS_save="$FCFLAGS"
FCFLAGS="$FCFLAGS $FCFLAGS_free_f90"
AC_MSG_CHECKING([for EOR character used by $FC])
AC_RUN_IFELSE([
       program output_eol
       open(unit=10, file="conf_eol.out")
       write(10,"(a)") ""
       end program
],
[
if ! test -f conf_eol.out; then
AC_MSG_ERROR([Could not find test output])
elif od -b conf_eol.out | grep 5015 >/dev/null; then
  ac_cv_FC_check_output_eol=CRLF
elif od -b conf_eol.out | grep 15 >/dev/null; then
  ac_cv_FC_check_output_eol=CR
elif od -b conf_eol.out | grep 12 >/dev/null; then
  ac_cv_FC_check_output_eol=LF
else
  ac_cv_FC_check_output_eol=UNKNOWN
fi
rm -rf conf_eol.out
if test $ac_cv_FC_check_output_eol = UNKNOWN; then
  AC_MSG_ERROR([Could not determine line-ending convention])
fi
], 
[AC_MSG_ERROR([Could not execute compiled program])],
[ac_cv_FC_check_output_eol=EOL_CR] dnl take a wild guess at Unix if x-compiling
)
AC_MSG_RESULT([$ac_cv_FC_check_output_eol])
])
dnl output EOL
dnl
AC_DEFUN([_TW_FC_CHECK_INPUT_EOL], [
AC_MSG_CHECKING([for input EOL character produced by $FC])
touch conf_empty.txt
# Surely there must be a better way to create a CRLF file than this!
echo 12 | tr -d "\012\015" | tr 12 "\015\012" > conf_crlf.txt
AC_RUN_IFELSE([
       program input_eol
       integer :: i, io_eof
       integer :: s1, result
       character :: c
       open(unit=10, file="conf_empty.txt")
       open(unit=11, file="conf_crlf.txt")
       open(unit=12, file="conf_result.txt")
       ! Pick up eof first
       read(10, "(a1)", iostat=io_eof) c
       i = 0
       n = 1
       s = 0
       result = 0
       read(11, "(a1)", iostat=i) c
       ! If we are on an LF-EOL machine,
       ! then we should get CR followed by EOR
       if (i==0) then
         if (iachar(c)==13) then
           s1 = 13
         elseif (iachar(c)==32) then
           ! some compilers translate it into a space, unhelpfully
           s1 = 32
         else
           write(12, *) "UNKNOWN"
           stop
         endif
       else
         s1 = -1 ! End of Record, we assume
       endif
       read(11, "(a1)", iostat=i) c
       if (i==0) then
         if (iachar(c)==10.and.s1==-1) then
	   ! Sequence was EOR, LF, therefore EOR=CR.
           ! Next must be EOF
           read(11, "(a1)", iostat=i)
           if (i==io_eof) result = 1 ! EOR_CR
         endif
       elseif (i==io_eof) then
         if (s1==-1) then
           ! Sequence was EOR, EOF, therefore EOR=CRLF
           result = 2 ! EOR_CRLF
         elseif (s1==32) then
           ! Sequence was SPACE, EOF. Empirically, this seems to happen on PPC Macs, so:
           result = 3 ! EOR_LF
         endif
       elseif (s1==13) then
         ! We assume this non-zero iostat is EOR
         ! Sequence was CR, EOR, therefore EOR=LF
         ! Next must be EOF
         read(11, "(a1)", iostat=i)
         if (i==io_eof) result = 3 ! EOR_LF
       endif
       select case(result)
       case (1)
         write(12,*) "CR"
       case (2)
         write(12,*) "CRLF"
       case (3)
         write(12,*) "LF"
       case default
         write(12,*) "UNKNOWN"
       end select
       end program
],
[
rm -f conf_empty.txt conf_crlf.txt
if ! test -f conf_result.txt; then
  AC_MSG_ERROR([Could not find test output])
elif grep CRLF conf_result.txt >/dev/null; then
  ac_cv_FC_check_input_eol=CRLF
elif grep CR conf_result.txt >/dev/null; then
  ac_cv_FC_check_input_eol=CR
elif grep LF conf_result.txt >/dev/null; then
  ac_cv_FC_check_input_eol=LF
else
  ac_cv_FC_check_input_eol=UNKNOWN
fi
dnl rm -f conf_result.txt
if test $ac_cv_FC_check_input_eol = UNKNOWN; then
  AC_MSG_ERROR([Could not determine input line-ending convention])
fi
], 
[AC_MSG_ERROR([Could not execute compiled program])],
[ac_cv_FC_check_input_eol=CR] dnl take a wild guess at Unix if x-compiling
)
AC_MSG_RESULT([$ac_cv_FC_check_output_eol])
dnl check input_eol
dnl
FCFLAGS="$FCFLAGS_save"
])])