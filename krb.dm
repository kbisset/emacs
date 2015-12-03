#
# ALIAS: dy	((chron) 22)
# ALIAS: dd	((chron) 8 10 :pad nil)
# ALIAS: dn	((month-num) :pad nil)

#######
# MODE:	nil 
#######
#######
dtstamp	expand	user id, date and time
 -~((user-initials) :down)~dn/~dd/~dy ~((hour) :pad nil):~min~ampm.
#
#######
dstamp	expand	user id and date
 -~((user-initials) :down)~dn/~dd/~dy.
#

fdesc	expand	Standard function comment block
//--------------------------------------------------------------------------
// Name: ~(prompt func_name "Function Name: ")
//
// Arguments:
//  ~@
// Returns:
//  ~mark
// 
//  ~mark
//
#

cdesc	expand	Standard class comment block
//--------------------------------------------------------------------------
// Name: ~(prompt func_name "Class Name: ")
// 
//  ~@
//
#

voidcfunc	expand	Standard function comment block for void class
//--------------------------------------------------------------------------
// Name: ~(prompt func_name "Function Name: ")
//
// Arguments:
//  ~@
//
//  ~mark
//
void ~(prompt class_name "Class Name: ")::~(prompt func_name "Function Name: ")(~mark)
{
~mark
}
#

voidcfunchdr	expand	Standard function comment block for void class
//--------------------------------------------------------------------------
// Name: ~(prompt func_name "Function Name: ")
//
// Arguments:
//  ~@
//
//  ~mark
//
#

cfunc	expand	Standard function comment block for class function
//--------------------------------------------------------------------------
// Name: ~(prompt func_name "Function Name: ")
//
// Arguments:
//  ~@
// Returns:
//  ~mark
// 
//  ~mark
//
~(prompt return_type "Return Type: ") ~(prompt class_name "Class Name: ")::~(prompt func_name "Function Name: ")(~mark)
{
~mark
}
#

cfunchdr	expand	Standard function comment block for class function
//--------------------------------------------------------------------------
// Name: ~(prompt func_name "Function Name: ")
//
// Arguments:
//  ~@
// Returns:
//  ~mark
// 
//  ~mark
//
#

srcmasthead	expand	Standard source file masthead
//--------------------------------------------------------------------------
// File:    ~(file)
// Module:  ~(prompt module "Module: ")
// Author:  ~(user-name)
// Created: ~(month) ~(date) ~(year)
//
// @@COPYRIGHT@@
//
//--------------------------------------------------------------------------

#include "~(prompt module "Module: ")/~(file-name).h"
using namespace std;

namespace ~(prompt module "Module: ") {

~(file-name)::~(file-name)()
{
~@}

~(file-name)::~~~(file-name)()
{
}
} // namespace
#

rcsid expand RCS id string
static char *ident_str_~((file-name) :down)_~((file-ext) :down) =
   "$Id:$";
#

timestamp expand Insert Timestamp string
// Time-stamp: " "

#

hdrmasthead	expand	Standard header file masthead
//--------------------------------------------------------------------------
// File:    ~(file)
// Module:  ~(prompt module "Module: ")
// Author:  ~(user-name)
// Created: ~(month) ~(date) ~(year)
//
// Description: ~@
//
// @@COPYRIGHT@@
//
//--------------------------------------------------------------------------

#ifndef INCLUDE_~((prompt module "Module: ") :up)_~((file-name) :up)
#define INCLUDE_~((prompt module "Module: ") :up)_~((file-name) :up)
~@

#endif // INCLUDE_~((prompt module "Module: ") :up)_~((file-name) :up)
#

hdrmasthead_short	expand	Standard header file masthead
//--------------------------------------------------------------------------
// File:    ~(file)
// Module:  ~(prompt module "Module: ")
// Author:  ~(user-name)
// Created: ~(month) ~(date) ~(year)
//
// Description: ~@
//
// @@COPYRIGHT@@
//
//--------------------------------------------------------------------------

#ifndef NISAC_~((prompt module "Module: ") :up)_~((file-name) :up)
#define NISAC_~((prompt module "Module: ") :up)_~((file-name) :up)
namespace ~(prompt module "Module: ") {
  class ~(file-name)
  {
  public:
    ~(file-name)();
    ~~~(file-name)();
    ~@
  protected:
  private:
  };
} // namespace
#endif // NISAC_~((prompt module "Module: ") :up)_~((file-name) :up)
#

testhdrmasthead_short	expand	Unit test source header file masthead
//--------------------------------------------------------------------------
// File:    ~(file)
// Module:  ~(prompt module "Module: ")
// Author:  ~(user-name)
// Created: ~(month) ~(date) ~(year)
//
// Description: ~@
//
// @@COPYRIGHT@@
//
//--------------------------------------------------------------------------

#ifndef NISAC_~((prompt module "Module: ") :up)_~((file-name) :up)
#define NISAC_~((prompt module "Module: ") :up)_~((file-name) :up)
#include "cppunit/TestCase.h"
#include "cppunit/extensions/HelperMacros.h"

namespace ~(prompt module "Module: ") {
  class ~(file-name) : public CppUnit::TestCase
  {
    CPPUNIT_TEST_SUITE(~(file-name));
    CPPUNIT_TEST(test);
    CPPUNIT_TEST_SUITE_END();
  public:
    void test~@();
  protected:
  private:
  };
} // namespace
#endif // NISAC_~((prompt module "Module: ") :up)_~((file-name) :up)
#

testsrcmasthead	expand	Unit test source file masthead
//--------------------------------------------------------------------------
// File:    ~(file)
// Module:  ~(prompt module "Module: ")
// Author:  ~(user-name)
// Created: ~(month) ~(date) ~(year)
//
// @@COPYRIGHT@@
//
//--------------------------------------------------------------------------

#include "~(prompt module "Module: ")/~(file-name).h"
using namespace std;

namespace ~(prompt module "Module: ") {
CPPUNIT_TEST_SUITE_REGISTRATION(~(file-name));
} // namespace
#
doxygenclasscomment expand Doxygen Class comment
/// \class ~(prompt class "Class: ") ~(prompt file "File: ").h "~(prompt module "Module: ")/~(prompt file "File: ").h"
///
/// \brief~@
#
slide	expand	Standard title and list slide
\begin{slide}
\slideheading{~(prompt title "Title: ")}
\begin{itemize}
\item ~@
\end{itemize}
\end{slide}
#
islide	expand	Graphics title and list slide
\begin{slide}
\slideheading{~(prompt title "Title: ")}
\begin{center}
  \includegraphics[~(prompt opts "Options: ")]{~(prompt file "File: ")}
\end{center}
\vfil
\end{slide}
#
changebar Insert  changebars
\begin{changebar}
~@
\end{changebar}
#

latex-inc expand LaTeX included/inputed file
%% -*-latex-*-
\rcsInfo $Id: $
%%% Local Variables: %%%
%%% TeX-parse-self: t %%%
%%% TeX-auto-save: t %%%
%%% TeX-master: "~@.tex" %%%
%%% End: %%%
#
