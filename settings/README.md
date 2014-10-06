Eclipse setup instructions

These instructions are intended for contributors to the GWT source
code repository that want to run the Eclipse IDE. It describes how to
configure Eclipse for the correct coding styles and how to setup a GWT
project for debugging the core GWT code.


== Configure Eclipse Environment==

All relative paths are relative to the GWT source repository's
'trunk/eclipse' folder.

---------------- Encoding -----------------

Window->Preferences->General->Workspace->Text file encoding
Use "UTF-8"

---------------- Spelling -----------------

Window->Preferences->General->Editors->Text Editors->Spelling
Enable spell checking
Use "settings/english.dictionary".

---------- Code style/formatting ----------

Window->Preferences->Java->Code Style->Formatter->Import...
  settings/code-style/pwt-format.xml

----------- Import organization -----------

Window->Preferences->Java->Code Style->Organize Imports->Import...
  settings/code-style/pwt.importorder

------------ Member sort order ------------

Window->Preferences->Java->Appearance->Members Sort Order

First, members should be sorted by category.
1) Types
2) Static Fields
3) Static Initializers
4) Static Methods
5) Fields
6) Initializers
7) Constructors
8) Methods

Second, members in the same category should be sorted by visibility.
1) Public
2) Protected
3) Default
4) Private

Third, within a category/visibility combination, members should be sorted
alphabetically.
 
------- Compiler errors & warnings --------
Window->Preferences->Java->Compiler->Errors/Warnings

The following errors are suggested.

Code Style:
- Method with a constructor name

Potential programming problems:
- Assignment has no effect
- Accidental boolean assignment
- 'finally' does not complete normally
- Using a char array in string concatentation
- Hidden catch block
- Inexact type match for vararg arguments

Name shadowing and conflicts: all except "Local variable" hiding

Deprecated and restricted API: all

Unnecessary code: all except "Unnecessary 'else' statement"

Generic types: all except "Generic type parameter declared with final type bound"

Annotations:
- Annotation is used as super interface
- Enable @SuppressWarnings annotations

== Checkstyle ==

Checkstyle is used to enforce good programming style. Its use in
Eclipse is optional, since it is also run as part of the acceptance
test suite.

1. Install Checkstyle version 5.7.

The Eclipse Checkstyle plugin can be found at:
  http://eclipse-cs.sourceforge.net/

2. Import PWT Checks:

Window->Preferences->Checkstyle->New...
Set the Type to "External Configuration File"
Set the Name to "PWT Checks" (important)
Set the location to "settings/checkstyle/pwt-checkstyle.xml".
Suggested: Check "Protect Checkstyle configuration file".
Click "Ok".


