---
project: miniFAVOR
summary: Proxy Application miniFAVOR 
src_dir: ../app
         ../src
exclude_dir: ../doc/miniFAVOR-doc
output_dir: ../doc/miniFAVOR-doc
preprocess: true
macro: FORD
preprocessor: gfortran -E
display: public
         protected
         private
source: true
graph: true
md_extensions: markdown.extensions.toc
css: ../doc/miniFAVOR-doc-style.css
coloured_edges: true
sort: permission-alpha
extra_mods: iso_fortran_env:https://gcc.gnu.org/onlinedocs/gfortran/ISO_005fFORTRAN_005fENV.html
            iso_c_binding:https://gcc.gnu.org/onlinedocs/gfortran/ISO_005fC_005fBINDING.html#ISO_005fC_005fBINDING
lower: true
print_creation_date: true
creation_date: %Y-%m-%d %H:%M %z
project_github: https://github.com/everythingfunctional/miniFAVOR
project_download: hhttps://github.com/everythingfunctional/miniFAVOR
github: https://github.com/everythingfunctional/miniFAVOR

---

[_____ Comments _______]:#
[source: display source code corresponding to item being documented]:#
[graph: generate call graphs, module dependency graphs, derive type composition/inheritance graphs ]:#
[sort: different sorting schemes for the modules or procedures or programs or derived types (alpha = alphabetical see wiki).]:#
[extra_mods: documentation for intrinsic modules]:#

[This document is a FORD project file, formatted with Pythonic Markdown                                      ]:#
[See https://github.com/Fortran-FOSS-programmers/ford/wiki/Project-File-Options for more info on writing FORD project files]:#

[TOC]

miniFAVOR Documentation
===============================

Welcome to the miniFAVOR documentation.
This online documentation is automatically generated from inline comments and static analysis using the [FORD] tool.

[FORD]: https://github.com/Fortran-FOSS-Programmers/ford#readme


Organization
------------

The [FORD] tool is used to document Modern Fortran source code.

### Top Navigation Bar

You can navigate through the source code by using the black navigation bar at the top of this landing page. In addition, the search box on the right side of the top navbar, the following link is available:

* [Source Files]:  
  This landing page enumerates the source files associated with the project,
  includes a graph depicting their interdependencies and links to their dedicated pages.

[Source Files]: ./lists/files.html


Getting Help
------------

If you encounter a problem, have a suggestion, or want to ask a question,
we encourage you to post an issue in [this projects Github repository] by
[opening a new issue]. 

[this projects Github repository]: https://github.com/everythingfunctional/miniFAVOR
[opening a new issue]: https://github.com/everythingfunctional/miniFAVOR/issues/new