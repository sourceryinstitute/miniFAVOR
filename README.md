[This document is formatted with GitHub-Flavored Markdown.     ]:#
[For better viewing, read it in a browser at                   ]:#
[https://github.com/everythingfunctional/miniFAVOR/tree/main/README.md ]:#

miniFAVOR
=========

```
        .__       .__
  _____ |__| ____ |__|                                |_|_|_|
 /     \|  |/    \|  |                               /| | | |\
|  Y Y  \  |   |  \  |                             _/_|_|_|_|_\_
|__|_|  /__|___|  /__|                              | | | | | |
      \/        \/                                  | | | | | |
____________________   ____________ __________    __|_|_|_|_|_|__
\_   _____/  _  \   \ /   /\_____  \\______   \     | | | | | |
 |    __)/  /_\  \   Y   /  /   |   \|       _/     | | | | | |
 |     \/    |    \     /  /    |    \    |   \     |_|_|_|_|_|
 \___  /\____|__  /\___/   \_______  /____|_  /     \         /
     \/         \/                 \/       \/       \_______/
```

CONTENTS
--------

* [Prerequisites](#prerequisites)
* [Cloning miniFAVOR](#cloning-build-and-test)
* [Building, and testing miniFAVOR](#building-and-testing-minifavor)

All software version numbers below are the tested versions.
Earlier versions might work.

Prerequisites
-------------
1. [`gfortran`] 10.2 for compiling Fortran 2018.
2. [`cmake`] 3.16 for building miniFAVOR executable programs.
3. [`ford`] 6 for building documentation.

Cloning FAVOR
------------------
* To build miniFAVOR from source, clone this repository,
  execute the following command at a command line:
```
git clone git@github.com:everythingfunctional/miniFAVOR.git
```

Building, and testing miniFAVOR
---------------------------------------
In a `bash` shell, enter the following commands:
```
mkdir -p miniFAVOR/build
cd miniFAVOR/build
export FC=gfortran
cmake ..
make
ctest -v
```
Please report any test failures by submitting a [new issue].

The [developer documentation] is published via GitHub Pages.

[Hyperlinks]:#
[`gfortran`]: https://gcc.gnu.org
[`cmake`]: https://www.cmake.org
[`ford`]: https://github.com/Fortran-FOSS-Programmers/ford
[new issue]: https://github.com/everythingfunctional/miniFAVOR/issues/new
[developer documentation]: https://sourceryinstitute.github.io/miniFAVOR/
