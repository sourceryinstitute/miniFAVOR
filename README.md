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
Below are the versions of each prerequsite currently employed in developing
miniFAVOR:

1. [`gfortran`] 10.2 for compiling Fortran 2018.
2. [`fpm`] 0.2.0 for building and testing miniFAVOR.
3. [`ford`] 6 for building documentation.
4. [OpenCoarrays] 2.9.2 for compiling and launching parallel executable programs.

Other versions might work as well.

Cloning, building, and testing miniFAVOR
----------------------------------------
To obtain, build, and test miniFAVOR, execute the following command in a shell:
```
git clone git@github.com:sourceryinstitute/miniFAVOR.git
fpm test --compiler caf --runner "cafrun -n 2" --target "*"
```
whereupon `fpm` will recursively download and build each of the prerequisites'
prerequisite(s) before building miniFAVOR and excecuting parallel unit and integration
tests using two images in the above.  Please report any build errors or test failures by
submitting a [new issue].

Documentation
-------------

The [developer documentation] is published via GitHub Pages.

[Hyperlinks]:#
[`gfortran`]: https://gcc.gnu.org
[`cmake`]: https://www.cmake.org
[`ford`]: https://github.com/Fortran-FOSS-Programmers/ford
[`fpm`]: https://github.com/fortran-lang/fpm
[new issue]: https://github.com/everythingfunctional/miniFAVOR/issues/new
[developer documentation]: https://sourceryinstitute.github.io/miniFAVOR/
[OpenCoarrays]: https://github.com/sourceryinstitute/opencoarrays
