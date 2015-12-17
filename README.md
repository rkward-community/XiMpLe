# XiMpLe

XiMpLe is a simple XML tree parser and generator for [R](https://r-project.org).

It includes functions to read XML files into R objects, get information out of and into nodes, and
write R objects back to XML code. It's not as powerful as the 'XML' package and doesn't aim to be,
but for simple XML handling it could be useful.

It was originally developed for [RKWard](https://rkward.kde.org), a powerful GUI and IDE for
[R](https://r-project.org), to make plugin development easier. But it's a generic package and does
not depend on RKWard in any way.

## Installation

### Installation via R

There's a stable version on [CRAN](https://cran.r-project.org), so you can install it the usual R way. A probably
more recent development version can be found in [RKWard's package repository](https://files.kde.org/rkward/R):

```
install.packages("XiMpLe", repos="https://files.kde.org/rkward/R/")
```

There are also pre-built [Debian/Ubuntu packages](https://files.kde.org/rkward/R/pckg/XiMpLe/deb_repo.html).

### Installation via GitHub

To install it directly from GitHub, you can use `install_github()` from the [devtools](https://github.com/hadley/devtools) package:

```
library(devtools)
install_github("rkward-community/XiMpLe")
```
 
## Licence

Copyright 2015 Meik Michalke <meik.michalke@hhu.de>

XiMpLe is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

XiMpLe is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with XiMpLe.  If not, see <http://www.gnu.org/licenses/>.