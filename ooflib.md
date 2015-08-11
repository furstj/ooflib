project: ooflib
project_dir: ./src
output_dir: ./doc
exclude_dir: ./doc
project_github: https://github.com/furstj/ooflib.git
project_website: http://github.com
summary: Object Oriented Fortran Library
author: Jiri Furst
github: https://github.com/furstj
email: jiri.furst@gmail.com
docmark_alt: #
display: public
	protected
	private
macro: TEST
       LOGIC=.true.

This is my attempt to develop pure fortran library approximating some usefull
concepts from C++ STL or [Boost](http://www.boost.org) libraries.

@Note
- the library relies highly on Fortran 2003/2008 features, so it needs decent
  compiler (ested with gfortran-5.2)
