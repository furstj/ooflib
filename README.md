ooflib
======

Object Oriented Fortran Library

Compatibility:
==============

ifort 13.0.1
------------
This is my deveolpment platform, therefore it should be compatible

gfortran 4.8.0
--------------
It doesn't suppot character(len=:), allocatable, therefore
it fails in operations with strings. It should work if one
uses iso_varying_string instead of native strings 
