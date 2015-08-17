cmake_minimum_required (VERSION 2.8)
project (OOFLib)

enable_language (Fortran)

link_directories(
) 

add_library(ooflib
	objects.F90
	iterators.F90
	array_iterators.F90
	collections.F90
	lists.F90
	rationals.F90
	queues.F90
	property_tree.F90
)

add_executable(ooflib_test
	../tests/ooflib_test.F90
	../tests/lists_test.F90
	../tests/property_tree_test.F90
)

target_link_libraries(ooflib_test
	ooflib
)


add_custom_target(doc
ford ooflib.md
WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/..
COMMENT "Generating API documentation with FORD" VERBATIM
)

