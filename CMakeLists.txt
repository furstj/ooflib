cmake_minimum_required (VERSION 2.8)
project (SEAL)

set(CMAKE_Fortran_COMPILER "ifort")
enable_language (Fortran)

set(CMAKE_BUILD_TYPE Debug)

link_directories(
) 

add_library(ooflib
	objects.F90
	iterators.F90
	array_iterators.F90
	collections.F90
	lists.F90
	queues.F90
)

add_executable(ooflib_test
	ooflib_test.F90
)

target_link_libraries(ooflib_test
	ooflib
)


#
# add a target to generate API documentation with Doxygen
#
find_package(Doxygen)
if(DOXYGEN_FOUND)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/Doxyfile.in ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile @ONLY)
add_custom_target(doc
${DOXYGEN_EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile
WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
COMMENT "Generating API documentation with Doxygen" VERBATIM
)
endif(DOXYGEN_FOUND)

