# gmeshboundaryadd
Adds boundary faces to a standard gmesh file

This program takes a gmesh file WITHOUT boundary faces appended on the end, and makes a new gmesh file WITH boundary conditions appended on the end.
If the compiling user does not openmp installed on their system, remove the openmp flag from the make file before compilation.