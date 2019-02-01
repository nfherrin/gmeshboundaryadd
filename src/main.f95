!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
! GMESH BOUNDARY ADDING UTILITY
!   Adds boundary faces to a Gmesh file:
!
!> @mainpage Gmesh Boundary Adder
!!    This pre-processor provides boundary adding capabilities for a Gmesh file
!!    without boundary information. It has been run and tested when compiled
!!    with GFortran v.5.4.0 using the parallel programming API OpenMP v.4.0.
!!    The program does work completely fine in serial and can be compiled as is
!!    without OpenMP.
!
!>   This driver provides the control structure for all entities within the
!!   BMESHBOUNDARYADD utility. It parses command line input, opens the input
!!   files, and prints out the modified Gmesh file.
!
!> @author Nicholas Herring
!> @version 1.0
!> @date February, 2018
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
PROGRAM hammercoupler
    USE globalvariables
    USE readinfunctions
    USE printoutfunctions
    USE globalfunctions
    USE boundaryfunctions
    IMPLICIT NONE

    !get command line information
    CALL readincommandline()

    !read in all file information
    CALL readinmesh()
    WRITE(*,'(A)')"Mesh read in without error"

    !calculate boundaries
    !$ CALL getfaces(faces,elements,numelements,numproc)
    !$ CALL findboundaries(faces,numelements,boundaryface,totbound,numproc)
    !$ IF(.FALSE.)THEN
        CALL getfaces(faces,elements,numelements,1)
        CALL findboundaries(faces,numelements,boundaryface,totbound,1)
    !$ END IF
    WRITE(*,'(A)')"Boundary faces found without error"

    !print out output
    CALL printoutmesh()

    !inform user program has completed.
    WRITE(*,'(A)')"Program completed without error"
END PROGRAM hammercoupler
