!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
! GMESH BOUNDARY ADDING UTILITY
!   Input Functions Module:
!
!>    This module contains functionality necessary to ingest all file and
!!    command line input
!
!> @author Nicholas Herring
!> @version 1.0
!> @date February, 2018
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
MODULE readinfunctions
    USE globalvariables
    USE globalfunctions
    IMPLICIT NONE
CONTAINS

    !-------------------------------------------------------------------------------
    !-------------------------------------------------------------------------------
    !> Gets command line arguments, and prompts for input if command line arguments
    !! not present
    !-------------------------------------------------------------------------------
    !-------------------------------------------------------------------------------
    SUBROUTINE readincommandline()
        !get command line aguments
        arg_count=COMMAND_ARGUMENT_COUNT()
        !$ IF(arg_count .GT. 3)STOP 'Only three argument variables are allowed for this program! "filein" "fileout" "numproc"'
        !$ IF(.FALSE.)THEN
            IF(arg_count .GT. 2)STOP 'Only two argument variables are allowed for this program! "filein" "fileout"'
        !$ END IF

        !either use or prompt for input mesh file name
        IF(arg_count .GE. 1)THEN
            CALL GET_COMMAND_ARGUMENT(1, filein)
        ELSE
            WRITE(*,'(A)')'Input file name?'
            WRITE(*,'(A)',ADVANCE='NO')'> '
            READ(*,*)filein
        END IF

        !either use or prompt for output mesh file name
        IF(arg_count .GE. 2)THEN
            CALL GET_COMMAND_ARGUMENT(2, fileout)
        ELSE
            WRITE(*,'(A)')'Output file name?'
            WRITE(*,'(A)',ADVANCE='NO')'> '
            READ(*,*)fileout
        END IF

        !either use or prompt for number processors
        !$ numproc=1
        !$ IF(arg_count .GE. 3)THEN
        !$     CALL GET_COMMAND_ARGUMENT(3, tempcharacter)
        !$     READ(tempcharacter,*)numproc
        !$ ELSE
        !$     WRITE(*,'(A)')'Number of threads?'
        !$     WRITE(*,'(A)',ADVANCE='NO')'> '
        !$     READ(*,*)numproc
        !$ END IF
    END SUBROUTINE readincommandline

    !-------------------------------------------------------------------------------
    !-------------------------------------------------------------------------------
    !> Reads in input file information
    !-------------------------------------------------------------------------------
    !-------------------------------------------------------------------------------
    SUBROUTINE readinmesh()
        !open input file
        OPEN(UNIT=11,FILE=filein,STATUS='OLD',ACTION='READ',IOSTAT=ios,IOMSG=tempcharacter)
        IF(ios .NE. 0)THEN
            WRITE(*,*)tempcharacter
            STOP
        END IF

        !reads the number of nodes
        DO
            READ(11,*)tempcharacter
            IF(tempcharacter .EQ. "$Nodes")EXIT
        END DO
        READ(11,*)numnodes

        !allocates the node array
        ALLOCATE(nodes(numnodes,3))

        !reads in all nodes
        DO i=1,numnodes
            READ(11,*)tempcharacter,nodes(i,:)
        END DO

        !reads number of elements
        DO
            READ(11,*)tempcharacter
            IF(tempcharacter .EQ. "$Elements")EXIT
        END DO
        READ(11,*)numelements

        !allocates the elements array, the faces array, and the adjacency array
        ALLOCATE(elements(numelements,9),faces(numelements,4,3),adjcount(numelements,4))

        !reads in all the tetrahedral elements
        DO i=1,numelements
            READ(11,*)tempcharacter,elements(i,:)
        END DO

        !close input file
        CLOSE(UNIT=11,IOSTAT=ios)
        IF(ios .NE. 0)THEN
            WRITE(*,*)'Could not close input file ', filein
            STOP
        END IF
    END SUBROUTINE readinmesh
END MODULE readinfunctions
