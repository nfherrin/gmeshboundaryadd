!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
! GMESH BOUNDARY ADDING UTILITY
!   Output Functions Module:
!
!>    This module contains functionality necessary to output the new Gmesh
!!    with boundary faces.
!
!> @author Nicholas Herring
!> @version 1.0
!> @date February, 2018
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
MODULE printoutfunctions
    USE globalvariables
    USE globalfunctions
    IMPLICIT NONE
CONTAINS

    !-------------------------------------------------------------------------------
    !-------------------------------------------------------------------------------
    !> Output elements, node, and material data to the Gmesh file
    !-------------------------------------------------------------------------------
    !-------------------------------------------------------------------------------
    SUBROUTINE printoutmesh
        !open outputfile
        OPEN(UNIT=12,FILE=fileout,STATUS='REPLACE',ACTION='WRITE',IOSTAT=ios,IOMSG=tempcharacter)
        IF(ios .NE. 0)THEN
            WRITE(*,*)tempcharacter
            STOP
        END IF

        !output the original file, we will just append the boundary face data. Probably would be better to do this with a file copy, read to end and append beginning at where the #EndElements delim is....
        WRITE(12,'(A)')'$MeshFormat'
        WRITE(12,'(A)')'2.0 0 8'
        WRITE(12,'(A)')'$EndMeshFormat'
        WRITE(12,'(A)')'$Nodes'
        WRITE(12,'(I0)')numnodes
        DO i=1,numnodes
            WRITE(12,'(I0,3ES26.16)')i,nodes(i,:)
        END DO
        WRITE(12,'(A)')'$EndNodes'
        WRITE(12,'(A)')'$Elements'
        WRITE(12,'(I0)')numelements+totbound
        DO i=1,numelements
            WRITE(12,'(I0)',ADVANCE='NO')i
            DO j=1,9
                WRITE(12,'(A,I0)',ADVANCE='NO')' ',elements(i,j)
            END DO
            WRITE(12,*)
        END DO

        !this right here is the new stuff. Boundary faces man...
        DO j=1,totbound
            WRITE(12,'(I0,A)',ADVANCE='NO')numelements+j,' 2 3 0 0 1'
            DO k=1,3
                WRITE(12,'(A,I0)',ADVANCE='NO')' ',boundaryface(j,k)
            END DO
            WRITE(12,*)
        END DO
        WRITE(12,'(A)')'$EndElements'

        !close output file
        CLOSE(UNIT=12,IOSTAT=ios)
        IF(ios .NE. 0)THEN
            WRITE(*,*)'Could not close output file ', fileout
            STOP
        END IF
    END SUBROUTINE printoutmesh
END MODULE printoutfunctions
