!Purpose of this program is to add boundary faces to a general tetrahedral gmesh file that otherwise does not have them.
!For use with tetrahedral meshes of gmesh format "2.0 0 8". May be modified to input/output other mesh formats if format is known by developer
PROGRAM boundaryadd
CHARACTER(32)::tempcharacter,filein,fileout,arg
INTEGER::ios,numnodes,i,numelements,j,tempint,k,totbound,l,numproc
INTEGER,ALLOCATABLE::elements(:,:),faces(:,:,:),adjcount(:,:),boundaryface(:,:)
REAL(8),ALLOCATABLE::nodes(:,:)

numproc=1
!$ IF(iargc() .GT. 3)STOP 'Only three argument variables are allowed for this program! "filein" "fileout" "numproc"'
!$ IF(.FALSE.)THEN
    IF(iargc() .GT. 2)STOP 'Only two argument variables are allowed for this program! "filein" "fileout"'
!$ END IF
!Query for input and outputfile names
IF(iargc() .GE. 1)THEN
    CALL getarg(1, arg)
    READ(arg,*)filein
ELSE
    WRITE(*,'(A)')'Input file name?'
    WRITE(*,'(A)',ADVANCE='NO')'> '
    READ(*,*)filein
END IF

IF(iargc() .GE. 2)THEN
    CALL getarg(2, arg)
    READ(arg,*)fileout
ELSE
    WRITE(*,'(A)')'Output file name?'
    WRITE(*,'(A)',ADVANCE='NO')'> '
    READ(*,*)fileout
END IF

!$ IF(iargc() .GE. 3)THEN
!$     CALL getarg(3, arg)
!$     READ(arg,*)numproc
!$ ELSE
!$     WRITE(*,'(A)')'Number of processors to use?'
!$     WRITE(*,'(A)',ADVANCE='NO')'> '
!$     READ(*,*)numproc
!$ END IF

!open input file
OPEN(UNIT=11,FILE=filein,STATUS='OLD',ACTION='READ',IOSTAT=ios,IOMSG=tempcharacter)
IF(ios .NE. 0)THEN
    WRITE(*,*)tempcharacter
    WRITE(*,*)'Could not open', filein
    STOP
END IF

!reads the number of nodes
DO
    READ(11,*)tempcharacter
    IF(tempcharacter .EQ. "$Nodes")EXIT
END DO

READ(11,*)numnodes
WRITE(*,*)numnodes, " nodes"

!allocates the node array
ALLOCATE(nodes(numnodes,3))

!reads in all nodes
DO i=1,numnodes
    READ(11,*)tempcharacter,nodes(i,:)
END DO

REWIND(11)

!reads number of elements
DO
    READ(11,*)tempcharacter
    IF(tempcharacter .EQ. "$Elements")EXIT
END DO

READ(11,*)numelements
WRITE(*,*)numelements, " elements"

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

!get faces for every single tet, make sure they are in order from low to high in this local list. i.e. 3 1 2 is represented always as 1 2 3. This prevents redundancy in the adjancency checking protocol
!loop over all elements
!$OMP PARALLEL DEFAULT(SHARED) PRIVATE(i,j,tempint) NUM_THREADS(numproc)
!$OMP DO
DO i=1,numelements
    !loop over all faces for a given element
    DO j=1,4
        !gets data for nodes of a given face of an element
        faces(i,j,1)=elements(i,6+INT(AINT(j/4.)))
        faces(i,j,2)=elements(i,7+INT(AINT(j/3.)))
        faces(i,j,3)=elements(i,8+INT(AINT((j+1)/3.)))
        
        !orders face nodes properly for the given element, could write a sorting algortihm, but there are only three nodes for a face so it's not worth it.
        IF((faces(i,j,2) .LT. faces(i,j,1)) .AND. (faces(i,j,2) .LT. faces(i,j,3)))THEN
            tempint=faces(i,j,1)
            faces(i,j,1)=faces(i,j,2)
            faces(i,j,2)=tempint
        ELSE IF((faces(i,j,3) .LT. faces(i,j,1)) .AND. (faces(i,j,3) .LT. faces(i,j,2)))THEN
            tempint=faces(i,j,1)
            faces(i,j,1)=faces(i,j,3)
            faces(i,j,3)=tempint
        END IF
        IF(faces(i,j,3) .LT. faces(i,j,2))THEN
            tempint=faces(i,j,2)
            faces(i,j,2)=faces(i,j,3)
            faces(i,j,3)=tempint
        END IF
    END DO
END DO
!$OMP END DO
!$OMP END PARALLEL

!preassign all adjancencies for each elements face as -1 
!$OMP PARALLEL DEFAULT(SHARED) PRIVATE(i,k) NUM_THREADS(numproc)
!$OMP DO
DO i=1,numelements
    DO k=1,4
        adjcount(i,k)=-1
    END DO
END DO
!$OMP END DO
!$OMP END PARALLEL

! when an adjancency hit is registered, the adjacency count will increment by 1. One of the hits will be the face originally pulled from, thus the -1 preassignment
!loop over all faces for the checked face
!$OMP PARALLEL DEFAULT(SHARED) PRIVATE(i,k,j,l) NUM_THREADS(numproc)
!$OMP DO
DO i=1,numelements
    DO k=1,4
        !loop over all faces to check against the face
        DO j=1,numelements
            DO l=1,4
                !check against all nodes of a face, if those same nodes are all found on another face it has an adjacency and is not a boundary face
                IF((faces(i,k,1) .EQ. faces(j,l,1)) .AND. (faces(i,k,2) .EQ. &
                faces(j,l,2)) .AND. (faces(i,k,3) .EQ. faces(j,l,3)))THEN
                    adjcount(i,k)=adjcount(i,k)+1
                END IF
            END DO
        END DO
    END DO
END DO
!$OMP END DO
!$OMP END PARALLEL

!at this point any boundary faces will have an adjancency count of 0, and all other faces will have a greater than 0 adjancency count (should be 1 I think, wouldn't make sense to have more than 1...)
!here we tally up the boundary faces as a sanity check, and inform the user of how many faces are boundaries, vs how many faces their are total
totbound=0
DO i=1,numelements
    DO k=1,4
        IF(adjcount(i,k) .EQ. 0)THEN
            totbound=totbound+1
        END IF
    END DO
END DO
WRITE(*,*)totbound, ' boundary faces, ', 4*numelements, ' faces total'

!assign all boundary faces, only assign if the adjancency count for that face is 0
j=0
ALLOCATE(boundaryface(totbound,3))
DO i=1,numelements
    DO k=1,4
        IF(adjcount(i,k) .EQ. 0)THEN
            j=j+1
            boundaryface(j,1)=faces(i,k,1)
            boundaryface(j,2)=faces(i,k,2)
            boundaryface(j,3)=faces(i,k,3)
        END IF
    END DO
END DO

!open outputfile
OPEN(UNIT=11,FILE=fileout,STATUS='REPLACE',ACTION='WRITE',IOSTAT=ios,IOMSG=tempcharacter)
IF(ios .NE. 0)THEN
    WRITE(*,*)tempcharacter
    WRITE(*,*)'Could not open',fileout
    STOP
END IF

!output the original file, we will just append the boundary face data. Probably would be better to do this with a file copy, read to end and append beginning at where the #EndElements delim is....
WRITE(11,'(A)')'$MeshFormat'
WRITE(11,'(A)')'2.0 0 8'
WRITE(11,'(A)')'$EndMeshFormat'
WRITE(11,'(A)')'$Nodes'
WRITE(tempcharacter,*)numnodes
tempcharacter=ADJUSTL(tempcharacter)
WRITE(11,'(A)')TRIM(tempcharacter)

DO i=1,numnodes
    WRITE(tempcharacter,*)i
    tempcharacter=ADJUSTL(tempcharacter)
    WRITE(11,'(A,3ES26.16)')TRIM(tempcharacter),nodes(i,:)
END DO

WRITE(11,'(A)')'$EndNodes'
WRITE(11,'(A)')'$Elements'
WRITE(tempcharacter,*)numelements+totbound
tempcharacter=ADJUSTL(tempcharacter)
WRITE(11,'(A)')TRIM(tempcharacter)

DO i=1,numelements
    WRITE(tempcharacter,*)i
    tempcharacter=ADJUSTL(tempcharacter)
    WRITE(11,'(A)',ADVANCE='NO')TRIM(tempcharacter)
    DO j=1,9
        WRITE(tempcharacter,*)elements(i,j)
        tempcharacter=ADJUSTL(tempcharacter)
        WRITE(11,'(2A)',ADVANCE='NO')' ',TRIM(tempcharacter)
    END DO
    WRITE(11,*)
END DO

!this right here is the new stuff. Boundary faces man...
DO j=1,totbound
    WRITE(tempcharacter,*)i
    i=i+1
    tempcharacter=ADJUSTL(tempcharacter)
    WRITE(11,'(2A)',ADVANCE='NO')TRIM(tempcharacter),' 2 3 0 0 1'
    DO k=1,3
        WRITE(tempcharacter,*)boundaryface(j,k)
        tempcharacter=ADJUSTL(tempcharacter)
        WRITE(11,'(2A)',ADVANCE='NO')' ',TRIM(tempcharacter)
    END DO
    WRITE(11,*)
END DO

WRITE(11,'(A)')'$EndElements'

!close output file
CLOSE(UNIT=11,IOSTAT=ios)
IF(ios .NE. 0)THEN
    WRITE(*,*)'Could not close input file ', fileout
    STOP
END IF

!inform user program has completed.
WRITE(*,'(A)')'Boundary addition complete.'

END PROGRAM boundaryadd