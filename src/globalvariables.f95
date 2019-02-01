!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
! GMESH BOUNDARY ADDING UTILITY
!   Global Variables Module:
!
!>    This module contains variables used to store data common to all other
!!    modules in the utility.
!
!> @author Nicholas Herring
!> @version 1.0
!> @date February, 2018
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
MODULE globalvariables
    IMPLICIT NONE

    !> Mesh in filename
    CHARACTER(64)::filein
    !> Mesh out filename
    CHARACTER(64)::fileout
    !> Temporary data holder for the whole system
    CHARACTER(64)::tempcharacter
    !> Number of processors for openmp calculations
    !$ INTEGER::numproc
    !> Number of arguments in the command line
    INTEGER::arg_count
    !> Number of nodes in the mesh
    INTEGER::numnodes
    !> Number of elements in the mesh
    INTEGER::numelements
    !> File error detector
    INTEGER::ios
    !> Number of bounds in the mesh
    INTEGER::totbound
    !> Loop control variables
    INTEGER::i,j,k,l
    !> Element information, entries 6-9 are nodes, allocate as (numelements,9)
    INTEGER,ALLOCATABLE::elements(:,:)
    !> All faces present in the problem with first dimension being element,
    !! second being different faces on element, and third being nodes of the
    !! face allocate as (numelements,4,3)
    INTEGER,ALLOCATABLE::faces(:,:,:)
    !> Adjacency count for every face in the problem allocate as (numelements,4)
    INTEGER,ALLOCATABLE::adjcount(:,:)
    !> Boundary faces of the mesh allocate as (totbound,3)
    INTEGER,ALLOCATABLE::boundaryface(:,:)
    !> Node coordinates, x,y,z, for each node, allocated as (numnodes,3)
    REAL(8),ALLOCATABLE::nodes(:,:)
END MODULE globalvariables
