! Simple update distance matrix routine

! Given the distance matrix and a station id, remove
! all rows that include the station id and return the matrix.

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Input: distMat - lookup distance matrix, double precision (dn x 3)
!        dn - row count of distMat, integer
!        stID - station id to remove, integer
!        returnMat - return matrix w/o stID, double precison (rn x 3)
!        rn - row count of returnMat, integer

! Function:  Remove all rows from distMat that contain stID in col 1 or col 2.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      SUBROUTINE update(distMat,dn,stID,returnMat,rn)

      implicit none
      
      ! args to function
      integer dn,rn,stID
      double precision distMat(dn,3)
      double precision returnMat(rn,3)
      
      ! internal variables
      integer i
      