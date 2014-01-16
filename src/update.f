! Simple update distance matrix routine

! Given the distance matrix and a station id, remove
! all rows that include the station id and return the matrix.

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Input: distMat - lookup distance matrix, double precision (dn x 3)
!        dn - row count of distMat, integer
!        stID - station id to remove, integer
!        returnMat - return matrix w/o stID, double precison (rn x 3)
!        rn - row count of returnMat, integer
!             rn =  .5 * (dn^2 - dn) # sum (i..(dn-1))

! Function:  Remove all rows from distMat that contain stID in col 1 or col 2.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      SUBROUTINE update(distMat,dn,stID,returnMat,rn)

      implicit none
      
      ! args to function
      integer dn,rn,stID
      double precision distMat(dn,3)
      double precision returnMat(rn,3)
      
      
      ! internal variables
      integer di  ! this is the counter for the distMat
      integer ri  ! this is the counter for the returnMat
      integer stCmp1,stCmp2 ! the comparison variable
      double precision dist ! the distance between the points
      
      ri = 1
      DO di = 1, dn
        ! Get  (di,1) from distMat
        stCmp1 = distMat(di,1)
        ! If it equals stID break
        if(stID .EQ. stCmp1) goto 10
        ! Get (di,2) from distMat
        stCmp2 = distMat(di,2)

        if (stID .EQ. stCmp2) goto 10


        ! Else returnMat row ri = distMat row row di
        returnMat(ri,1) = stCmp1
        returnMat(ri,2) = stCmp2
        dist = distMat(di,3)
        returnMat(ri,3) = dist
        ! Increment ri

        ri = ri + 1
10      CONTINUE
      END DO
      
      END SUBROUTINE update
     
