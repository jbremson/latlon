! latlon calculator
! input: a latitude longitude point matrix (n x 3)
!         format = station id | lat | long 
! output: a lookup distance matrix - this allows us to look up 
!         directly the distance between any two points.
!         format = station id1 | station id2 | distance (n! x 3)
!         ALSO, minDist between and station pair with that minDist
      SUBROUTINE latlon(points, n, output, nsum, minDist, st1, st2)
      
      implicit none
      integer n,stx,sty,count,i,j,nsum,st1,st2,freq
      

      double precision points(n,3)
      double precision output(nsum,3)
      double precision xlong,xlat,ylong,ylat,x,y,minDist,dist
      
      count = 1 ! this is the row of the output matrix
      freq = 1 ! this is the frequency count of this exact distance
! Iterate over the output matrix diagonal, col .

      do i = 1,n-1
        stx = points(i,1)
        
        xlong = points(i,3)
        
        xlat = points(i,2)
        
        
        do j = i+1,n
          ! Get look up points
          sty = points(j,1)
          
          ylong = points(j,3)
          ylat = points(j,2)
          
          
! use great circle distance for improved accuracy
          x = 69.1 * (ylat - xlat)

          y = 69.1 * (ylong - xlong) * cos(xlat/57.3)
          output(count,1) = stx
          output(count,2) = sty
          
          dist = sqrt(x*x + y*y)
          output(count,3) = dist
          if (count .EQ. 1) then
            minDist = dist
            goto 50
          end if
          
          if (dist .LT. minDist) then
            minDist = dist
            st1 = stx
            st2 = sty
            freq = freq + 1
          end if
50        count = count + 1
        end do
      end do
        



      END
    