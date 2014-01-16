# Processor for latlon pkg
# =========================
#
# Take points file and process down to a single point
# while conserving volume.

# input  -  a points matrix (latitude-longitude-volume) (n x 3)
# output - a vdm object containing the elimination matrix and
#           the volume-distance-matrix (vdm)

# changes:
# 7/13 - need to put the original data in vol.dist.mat

fill.up = 10  # number of gallons of average fill up.
time.cost = 15 # cost per hour of travel  ($)
mph = 25 # average mph

mile.cost = time.cost / mph # the cost per mile of travel

proc.latlon = function(ps,city.name=NA,do.plot=TRUE){
    conserved.vol = sum(ps[,4]) # all subsequent vol sums must equal this
    output = list()
    vol.dist.mat = NULL
    vol.dist.rows = NULL
    total.time.cost = 0
    time.cost.df = as.data.frame(matrix(NA,nrow=nrow(ps),ncol=4))
    names(time.cost.df) = c("elim.stn","kept.stn","distance","cost")
  
    elim.mat = matrix(-1,nrow(ps),2)
    i = 1
   # Get latlon obj
   dat = new.latlon(ps)
   go = TRUE
   while( go == TRUE){
     
     elim.mat[i,] = c(nrow(dat$ps),dat$min.dist)
     i = i+1
    
     # store info
     # Find the min dist value.
    
     # Find the frequency count at that dist.
     if(dat$freq > 1){
        warning("There were ", dat$freq, " instances of min dist ", dat$min.dist,
              " found.")
        }
     # If the freq count is > 1 store all that
     #
     # info for later analysis.
     #  TODO
     
     # Look at the two stations with min distance.
     
     # The ROW entry is wrong - dat$st1 is not the row index, it's the
     # value in in col 1. !!!
 
     v1 = dat$ps[dat$ps[,1]==dat$st1,4]      # get volume station 1
     v2 = dat$ps[dat$ps[,1]==dat$st2,4]      # " " "        ""    2
     
     # get the row number for st1 and st2

     loc.1 = grep(paste("^",dat$st1,"$",sep=''), dat$ps[,1])
     loc.2 = grep(paste("^",dat$st2,"$",sep=''), dat$ps[,1])
     
     
     if(dat$n > 1){  # need to handle final station !!!

       my.ps = as.data.frame(dat$ps)

       save.vols = subset(my.ps, vol > 0, 
            select=c(latitude,longitude,vol))
        rm(my.ps)
       if(v1 >= v2){
        # v2 has been eliminated
        # displace v2 
        #total.time.cost = total.time.cost + (v2/fill.up)*dat$min.dist*time.cost
        marginal.time.cost = (v2/fill.up)*dat$min.dist*time.cost
        total.time.cost = total.time.cost + marginal.time.cost
        time.cost.df[i,] = c(dat$st2, dat$st1,dat$min.dist, marginal.time.cost)
        # add lesser volume to greater
        dat$ps[loc.1,4] =  dat$ps[loc.1,4] + v2
        dat$ps[loc.2,4] = 0

        
        # remove lesser volume from the dat
        dat = update(dat,dat$st2)
       } else if ( v1 < v2) {
        #total.time.cost = total.time.cost + (v1/fill.up)*dat$min.dist*time.cost
        marginal.time.cost = (v1/fill.up)*dat$min.dist*time.cost
        total.time.cost = total.time.cost + marginal.time.cost
        time.cost.df[i,] = c(dat$st1,dat$st2, dat$min.dist, marginal.time.cost)
        
        dat$ps[loc.2,4] =  dat$ps[loc.2,4] + v1
        dat$ps[loc.1,4]=0
        dat = update(dat,dat$st1)
       } 
       cat(i,": mean",mean(dat$mat[,3])," :median",median(dat$mat[,3])," :sd",sd(dat$mat[,3]),"\n")
       
       if( v1 == v2){
        # volume conservation is handle above
        cat("station 1: ", dat$st1, " volume: ", v1,"\n")
        cat("station 2: ", dat$st2, " volume: ", v2,"\n")

        warning("Equal station volumes. Kept station 1.")

      }
      if(sum(dat$ps[,4],na.rm=TRUE) != conserved.vol){
          cat("ERROR: DEBUGGING MODE: conserved.vol= ", conserved.vol,"\n")
          cat("i = ", i, " this volume = ", sum(dat$ps[,4],na.rm=TRUE) ,"\n")
          browser() 
      } 
      out = found.boundary(dat$min.dist,dat$old.min.dist)
      if(out$bool == TRUE){
        if(is.null(vol.dist.mat)){

          vol.dist.mat = ps[,2:4]
          names(vol.dist.mat)[1:3] = set.names("orig")
          
          vol.dist.rows = nrow(ps)
          #names(vol.dist.mat) = c("lat","long",out$boundary)
        } 
        # pad save.vols out to lenth of vol.dist.rows

        
        # Now we need to pad the save.vols matrix out with NAs
        # The pad length is:

        pad.len = vol.dist.rows - nrow(save.vols)
        na.mat = as.data.frame(matrix(NA,nrow=pad.len,ncol=3))
        names(na.mat) = c("latitude","longitude","vol")
        # We must keep the names of the NA matrix the same as
        # in the save.vols in order to append them.
        # Use an rbind to append.
        
        
        new.cols = as.data.frame(rbind(save.vols,na.mat))
        names(new.cols) = set.names(out$boundary)
        vol.dist.mat = as.data.frame(cbind(vol.dist.mat,new.cols))
    

        
      }
        
    } else {
      go = FALSE # ending
      i = nrow(elim.mat)-1
      # get the distance between the last two stations
      #! Get look up points
        xlong = dat$ps[1,3]
#          
        xlat = dat$ps[1,2]
        ylong = dat$ps[2,3]
        ylat = dat$ps[2,2]
#          
#! use great circle distance for improved accuracy
        x = 69.1 * (ylat - xlat)
#
        y = 69.1 * (ylong - xlong) * cos(xlat/57.3)
#
#          
#
        final.dist = sqrt(x*x + y*y)

        v1 = dat$ps[1,4]
        v2 = dat$ps[2,4]
        
        if(v1 > v2){
          # second row is minimum station
          elim.mat[i,]= c(2, final.dist)
          elim.mat[(i+1),] = c(1, NA)
        } else {
          # ignoring the case in which v1 = v2
          # because if it happens we don't care
          elim.mat[i,]= c(2, final.dist)
          elim.mat[(i+1),] = c(1, NA)
        }  
        
    }
 
     # The station with the lower volume should be eliminated,
     # its volume should be added to the higher vol station.
     # Eliminate low volume station.
     # Store station count and elimination distance
     # Continue while loop.
     #print(summary(dat))
     if(do.plot == TRUE){
             
            if(length(dat$mat) > 3){
              plot(dat)
            }
     }       
   }
   output$elim.mat = elim.mat
   output$vol.dist.mat = vol.dist.mat
   output$total.time.cost = total.time.cost
   output$city.name = city.name
   output$time.cost = time.cost.df
   class(output) = "vdm"
   return(output)
}