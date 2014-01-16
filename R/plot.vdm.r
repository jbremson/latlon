# plot.vdm

# input: a vdm object from proc.latlon
# output: a plot set of the vdm

plot.vdm = function(obj){

  vdm = obj$vol.dist.mat
  elim = obj$elim.mat
  
  vdm.vol.cols = ncol(vdm)/3
  palette(rainbow(vdm.vol.cols))
 # setup par call
 par(mfrow=c(1,1),ask=FALSE)
 # setup with an empty plot

 # plot 1 : y axis  :   percent of average station output
 #          x axis  :   percent of stations remaining
 
 # plot 1 has a general shape of e^(1/x) - 1; it goes to infinity
 #        as the x axis decreases, and 0 as it increases
 
 # There is a line for each distance cluster.
 for(i in 1:vdm.vol.cols){

    j = i*3
    txt = gsub("^vol-","",names(vdm)[j])
   # Start with the first volume set.
   vol.col = vdm[!is.na(vdm[,j]),j]
   # ps.n = number of stations in first volume set
   ps.n= length(vol.col)
   # ps.ave = average station size
   ps.ave = mean(vol.col)
   # point 1 y.val is found by ...
       # divide vol.col by ps.ave
   y.val = (vol.col/ps.ave) * 100
   # point 1 x.val is found by ...
   
       # x.val is a reversed percentile - 1% is high, 100% is low.
       # We need to sort the y.val vector, high to low.
   y.val = sort(y.val,dec=TRUE)
       # We then take 1/ps.n as separation value and create
       # a sequence from 0 to 1. Those are the x.vals.
   inc = 1/ps.n
   x.val = seq(1:ps.n) * inc * 100
   # plot the line
   if(i == 1){
    plot(x.val,y.val,type="l",col=i+3,xlab="Percent of Remaining Stations",
          ylab="Percent of Average Station Output")
   } else {
   # repeat for all points
    points(x.val,y.val,type="l",col=i+3)
   }
   text(x.val[1]+(-1)^i*2,y.val[1]+(-1)^(i+1)*2,labels=txt,col=i+3)
 }
   
 # plot line ( this will be an over plot)
 
 # repeat for all volume sets.



}











