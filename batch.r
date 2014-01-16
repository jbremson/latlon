#Make a directory with all files, or, figure out how to cut up the
#original input file. The latter is probably the best way to go.
# -> Take the original file and edit into a good, easy, standard shape.
#
#From each of the files we need the city name.
#
#Set working dir to output.
#
#Grab next 3 columns of input file. Get the name.
#
#Run the proc.latlon on the columns ( o = proc.latlon)
#
#Write the results to cur dir (write(o,'city name')
#
#We are done.
#
#

# batch script

cat("Set your working directory.!!\n")

dat = read.csv(file.choose(),header=TRUE)

# Data sets are 4 columns wide and separated by an empty column.
# The first column header is the city name and other info.
# The next three are Latitude, Longitude and volume.
# They can be in any order.

cat("Set working dir now\n")
input = scan()
# first columns will be 1,6,11,16 = 1 + 5*i
len = length(names(dat))/5
for(i in 0:(len-1)){

  j = 1 + (i*5)
  work.set = dat[,j:(j+3)]
  file.name = names(work.set)[1]
  xls.name = paste(file.name,".xls",sep='')
  cat("Starting ", file.name, " .\n")
  if(!file.exists(xls.name)){
    lat.col = grep("^latitude",names(work.set),ignore.case=TRUE)
    lon.col = grep("^longitude",names(work.set),ignore.case=TRUE)
    vol.col = grep("^volume",names(work.set),ignore.case=TRUE)
    stid.col = 1
    my.ps = cbind(work.set[stid.col],work.set[lat.col],work.set[lon.col],
                    work.set[vol.col])
    # remove NAs
    my.ps = my.ps[!is.na(my.ps[,2]),]
    names(my.ps) = c("stid","latitude","longitude","vol")
    # rows variable is just used for testing
    rows=nrow(my.ps)
    if (rows > 40){
      rows = 40
    }
    o = proc.latlon(my.ps[1:rows,])
    cat("writing ", file.name ,"...\n")
  
    write.table(o$vol.dist.mat,xls.name,row.names=FALSE)
    save(o,file=paste(file.name,".rda",sep=''))
    # write the file to an output
    print(head(my.ps))
 } else {
  cat("Skipping ", file.name , ". Delete it from working dir to run again.\n")
 }
  
}