# setup a db connection

h2.handle = function(){
  library("RMySQL")

  drv=dbDriver("MySQL")

  con = dbConnect(drv,dbname="h2",user="joel",password="joel")
}