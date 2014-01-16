# db.batch.r

# This is batch.r except it runs off the database instead of the file
# system. Yeah!!!

# All data should be written to the database, with perhaps, a flag
# to send stuff to file system as .xls csv files.

# batch script

# input: my.city - the city name in the database
#        state.id - if there is a conflict in city names the state abrev
#                   resolves it.         
#        write.xls - boolean, should the output be written to a csv file
#                    in the current working directory?
#         write.db - boolean, should we write results to the database?

super.batch = function(){
  for (i in 1:86){           
    db.batch(city.id = i)
    cat("Completed city ", i, " of 86\n")
  
  }
  cat("\n***done***\n")

}
db.batch = function(city.id = NA, my.city = NA, state.abrev = NA, write.xls = FALSE
                    , write.db = TRUE){
                    
    #browser()
    library("latlon")
    
    if (write.xls == FALSE & write.db == FALSE){
      stop("No output. write.xls and write.db are both set to FALSE.")
    }
    
    if(is.na(my.city) & is.na(city.id)){
      stop("Set a city to execute on.")
    }
    
    if(write.xls == TRUE){
      cat("Set working dir now (File-> Change dir...)\n")
      #input = scan()
      stop("Option not implemented.")
    }
    
    # set up the database handle
    
    # setup a db connection

    h2.handle = function(){
      library("RMySQL")

      drv=dbDriver("MySQL")

      con = dbConnect(drv,dbname="h2",user="joel",password="joel")
      return(con)
    }
    
    con = h2.handle()
    
    
    # setup the database select
    
    # Find the city_id using the city name and state.abrev

    
    state.id = -1
    if(is.na(city.id)){
      if(!is.na(state.abrev)){
        query = paste("SELECT state_id FROM states WHERE state = '",state.abrev,"'",sep="")
        out = dbGetQuery(con,query)
        state.id = out$state_id
      }
      # Now that we have the state_id, get the city_id
      # This only requires getting the first part of the name for a city correct
      
      if(is.null(state.id) | is.na(state.abrev)){ # note that bad state abrev is
                                                   # treated as no state.abrev
                                                   
          # just try to match the city name                                          
          query = paste("SELECT city,city2_id FROM city2 WHERE city LIKE '", my.city,"%'",sep="")
      } else {
      
        query = paste("SELECT city,city2_id FROM city2 WHERE city LIKE '", my.city,
                "%' AND state_id =", state.id ,sep="")
      }
      out = dbGetQuery(con,query)
      if(nrow(out) != 1){
        cat(my.city," : invalid entry - results:\n")
        print(out)
        dbDisconnect(con)                               
        stop("Quiting")
      }
      city2_id = out$city2_id
      city.name = out$city
    } else {
       # this branch executed if city2_id is provided
      query = paste("DELETE FROM cluster WHERE city2_id = ", city.id)
      out = dbSendQuery(con,query)
      query = paste("DELETE FROM time_cost WHERE city2_id = ", city.id)
      out = dbSendQuery(con,query)
      query = paste("SELECT city FROM city2 WHERE city2_id = ", city.id, sep ="")
      out = dbGetQuery(con,query)
      city.name = out$city
      city2_id = city.id
    }
    # The select is, SELECT station_id, lat,long, vol, WHERE city_id = something 
    query = (paste("SELECT mpsi_id,latitude,longitude,volume FROM mpsi_raw_data",
                    " WHERE city2_id = ", city2_id, " ORDER BY volume", sep=''))
    
    out = dbGetQuery(con,query)
    browser()
    if(nrow(out) > 0){
      
      # match names to proc.latlon data structure
      names(out) = c("stid","latitude","longitude","vol")
    
      # execute proc.latlon on resulting data set
      
      o = proc.latlon(out,city.name)
      
      time.cost = o$time.cost
      
      # put time.cost into time_cost table
      time.cost = na.omit(time.cost)
      
      sql = paste("INSERT INTO time_cost(city2_id, elim, kept, distance, cost, volume)",
                  " VALUES ")
      
      for(i in 1:nrow(time.cost)){
        # 10/15 - why did i use 5/3 here?
        # I think it should be 2/3 to get back to original volume
        # vol = (5/3) * (time.cost[i,4]/time.cost[i,3])
        
        # fixing bug - next line 10/15/2007 - jb
        
        # FLIPPED IT - IT'S RIGHT NOW - 
        
        # this should be the real line , I think
         vol = (2/3) * (time.cost[i,4]/time.cost[i,3])
        # I'm going to convert my city 17 back to the wrong data
        # and then convert all my time_costs values in the db
        # to the 2/3 number by multiplying them by 2/5,
        # i.e. 2/5 * 5/3 x = 2/3 x 
        
        # above - vol = 10/15 * marginal.time.cost/distance
        vals = paste("(",city2_id,",",time.cost[i,1],",",time.cost[i,2],",",
                      time.cost[i,3],",",time.cost[i,4],",",vol,")",sep='')
          if(i == 1){
            my.sep = " "
          } else {
            my.sep = ","
          }
          sql = paste(sql,vals,sep=my.sep) 
        
      
      
      }
      print(sql)
      dbSendQuery(con,sql)
      # write the results to db and/or xls files
      
      results = o$vol.dist.mat
      
      
      results.sets = ncol(results)/3
      for(i in 1:results.sets){
        
        start.col = i*3-2
        end.col = i*3
        set = na.omit(results[,start.col:end.col])
        # get the distance - only have to do it once
        dist.name = names(set)[1]
        distance = as.numeric(substr(dist.name,5,10))
        if (is.na(distance)){
          distance = 0
        }
        sql = "INSERT INTO cluster(city2_id, mpsi_id, distance, volume) VALUES "
        for(j in 1:nrow(set)){
          
          # find the mpsi_id
          #pull the mpsi_id from the subset out based on a lookup of lat, long
          mpsi_id = subset(out,latitude==set[j,1]&longitude==set[j,2],stid)
          vals = paste("( ", city2_id,",", mpsi_id, ",", distance, "," , set[j,3],")")
          if(j == 1){
            my.sep = " "
          } else {
            my.sep = ","
          }
          sql = paste(sql,vals,sep=my.sep) 
        }    
        #print(sql)
        result.set = dbSendQuery(con,sql)
        # cluster table
        # | dist_id (auto inc) | city2_id | mpsi_id(stid) | distance | volume (vol)|
        
        # get mpsi_id from stid by looking up lat and long in out table
        # stid = ...
        
        # get distance --- have to be careful here because the data is jagged
        # and unusual in pattern - have to skip across in 3 col subsets at a time.
            #vals = paste(city2_id, stid) 
        
           
      
      }
    } else {
      cat("No results to process for city2_id ",city2_id,"\n")
    }
    
    # close the db handle
    dbDisconnect(con)
    return()    
} # END CODE

    
    ##### OLD STUFF #######################################
#    # first columns will be 1,6,11,16 = 1 + 5*i
#    len = length(names(dat))/5
#    for(i in 0:(len-1)){
#    
#      j = 1 + (i*5)
#      work.set = dat[,j:(j+3)]
#      file.name = names(work.set)[1]
#      xls.name = paste(file.name,".xls",sep='')
#      cat("Starting ", file.name, " .\n")
#      if(!file.exists(xls.name)){
#        lat.col = grep("^latitude",names(work.set),ignore.case=TRUE)
#        lon.col = grep("^longitude",names(work.set),ignore.case=TRUE)
#        vol.col = grep("^volume",names(work.set),ignore.case=TRUE)
#        stid.col = 1
#        my.ps = cbind(work.set[stid.col],work.set[lat.col],work.set[lon.col],
#                        work.set[vol.col])
#        # remove NAs
#        my.ps = my.ps[!is.na(my.ps[,2]),]
#        names(my.ps) = c("stid","latitude","longitude","vol")
#        # rows variable is just used for testing
#        rows=nrow(my.ps)
#        if (rows > 40){
#          rows = 40
#        }
#        o = proc.latlon(my.ps[1:rows,])
#        cat("writing ", file.name ,"...\n")
#      
#        write.table(o$vol.dist.mat,xls.name,row.names=FALSE)
#        save(o,file=paste(file.name,".rda",sep=''))
#        # write the file to an output
#        print(head(my.ps))
#     } else {
#      cat("Skipping ", file.name , ". Delete it from working dir to run again.\n")
#     }
#      
#    }
#}