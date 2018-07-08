library(DBI)
library(dplyr)
library(dbplyr)
source("auth.R")
source("server/serverFunctions.R")

addCats <- function(){
  tryCatch({
    # Connection setup
    drv <- dbDriver("PostgreSQL")
    conn <- dbConnect(drv, 
                      dbname = db, 
                      host = host,
                      user = user,
                      password = password)
    
    mackeTable <- data.frame(catid = c(1,2,3,4,5),
                             breed = c("Chartreoux", "Persian", "Korat", "Rusian Blue", "Maine Coon"))
    dbWriteTable(conn,name="cat", mackeTable, append=TRUE, row.names = FALSE)
  },finally = {
    dbDisconnect(conn)
  }
  )
}
addCats()
addUsers <- function(number = 8){
  lapply(1:number, function(x) sign.up.user(paste0("name",x), paste0("surname",x),
                                            paste0("address",x), paste0("city",x),
                                            paste0("country",x), paste0("emso",x),
                                            paste0("mail",x), paste0("username",x),
                                            paste0("password",x))
         )
}
addUsers()
