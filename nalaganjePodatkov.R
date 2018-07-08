library(DBI)
library(dplyr)
library(dbplyr)
source("auth.R")

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
