library(DBI)
library(dplyr)
library(dbplyr)
library(bcrypt)
source("auth.R")
source("server/serverFunctions.R")

addUsers <- function(number = 8){
  lapply(1:number, function(x) sign.up.user(paste0("name",x), paste0("surname",x),
                                            paste0("address",x), paste0("city",x),
                                            paste0("country",x), paste0("emso",x),
                                            paste0("mail",x), paste0("username",x),
                                            paste0("password",x))
         )
}
addUsers()


df <- data.frame(userid=c(1,1,2,4,5,3), balance=c(100,50,1000,3,1,890), 
                 type=c("deposit", "withdrawal", "deposit","deposit", "deposit", "deposit"))
dbWriteTable(conn, name="wallet", df,append=TRUE, row.names=FALSE)