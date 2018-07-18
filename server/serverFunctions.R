
sign.up.user <- function(name, surname, address, city, country, emso, mail, username, pass){
  # Return values:
  # 1 ... success
  # 0 ... error
  # -10 ... username exists
  success <- 0      # Boolean, if the insertion into the db was successful
  
  useraccount <- data.frame(name, surname, address, city, country, emso, mail, username, password=pass)
  tryCatch({
    drv <- dbDriver("PostgreSQL")
    conn <- dbConnect(drv, dbname = db, host = host, user = user, password = password)
    userTable <- tbl(conn, "useraccount")
    # Checks if username is already in the database
    if(0 != dim((userTable %>% filter(username == username) %>% collect()))[1]){
      success <- -10
    }
    dbWriteTable(conn,name="useraccount", useraccount, append=TRUE, row.names = FALSE)
    success <- 1
    
  }, finally = {
    dbDisconnect(conn)
    return(success)
  })
}

sign.in.user <- function(username, pass){
  # Return a list. In the first place is an indicator of success:
  # 1 ... success
  # 0 ... error
  # -10 ... wrong username
  # The second place represents the userid if the login info is correct,
  # otherwise it's NULL
  success <- 0
  uporabnikID <- NULL
  tryCatch({
    drv <- dbDriver("PostgreSQL")
    conn <- dbConnect(drv, dbname = db, host = host, user = user, password = password)
    userTable <- tbl(conn, "useraccount")
    # obstoj = 0, ce username in geslo ne obstajata,  1 ce obstaja
    obstoj <- dim((userTable %>% filter(username == username, password == pass) %>% collect()))[1]
    if(obstoj == 0){
      success <- -10
    }else{
      uporabnikID <- (userTable %>% filter(username == username, password == pass) %>%
                        select(userid) %>% collect())[[1]]
      success <- 1
    }
  },warning = function(w){
    print(w)
  },error = function(e){
    print(e)
  }, finally = {
    dbDisconnect(conn)
    return(list(success, uporabnikID))
  })
}

pridobi.imena.mack <- function(){
  # Pridobi imena mack iz baze in jih vrne
  tryCatch({
    drv <- dbDriver("PostgreSQL")
    conn <- dbConnect(drv, dbname = db, host = host, user = user, password = password)
    sqlInput<- paste("SELECT DISTINCT breed FROM cat")
    
    macke <- dbGetQuery(conn, sqlInput)
  },finally = {
    dbDisconnect(conn)
    return(unname(unlist(macke)))
  }
  )
}

execute.buy.order <- function(cat, price, quantity, userID){
  tryCatch({
    drv <- dbDriver("PostgreSQL")
    conn <- dbConnect(drv, dbname = db, host = host, user = user, password = password)
    userTable <- tbl(conn, "useraccount")
    #TODO
  },warning = function(w){
    print(w)
  },error = function(e){
    print(e)
  }, finally = {
    dbDisconnect(conn)
    #TODO
  })
}
 
check.wallet.balance <- function(userID){
  tryCatch({
    drv <- dbDriver("PostgreSQL")
    conn <- dbConnect(drv, dbname = db, host = host, user = user, password = password)
    sqlInput <- build_sql("SELECT SUM(CASE WHEN (type = 'deposit' or type = 'sold') THEN 1 ELSE -1 END * balance) AS balance 
                                    FROM wallet WHERE userid = ", userID,";")
    balance <- dbGetQuery(conn, sqlInput)
  },warning = function(w){
    print(w)
  },error = function(e){
    print(e)
  }, finally = {
    dbDisconnect(conn)
    return(balance[[1]])
  })
}



