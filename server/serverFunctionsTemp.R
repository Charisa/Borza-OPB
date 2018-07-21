pridobi.cene.macke <- function(cat){
  tryCatch({
    drv <- dbDriver("PostgreSQL")
    conn <- dbConnect(drv, dbname = db, host = host, user = user, password = password)
    sqlInput<- build_sql("SELECT breed, price, SUM(current) AS number FROM orderbook 
                          LEFT JOIN cat ON orderbook.catid = cat.catid
                          WHERE current>0 AND cat.breed = ",cat,"
                          GROUP BY breed, price
                          ORDER BY price ASC;")
    
    macke <- dbGetQuery(conn, sqlInput)
  },finally = {
    dbDisconnect(conn)
    return(macke)
  }
  )
}

pridobi.ime.uporabnika <- function(userID){
  # Pridobi ime vpisanega glede na userID
  tryCatch({
    drv <- dbDriver("PostgreSQL")
    conn <- dbConnect(drv, dbname = db, host = host, user = user, password = password)
    sqlInput<- build_sql("SELECT username FROM useraccount WHERE userid=",userID)
    
    userid <- dbGetQuery(conn, sqlInput)
  },finally = {
    dbDisconnect(conn)
    return(unname(unlist(userid)))
  }
  )
}

pridobi.zgodovino.transakcij <- function(userID){
  # Pridobi zgodovino transakcij za uporabnika userID
  tryCatch({
    drv <- dbDriver("PostgreSQL")
    conn <- dbConnect(drv, dbname = db, host = host, user = user, password = password)
    sqlInput<- build_sql("SELECT transaction.transactionid AS id, 
                         useraccount.username, useraccount.mail, 
                         transaction.ordertype AS action, cat.breed, 
                         transaction.price, transaction.quantity FROM transaction 
                         LEFT JOIN useraccount ON transaction.user2id = useraccount.userid 
                         LEFT JOIN cat ON transaction.catid=cat.catid WHERE 
                         transaction.userid = ",userID)
    
    zgodovina <- dbGetQuery(conn, sqlInput)
  },finally = {
    dbDisconnect(conn)
    if(ncol(zgodovina)<1){
      return(data.frame(id=numeric(), username=character(), mail=character(), 
                        action=character(), breed=character(), price=numeric(), 
                        quantity=integer()))
    }
    return(zgodovina)
  }
  )
}
sign.up.user <- function(name, surname, address, city, country, emso, mail, username, pass){
  # Return values:
  # 1 ... success
  # 0 ... error
  # -10 ... username exists
  success <- 0      # Boolean, if the insertion into the db was successful
  
  # hashing of password
  pass <- hashpw(pass)
  uporabnik <- username
  useraccount <- data.frame(name, surname, address, city, country, emso, mail, username, password=pass)
  tryCatch({
    drv <- dbDriver("PostgreSQL")
    conn <- dbConnect(drv, dbname = db, host = host, user = user, password = password)
    userTable <- tbl(conn, "useraccount")
    # Checks if username is already in the database
    if(0 != dim((userTable %>% filter(username == uporabnik) %>% collect()))[1]){
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
    uporabnik <- username
    geslo <- (userTable %>% filter(username == uporabnik) %>% select(password) %>% collect())[[1]]
    
    if(checkpw(pass,geslo)){
      obstoj <- 1
    }
    if(obstoj == 0){
      success <- -10
    }else{
      uporabnikID <- (userTable %>% filter(username == uporabnik) %>%
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

post.sell.order <- function(userID, catBreed, price, quantity){
  # Posts a sell order
  # if it was successful, it returns 1, otherwise it returns:
  #   -1 if price was nonpositive
  #   -2 if quantity was nonpositive
  #    0 if there was any other error
  tryCatch({
    drv <- dbDriver("PostgreSQL")
    conn <- dbConnect(drv, dbname = db, host = host, user = user, password = password)
    success <- 0
    Q <- quantity
    P <- price
    if(P>0 & quantity>0){
      sqlInput <- build_sql("INSERT INTO orderbook (userid, ordertype, price,quantity , catid, current)
                              SELECT ",userID,", 'sell' , ",P,", ",Q,", catid ,",Q,"
                              FROM  cat
                              WHERE breed = ",catBreed,";")
      dbGetQuery(conn, sqlInput)
      success <- 1
    }else if(P<=0){
      success = -1
    }else{
      success <- -2
    }
  },warning = function(w){
    print(w)
  },error = function(e){
    print(e)
  }, finally = {
    dbDisconnect(conn)
    return(success)
  })
  
}
