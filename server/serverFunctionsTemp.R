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
