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
