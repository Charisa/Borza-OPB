pridobi.cene.macke <- function(cat, ID){
  # TODO realna funkcija za pridobitev cen
  df <- data.frame(test1 = 1, test2 = cat)
  df
}

pridobi.ime.uporabnika <- function(userID){
  # Pridobi ime vpisanega glede na userID
  tryCatch({
    drv <- dbDriver("PostgreSQL")
    conn <- dbConnect(drv, dbname = db, host = host, user = user, password = password)
    sqlInput<- paste("SELECT username FROM useraccount WHERE userid=",userID)
    
    userid <- dbGetQuery(conn, sqlInput)
  },finally = {
    dbDisconnect(conn)
    return(unname(unlist(userid)))
  }
  )
}
