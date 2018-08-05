
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
    obstoj <- 0
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

user.change.balance <- function(userID, quantity, type){
  tryCatch({
    drv <- dbDriver("PostgreSQL")
    conn <- dbConnect(drv, dbname = db, host = host, user = user, password = password)
    status <- FALSE
    if ((type == "withdraw" | type ==  "bought") & check.wallet.balance(userID) < quantity){
      status <- FALSE
    } else {
      sqlInput <- build_sql("INSERT INTO wallet VALUES (", userID, ",", quantity, ",", type, ");")
      dbGetQuery(conn, sqlInput)
      status <- TRUE
    }
  },warning = function(w){
    print(w)
    status <- "napaka"
  },error = function(e){
    print(e)
    status <- "napaka"
  }, finally = {
    dbDisconnect(conn)
    return(status)
  })
}

check.total.price <- function(cat, quantity){
  tryCatch({
    drv <- dbDriver("PostgreSQL")
    conn <- dbConnect(drv, dbname = db, host = host, user = user, password = password)
    sqlInput1 <- build_sql("SELECT catid FROM cat WHERE breed =", cat, ";")
    catID <- dbGetQuery(conn, sqlInput1)[[1]]
    sqlInput2 <- build_sql("SELECT price, current FROM orderbook WHERE ((catid =", catID, ") AND (current > 0));")
    tabela_cen <- dbGetQuery(conn, sqlInput2)
    status <- 0
    st_mack_na_razpolago <- sum(tabela_cen[2])
    if (st_mack_na_razpolago < quantity) {
      status <- FALSE
    } else {
      counter <- 0
      while (counter < quantity) {
        cena <- min(tabela_cen[1])
        index <- which(tabela_cen[1] == cena)[1]
        tabela_cen[index, 2] <- tabela_cen[index, 2] - 1
        if (tabela_cen[index, 2] == 0) {
          tabela_cen <- tabela_cen[-index,]
          tabela_cen <- bind_rows(tabela_cen)
        }
        status <- status + cena
        counter <- counter + 1
      }
    }
  },warning = function(w){
    print(w)
    status <- "napaka"
  },error = function(e){
    print(e)
    status <- "napaka"
  }, finally = {
    dbDisconnect(conn)
    return(status)
  })
}

post.sell.order <- function(userID, catBreed, price, quantity){
  # Posts a sell order
  # if it was successful, it returns 1, otherwise it returns:
  #   -1 if price was nonpositive
  #   -2 if quantity was nonpositive
  #    0 if there was any other error
  #   -3 if quantity not integer
  tryCatch({
    drv <- dbDriver("PostgreSQL")
    conn <- dbConnect(drv, dbname = db, host = host, user = user, password = password)
    success <- 0
    Q <- quantity
    P <- price
    if (as.integer(quantity) != quantity) {
      status <- -3
    } else if (P > 0 & quantity > 0){
      sqlInput <- build_sql("INSERT INTO orderbook (userid, ordertype, price,quantity , catid, current)
                            SELECT ",userID,", 'sell' , ",P,", ",Q,", catid ,",Q,"
                            FROM  cat
                            WHERE breed = ",catBreed,";")
      dbGetQuery(conn, sqlInput)
      success <- 1
    } else if(P <= 0){
      success = -1
    } else {
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
    if(ncol(macke)<1){
      return(data.frame(breed = character(), price = numeric(), number = integer()))
    }
    return(macke)
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
    zgodovina <- zgodovina %>% mutate(action = ifelse(action=="sold", "bought", "sold"))
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

execute.buy.order <- function(userID_buyer, cat, quantity){
  # kupi po minimalni ceni in spremeni razpolozljivost mack v orderbooku, kjer se current zmanjša za stevilo kupljenih mack
  # lahko se zgodi, da user kupi svojo macko.
  
  izvedi_vmesno_transakcijo <- function(buyer, seller, price, quantity, catID, orderid, st_prodanih) {
    # sqlInputBuyer <- build_sql("INSERT INTO transaction (userid, user2id, ordertype, price, quantity, catid)
    #                       VALUES (", as.integer(buyer), ",", as.integer(seller), ", 'bought',", 
    #                       price, ",", as.integer(st_prodanih), ",", as.integer(catID),");")
    # sqlInputSeller <- build_sql("INSERT INTO transaction (userid, user2id, ordertype, price, quantity, catid)
    #                       VALUES (",as.integer(seller), ",", as.integer(buyer), ", 'sold',", 
    #                             price, ",", as.integer(st_prodanih), ",", as.integer(catID),");")
    # sqlInputUpdateOrderBook <- build_sql("UPDATE orderbook SET current = current -", st_prodanih, "WHERE orderid =", as.integer(orderid), ";")
    # sqlInputUpdateWalletBuyer <- build_sql("INSERT INTO wallet (userid, balance, type) VALUES (", buyer, ",", st_prodanih * price, ", 'bought');")
    # sqlInputUpdateWalletSeller <- build_sql("INSERT INTO wallet (userid, balance, type) VALUES (", seller, ",", st_prodanih * price, ", 'sold');")
    sqlInput <- build_sql("INSERT INTO transaction (userid, user2id, ordertype, price, quantity, catid)
                          VALUES (", as.integer(buyer), ",", as.integer(seller), ", 'bought',", 
                               price, ",", as.integer(st_prodanih), ",", as.integer(catID),");
                          INSERT INTO transaction (userid, user2id, ordertype, price, quantity, catid)
                                VALUES (",as.integer(seller), ",", as.integer(buyer), ", 'sold',", 
                                price, ",", as.integer(st_prodanih), ",", as.integer(catID),");
                          UPDATE orderbook SET current = current -", st_prodanih, "WHERE orderid =", as.integer(orderid), ";
                          INSERT INTO wallet (userid, balance, type) VALUES (", buyer, ",", st_prodanih * price, ", 'bought');
                          INSERT INTO wallet (userid, balance, type) VALUES (", seller, ",", st_prodanih * price, ", 'sold');")
    # return(c(sqlInputBuyer, sqlInputSeller, sqlInputUpdateOrderBook, 
    #          sqlInputUpdateWalletBuyer, sqlInputUpdateWalletSeller))
    return(sqlInput)
  }
  
  tryCatch({
    drv <- dbDriver("PostgreSQL")
    conn <- dbConnect(drv, dbname = db, host = host, user = user, password = password)
    status <- 2
    
    sqlCheck <- build_sql("SELECT EXISTS(SELECT * FROM cat WHERE breed =", cat, ");")
    if (dbGetQuery(con, sqlCheck)[1] == TRUE) {
      
      sqlInput1 <- build_sql("SELECT catid FROM cat WHERE breed =", cat, ";")
      catID <- dbGetQuery(conn, sqlInput1)[[1]]
      sqlInput2 <- build_sql("SELECT orderid, userid, price, current FROM orderbook WHERE ((catid =", catID, ") AND (current > 0)) ORDER BY time ASC ;")
      tabela_cen <- dbGetQuery(conn, sqlInput2)
      
      st_mack_na_razpolago <- sum(tabela_cen[4])
      min_cena <- min(tabela_cen[3])
      index_min_cene <- which(tabela_cen[3] == min_cena)[1]
      userid_seller_min_cene <- tabela_cen[index_min_cene, 2]
      orderid_min_cene <- tabela_cen[index_min_cene, 1]
      status <- 1
      
      if (st_mack_na_razpolago < quantity) {
        status <- 0
      } else {
        counter <- 0 
        kolicina_prodanih <- 0
        while (counter < quantity){
          while ((tabela_cen[index_min_cene, 4] > 0) & (counter < quantity)) {
            kolicina_prodanih <- kolicina_prodanih + 1
            counter <- counter + 1
            tabela_cen[index_min_cene, 4] <- tabela_cen[index_min_cene, 4] - 1
          }
          dbGetQuery(conn, izvedi_vmesno_transakcijo(userID_buyer, userid_seller_min_cene, min_cena, 
                                                     quantity, catID, orderid_min_cene, kolicina_prodanih))
          
          kolicina_prodanih <- 0
          tabela_cen <- tabela_cen[-index_min_cene,]
          min_cena <- min(tabela_cen[3])
          index_min_cene <- which(tabela_cen[3] == min_cena)[1]
          userid_seller_min_cene <- tabela_cen[index_min_cene, 2]
          orderid_min_cene <- tabela_cen[index_min_cene, 1]
        }
      }
    }
  
  },warning = function(w){
    print(w)
  },error = function(e){
    print(e)
  }, finally = {
    dbDisconnect(conn)
    return(status)
  }) }



