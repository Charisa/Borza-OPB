source("lib.R")

source("auth.R")



drv <- dbDriver("PostgreSQL")

deleteTable <- function(tableName,conn){
    dbSendQuery(conn,build_sql("DROP TABLE IF EXISTS ",sql(tableName), " CASCADE"))
}

deleteTables <- function(tableNameList){
  # This function deletes tables with names in tableNameList
  # It requires the following global variables: drv, dbname, host, user, password
  tryCatch({
    # Connection setup
    conn <- dbConnect(drv, dbname = db, host = host, user = user, password = password)
    # If the table exists, we delete it
    lapply(tableNameList, deleteTable, conn)
  }, finally = {
    dbDisconnect(conn)
  })

}

grant <- function(name, conn){
  # This function executes the following GRANT sql commands
  dbSendQuery(conn, build_sql("GRANT ALL ON ALL TABLES IN SCHEMA public TO ", 
                              sql(name), " WITH GRANT OPTION;"))
  dbSendQuery(conn, build_sql("GRANT ALL ON ALL SEQUENCES IN SCHEMA public TO ", 
                              sql(name), " WITH GRANT OPTION;"))
}

grantPublic <- function(name = "javnost", conn){
  # Grants basics rights to public, so that the database can be viewed
  dbSendQuery(conn, build_sql("GRANT CONNECT ON DATABASE sem2018_andrazp TO ", sql(name), ";"))
  dbSendQuery(conn, build_sql("GRANT SELECT ON ALL TABLES IN SCHEMA public TO ", sql(name), ";"))
}
createTables <- function(users = c("sarak, kajav, andrazp")){
  # This function creates the base tables needed the application
  # It requires the following global variables: drv, dbname, host, user, password
  tryCatch({
    # Connection setup
    conn <- dbConnect(drv, 
                      dbname = db, 
                      host = host,
                      user = user,
                      password = password)
    
    # Main tables. If they do not already exist, this function creates them (working for
    # PostgreSQL ver. 9.1 and over)
    # user is a reserved name, therefore we user userAccount
    dbSendQuery(conn, build_sql("CREATE TABLE IF NOT EXISTS userAccount (
                                userID SERIAL PRIMARY KEY,
                                name TEXT,
                                surname TEXT,
                                address TEXT,
                                city TEXT,
                                country TEXT,
                                EMSO TEXT,
                                mail TEXT,
                                username TEXT,
                                password TEXT);"
                                ))
    
    dbSendQuery(conn, build_sql("CREATE TABLE IF NOT EXISTS cat (
                                catID INTEGER PRIMARY KEY,
                                breed TEXT);"
                                ))
    
    dbSendQuery(conn, build_sql("CREATE TABLE IF NOT EXISTS transaction (
                                transactionID INTEGER PRIMARY KEY,
                                userID INTEGER REFERENCES userAccount (userID),
                                orderType TEXT,
                                price FLOAT,
                                quantity INTEGER,
                                catID INTEGER REFERENCES cat (catID),
                                time DATE);"
                                ))
    
    dbSendQuery(conn, build_sql("CREATE TABLE IF NOT EXISTS orderbook (
                                orderID INTEGER PRIMARY KEY,
                                userID INTEGER REFERENCES userAccount (userID),
                                orderType TEXT,
                                price FLOAT,
                                quantity INTEGER,
                                catID INTEGER REFERENCES cat (catID),
                                time DATE,
                                status TEXT)"
                                ))
    
    lapply(users, grant, conn)
    grantPublic(conn = conn)
    
  }, finally = {
    dbDisconnect(conn)
  })
}

deleteTables(c("userAccount", "cat", "transaction","orderbook"))
createTables()


source("auth_public.R")