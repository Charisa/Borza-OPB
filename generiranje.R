install.packages('dplyr')

df <- read.csv2("podatki.csv", header = TRUE, blank.lines.skip = TRUE,na.strings = "")
df <- subset(df, select = c(1,2,3,4))

names_list <- df[,1]
surnames_list <- na.omit(df[,2])
street_list <- na.omit(df[,3])
city_list <- na.omit(df[,4])

gen.names <- function(n){
  i=1
  users <- data.frame()
  while (i < n+1){
      users [i,1] <- as.character(sample(names_list,1), select=c(1))        
      users [i,2] <- as.character(sample(surnames_list,1), select=c(1))
      users [i,3] <- as.character(paste(sample(street_list,1),sample(1:100,1)))
      users [i,4] <- as.character(sample(city_list,1), select=c(1))
      users [i,5] <- as.character('Slovenia')
      users [i,6] <- as.character(paste( sample( 0:9, 10, replace=TRUE ), collapse="" ))
      users [i,7] <- as.character(paste((users[i,1]),'.',users[i,2],'@gmail.com',collapse = NULL,sep = ''))
      users [i,8] <- as.character(paste((users[i,1]),users[i,2],sample(1:10,1),collapse = NULL,sep = ''))
      users [i,9] <- as.character(paste(sample(1:100,1),users[i,2],sample(1:10,1),collapse = NULL,sep = ''))
      i = i+1
  }
  return(users)
}

order_type = c('sell', 'buy')
cats_list = c('Korat','Perzijska')
order_status = c('Online', 'Done', 'Overrun')

gen.orders <- function(n){
  i=1
  orders <- data.frame()
  while (i < n+1){
    orders [i,1] <- as.character(sample(order_type,1))
    orders [i,2] <- as.numeric(sample(10:100,1)) #dodat še relacijo med buy in sell različico
    orders [i,3] <- sample(1:3,1)
    orders [i,4] <- as.character(sample(cats_list,1), select=c(1)) #oziroma catID
    orders [i,5] <- paste(format(sample(seq(as.Date('2017/01/01'), as.Date('2018/06/01'), by="day"), 1),format="%d %B %Y"),', ', sample(00:23,1),':',sample(00:59,1),collapse = NULL,sep = '')
    orders [i,6] <- as.character(sample(order_status,1))
    i = i+1
  }
  return(orders)
}
