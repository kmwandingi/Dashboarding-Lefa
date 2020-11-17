library(data.table)
library(mltools)
library(dplyr)

userc <- condat %>% dplyr::select(passenger, email, phone, 'order status', 'origin type', Constituency, bundle)

userc$bundle <- as.factor(sub('\\..*', '', userc$bundle))

userc$noinemail <- ifelse(grepl("\\d", userc$email), 1,0)

userc$`order status` <- ifelse(userc$`order status` == "FINISHED_PAID", 1,0)

userc <- userc %>% rename(origintype = `origin type`)

userc$origintype <- as.factor(userc$origintype ) 

userc_1h <- one_hot(as.data.table(userc), cols = c("Constituency", "bundle", "origintype"), naCols = TRUE)

userc_1h <- one_hot(as.data.table(userc_1h, naCols = TRUE))