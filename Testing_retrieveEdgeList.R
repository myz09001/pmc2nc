library(rentrez)
library(progress)
library(RMariaDB)

library(devtools)
devtools::load_all("/home/mz/pmc2nc/")

con_mysql = dbConnect(MariaDB(), group = "CPP")

pmid <- 21876761
pmids1 <- c(21876761, 311, 29463753, 21876726)

# This will create tables and insert edge list from one pmid "21876761"
# This gets everything from NCBI if there is no database
res1 <- retrieveEdgeList(pmid, conMysql = con_mysql)

# This will not create new tables or insert edge list. This will just take everything from DB.
# res2 == res1
res2 <- retrieveEdgeList(pmid, conMysql = con_mysql)

# This will not create new table. This will insert 29463753, 21876726
# This will qry db for 21876761
# result will be like res1 with the combine 29463753, 21876726 edge list
res3 <- retrieveEdgeList(pmids1, conMysql = con_mysql)

# This will insert Source with PMID 1000001 This will use to test lastUpdate
# insertEdgeList will delete everything with "Target" = 21876761 and insert e1
e1 <- data.frame("Source" = 1000001, "Target" = 21876761 )
insertEdgeList(con_mysql, e1)

# This should only return one edge list (1000001, 21876761)
res4 <- retrieveEdgeList(pmid, conMysql = con_mysql)

# "Source" = 1000001 should not be there anymore because lastUpdate forced it to update
# This should be the same as res1
res5 <- retrieveEdgeList(pmid, conMysql = con_mysql, lastUpdate = "2022-5-2")

# This will insert Source with PMID 1000001 This will use to test lastUpdate
# insertEdgeList will delete everything with "Target" = 21876761 and insert e1
e1 <- data.frame("Source" = 1000001, "Target" = 21876761 )
insertEdgeList(con_mysql, e1)

# This will have a result of 218 objects. The insertEdgeList removed 30 objects of pmid "21876761" and 
# replaced it with 1 ("Source" = 1000001, "Target" = 21876761 ). So from res3 247 - 30 + 1 = 218 
res6 <- retrieveEdgeList(pmids1, conMysql = con_mysql)

# "Source" = 1000001 should not be there anymore because lastUpdate forced it to update
# This should be the same as res3
res7 <- retrieveEdgeList(pmids1, conMysql = con_mysql, lastUpdate = "2022-5-2")


dbDisconnect(con_mysql)
