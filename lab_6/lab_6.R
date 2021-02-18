library(RSQLite)
library(DBI)

conn <- dbConnect(SQLite(), 'database.sqlite')
