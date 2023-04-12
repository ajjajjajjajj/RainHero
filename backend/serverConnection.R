install.packages("odbc")
library(odbc)

con <- dbConnect(odbc(),
                 Driver = "SQL Server",
                 Server = "SQLEXPRESS02",
                 Database = "rainfall",
                 UID = "group10",
                 PWD = rstudioapi::askForPassword("password"),
                 Port = 1433)
