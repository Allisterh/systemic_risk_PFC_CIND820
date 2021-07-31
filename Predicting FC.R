attach(Global_Debt_Database)
str(Global_Debt_Database)
summary(Global_Debt_Database)
G7 <- c("Canada", "France", "Germany", "Italy", "Japan", "United Kingdom", "United States")
GDBG7 <- subset(Global_Debt_Database,Global_Debt_Database$country %in% G7)


