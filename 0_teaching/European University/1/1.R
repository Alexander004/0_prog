
## В этом векторе задаем нужные нам пакеты (займет около 10 минут)
#pkgs <- c("data.table", "R.utils", "stringi", "stringr", "lubridate", "httr", "jsonlite", "ndjson", "text2vec", "DBI", "RSQLite", "devtools", "Matrix", "stats", "udpipe", "glmnet", "caret")

#install.packages(pkgs)

download.file("https://github.com/wb-08/SlavicNames/raw/main/db/fio.db", destfile = "C:/Users/Alex/OneDrive/Документы/0_machine_learning/European University/1/fio.sqlite")

# Установка пакета RSQLite, если он не установлен
#install.packages("RSQLite")

library(RSQLite)
library(data.table)

# Соединение с базой данных SQLite
fio <- dbConnect(RSQLite::SQLite(), destfile = "C:/Users/Alex/OneDrive/Документы/0_machine_learning/European University/1/fio.sqlite")

dbListTables(fio)
dbWriteTable(fio, "names", names)

# Загрузка таблиц с именами, фамилиями и отчествами
surnames <- dbReadTable(fio, "surnames")
names <- dbReadTable(fio, "names")
patronymics <- dbReadTable(fio, "midnames")

# Объединение таблиц в одну с добавлением столбца gender
fio <- data.table(
  surname = surnames$surname,
  name = names$name,
  patronymic = patronymics$patronymic,
  gender = c(rep("M", nrow(surnames)), rep("F", nrow(surnames)))
)
