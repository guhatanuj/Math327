library(tidyverse)
library(readxl)
library(xlsx)
library(datadictionary)
data = read.csv("adm2021.csv")
dictData = read_xlsx("adm2021Dict.xlsx", sheet=2)
dict = data.frame(dictData$varname, dictData$varTitle)
names(data) <- dict$dictData.varTitle[match(names(data), dict$dictData.varname)]
write.xlsx(data, file = "appendedData.xlsx", sheetName="Colleges")