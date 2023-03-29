---
title: "Sample"
output: html_document
date: "2023-01-14"
---
  {r}
install.packages('car')
install.packages('foreign')
install.packages('ggplot2')

install.packages("rstudioapi")
oceanic_theme <- "https://raw.githubusercontent.com/gadenbuie/oceanic-eighties/master/oceanic-eighties.rstheme"
rstudioapi::addTheme(oceanic_theme, apply = TRUE)