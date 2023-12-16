library(ggplot2)
library(readxl)
dane_clean <- read_excel("dane_clean.xlsx")

dane_clean$ICNPO = as.factor(dane_clean$ICNPO)
dane_clean$age = as.factor(dane_clean$age)
lev = levels(dane_clean$ICNPO)
dane_clean$ICNPO <- factor(dane_clean$ICNPO, c(lev[1],lev[2],lev[4],lev[3],lev[7],lev[6],lev[5]))

ggplot(dane_clean, aes(x = age, fill = ICNPO)) +
    geom_bar(position = "stack") +
    theme_classic()

ggplot(dane_clean, aes(x = age, fill = ICNPO)) +
    geom_bar(position = "fill", width = 0.6) + scale_fill_brewer(palette = "Set2") + 
    theme_classic() + ylab("") + theme(axis.title= element_text(size=12),
        axis.text= element_text(size=10),legend.text = element_text(size=12))

ggplot(dane_clean) + 
    geom_bar( aes(x = age), fill = "steelblue", width = 0.4) + theme_classic() +
    theme(axis.title= element_text(size=12), axis.text= element_text(size=11))+ ylab("")


ggplot(dane_clean) + 
    geom_bar( aes(x = ICNPO), fill = "steelblue", width = 0.5) + theme_classic() +
    theme(axis.title= element_text(size=12), axis.text= element_text(size=10))+ ylab("")


remove(dane_clean)
as.factor


scale_fill_brewer(palette = "Set2")
getwd()

