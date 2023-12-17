library(ggplot2)
library(readxl)
dane_clean <- read_excel("dane_clean.xlsx")

dane_clean$ICNPO = as.factor(dane_clean$ICNPO)
dane_clean$age = as.factor(dane_clean$age)
dane_clean$income = as.factor(dane_clean$income)
lev = levels(dane_clean$ICNPO)
dane_clean$ICNPO <- factor(dane_clean$ICNPO, c(lev[1],lev[2],lev[4],lev[3],lev[7],lev[6],lev[5]))
lev <- levels(dane_clean$income)
dane_clean$income <- factor(dane_clean$income, c(lev[6],lev[3],lev[4],lev[5],lev[1],lev[2]))

ggplot(dane_clean, aes(x = age, fill = ICNPO)) +
    geom_bar(position = "stack") +
    theme_classic()

ggplot(dane_clean, aes(x = age, fill = ICNPO)) +
    geom_bar(position = "fill", width = 0.6) + scale_fill_brewer(palette = "Set2") + 
    theme_classic() + ylab("") + theme(axis.title= element_text(size=12),
        axis.text= element_text(size=10),legend.text = element_text(size=12))


ggplot(dane_clean, aes(x = income, fill = ICNPO)) +
    geom_bar(position = "fill", width = 0.6) + scale_fill_brewer(palette = "Set2") + 
    theme_classic() + ylab("") + theme(axis.title= element_text(size=12),
                                       axis.text= element_text(size=10),legend.text = element_text(size=12))

ggplot(dane_clean, aes(x = gender, fill = ICNPO)) +
    geom_bar(position = "fill", width = 0.5, show.legend = FALSE) + scale_fill_brewer(palette = "Set2") + 
    theme_classic() + ylab("") + theme(axis.title= element_text(size=12),
                                       axis.text= element_text(size=11),legend.text = element_text(size=12))+
    scale_x_discrete(labels=c("Female", "Male"))

ggplot(dane_clean, aes(x = partner, fill = ICNPO)) +
    geom_bar(position = "fill", width = 0.5) + scale_fill_brewer(palette = "Set2") + 
    theme_classic() + ylab("") + theme(axis.title= element_text(size=12),
                                       axis.text= element_text(size=11),legend.text = element_text(size=12))

ggplot(dane_clean, aes(x = child_under6, fill = ICNPO)) +
    geom_bar(position = "fill", width = 0.5, show.legend = FALSE) + scale_fill_brewer(palette = "Set2") + 
    theme_classic() + ylab("") + theme(axis.title= element_text(size=13),
                                       axis.text= element_text(size=12),legend.text = element_text(size=12))

ggplot(dane_clean, aes(x = child_older, fill = ICNPO)) +
    geom_bar(position = "fill", width = 0.5) + scale_fill_brewer(palette = "Set2") + 
    theme_classic() + ylab("") + theme(axis.title= element_text(size=13),
                                       axis.text= element_text(size=12),legend.text = element_text(size=12))


ggplot(dane_clean, aes(x = educ, fill = ICNPO)) +
    geom_bar(position = "fill", width = 0.5) + scale_fill_brewer(palette = "Set2") + 
    theme_classic() + ylab("") + theme(axis.title= element_text(size=13),
                                       axis.text= element_text(size=12),legend.text = element_text(size=12))+
    scale_x_discrete(labels=c(0,1,2,3,"Not stated"))


ggplot(dane_clean, aes(x = age, fill = ICNPO)) +
    geom_bar(position = "fill", width = 0.5) + scale_fill_brewer(palette = "Set2") + 
    theme_classic() + ylab("") + theme(axis.title= element_text(size=13),
                                       axis.text= element_text(size=12),legend.text = element_text(size=12))+
    scale_x_discrete(labels=c("15 - 24","25 - 34","35 - 44","45 - 54","55 - 64","65 - 74","75+"))


immigr <- factor(immigr,c("Yes","No","Valid skip","Not stated"))
ggplot(dane_clean, aes(x =  immigr, fill = ICNPO)) +
    geom_bar(position = "fill", width = 0.5) + scale_fill_brewer(palette = "Set2") + 
    theme_classic() + xlab("immigr") + ylab("") + theme(axis.title= element_text(size=13),
                                       axis.text= element_text(size=12),legend.text = element_text(size=12))

ggplot(dane_clean, aes(x = income, fill = ICNPO)) +
    geom_bar(position = "fill", width = 0.5) + scale_fill_brewer(palette = "Set2") + 
    theme_classic() + ylab("") + theme(axis.title= element_text(size=13),
                                       axis.text= element_text(size=12),legend.text = element_text(size=12))+
    scale_x_discrete(labels=c("<25","25-49","50-74","75-99","100-124","125+"))


ggplot(dane_clean) + 
    geom_bar( aes(x = age), fill = "steelblue", width = 0.5) + theme_classic() +
    theme(axis.title= element_text(size=12), axis.text= element_text(size=11))+ ylab("")+
    scale_x_discrete(labels=c("15-24","25-34","35-44","45-54","55-64","65-74","75+"))

ggplot(dane_clean) + 
    geom_bar( aes(x = income), fill = "steelblue", width = 0.5) + theme_classic() +
    theme(axis.title= element_text(size=12), axis.text= element_text(size=11))+ ylab("") + scale_x_discrete(labels=c("<25","25-49","50-74","75-99","100-124","125+"))

sum(age == "35 to 44 years" & ICNPO == "Not involved")/sum(age == "35 to 44 years")

attach(dane_clean)

ggplot(dane_clean) + 
    geom_bar( aes(forcats::fct_rev(forcats::fct_infreq(ICNPO))), fill = "steelblue", width = 0.5) + theme_classic() +
    theme(axis.title= element_text(size=12), axis.text= element_text(size=10))+ xlab("") + ylab("ICNPO") + coord_flip()


remove(dane_clean)
as.factor


scale_fill_brewer(palette = "Set2")
getwd()

