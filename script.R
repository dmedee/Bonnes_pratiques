######### SCRIPT #################""

# ENVIRONNEMENT ----------------
source("R/functions.R", encoding = "UTF-8")

rm(list = ls())

api_token <- yaml::read_yaml("secrets.yaml")$key

if (!require("ggplot2")) install.packages("ggplot2")
if (!require("stringr")) install.packages("stringr")
if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("MASS")) install.packages("MASS")


library(tidyverse)
library(dplyr)
library(forcats)
library(MASS)
library(arrow)

# FONCTIONS ----------------

#recode NA
recode_na <- function(data, variable_name, value ){
  data %>%
    dplyr::mutate(
      !!rlang::sym(variable_name) := na_if(!!rlang::sym(variable_name),value)
    )
}


ignoreNA <- TRUE

decennie_a_partir_annee <- function(ANNEE) {
  return(ANNEE - ANNEE %%
           10)
}

# fonction de stat agregee

stats_agregees <- function(a,b,na.rm=TRUE,
                           ...) {
  match.arg(b,
            c("moyenne",
              "variance",
              "ecart-type",
              "sd",
              "ecart type")
  )
  
  switch(b,
         moyenne = mean(a,na.rm = na.rm),
         variance = var(a, na.rm = na.rm),
         sd(a, na.rm=na.rm)
  )
  
}

# IMPORT DONNEES --------------------

#j'importe les données avec read_csv2 parce que c'est un csv avec des ; et que
#read_csv attend comme separateur des ,
#df <- readr::read_csv2("individu_reg.csv",
  #col_names = c("region", "aemm", "aged", "anai", "catl", "cs1", "cs2", "cs3",
  #              "couple", "na38", "naf08", "pnai12", "sexe", "surf", "tp",
  #              "trans", "ur")
#)

df <-arrow::read_parquet("individu_reg.parquet")

# y a un truc qui va pas avec l'import, je corrige
#colnames(df) <- df[1, ]
#df <- df[2:nrow(df), ]

df2<-df[,c("region", "dept", "aemm", "aged", "anai", "catl", "cs1", "cs2", "cs3", "couple", "na38", "naf08", "pnai12", "sexe", "surf", "tp", "trans", "ur")]

print(df2, 20)

# RETRAITEMENT DONNEES --------------------


df2<-recode_na(df2,"na38","ZZ")
df2<-recode_na(df2,"trans","Z")
df2<-recode_na(df2,"tp","Z")


#df2[df2$na38 == "ZZ", "na38"] <- NA
#df2[df2$na38 == "Z", "trans"] <- NA
#df2[df2$tp == "Z", "tp"] <- NA
df2[endsWith(df2$naf08, "Z"), "naf08"] <- NA

str(df2)
#df2[, ncol(df2)] <- factor(df2[, nrow(df2)])
df2$ur <- factor(df2$ur)

df2$sexe <- factor(df2$sexe)
df2$sexe <-
  fct_recode(df2$sexe, "Homme" = "1", "Femme" = "2")

# STATISTIQUES DESCRIPTIVES --------------------

# combien de professions
print("Nombre de professions :")
print(summarise(df2, length(unique(unlist(cs3[!is.na(cs1)])))))
print("Nombre de professions :")
print(summarise(df2, length(unique(unlist(cs3[!is.na(cs2)])))))
print("Nombre de professions :")
print(summarise(df2, length(unique(unlist(cs3[!is.na(cs3)])))))

print.data.frame <- summarise(group_by(df2, aged), n())
print(print.data.frame)
u=rnorm(10);
stats_agregees(u,"moyenne")
stats_agregees(u, "ecart type")
stats_agregees(u, "variance")


stats_agregees(df2 %>% filter(sexe == "Homme") %>% mutate(aged = as.numeric(aged)) %>% pull(aged), "moyenne",na.rm = TRUE)
stats_agregees(df2 %>% filter(sexe == "Femme") %>% mutate(aged = as.numeric(aged)) %>% pull(aged),"moyenne", na.rm = TRUE)
stats_agregees(df2 %>% filter(sexe == "Homme" & couple == "2") %>% mutate(aged = as.numeric(aged)) %>% pull(aged), "moyenne",na.rm = TRUE)
stats_agregees(df2 %>% filter(sexe == "Femme" & couple == "2") %>% mutate(aged = as.numeric(aged)) %>% pull(aged),"moyenne", na.rm = TRUE)

#part d'hommes dans chaque cohorte
temp <- part_total(df2) |> filter(sexe == "Homme")

# GRAPHIQUES ------------------

df2 %>%
  dplyr::select(aged) %>%
  ggplot(.) +
  geom_histogram(aes(x = 5 * floor(as.numeric(aged) / 5)), stat = "count")

ggplot(df2[as.numeric(df2$aged) > 50, c(3, 4)], aes(
  x = as.numeric(aged), # x = as.numeric(aged) - as.numeric(aged) %% 5,
  y = ..density.., fill = factor(decennie_a_partir_annee(as.numeric(aemm)))
), alpha = 0.2) +
  geom_histogram()

# part d'homme dans chaque cohort
ggplot(temp) +
    geom_bar(aes(x = as.numeric(aged), y = share), stat = "identity") + 
  geom_point(aes(x = as.numeric(aged), y = share), stat = "identity", color = "red") + 
  coord_cartesian(c(0, 100))
# correction (qu'il faudra retirer)
# ggplot(
#   df2 %>% group_by(aged, sexe) %>% summarise(SH_sexe = n()) %>% group_by(aged) %>% mutate(SH_sexe = SH_sexe/sum(SH_sexe)) %>% filter(sexe==1)
# ) + geom_bar(aes(x = as.numeric(aged), y = SH_sexe), stat="identity") + geom_point(aes(x = as.numeric(aged), y = SH_sexe), stat="identity", color = "red") + coord_cartesian(c(0,100))


# stats surf par statut
df3 <- tibble(df2 |> group_by(couple, surf) %>% summarise(x = n()) %>% group_by(couple) |> mutate(y = 100 * x / sum(x)))
ggplot(df3) +
  geom_bar(aes(x = surf, y = y, color = couple), stat = "identity", position = "dodge")

# stats trans par statut
df3 <- tibble(df2 |> group_by(couple, trans) %>% summarise(x = n()) %>% group_by(couple) |> mutate(y = 100 * x / sum(x)))
ggplot(df3) +
  geom_bar(aes(x = trans, y = y, color = couple), stat = "identity", position = "dodge")

#dir.create("/home/onyxia/formation-bonnes-pratiques-R/output")
#setwd("ome/onyxia/formation-bonnes-pratiques-R/output")

ggsave(p, "p.png")

# recode valeurs manquantes
# valeursManquantes <- data.frame(colonne = c(""), NBRE = c(NA))
# for (i in 1:length(colnames(df2))){
#  x = df2[,i]
#  j=0
#  t <-0
#  for (j in 1:nrow(x)){
#    if (is.na(pull(x[j,])) == TRUE) t <- t+1
#  }
#  data.frame(
#
#  )
# }





# MODELISATION ------------------

df3 <- df2 %>%
  dplyr::select(surf, cs1, ur, couple, aged) %>%
  filter(surf != "Z")
df3[, 1] <- factor(df3$surf, ordered = TRUE)
df3[, "cs1"] <- factor(df3$cs1)
polr(surf ~ cs1 + factor(ur), 
     df3 %>% filter(couple == "2" && as.numeric(aged > 40 && aged < 60)))
