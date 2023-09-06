
setwd("C:/Users/christian.thorjussen/Project Nortura/")
rm(list = ls())
load("wide_data_for_analysis.Rda")

# Naming production types in wide_data
wide_data$type_names <- ifelse(wide_data$prod_type == 4, "Foredling",
                        ifelse(wide_data$prod_type == 6, "Grillkylling",
                        ifelse(wide_data$prod_type == 17, "McDonaldskylling",
                        ifelse(wide_data$prod_type == 21, "Hubbard",
                        ifelse(wide_data$prod_type == 22, "Kyllinggaarden", "Unknown")))))
# Check if all types have names
table(wide_data$type_names)

# Load in Innsett data 
load("raw data/Innsett.Rdata")

# Naming production types in Innsett
Innsett$type_names <- ifelse(Innsett$FK_TypeProduksjon_Dim == 4, "Foredling",
                             ifelse(Innsett$FK_TypeProduksjon_Dim == 6, "Grillkylling",
                             ifelse(Innsett$FK_TypeProduksjon_Dim == 17, "McDonaldskylling",
                             ifelse(Innsett$FK_TypeProduksjon_Dim == 21, "Hubbard",
                             ifelse(Innsett$FK_TypeProduksjon_Dim == 22, "Kyllinggaarden", "Unknown")))))
# Checking for unknowns
table(Innsett$type_names)
# Dropping unknown types 
Innsett <- subset(Innsett, type_names != "Unknown")
# Counting the different types
obs_count <- table(Innsett$type_names)
# Create proportions 
obs_share <- prop.table(obs_count)
names_vector <- names(obs_share)
numbers_vector <- as.numeric(obs_share)

type.dist <- data.frame(type_names = c(names_vector),
                        Freq = nrow(wide_data) * c(numbers_vector))

# slaughterhouse wights
table(wide_data$id_slaughterhouse)
wide_data$slaughterhouse <- ifelse(wide_data$id_slaughterhouse == 2, "Haa",
                            ifelse(wide_data$id_slaughterhouse == 3, "Elverum",
                            ifelse(wide_data$id_slaughterhouse == 5, "Norsk Kylling",
                            ifelse(wide_data$id_slaughterhouse == 6, "Haerland",
                            ifelse(wide_data$id_slaughterhouse == 7, "Ytteroykylling", "Unknown")))))
table(wide_data$slaughterhouse)

table(wide_data$id_hatchery)
wide_data$hatchery <- ifelse(wide_data$id_hatchery == 1, "Nortura",
                      ifelse(wide_data$id_hatchery == 2, "Haa",
                      ifelse(wide_data$id_hatchery == 3,"Hugaas","Unknown")))
table(wide_data$hatchery)


table(wide_data$id_feedfirm)
wide_data$feedfirm <- ifelse(wide_data$id_feedfirm == 4, "FKRA",
                      ifelse(wide_data$id_feedfirm == 9, "Strand Unikorn",
                      ifelse(wide_data$id_feedfirm == 11, "Vestfoldmollene",
                      ifelse(wide_data$id_feedfirm == 14, "Fiskaa Molle Flisa",
                      ifelse(wide_data$id_feedfirm == 15, "Fiskaa Molle Rogaland",
                      ifelse(wide_data$id_feedfirm == 16, "FKA Ila",
                      ifelse(wide_data$id_feedfirm == 17, "FKA Kambo",
                      ifelse(wide_data$id_feedfirm == 19, "Norgesfor Raade Molle",
                      ifelse(wide_data$id_feedfirm == 22, "Ostmollene AS",
                      ifelse(wide_data$id_feedfirm == 24, "Norgesfor Mysen",
                      ifelse(wide_data$id_feedfirm == 31, "Norgesfor Orkla",
                      ifelse(wide_data$id_feedfirm == 35, "Raade Molle", "Unknown"))))))))))))
table(wide_data$feedfirm)

small.svy.unweighted <- svydesign(ids=~1, data=wide_data)

# Share of slaughterhouse

load("raw data/Utslaktning.Rdata")
table(wide_data$slaughterhouse)
table(Utslaktning$FK_Slakteri_Dim)
Utslaktning$slaughterhouse <- ifelse(Utslaktning$FK_Slakteri_Dim == 2, "Haa",
                              ifelse(Utslaktning$FK_Slakteri_Dim == 3, "Elverum",
                              ifelse(Utslaktning$FK_Slakteri_Dim == 5, "Norsk Kylling",
                              ifelse(Utslaktning$FK_Slakteri_Dim == 6, "Haerland",
                              ifelse(Utslaktning$FK_Slakteri_Dim == 7, "Ytteroykylling", "Unknown")))))
table(Utslaktning$slaughterhouse)
# Drop rows where FK_Slakteri_Dim equals -1
Utslaktning <- subset(Utslaktning, FK_Slakteri_Dim != -1)
table(Utslaktning$slaughterhouse)
obs_count <- table(Utslaktning$slaughterhouse)
obs_share <- prop.table(obs_count)

slaughter.dist <- data.frame(slaughterhouse = c('Elverum','Haa', 'Haerland','Norsk Kylling','Ytteroykylling'),
                       Freq = nrow(wide_data) * c(0.226137566,0.112804233, 0.648730159,0.010052910,0.002275132 ))

rm(Utslaktning)

# Share of hatchery
load("raw data/Innsett.Rdata")

table(Innsett$FK_Rugeri_Dim)
Innsett$hatchery <- ifelse(Innsett$FK_Rugeri_Dim == 1, "Nortura",
                             ifelse(Innsett$FK_Rugeri_Dim == 2, "Haa",
                                    ifelse(Innsett$FK_Rugeri_Dim == 3,"Hugaas","Unknown")))
table(Innsett$hatchery)

Innsett <- subset(Innsett, hatchery != "Unknown")
obs_count <- table(Innsett$hatchery)
obs_share <- prop.table(obs_count)
obs_share
hatch.dist <- data.frame(hatchery = c('Haa', 'Hugaas', 'Nortura'),
                        Freq = nrow(wide_data) * c(0.15659889,0.01641405,0.82698706))




# Feed firm
load("raw data/Innsett.Rdata")

table(Innsett$FK_Forfirma_Dim)
Innsett$feedfirm <- ifelse(Innsett$FK_Forfirma_Dim == 4, "FKRA",
                          ifelse(Innsett$FK_Forfirma_Dim == 9, "Strand Unikorn",
                          ifelse(Innsett$FK_Forfirma_Dim == 11, "Vestfoldmollene",
                          ifelse(Innsett$FK_Forfirma_Dim == 14, "Fiskaa Molle Flisa",
                          ifelse(Innsett$FK_Forfirma_Dim == 15, "Fiskaa Molle Rogaland",
                          ifelse(Innsett$FK_Forfirma_Dim == 16, "FKA Ila",
                          ifelse(Innsett$FK_Forfirma_Dim == 17, "FKA Kambo",
                          ifelse(Innsett$FK_Forfirma_Dim == 19, "Norgesfor Raade Molle",
                          ifelse(Innsett$FK_Forfirma_Dim == 22, "Ostmollene AS",
                          ifelse(Innsett$FK_Forfirma_Dim == 24, "Norgesfor Mysen",
                          ifelse(Innsett$FK_Forfirma_Dim == 31,"Norgesfor Orkla",
                          ifelse(Innsett$FK_Forfirma_Dim == 35, "Raade Molle", "Unknown"))))))))))))
table(Innsett$feedfirm)
Innsett <- subset(Innsett, feedfirm != "Unknown")
obs_count <- table(Innsett$feedfirm)
obs_share <- prop.table(obs_count)
obs_share
hatch.dist <- data.frame(forfirma = c('FKA Ila','FKA Kambo','FKRA','Fiskaa Molle Flisa','Fiskaa Molle Rogaland','Norgesfor Mysen','Norgesfor Orkla','Norgesfor Raade Molle',
                                      'Ostmollene AS','Raade Molle','Strand Unikorn','Vestfoldmollene'),
                         Freq = nrow(wide_data) * c(1.109754e-01,3.136451e-01,1.177805e-01,8.358053e-02,2.835456e-02,3.926016e-03,8.724481e-05,3.777700e-02,1.507590e-01,1.518060e-02,1.110626e-01,2.687140e-02))



 













