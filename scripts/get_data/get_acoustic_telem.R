library(data.table)


lan_folder = "//SFP.IDIR.BCGOV/S140/S40203/RSD_ FISH & AQUATIC HABITAT BRANCH/General/2 SCIENCE - Invasives/SPECIES/Smallmouth Bass/Cultus lake/"

list.files(lan_folder)
tag_names<-list.files(path = paste0(lan_folder,"2023 projects/acoustic telemetry/2023-10 downloads (Acoustic tag receiver data)"),
                    pattern = ".csv", full.names = T)
tag_data23<-rbindlist(lapply(tag_names, fread), fill = T)

head(tag_data23)

