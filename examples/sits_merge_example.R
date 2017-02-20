library(sits)

# select samples for pasture and savanna
cerrado.tb <- sits_getdata ("./data/Samples/cerrado.json")
# filter only those labelled as savanna
savanna.tb <- filter (cerrado.tb, label == "Savanna")

savanna2.tb <- sits_group_bylatlong(savanna.tb)
