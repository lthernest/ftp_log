##########map visualization using rworldmap##########
install.packages("rworldmap")
library(rworldmap)
join_complete_map <- joinCountryData2Map(total_ip_result, joinCode="NAME", nameJoinColumn="total_ip$complete_ip_country")
rmap <- mapCountryData(join_complete_map, nameColumnToPlot="Freq", mapTitle="Completed file download results organised by countries", catMethod='fixedWidth')
