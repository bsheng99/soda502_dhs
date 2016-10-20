## SODA 502 
## Map Total Fertility Rate of Japanese Perfectures from 2010 - 2013

library(choroplethr)
library(choroplethrAdmin1)
library(ggplot2)
library(data.table)

#CRS.WGS84 = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

data <- read.csv("Japan TFR.csv")

# Format geographical names; remove numbers, space, drop total, sort df alphabetically
data$Prefecture = gsub('[0-9]+', '', tolower(data$Prefecture))
data$Prefecture = gsub("^\\s+|\\s+$", "", data$Prefecture)
data = data[2:length(data$Prefecture),]
data <- with(data,  data[order(data$Prefecture) , ])

## Map

data(admin1.map)
data(admin1.regions)
unique(admin1.regions$country)

japan.map = get_admin1_map("japan")
ggplot(japan.map, aes(long, lat, group=group)) + 
  geom_polygon() +
  ggtitle("Japan")
data(df_japan_census)

data$change0607 <- data[,3]-data[,2]
data$change0708 <- data[,4]-data[,3]
data$change0809 <- data[,5]-data[,4]
data$change0910 <- data[,6]-data[,5]
data$change1011 <- data[,7]-data[,6]
data$change1112 <- data[,8]-data[,7]
data$change1213 <- data[,9]-data[,8]

df_japan_census$value = data[,5]
admin1_choropleth("japan", df_japan_census, num_colors = 1, title = 'Japanese Fertility by Prefecture in 2009', legend = "Total Fertility Rate")

df_japan_census$value = data[,6]
admin1_choropleth("japan", df_japan_census, num_colors = 1, title = 'Japanese Fertility by Prefecture in 2010', legend = "Total Fertility Rate")

df_japan_census$value = data[,7]
admin1_choropleth("japan", df_japan_census, num_colors = 1, title = 'Japanese Fertility by Prefecture in 2011', legend = "Total Fertility Rate")

df_japan_census$value = data[,8]
admin1_choropleth("japan", df_japan_census, num_colors = 1, title = 'Japanese Fertility by Prefecture in 2012', legend = "Total Fertility Rate")

df_japan_census$value = data[,9]
admin1_choropleth("japan", df_japan_census, num_colors = 1, title = 'Japanese Fertility by Prefecture in 2013', legend = "Total Fertility Rate")


df_japan_census$value = data[,"change0607"]
admin1_choropleth("japan", df_japan_census, num_colors = 1, title = 'Change in Japanese Fertility by Prefecture 2006-2007', legend = "Change in TFR")

df_japan_census$value = data[,"change0708"]
admin1_choropleth("japan", df_japan_census, num_colors = 1, title = 'Change in Japanese Fertility by Prefecture 2007-2008', legend = "Change in TFR")

df_japan_census$value = data[,"change0809"]
admin1_choropleth("japan", df_japan_census, num_colors = 1, title = 'Change in Japanese Fertility by Prefecture 2008-2009', legend = "Change in TFR")

df_japan_census$value = data[,"change0910"]
admin1_choropleth("japan", df_japan_census, num_colors = 1, title = 'Change in Japanese Fertility by Prefecture 2009-2010', legend = "Change in TFR")

df_japan_census$value = data[,"change1011"]
admin1_choropleth("japan", df_japan_census, num_colors = 1, title = 'Change in Japanese Fertility by Prefecture 2010-2011', legend = "Change in TFR")

df_japan_census$value = data[,"change1112"]
admin1_choropleth("japan", df_japan_census, num_colors = 1, title = 'Change in Japanese Fertility by Prefecture 2011-2012', legend = "Change in TFR")

df_japan_census$value = data[,"change1213"]
admin1_choropleth("japan", df_japan_census, num_colors = 1, title = 'Change in Japanese Fertility by Prefecture 2012-2013', legend = "Change in TFR")

