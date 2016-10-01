## SODA 502 
## Map Total Fertility Rate of Japanese Perfectures from 2010 - 2013

library(choroplethr)
library(choroplethrAdmin1)
library(ggplot2)
library(data.table)

#CRS.WGS84 = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

filepath = 'soda502_dhs-master/Japan TFR.csv'
data <- fread(paste(filepath))

# Format geographical names; remove numbers, space, drop total, sort df alphabetically
data$V1 = gsub('[0-9]+', '', tolower(data$V1))
data$V1 = gsub("^\\s+|\\s+$", "", data$V1)
data = data[3:length(data$V1)]
data <- with(data,  data[order(data$V1) , ])

## Map

data(admin1.map)
data(admin1.regions)
unique(admin1.regions$country)

japan.map = get_admin1_map("japan")
ggplot(japan.map, aes(long, lat, group=group)) + 
  geom_polygon() +
  ggtitle("Japan")
data(df_japan_census)


df_japan_census$value = data$V6
admin1_choropleth("japan", df_japan_census, num_colors = 5, title = 'Japanese Fertility by Prefecture in 2010', legend = "Total Fertility Rate")

df_japan_census$value = data$V7
admin1_choropleth("japan", df_japan_census, num_colors = 5, title = 'Japanese Fertility by Prefecture in 2011', legend = "Total Fertility Rate")

df_japan_census$value = data$V8
admin1_choropleth("japan", df_japan_census, num_colors = 5, title = 'Japanese Fertility by Prefecture in 2012', legend = "Total Fertility Rate")

df_japan_census$value = data$V9
admin1_choropleth("japan", df_japan_census, num_colors = 5, title = 'Japanese Fertility by Prefecture in 2013', legend = "Total Fertility Rate")


