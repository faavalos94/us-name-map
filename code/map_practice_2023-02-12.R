install.packages("tidyverse")
install.packages("usmap")
install.packages("sf")
install.packages("ggrepel")
install.packages("maptools")
install.packages("glue")
install.packages("skimr")
library(tidyverse)
library(usmap)
library(sf)
library(ggrepel)
library(maptools)
library(glue)
library(skimr)

states_map <- us_map("states")
states_map

states_map <- states_map %>% rename(region = abbr, long=x, lat=y)
states_map %>% head()

df_sm <- st_as_sf(states_map, coords = c("long", "lat"))

centroids_sf <- df_sm %>%
  group_by(region) %>% 
  summarize(geometry = st_union(geometry)) %>%
  st_centroid()

sm_center <- data.frame(centroids_sf$region, st_coordinates(centroids_sf)) %>% 
  rename(long=X, lat=Y, region = centroids_sf.region)

ggplot(data = states_map, aes(x=long, y=lat, map_id = region)) +
  geom_map(map = states_map, fill = "white", color = "black") +
  coord_fixed() +
  theme(panel.background = element_rect(fill = "black", color = "black"),
        plot.background = element_rect(fill="black", color="black"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())

name_data <- read_csv("data/name_data.csv")
skim_without_charts(name_data)
fran_data <- name_data %>% filter(name == "Francisco")
fran_data <- fran_data %>% select(-year) %>% group_by(state) %>% summarize(total = sum(year_total)) %>% 
  ungroup()

states_only <- states_map %>% distinct(region)

fran_map <- left_join(states_only, fran_data, by=c("region" = "state")) %>% 
  replace_na(list(total=0))

fran_sm <- full_join(sm_center, fran_map, by="region")

max_sm <- max(fran_sm$total)
min_sm <- min(fran_sm$total)
mid_sm <- round(max_sm/2,0)

ggplot(fran_map, aes(fill=total)) +
  geom_map(aes(map_id = region), map = states_map, color = "white", linewidth=0.25, show.legend=FALSE) +
  expand_limits(x=states_map$long, y=states_map$lat) +
  coord_fixed() +
  scale_fill_gradient(low="#a6611a", high="#018571") +
  theme(panel.background = element_rect(fill = "black", color="black"),
        plot.background = element_rect(fill="black", color="black"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())

# constructing the map
fran_sm %>% 
  ggplot(aes(fill=total)) + 
  geom_map(aes(map_id = region), map = states_map,
           color = "white", linewidth=0.25, show.legend=TRUE) + 
  expand_limits(x=states_map$long, y=states_map$lat) +
  coord_fixed() +
  scale_fill_gradient(name=NULL,
                      low="#B5A89B", high="#EA6729",
                      breaks = c(min_sm, mid_sm,max_sm),
                      labels = c(glue("{min_sm}"),glue("{mid_sm}"),glue("{max_sm}"))) +
  geom_label_repel(aes(x=long,y=lat, 
                       label = paste("",region,"\n",total,"")), show.legend=FALSE,
                   min.segment.length = 0.10, max.overlaps = 51, force_pull=0.2,
                   size=1.75) +
  theme(panel.background = element_rect(fill = "#646754", color="#646754"),
        plot.background = element_rect(fill="#646754", color="#646754"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.background = element_blank(),
        legend.text = element_text(face="bold"),
        legend.position = c(0.75,0.030),
        legend.direction = "horizontal",
        legend.key.height = unit(0.15, "cm"))

ggsave("code/usmap.png", width = 8, height = 4)
