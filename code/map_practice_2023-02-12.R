install.packages("tidyverse")
install.packages("usmap")
install.packages("sf")
install.packages("ggrepel")
install.packages("maptools")
install.packages("glue")
install.packages("skimr")
install.packages("showtext")
install.packages("ggtext")
library(tidyverse)
library(usmap)
library(sf)
library(ggrepel)
library(maptools)
library(glue)
library(skimr)
library(showtext)
library(ggtext)

font_add_google(family = "alegreya", "Alegreya SC")
font_add_google(family = "lora", "Lora")

showtext_auto()
showtext_opts(dpi = 300)

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

name_data <- read_csv("data/name_data.csv")
skim_without_charts(name_data)

# Enter own name in filter() function to get own name data
own_name <- name_data %>% filter(name == "Francisco")

own_name <- own_name %>% select(-year) %>% group_by(state) %>% summarize(total = sum(year_total)) %>% 
  ungroup()

states_only <- states_map %>% distinct(region)

name_map <- left_join(states_only, own_name, by=c("region" = "state")) %>% 
  replace_na(list(total=0))

name_sm <- full_join(sm_center, name_map, by="region")

max_sm <- max(name_sm$total)
min_sm <- min(name_sm$total)
mid_sm <- round(max_sm/2,0)

# Enter own name in filter() function to get own name data
name_count <- name_data %>% filter(name == "Francisco") %>% count(name)
name_title <- name_count$name

# Constructing the map
name_sm %>%
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
                   size=1.75, family="lora") +
  theme(panel.background = element_rect(fill = "#646754", color="#646754"),
        plot.background = element_rect(fill="#646754", color="#646754"),
        panel.grid = element_blank(),
        plot.title = element_text(color="#ececec", size=18, face="bold",
                                  family="alegreya"),
        plot.title.position = "plot",
        plot.subtitle = element_text(color="#ececec", size=8,
                                     family="lora"),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.background = element_blank(),
        legend.text = element_text(face="bold", family="lora", color="#ececec"),
        legend.position = c(0.75,0.030),
        legend.direction = "horizontal",
        legend.key.height = unit(0.15, "cm")) +
  labs(title = glue("COUNT YOUR NAME!: {name_title}"),
       subtitle = "USA Name Count Data for 2019 - 2021")

ggsave("visuals/usmap.png", width = 8, height = 4)
