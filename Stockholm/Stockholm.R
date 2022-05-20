

# Lib
library(tidyverse)
library(showtext)
library(sf)
library(scico)

# Data
deso <- st_read("C:/Users/Gilbert/Desktop/deso_2018_2021-10-21/DeSO_2018_v2.gpkg")
df <-  read.csv("https://raw.githubusercontent.com/gilbertfontana/DataVisualization/main/Stockholm/000001SM_20220520-095955.csv")

# Misc
font <- "Fjalla One"
font_add_google(family=font, font)
theme_set(theme_minimal(base_family = font))
bg <- "#630000"
txt_col <- "grey85"
showtext_auto(enable = TRUE) 

# Plot
left_join(
  deso,
  df,
  by=c("deso"="region") 
) %>% 
  filter(kommunnamn=="Stockholm")%>% 
ggplot() + 
  geom_sf(aes(fill=X2020), color=NA) + 
  coord_sf() +
  #scale_fill_viridis_c(option = "E") +
  scale_fill_scico(palette = 'lajolla',
                   direction = 1,
                   begin = 0, 
                   end = 1, 
                   limits=c(0,75),
                   breaks=seq(0,75,25),
                   name = "Population at risk of poverty (%)") +
  labs(title = str_to_upper("Risk of poverty in Stockholm"),
       caption = "Gilbert Fontana | Data: Statistics Sweden"
  ) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    
    plot.margin = margin(30,30,30,30),
    plot.background = element_rect(color=bg, fill=bg),
    
    plot.title = element_text(hjust=.5,size=22, color=txt_col,lineheight=.8, face="bold", margin=margin(0,0,0,0)),
    plot.caption = element_text(hjust=.5,margin=margin(10,0,0,0), size=10, color=txt_col, face="bold"),
    legend.position = "bottom",
    legend.title = element_text(size=9, color=txt_col),
    legend.text = element_text(size=7, color=txt_col)
  ) +
  guides(fill = guide_colourbar(ticks.colour = NA,
                                title.position="top",
                                title.hjust = 0.5,
                                barwidth = unit(5, "cm")))

showtext_opts(dpi = 320) 

ggsave("Stockholm.png",
       height = 7,
       width = 7,
       dpi=320
       )  

showtext_auto(FALSE)  









