


# Libs
library(tidyverse)
library(janitor)
library(showtext)
library(scico)
library(sf)
library(eurostat)

# Data
df <- get_eurostat("tgs00101",time_format = "raw") %>% 
  filter(time==2020, sex=="T")
         
geo <- get_eurostat_geospatial( output_class = "sf",
                                 resolution = "60",
                                 nuts_level = 2,
                                 year = 2021
                                ) %>% 
  filter(CNTR_CODE %in% eu_countries$code,
         CNTR_CODE != "UK")

geo <- st_transform(geo, crs = 3035)

df2 <- 
  left_join(
    geo,
    df,
    by=c("NUTS_ID"="geo")
  )

# Misc
font <- "Space Grotesk"
font_add_google(family=font, font)
theme_set(theme_minimal(base_family = font))
bg <- "#112B3C"
txt_col <- "grey80"
showtext_auto(enable = TRUE) 

# Plot
df2 %>% 
  ggplot() +
  geom_sf(aes(fill=values, geometry=geometry), color="white", size=.1) + 
  scale_fill_scico(palette = "acton",
                   direction = -1,
                   na.value="#112B3C",
                   limits=c(70,85),
                   breaks=seq(70,85,5),
                   name = "Life expectancy at birth") +
  coord_sf(xlim = c(2800000, 5800000), ylim=c(1380000, 5300000)) + 
  labs(title = "Life Expectancy in EU-countries",
       subtitle = "The east-west divide is evident",
       caption = "Gilbert Fontana | Data: Eurostat (2020)"
       ) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    
    plot.margin = margin(30,30,30,30),
    plot.background = element_rect(color=bg, fill=bg),
    
    plot.title = element_text(hjust=.5,size=22, color=txt_col,lineheight=.8, face="bold", margin=margin(0,0,0,0)),
    plot.subtitle = element_text(hjust=.5,size=16, color=txt_col,lineheight=.8, margin=margin(10,0,0,0)),
    plot.caption = element_text(hjust=.5,margin=margin(10,0,0,0), size=10, color=txt_col, face="bold"),
    legend.position = "bottom",
    legend.title = element_text(size=9, color=txt_col),
    legend.text = element_text(size=7, color=txt_col)
  )  +
  guides(fill = guide_colourbar(ticks.colour = NA,
                                title.position="top",
                                title.hjust = 0.5,
                                barwidth = unit(8, "cm"),
                                barheight = unit(.25,"cm"),frame.colour = "white"))

# Save
showtext_opts(dpi = 320) 

ggsave("Lifeexp.png",
       height = 7,
       width = 7,
       dpi=320
)  

showtext_auto(FALSE)  

scico_palette_names()

