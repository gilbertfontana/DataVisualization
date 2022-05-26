

# Libs
library(tidyverse)
library(janitor)
library(MetBrewer)
library(showtext)
library(sf)
library(hhi)
library(giscoR)
library(scico)

# Data
df <- read_csv("C:/Users/Gilbert/Documents/GitHub/DataVisualization/Vote/eu_ned_ep_nuts2.csv") %>% 
  filter(year==2019)

df2 <- df %>% 
  select(nuts2, party_abbreviation,partyvote) %>% 
  group_by(nuts2) %>% 
  mutate(proc = (partyvote/sum(partyvote) * 100)) %>% 
  na.omit()

compute_hhi<-function(df){
  hhi=hhi( df %>% as.data.frame(.),"proc")
  id=df %>% pluck("nuts2") %>% head(1)
  data.frame(id,hhi)
}

df_hhi<- df2 %>%
  group_split(nuts2, .keep=TRUE) %>%
  map(compute_hhi) %>%
  bind_rows()

nuts <- gisco_nuts %>% 
  filter(LEVL_CODE==2) %>% 
  select(NUTS_ID, geometry)

nuts <- st_transform(nuts, crs = 3035)

df3 <- left_join(
  df_hhi,
  nuts,
  by=c("id"="NUTS_ID")
)


# Misc
showtext_auto(enable = TRUE) 
font <- "Space Grotesk"
font_add_google(family=font, font)
theme_set(theme_minimal(base_family = font))
bg <- "#FAF9F6"
txt_col <- "black"

# Plot
df3 %>% 
  ggplot() +
  geom_sf(aes(fill=hhi, geometry=geometry), color="white", size=.1) +
  scale_fill_scico(palette = "nuuk",
                   begin = 0,
                   end = 1,
                   direction = 1,
                   na.value="grey60",
                   name="Herfindahl-Hirschman Index",
                   limits=c(1000,7000),
                   breaks=seq(1000,7000,3000)
                   
                   
  ) +
  coord_sf(xlim = c(2800000, 5800000), ylim=c(1380000, 5300000),crs = 3035) + 
  labs(title = "Political fragmentation in the European Parliament election",
       subtitle = "The Herfindahl-Hirschman Index measures the fragmentation using vote shares.\nA higher value indicates less political fragmentation.",
       caption = "Gilbert Fontana | Data: Schraff, D., Vergioglou, I., & Demirci, B. B. (2022)"
  ) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    
    plot.margin = margin(30,30,30,30),
    plot.background = element_rect(color=bg, fill=bg),
    
    plot.title = element_text(hjust=.5,size=16, color=txt_col,lineheight=.8, face="bold", margin=margin(0,0,0,0)),
    plot.subtitle = element_text(hjust=.5,size=10, color=txt_col,lineheight=.8, margin=margin(10,0,0,0)),
    plot.caption = element_text(hjust=0,margin=margin(10,0,0,0), size=9, color=txt_col, face="bold"),
    plot.title.position = "plot",
    legend.position = "right",
    legend.title = element_text(size=8, color=txt_col, angle = -90),
    legend.text = element_text(size=7, color=txt_col)
  )  +
  guides(fill = guide_colourbar(ticks.colour = NA,
                                title.position = "right" ,
                                title.hjust = 0.5,
                                barwidth = unit(.25, "cm"),
                                barheight = unit(8,"cm"),label.position = "left"
                                )
         )


# Save
showtext_opts(dpi = 320) 

ggsave("Vote.png",
       height = 7,
       width = 7,
       dpi=320,
       
)  

showtext_auto(FALSE)  


