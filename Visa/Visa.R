


# Libs
library(tidyverse)
library(showtext)
library(rnaturalearth)
library(sf)
library(scico)


# Data
world <- ne_countries(scale = "medium", returnclass = "sf") %>% 
  select(iso_a3,geometry)

df <- read_csv("https://zenodo.org/record/3911062/files/GMP_GlobalVisaCostDataset_v1.0.csv?download=1") %>% 
  group_by(source, source_iso3) %>% 
  summarise(avg_work=mean(work_perdailyincome, na.rm=TRUE)) %>% 
  mutate_all(~ifelse(is.nan(.), NA, .))


df2 <- left_join(
  world,
  df,
  by=c("iso_a3"="source_iso3")
)

# Misc
showtext_auto(enable = TRUE)
font <- "Fira Sans Condensed"
font_add_google(family=font, font)
theme_set(theme_minimal(base_family = font))
bg <- "#1F1D36"
txt_col <- "grey95"

# Plot
df2 %>% 
  ggplot() +
  geom_sf(aes(fill=avg_work, geometry=geometry), color="grey20", size=.1) +
  scale_fill_scico(palette = "imola",
                   direction = 1,
                   na.value="grey95",
                   limits=c(0,180),
                   breaks=seq(0,180,45),
                   begin=.5,
                   end=1,
                   name = "Average costs expressed in average daily income\nby sender country (US dollars 2019)") +
  labs(
    title="Global costs for work visa",
    subtitle = "Sub-Saharan africans and South-asians generally meets the highest costs",
    caption = "Gilbert Fontana | Data: Recchi, E., Deutschmann, E., Gabrielli, L., & Kholmatova, N. (2020)"
    
  ) +
  coord_sf(crs = "+proj=merc", ylim = c(-7000000,11000000)) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    
    plot.margin = margin(30,30,30,30),
    plot.background = element_rect(color=bg, fill=bg),
    
    plot.title = element_text(hjust=0,size=18, color=txt_col,lineheight=.8, face="bold", margin=margin(0,0,0,0)),
    plot.subtitle = element_text(hjust=0,size=12, color=txt_col,lineheight=.8, margin=margin(10,0,20,0)),
    plot.caption = element_text(hjust=.5,margin=margin(10,0,0,0), size=8, color=txt_col, face="bold"),
    legend.position = "bottom",
    legend.title = element_text(size=7, color=txt_col),
    legend.text = element_text(size=6, color=txt_col)
  )  +
  guides(fill = guide_colourbar(ticks.colour = NA,
                                title.position="top",
                                title.hjust = 0.5,
                                barwidth = unit(6, "cm"),
                                barheight = unit(.4,"cm"),))

# Save
showtext_opts(dpi = 320) 

ggsave("Visa.png",
       height = 5,
       width = 7,
       dpi=320
)  

showtext_auto(FALSE)  

