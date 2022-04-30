
# Library
library(tidyverse)
library(janitor)
library(ggdist)
library(MetBrewer)
library(ggtext)
library(sysfonts)
library(showtext)
showtext_auto(enable = TRUE) 
showtext_opts(dpi = 320) 


# Data import and cleaning
df <- readxl::read_excel("") %>% 
  clean_names() %>% 
  select(country_name, country_code,x2017_yr2017)


region <- read.csv("https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv") %>% 
  clean_names() %>% 
  select(country_code=alpha_3, region, sub_region)


df2 <- left_join(
  df,
  region
) %>% 
drop_na() %>% 
filter(!x2017_yr2017=="..") %>% 
  mutate(values=as.numeric(x2017_yr2017)) %>% 
  select(-x2017_yr2017)


fctorder <- df2 %>% 
  group_by(region) %>% 
  summarise(mean=mean(values)) %>% 
  arrange(mean) %>% 
  pull(region)


# Misc

font_add_google(family="Fira Sans Condensed", "Fira Sans Condensed")
bg <- "#FBD6D2"

# Plot

df2 %>% 
  mutate(region = factor(region, levels=fctorder)) %>%
  ggplot(aes(x=region, y=values)) +
  stat_dots(aes(fill=region),
            side="bottom",
            dotsize=.8,
            color=NA,
            position = "dodge",
            stackratio=1.1) +
  stat_slab(aes(fill=region),position = "dodge", scale=.7) +
  coord_flip(clip = "off") +
  scale_fill_manual(values=met.brewer("Cross", 5)) +
  scale_y_continuous(breaks = seq(0,3000,500),
                     name = "Avg. precipitation in depth (mm)",
                     labels=function(x) format(x, big.mark = " ", scientific = FALSE)) +
  scale_x_discrete(expand = c(0,0)) +
  labs(title = "Regional Variation in Precipitation",
       subtitle = "Precipitation is defined as any kind of water that falls from clouds as a liquid or a solid.
              <br/>The amount of precipitation varies both within and between regions. According to data from
              <br/>the World Bank for 2017, the spread of precipitation in Europe was relatively small
              <br/>while it was very large in Africa. However, the largest rainfall in 2017 was noted
              <br/>in Colombia with a yearly average over 3 200 mm.",
       caption = "Gilbert Fontana | Data: World Bank") +
  theme_minimal(base_family = "Fira Sans Condensed") +
  theme(
    panel.grid = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_text(color=met.brewer(name="Cross",n=5),
                               face = "bold",
                               size=12),
    axis.text.x = element_text(color="black", size=12),
    axis.title.x = element_text(face="bold", size=10,margin=margin(10,0,0,0), color="black", hjust=1),
   
    legend.position = "none",
    legend.title = element_blank(),
    legend.text = element_text(),
    
    plot.title = element_text(hjust = 0.5, face = "bold", size=40),
    plot.subtitle = element_text(hjust = 0.5, size=12, lineheight = 1.1),
    plot.caption = element_text(hjust=.5,margin=margin(30,0,0,0), face="bold"),

    plot.margin = margin(30,30,30,30),
    plot.background = element_rect(color=bg, fill=bg)
  ) +
  theme(plot.subtitle = element_markdown())


# Save plot
ggsave("rain.png",
       height = 8,
       width = 12,
       dpi=320,
       
)  

showtext_auto(FALSE)






