
# Libs
library(tidyverse)
library(janitor)
library(MetBrewer)
library(statebins)
library(geofacet)
library(grid)
library(showtext)


# Data and cleaning
df <- readxl::read_excel("", range = "") %>%
  clean_names()

glimpse(df)

df2 <- df %>% 
  mutate(regcount = case_when(
    between(registration_count,0,10000) ~  "Less than 10 000",
    between(registration_count,10000,20000) ~ "10 000 - 20 000",
    between(registration_count,20000,30000) ~ "20 000 - 30 000",
    between(registration_count,30000,40000) ~ "30 000 - 40 000",
    between(registration_count,40000,Inf) ~ "More than 40 000")) %>% 
  mutate(regcount=as.factor(regcount))


#Misc
font <- "Koulen"
font_add_google(family=font, font)
bg <- "#069A8E"


# Plot
df2 %>% 
  mutate(regcount = factor(regcount, levels=c("Less than 10 000",
                                              "10 000 - 20 000",
                                              "20 000 - 30 000",
                                              "30 000 - 40 000",
                                              "More than 40 000"))) %>%
  ggplot(aes(y="",x="")) +
  geom_point(aes(color=regcount), size=10) +
  scale_color_brewer(palette = "OrRd",name="Number of electric vehicles",
                     guide = guide_legend(
                       direction = "horizontal",
                       title.position = "top"
                     )) +
  facet_geo(~ state, grid = "us_state_grid2", label = "code") +
  labs(title = "Electric vehicles\nin U.S. states",
       caption ="Gilbert Fontana | Data: United States Department of Energy (2021)"
  ) +
  theme_minimal(base_family = font) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    plot.title = element_text(size=30,margin = margin(0,0,30,0)),
    plot.caption = element_text(hjust=.5),
    legend.position = "bottom",
    legend.title = element_text(hjust = .5),
    plot.background = element_rect(fill = bg, color = bg),
    plot.margin = margin(30, 30, 30, 30),
  )


# Save
showtext_auto(enable = TRUE) 
showtext_opts(dpi = 320) 

ggsave("electric.png",
       height = 8,
       width = 8,
       dpi=320,
       
)  

showtext_auto(FALSE)
