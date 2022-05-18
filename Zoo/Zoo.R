

# Libs
library(tidyverse)
library(janitor)
library(ggdist)
library(MetBrewer)
library(showtext)

# Data
df <- read_csv("https://raw.githubusercontent.com/gilbertfontana/DataVisualization/main/Zoo/Data.csv") %>% 
  clean_names()

df$species_common_name <- gsub(r"{\s*\([^\)]+\)}","",df$species_common_name)

# Misc
font <- "Barlow Condensed"
font_add_google(family=font, font)
bg <- "#006778"
txt_col <- "grey95"
theme_set(theme_minimal(base_family = font))
showtext_auto(enable = TRUE) 

df %>% 
filter(!is.na(overall_mle)) %>% 
arrange(overall_mle) %>%
slice_tail(n=40) %>%
mutate(species_common_name=factor(species_common_name, levels=species_common_name)) %>% 
ggplot(aes(y=species_common_name, x=overall_mle, xmin = overall_ci_lower, xmax = overall_ci_upper)) +
  geom_pointinterval(aes(color=overall_mle), size=2) +
  scale_color_distiller(palette = "OrRd",direction = 1) +
  scale_x_continuous(limits = c(0,60), expand = c(0,0)) +
  labs(title="Median life expectancies for zoo animals in North America",
       subtitle = "- The Andean condor is expected to live for almost 50 years",
       caption = "Gilbert Fontana | Data: Che-Castaldo, J. P., Byrne, A., Perisin, K., & Faust, L. J. (2019)") +
  xlab("Median life expectancies (including 95% confidence limits)") +
  coord_cartesian(clip = "off") +
  theme(
    panel.grid = element_blank(),
    panel.grid.major.x = element_line(linetype = "dotted", color = txt_col),
    axis.title.x = element_text(hjust=.5, color=txt_col, size=10, margin = margin(10,0,0,0)),
    axis.title.y = element_blank(),
    axis.text = element_text(color=txt_col, size=8),
    axis.line.y = element_line(color = txt_col),
    plot.title = element_text(size=20, color=txt_col,lineheight=.8, face="bold", margin=margin(0,0,0,0)),
    plot.title.position = "plot",
    plot.subtitle = element_text(size=16, color=txt_col, margin=margin(10,0,20,0)),
    plot.caption = element_text(hjust=.5,margin=margin(10,0,0,0), size=8, color=txt_col, face="bold"),
    plot.background = element_rect(color=bg, fill=bg),
    plot.margin = margin(30,30,30,30),
    legend.position = "none"
  )

# Save
showtext_opts(dpi = 320) 

ggsave("Zoo.png",
       height = 7,
       width = 7,
       dpi=320,
       
)  

showtext_auto(FALSE)  
  