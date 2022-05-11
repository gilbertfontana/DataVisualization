
library(tidyverse)
library(ggstream)
library(MetBrewer)
library(showtext)

hbtq <- as_tibble(read.csv("https://github.com/gilbertfontana/DataVisualization/blob/main/LGBTQ/LGBTQData%20-%20Data.csv"))


font <- "Abril Fatface"
font_add_google(family=font, font)
showtext_auto(enable = TRUE) 
bg <- "#DE8971"
txt_col <- "black"
theme_set(theme_minimal(base_family = font))

hbtq %>% 
  mutate(ideology = case_when(
    Party.Ideology.Family %in% c("Left") ~ "Leftwing",
    Party.Ideology.Family %in% c("Center Right", "Green", "Center") ~ "Center/Green",
    Party.Ideology.Family %in% c("Radical Right", "Nationalist", "Religious Right") ~ "Rightwing",
    TRUE ~ "Other"
  )
  ) %>%
  mutate(LGBTQ_group= case_when(
    !LGBTQ %in% c("Gay", "Lesbian", "Bisexual") ~ "Other",
    TRUE ~ LGBTQ)
  ) %>% 
  distinct(Name, ideology, LGBTQ_group) %>% 
  group_by(ideology,LGBTQ_group) %>% 
  count() %>% 
  mutate(ideology = factor(ideology, levels=c("Leftwing", "Center/Green", "Rightwing", "Other"))) %>%
  ggplot(aes(fill=LGBTQ_group, y=n, x=ideology)) +
  geom_bar(position="stack", stat = "identity") +
  scale_fill_manual(values = met.brewer("Paquin", type = "discrete", n=4)) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_text(color=txt_col, size=12),
    plot.background = element_rect(color=bg, fill=bg),
    plot.margin = margin(30,30,30,30),
    legend.position = "bottom",
  )

showtext_opts(dpi = 320) 

ggsave("LGBTQ.png",
       height = 8,
       width = 12,
       dpi=320,
       
)  

showtext_auto(FALSE)



