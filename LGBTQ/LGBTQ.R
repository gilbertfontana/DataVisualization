
# Lib
library(tidyverse)
library(ggstream)
library(MetBrewer)
library(showtext)

# Data
hbtq <- as_tibble(read.csv("https://raw.githubusercontent.com/gilbertfontana/DataVisualization/main/LGBTQ/LGBTQData%20-%20Data.csv"))

# Misc
font <- "Karantina"
font_add_google(family=font, font)
bg <- "#30475E"
txt_col <- "grey80"
theme_set(theme_minimal(base_family = font))
showtext_auto(enable = TRUE) 

# Data cleaning + plot
hbtq %>% 
  mutate(ideology = case_when(
    Party.Ideology.Family %in% c("Left") ~ "Left-wing",
    Party.Ideology.Family %in% c("Center Right", "Green", "Center") ~ "Center/Green",
    Party.Ideology.Family %in% c("Radical Right", "Nationalist", "Religious Right") ~ "Right-wing",
    TRUE ~ "Other parties**"
  )
  ) %>%
  mutate(LGBTQ_group= case_when(
    !LGBTQ %in% c("Gay", "Lesbian", "Bisexual") ~ "Other*",
    TRUE ~ LGBTQ)
  ) %>% 
  distinct(Name, ideology, LGBTQ_group) %>% 
  group_by(ideology,LGBTQ_group) %>% 
  count() %>% 
  mutate(ideology = factor(ideology, levels=c("Left-wing", "Center/Green", "Right-wing", "Other parties**"))) %>%
  mutate(LGBTQ_group = factor(LGBTQ_group, levels=c("Gay", "Lesbian", "Bisexual", "Other*"))) %>%
  ggplot(aes(fill=LGBTQ_group, y=n, x=ideology)) +
  geom_bar(position="stack", stat = "identity") +
  scale_fill_manual(values = met.brewer("Cross", type = "discrete", n=4), name="LGBTQI") +
  labs(title = "Since 1976 more than a thousand people identifying as\nLGBTQI has been elected into national governments",
       subtitle = "Left-wing parties tend to have the most LBGTQI representatives",
       caption = "Gilbert Fontana | Data: Andrew Reynolds. Queer Politicians Data. QP@P. Princeton. 2022.") +
  xlab("Party ideology") +
  ylab("Number of elected officials") +
  annotate("text", x = 2.5, y = -175,
           label = "* Include people identifying as queer, transgender, intersexual, pansexual or twospirited\n ** Include independent and others",
           lineheight=.8,
           family=font,
           color=txt_col,
           hjust=.5) + 
  coord_cartesian(ylim = c(0, 625), clip="off") +
  theme(
    panel.grid = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_text(hjust=1, color=txt_col, size=16, margin = margin(0,10,0,0)),
    axis.text = element_text(color=txt_col, size=14),
    plot.title = element_text(hjust=0, size=30, color=txt_col,lineheight=.8, margin=margin(0,0,0,0)),
    plot.subtitle = element_text(hjust=0, size=20, color=txt_col, margin=margin(10,0,20,0)),
    plot.caption = element_text(hjust=.5,margin=margin(30,0,10,0), size=12, color=txt_col),
    plot.background = element_rect(color=bg, fill=bg),
    plot.margin = margin(30,30,30,30),
    legend.position = c(.8,.8),
    legend.title = element_text(color = txt_col, size=16),
    legend.text = element_text(color = txt_col, size=14)
  ) +
  guides(fill = guide_legend(title.position="top", title.hjust = 0))


# Save
showtext_opts(dpi = 320) 

ggsave("LGBTQ.png",
       height = 8,
       width = 8,
       dpi=320,
       
)  

showtext_auto(FALSE)


