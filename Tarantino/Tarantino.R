

# Libs
library(tidyverse)
library(janitor)
library(MetBrewer)
library(showtext)

# Data
df <- read_csv("https://raw.githubusercontent.com/fivethirtyeight/data/master/tarantino/tarantino.csv") %>% 
  mutate(total_runtime= case_when(
    movie == "Reservoir Dogs" ~ 99,
    movie ==  "Pulp Fiction" ~ 154,
    movie ==  "Kill Bill: Vol. 1" ~ 111,
    movie == "Kill Bill: Vol. 2" ~ 137,
    movie == "Inglorious Basterds" ~ 147,
    movie == "Django Unchained" ~ 165,
    movie == "Jackie Brown" ~ 148),
    minutes_in_standardised = minutes_in/total_runtime*100
    )

df2 <- df %>% filter(type=="death") %>% 
  arrange(movie, minutes_in_standardised) %>% 
  group_by(movie) %>% 
  mutate(cumsum=cumsum(type=="death")) %>%
  ungroup()

# Misc
font <- "Limelight"
font_add_google(family=font, font)
theme_set(theme_minimal(base_family = font))
bg <- "#D6D5CB"
txt_col <- "grey10"
showtext_auto(enable = TRUE) 

# Plot
df2 %>% 
  mutate(movie = fct_relevel(movie, rev(df2 %>% group_by(movie) %>% slice_tail(n=1) %>% arrange(cumsum) %>% pull(movie)))) %>% 
  ggplot() +
  geom_line(aes(x=minutes_in_standardised, y=cumsum, color=movie)) +
  geom_point(data = df2 %>% group_by(movie) %>% slice_tail(n=1), aes(x=minutes_in_standardised, y=cumsum, color=movie), size=2) +
  scale_color_manual(values = met.brewer("Degas")) +
  scale_y_continuous(expand = c(0,0), limits = c(0,75), breaks = seq(25,75,25)) +
  labs(title = "Bloody Tarantino Movies",
       caption = "Gilbert Fontana | Data: FiveThirtyEight",
       x="Standardized movie length",
       y="Cumulative number of deaths",
       ) + 
  coord_cartesian(clip="off") +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(color=txt_col, size=8),
    axis.title  = element_text(color=txt_col, size=9, hjust=1),
    axis.line = element_line(),
    plot.title = element_text(hjust=.5,size=30, color=txt_col,lineheight=.8, face="bold", margin=margin(0,0,30,0)),
    plot.caption = element_text(hjust=.5, margin=margin(20,0,0,0), size=8, color=txt_col, face="bold"),
    plot.background = element_rect(color=bg, fill=bg),
    plot.margin = margin(30,30,30,30),
    legend.position = "bottom",
    legend.title = element_blank()
  ) +
  guides(color = guide_legend(title.position= "top",
                             title.hjust = .5,
                             nrow=2,byrow=TRUE))


# Save
showtext_opts(dpi = 320) 

ggsave("Tarantino.png",
       height = 7,
       width = 7,
       dpi=320,
       
)  

showtext_auto(FALSE)  





