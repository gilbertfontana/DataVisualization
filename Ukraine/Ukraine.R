

# Libs
library(tidyverse)
library(MetBrewer)
library(ggtext)
library(showtext)

# Data
df <- read.csv("https://raw.githubusercontent.com/gilbertfontana/DataVisualization/main/Ukraine/data.csv")
codes <- read.csv("https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv")

# Data cleaning
df2 <- left_join(
  df,
  codes,
  by=c("country"="alpha.2")
) %>% 
  select(country,name, agree) %>%
  mutate(name = replace(name, country == "EU27", "EU27"),
         name = replace(name, country == "EL", "Greece")
         ) %>% 
  select(-country) %>% 
  arrange(agree) %>% 
  mutate(name=factor(name, levels=name))


# Misc
font <- "Josefin Sans"
font_add_google(family=font, font)
bg <- "#85586F"
txt_col <- "grey90"
theme_set(theme_minimal(base_family = font))
showtext_auto(enable = TRUE) 

eucol <- ifelse(df2$name == "EU27", "#ef8737", txt_col)

# Plot
df2 %>% 
  ggplot(aes(y=name, x=agree)) +
  geom_point(color=NA) +
  annotate("segment",x = 70,y=15,xend = 89 ,yend = 15, color=txt_col) +
  geom_point(aes(color=agree),size=3) +
  scale_color_gradientn(colors = rev(met.brewer("Tam"))) +
  scale_x_continuous(limits = c(70,100), expand = c(0,0)) +
  xlab("Share (%) agreeing to the statment: \"I feel sympathy towards Ukrainians\"") +
  labs(
    title = "A vast majority feel sympathy towards Ukrainians",
    subtitle = "- But Bulgarians and Hungarians feels the least",
    caption = "Gilbert Fontana | Data: Eurobarometer (May 2022)"
  )  +
  coord_cartesian(clip="off") +
  theme(
    panel.grid = element_blank(),
    panel.grid.major.x = element_line(linetype = "dotted", color = txt_col),
    axis.title.x = element_text(hjust=.5, color=txt_col, size=10, margin = margin(10,0,0,0)),
    axis.title.y = element_blank(),
    axis.text = element_text(color=txt_col, size=8),
    axis.text.y = element_text(color=eucol,
                               size=10),
    axis.line.y = element_line(color = txt_col),
    plot.title = element_text(hjust=-0, size=16, color=txt_col,lineheight=.8, face="bold", margin=margin(0,0,0,0)),
    plot.subtitle = element_text(hjust=0, size=14, color=txt_col, margin=margin(10,0,20,0)),
    plot.caption = element_text(hjust=.5,margin=margin(10,0,0,0), size=8, color=txt_col, face="bold"),
    plot.background = element_rect(color=bg, fill=bg),
    plot.margin = margin(30,30,30,30),
    legend.position = "none"
  )


# Save
showtext_opts(dpi = 320) 

ggsave("Ukraine.png",
       height = 7,
       width = 7,
       dpi=320,
       
)  

showtext_auto(FALSE)  

                                               