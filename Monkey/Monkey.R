


# Libs
library(tidyverse)
library(janitor)
library(MetBrewer)
library(showtext)


# Data
df <- read_csv("https://raw.githubusercontent.com/globaldothealth/monkeypox/main/timeseries-country-confirmed.csv")

top5 <- df %>% 
  group_by(Country) %>% 
  slice_max(n=1, order_by = Cumulative_cases) %>% 
  arrange(desc(Cumulative_cases)) %>% 
  ungroup() %>% 
  slice_head(n=5) %>% 
  pull(Country)

df2 <- df %>% 
  filter(Country %in% c(top5)) %>% 
  select(Country, Date, Cumulative_cases) 

# Misc
font <- "Marvel"
font_add_google(family=font, font)
theme_set(theme_minimal(base_family = font))
bg <- "#E8E8D0"
txt_col <- "grey20"
showtext_auto(enable = TRUE)


# Plot
df2 %>% 
  ggplot() +
  geom_line(aes(x=as.Date(Date), y=Cumulative_cases, color=Country))  +
  geom_point(data=df2 %>% group_by(Country) %>% slice_max(n=1,order_by = Date),
             aes(x=as.Date(Date), y=Cumulative_cases, color=Country)) +
  geom_text(data=df2 %>% group_by(Country) %>% slice_max(n=1,order_by = Date),
            aes(x = Date + .5, y = Cumulative_cases, label=Country, color=Country),
            hjust = 0,
            vjust = 0.5,
            size=3,
            family=font,
            fontface="bold") +
  labs(
    title = "The development of the Monkeypox virus\nin the five countries with most cases",
    caption = "Gilbert Fontana | Data: Global.health Monkeypox",
    y="Cumulative number of cases"
  ) +
  scale_x_date(expand = c(0,0), date_labels = "%Y-%m-%d") +
  scale_y_continuous(expand = c(0,0),
                     labels=function(x) format(x, big.mark = " ", scientific = FALSE)) +
  scale_color_manual(values = met.brewer("Johnson")) +
  coord_cartesian(clip="off") +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(color=txt_col),
    axis.title.y  = element_text(color=txt_col, hjust=.5),
    axis.title.x  = element_blank(),
    axis.line = element_line(),
    plot.title = element_text(hjust=0.5,size=26, color=txt_col,lineheight=1, face="bold", margin=margin(0,0,20,0)),
    plot.caption = element_text(hjust=.5, margin=margin(20,0,0,0), size=8, color=txt_col, face="bold"),
    plot.title.position = "plot",
    plot.background = element_rect(color=bg, fill=bg),
    plot.margin = margin(30,50,30,30),
    legend.position = "none"
  )


# Save
showtext_opts(dpi = 320) 

ggsave("Monkey.png",
       height = 7,
       width = 7,
       dpi=320,
       
)  

showtext_auto(FALSE)  
