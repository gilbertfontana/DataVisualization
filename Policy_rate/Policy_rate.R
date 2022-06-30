


# Libs
library(tidyverse)
library(janitor)
library(MetBrewer)
library(showtext)

# Data
df <- readxl::read_excel("C:/Users/Gilbert/Documents/GitHub/DataVisualization/Policy_rate/cbpol_2206.xlsx", sheet="Monthly Series",skip = 3) %>% 
  clean_names() %>% 
  select(period, m_se) 


# Misc
font <- "Sen"
font_add_google(family=font, font)
theme_set(theme_minimal(base_family = font))
bg <- "#F8EBE4"
txt_col <- "black"
showtext_auto(enable = TRUE) 

# Plot
df %>% 
  ggplot(aes(x=period, y=m_se)) +
  geom_line() +
  annotate("text", x=as.POSIXct("1994-01-01"), y=40,
           label="In 1992, the Swedish Krona collapses due\nto a currency crisis and speculative attacks\nin the foreign exchange market.\nThe Riksbank respond by rapidly\nincraseing the policy rate. For a short\ntime in September 1992, the policy rate\nis set to 500 percent.",
           color=txt_col,
           family=font,
           fontface="bold",
           size=3,
           hjust=0,
           vjust=1) +
  annotate("text", x=as.POSIXct("2008-01-01"), y=7,
           label="In 2014 the Riksbank\ncut the policy rate to 0\npercent in order to\nincrease demand in the\neconomy and bring\ninflation up.",
           color=txt_col,
           family=font,
           fontface="bold",
           size=3,
           hjust=0,
           vjust=0) +
  geom_curve(
    aes(x = as.POSIXct("2019-01-01"), y = 7, xend = as.POSIXct("2017-01-01"), yend = 0),
    curvature = -0.25,
    arrow = arrow(length = unit(0.015, "npc"))
  ) +
  scale_x_datetime(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), breaks = seq(0,40,5)) +
  labs(
    title = "The Swedish Riksbanks Policy Rate",
    subtitle = "1946 - 2022",
    caption = "Gilbert Fontana | Data: Bank for International Settlements",
    y = "Policy Rate (end of month)"
  ) +
  coord_cartesian(clip="off") +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(color=txt_col, size=8),
    axis.title.y  = element_text(color=txt_col, size=9, hjust=.5),
    axis.title.x  = element_blank(),
    axis.line = element_line(),
    plot.title = element_text(hjust=.5,size=24, color=txt_col,lineheight=.8, face="bold", margin=margin(0,0,0,0)),
    plot.subtitle = element_text(hjust=.5,size=20, color=txt_col,lineheight=.8, face="bold", margin=margin(10,0,20,0)),
    plot.caption = element_text(hjust=.5, margin=margin(20,0,0,0), size=8, color=txt_col, face="bold"),
    plot.background = element_rect(color=bg, fill=bg),
    plot.margin = margin(30,60,30,30),
    legend.position = c(.3,.85),
    legend.title = element_blank(),
    legend.spacing.x = unit(0, 'cm'),
    legend.spacing.y = unit(0, 'cm'),
  )

# Save
showtext_opts(dpi = 320) 

ggsave("Policy_rate.png",
       height = 7,
       width = 7,
       dpi=320,
       
)  

showtext_auto(FALSE)  


