

# Libs
library(tidyverse)
library(janitor)
library(MetBrewer)
library(showtext)
library(lubridate)
library(zoo)

# Data
df <- read_csv(
"C:/Users/Gilbert/Documents/GitHub/DataVisualization/Wealth/dfa-networth-shares.csv"
) %>% clean_names()


df2 <- df %>% 
  select(date, category, net_worth) %>% 
  mutate(year=as.numeric(substr(date,1,4)),
         quarter=as.numeric(str_sub(date,-1,-1))) %>% 
  mutate(date2=as.yearqtr(paste(year,quarter,sep="-"))) %>% 
  select(-year, -quarter, -date) %>% 
  pivot_wider(
    names_from = category,
    values_from = net_worth
  ) %>% 
  clean_names() %>% 
  mutate(top10=top1+next9,
         bottom90=next40+bottom50) %>% 
  select(date2,top10,bottom90) %>% 
  pivot_longer(
    !date2,
    names_to = "group",
    values_to = "values"
  ) %>% 
  mutate(label =
           case_when(
             group=="top10" ~ "Top 10 percent",
             TRUE ~ "Bottom 90 percent"
           ))

# Misc
font <- "Copse"
font_add_google(family=font, font)
theme_set(theme_minimal(base_family = font))
bg <- "white"
txt_col <- "grey20"
showtext_auto(enable = TRUE)
col <- c("#006491", "#912D00")

# Plot
df2 %>% 
  ggplot() +
  geom_line(aes(x=date2, y=values, color=group)) +
  geom_point(data=df2 %>% filter(date2=="2021 Q4"),aes(x=date2, y=values, color=group)) +
  geom_text(data=df2 %>% filter(date2=="2021 Q4"),
            aes(x = max(date2) + .5, y = values, label=label, color=group),
            hjust = 0,
            vjust = 0.5,
            size=3,
            family=font,
            fontface="bold") +
  scale_y_continuous(limits = c(0,100),expand = c(0,0), breaks = seq(25,100,25)) +
  scale_x_continuous(expand = c(0,0)) +
  scale_color_manual(values = col) +
  coord_cartesian(clip="off") +
  labs(
    title="The U.S. Household Wealth Distribution",
    subtitle = "The 10 percent wealthiest households possess almost\n70 percent of the total household wealth",
    caption = "Gilbert Fontana | Data: The Federal Reserve",
    y="Share of total wealth (%)"
  ) +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(color=txt_col, size=9),
    axis.title.y  = element_text(color=txt_col, size=10, hjust=.5),
    axis.title.x  = element_blank(),
    axis.line = element_line(),
    plot.title = element_text(hjust=0,size=22, color=txt_col,lineheight=.8, face="bold", margin=margin(0,0,10,0)),
    plot.subtitle = element_text(hjust=0,size=14, color=txt_col, margin=margin(0,0,20,0)),
    plot.caption = element_text(hjust=.5, margin=margin(20,0,0,0), size=10, color=txt_col, face="bold"),
    plot.title.position = "plot",
    plot.background = element_rect(color=bg, fill=bg),
    plot.margin = margin(30,120,30,30),
    legend.position = "none"
  )

# Save
showtext_opts(dpi = 320) 

ggsave("Wealth.png",
       height = 7,
       width = 7,
       dpi=320,
       
)  

showtext_auto(FALSE)  




