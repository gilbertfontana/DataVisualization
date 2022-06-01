

# Lib
library(tidyverse)
library(janitor)
library(MetBrewer)
library(showtext)


# Data
df <- read_csv("https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2019_TotalPopulationBySex.csv")

df2 <- df %>% 
  filter(Variant=="Medium", LocID<900) %>% 
  select(Location,Time, PopTotal)

# Misc
font <- "Raleway"
font_add_google(family=font, font)
theme_set(theme_minimal(base_family = font))
bg <- "#F5F5F3"
txt_col <- "grey20"
showtext_auto(enable = TRUE)
pal <- c("#A06570","#A07865","#A0658E")


# Plot
df2 %>% 
  ggplot() +
  geom_line(data=df2 %>% filter(!Location %in% c("India", "China", "Nigeria"), Time<=2019), aes(x=Time, y=PopTotal, group=Location), color="grey80") +
  geom_line(data=df2 %>% filter(!Location %in% c("India", "China", "Nigeria"), Time>2019), aes(x=Time, y=PopTotal, group=Location), color="grey80", linetype="dashed") +
  geom_line(data=df2 %>% filter(Location %in% c("India"), Time<=2019), aes(x=Time, y=PopTotal), color="#A06570")  +
  geom_line(data=df2 %>% filter(Location %in% c("China"), Time<=2019), aes(x=Time, y=PopTotal), color="#A07865")  +
  geom_line(data=df2 %>% filter(Location %in% c("Nigeria"), Time<=2019), aes(x=Time, y=PopTotal), color="#A0658E")  +
  
  geom_line(data=df2 %>% filter(Location %in% c("India"), Time>2019), aes(x=Time, y=PopTotal), color="#A06570", linetype="dashed")  +
  geom_line(data=df2 %>% filter(Location %in% c("China"), Time>2019), aes(x=Time, y=PopTotal), color="#A07865", linetype="dashed")  +
  geom_line(data=df2 %>% filter(Location %in% c("Nigeria"), Time>2019), aes(x=Time, y=PopTotal), color="#A0658E", linetype="dashed")  +
  

    geom_point(data=df2 %>% filter(Location %in% c("India", "China", "Nigeria"), Time==2100), aes(x=Time, y=PopTotal, group=Location),
             color=pal) +
  geom_text(data=df2 %>% filter(Location %in% c("India", "China", "Nigeria"), Time==2100),
            aes(x = max(Time) + 2, y = PopTotal, label=Location),
            hjust = 0,
            vjust = 0.5,
            size=3,
            family=font,
            fontface="bold",
            color=pal) +
  geom_vline(xintercept = 2019) +
  scale_x_continuous(expand = c(0,0),
                     breaks = seq(1950,2100,25)) +
  scale_y_continuous(expand = c(0,0),
                     labels=function(x) format(x, big.mark = " ", scientific = FALSE)) +
  coord_cartesian(clip="off") +
  labs(
    title = "World Population Today\n- And Tomorrow",
    caption = "Gilbert Fontana | Data: UN World Population Prospects 2019",
    y="Total population (thousands)"
  ) +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(color=txt_col, size=8),
    axis.title.y  = element_text(color=txt_col, size=9, hjust=.5),
    axis.title.x  = element_blank(),
    axis.line = element_line(),
    plot.title = element_text(hjust=0,size=22, color=txt_col,lineheight=.8, face="bold", margin=margin(0,0,30,0)),
    plot.caption = element_text(hjust=.5, margin=margin(20,0,0,0), size=8, color=txt_col, face="bold"),
    plot.title.position = "plot",
    plot.background = element_rect(color=bg, fill=bg),
    plot.margin = margin(20,60,20,20)
  )


# Save
showtext_opts(dpi = 320) 

ggsave("Population.png",
       height = 7,
       width = 7,
       dpi=320,
       
)  

showtext_auto(FALSE)  


