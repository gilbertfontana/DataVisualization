


# Libs

library(tidyverse)
library(janitor)
library(MetBrewer)
library(showtext)
library(ggstream)


# Data
df <- read_csv("https://raw.githubusercontent.com/cghss/dons/master/Data/DONdatabase.csv")

top <- df %>% 
    group_by(DiseaseLevel1) %>% 
    count(sort = TRUE) %>% 
    ungroup() %>% 
    slice_head(n=8)
  
df2 <- df %>% 
mutate(year=str_sub(ReportDate,-4,-1)) %>% 
mutate(disease = case_when( DiseaseLevel1 %in% top$DiseaseLevel1 ~ DiseaseLevel1,
    TRUE ~ "Other")
  ) %>% 
  group_by(year,disease) %>% 
  count() %>% 
  mutate(year=as.numeric(year)) %>% 
  arrange(year,n)

# Misc
font <- "Overpass"
font_add_google(family=font, font)
theme_set(theme_minimal(base_family = font))
bg <- "#EDEADE"
txt_col <- "grey10"
showtext_auto(enable = TRUE) 


# Plot
df2 %>% 
ggplot() +
  geom_stream(aes(year, n, fill=disease),extra_span = .5,true_range = "none") +
  scale_fill_manual(values = met.brewer("Cross", type = "discrete", n=9),name = "Disease type") +
  scale_x_continuous(labels = seq(1995,2020,5),
                     breaks = seq(1995,2020,5),
                     sec.axis = sec_axis(trans=~.)
                     ) +
  coord_cartesian(xlim = c(1985,2030), clip = "off") +
  labs(title = "Pre Covid Disease Outbreaks",
       subtitle= "Diseases covered in WHO:s Disease Outbreak News",
       caption = "Gilbert Fontana | Data: Carlson, Colin J., et al. (2022)"
  ) +
  theme(
    panel.grid = element_blank(),
    panel.grid.major.x = element_line(linetype = "dotted"),
    axis.line.x = element_line(),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(color=txt_col, size=10),
    axis.text.x.top = element_blank(),
    plot.title = element_text(hjust=.5,size=24, color=txt_col,lineheight=.8, face="bold", margin=margin(0,0,0,0)),
    plot.subtitle = element_text(hjust=.5,size=14, color=txt_col, margin=margin(10,0,20,0)),
    plot.caption = element_text(hjust=.5, margin=margin(20,0,0,0), size=8, color=txt_col, face="bold"),
    plot.background = element_rect(color=bg, fill=bg),
    plot.margin = margin(30,30,30,30),
    legend.position = "bottom",
    legend.title = element_text(face="bold")
  ) +
  guides(fill = guide_legend(title.position= "top",
                             title.hjust = .5,
                             nrow=3,byrow=TRUE))


# Save
showtext_opts(dpi = 320) 

ggsave("Disease.png",
       height = 7,
       width = 7,
       dpi=320,
       
)  

showtext_auto(FALSE)  



