# I am confident in my ability, as a farmer, to adapt to climate change in my operation

#no similar question in application

# In 2021 workshop evaluation data, Q31.2  "I am confident in my ability, as a farmer, to adapt to climate change in my operation"  ####
TW1_Q31.2 <- COMBOdata%>%
  group_by(wkshp1.Q31.2)%>%
  summarize(n = n(), Statement = "2021 post-workshop")%>%  
  drop_na() %>%
  mutate(perc = round(n/sum(n)*100, 0))%>%
  rename(Response = wkshp1.Q31.2) 

# In 2022 workshop evaluation data, Q2.1.1  I am confident in my ability, as a farmer, to adapt to climate change in my operation ####
TW2_2.1.2 <- COMBOdata %>%
  group_by(wkshp2.Q2.1.2) %>%
  summarize(n = n(), Statement = "2022 post-workshop")%>%  
  drop_na() %>%
  mutate(perc = round(n/sum(n)*100, 0))%>%
  rename(Response = wkshp2.Q2.1.2) 

#add in 2023 survey!
TS1_Q2.1_2 <- CAF2023data %>%
  group_by(Q2.1_2) %>%
  summarize(n = n(), Statement = "2023 1-yr post-CAF")%>%  
  drop_na() %>%
  mutate(perc = round(n/sum(n)*100, 0))%>%
  rename(Response = Q2.1_2) 

#Join dataframes #### 
Farmeradapt_combo <- rbind(TW1_Q31.2,
                         TW2_21.2, TS1_Q2.1_2)

Farmeradapt_combo$Statement <- factor(Farmeradapt_combo$Statement, levels=c("2023 1-yr post-CAF", "2022 post-workshop", "2021 post-workshop"))

as.tibble(Farmeradapt_combo)

# STEP 3: Create stacked bar chart ####
Farmeradapt_plot <- ggplot(Farmeradapt_combo, aes(             
  x = Statement,                             # ID data frame column for X axis
  y = perc,  # ID data frame column for Y axis
  fill = factor(                             
    Response,                                # ID data frame column for fill (bars)
    levels = c("Strongly agree",                 # set string values in manual order
               "Somewhat agree",
               "Neutral",
               "Somewhat disagree",
               "Strongly disagree")
  )
)) +
  geom_bar(stat = "identity", width = 0.5) + # manually set bar width
  # stat="identity" to geom_bar() then you're telling R to calculate the sum of the y variable, grouped by the x variable and use bars to display the sums
  geom_text(
    aes(label = paste0(perc, "%")),          # add % labels #paste0 is for concatenating character vectors
    # add %
    position = "stack",                      # position % labels and use adjust to tweak placement
    #align labels with bars
    vjust = .5,
    hjust = .75,
    size = 4,                                # label text size
    color = 'black'                          # label text color
  ) +
  scale_x_discrete(drop = FALSE) + #show levels with empty data to make charts consistent
  scale_fill_brewer(palette = "BrBG", direction = -1, drop = FALSE) +      # set bar color scheme
  #facet_grid( ~ Statement) +                 # ID data from column if more than one facet
  coord_flip() +
  labs(
    y = "percent of respondents",
    fill = "",
    x = "",
    title = "CAF Fellow agreement with statement: 'I am confident in my ability, as a farmer, to adapt to climate change in my operation'"
  ) +
  guides(fill = guide_legend(reverse = TRUE))+
  #theme(axis.text.x = element_text(angle = 45)) +
  theme(legend.position = "bottom") 

Farmeradapt_plot
