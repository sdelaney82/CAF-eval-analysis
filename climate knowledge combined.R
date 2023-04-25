#recreate Rachel's climate perspectives work so can build on it
#first load data

#make a dataframe from the loaded data, preserve headers and put NA into the blank cells
COMBOdata <- read.csv("~/R/CAF-eval-analysis/Combined_dataset.csv", 
                      header=TRUE, sep = ",", na.strings=c("", "NA"))

#remove top header rows with questions and import id descriptions. 
COMBOdata <- COMBOdata[-c(1,2),]

CAF2023data<- read.csv("~/R/CAF-eval-analysis/CAF_2023.csv", 
                      header=TRUE, sep = ",", na.strings=c("", "NA"))
CAF2023data <- CAF2023data[-c(1,2),]

CAF2023data <- CAF2023data %>%
  rename(NAME = Q1.3_4)


#app.Q19  Please complete the following statement. "I consider myself to be..." ####
TappQ19 <- COMBOdata %>%
  group_by(app.Q19) %>%
  summarize(n = n(), Statement = "I consider myself to be knowledgeable about climate change")%>%  
  drop_na() %>%
  mutate(perc = round(n/sum(n)*100, 0))%>%
  rename(Response = app.Q19)

Q1.3_4

#modify the responses in the application data to match those in the evaluation data. Could not figure out how to do it in one argument,
#but this works. 
TappQ19rev1 <-data.frame(lapply(TappQ19, function(x) {
  gsub("Very knowledgeable about climate change", "Strongly agree", x)
  }))
TappQ19rev2 <-data.frame(lapply(TappQ19rev1, function(x) {
  gsub("Somewhat knowledgeable about climate change", "Somewhat agree", x)
}))
TappQ19rev3 <-data.frame(lapply(TappQ19rev2, function(x) {
  gsub("Neither knowledgeable nor unknowledgeable about climate change", "Neutral", x)
}))
TappQ19rev <-data.frame(lapply(TappQ19rev3, function(x) {
  gsub("Unknowledgeable about climate change", "Somewhat disagree", x)
}))

#this should have worked but it didn't work!
TappQ19rev$Response <- factor(TappQ19rev$Response, levels = c("Strongly agree","Somewhat agree","Neutral","Somewhat disagree","Strongly disagree"))
as.tibble(TappQ19rev)

#trying again, manually making a new df
TappQ19revA <- data.frame(Response=c("Strongly agree","Somewhat agree","Neutral","Somewhat disagree","Strongly disagree"), Statement=c("2020 Application", "2020 Application", "2020 Application","2020 Application", "2020 Application"),
n=c(8,22,3,1,0), perc=c(24,65,9,3,0))

# In 2021 workshop evaluation data, Q31.10  I consider myself to be knowledgeable about climate change ####
TW1_Q31.10 <- COMBOdata %>%
  group_by(wkshp1.Q31.10) %>%
  summarize(n = n(), Statement = "2021 post-workshop")%>%  
  drop_na() %>%
  mutate(perc = round(n/sum(n)*100, 0))%>%
  rename(Response = wkshp1.Q31.10) 

# In 2022 workshop evaluation data, Q2.1.10  I consider myself to be knowledgeable about climate change ####
TW2_2110 <- COMBOdata %>%
  group_by(wkshp2.Q2.1.10) %>%
  summarize(n = n(), Statement = "2022 post-workshop")%>%  
  drop_na() %>%
  mutate(perc = round(n/sum(n)*100, 0))%>%
  rename(Response = wkshp2.Q2.1.10) 

#add in 2023 survey!
TS1_Q2.1_4 <- CAF2023data %>%
  group_by(Q2.1_4) %>%
  summarize(n = n(), Statement = "2023 1-yr post-CAF")%>%  
  drop_na() %>%
  mutate(perc = round(n/sum(n)*100, 0))%>%
  rename(Response = Q2.1_4) 

#Join dataframes #### 
knowledge_CC_combo <- rbind(TappQ19revA,
                 TW1_Q31.10,
                 TW2_2110, TS1_Q2.1_4)

knowledge_CC_combo$Statement <- factor(knowledge_CC_combo$Statement, levels=c("2023 1-yr post-CAF", "2022 post-workshop", "2021 post-workshop","2020 Application"))
  
as.tibble(knowledge_CC_combo)

# STEP 3: Create stacked bar chart ####
knowledge_cc_plot <- ggplot(knowledge_CC_combo, aes(             
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
  scale_fill_brewer(palette = "BrBG", direction = -1, drop = FALSE) +      # set bar color scheme, green as agree, show levels with empty data
  #facet_grid( ~ Statement) +                 # ID data from column if more than one facet
  coord_flip() +
  labs(
    y = "percent of respondents",
    fill = "",
    x = "",
    title = "CAF Fellow agreement with statement: 'I consider myself to be knowledgeable about climate change'"
  ) +
  guides(fill = guide_legend(reverse = TRUE))+
  #theme(axis.text.x = element_text(angle = 45)) +
  theme(legend.position = "bottom") 

knowledge_cc_plot
