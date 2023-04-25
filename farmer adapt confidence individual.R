#visualize farmer's confidence in ability to adapt as individual plots over time, use R's knowledge script

## Dataframe subset ####

farmeradapt1 <- COMBOdata[, c("NAME", "wkshp1.Q31.2", "wkshp2.Q2.1.2")]

CAF2023data$Q2.1_2 <- as.factor(CAF2023data$Q2.1_2)
as.tibble(farmeradapt)

farmeradapt2 <- CAF2023data[, c("NAME", "Q2.1_2")]

#need to merge the two dataframes. However, some mismatch in names do to different survey entries. 
#Export data to clean it. Combine duplicate rows, and remove advisors who didn't answer this question at the same time.
#then import cleaned csv
farmeradapt <- merge(farmeradapt1, farmeradapt2, by = "NAME", all = TRUE)
farmeradapt_cleaned <- read.csv("~/R/CAF-eval-analysis/farmeradapt_cleaned.csv", 
                              header=TRUE)
#change all the agreement rankings to numbers, one column at a time, because couldn't figure out how to do it all at once
farmeradapt_num <- farmeradapt_cleaned %>%
  mutate(
    wkshp1.Q31.2 = case_when(
      wkshp1.Q31.2 == "Strongly agree" ~ "5",
      wkshp1.Q31.2 == "Somewhat agree" ~ "4",
      wkshp1.Q31.2 == "Neutral" ~ "3",
      wkshp1.Q31.2 == "Somewhat disagree" ~ "2",
      wkshp1.Q31.2 == "Strongly disagree" ~ "1",
      TRUE ~ wkshp1.Q31.2), 
      ) 
farmeradapt_num <- farmeradapt_num %>%
  mutate(
    wkshp2.Q2.1.2  = case_when(
      wkshp2.Q2.1.2 == "Strongly agree" ~ "5",
      wkshp2.Q2.1.2 == "Somewhat agree" ~ "4",
      wkshp2.Q2.1.2 == "Neutral" ~ "3",
      wkshp2.Q2.1.2 == "Somewhat disagree" ~ "2",
      wkshp2.Q2.1.2 == "Strongly disagree" ~ "1",
      TRUE ~ wkshp2.Q2.1.2), 
  ) 
farmeradapt_num <- farmeradapt_num %>%
  mutate(
    Q2.1_2  = case_when(
      Q2.1_2 == "Strongly agree" ~ "5",
      Q2.1_2 == "Somewhat agree" ~ "4",
      Q2.1_2 == "Neutral" ~ "3",
      Q2.1_2 == "Somewhat disagree" ~ "2",
      Q2.1_2 == "Strongly disagree" ~ "1",
      TRUE ~ Q2.1_2), 
  ) 

farmeradapt_num <- farmeradapt_num %>%
  rename("2021" = "2021") %>% #had already changed this one, so code wasn't running
  rename("2022" = "wkshp2.Q2.1.2") %>%
  rename("2023" = "Q2.1_2")

ggplot(farmeradapt_num, aes(x=wt, y=)) + geom_

#rearrange the farmer adapt df so that all of the data is in two columns, year and confidence score, so that it can be graphed
farmeradapt_tidy <-
  gather(
    farmeradapt_num,
    key="Year", value=
    "Confidence",
    2:4,
      )
tibble(farmeradapt_tidy)

#order the rows in the df, using the order specified, drawing on these two columns. Order all columns.
farmeradapt_tidy <- farmeradapt_tidy[with(farmeradapt_tidy, order(farmeradapt_tidy$NAME, farmeradapt_tidy$Year)), 1:3]


#set the levels like this...
farmeradapt_tidy$Year <- factor(c("2021", "2022", "2023"), levels = c("2021", "2022", "2023"))
farmeradapt_tidy$Confidence <- as.numeric(farmeradapt_tidy$Confidence)

#individual box plots for each fellow####
#co plot is a conditioning plot
#A formula of the form y ~ x | a indicates that plots of y versus x should be produced conditional on the variable a.
#You can also specify the type of graph you wish to plot using the argument type =. You can plot just the points (type = "p", this is the default), just lines (type = "l"), both points and lines connected (type = "b"), both points and lines with the lines running through the points (type = "o") and empty points joined by lines (type = "c").

PANELplot_farmeradapt <-
  coplot(
    Confidence ~ Year |
      NAME,
    type = "b", 
    data = farmeradapt_tidy, 
    panel = points,
    rows=2, columns=11,
    col = farmeradapt_tidy$Confidence,
    pch = 10,
    show.given = FALSE, 
  )

#can try to do a confidence and year, conditional on name, but in a bar chart. 
library(lattice)
BARplot_farmeradapt <-barchart(Confidence ~ Year | NAME, data = farmeradapt_tidy)
BARplot_farmeradapt    
