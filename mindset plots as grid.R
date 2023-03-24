#modify plots to make them viewable as one grid
#can use plot_grid to display multiple plots on a grid, need cowplot library
#need to make new versions of the plots that don't have long titles for it to work?
####create plots for farmers' responses to these 3 questions about change in mindset

plot_AgreeRisk.F <- 
  ggplot(data=Mindset_f, mapping = aes(x = AgreeRisk.F, fill=AgreeRisk.F)) +
  geom_bar() +
  labs(x = " ",
       y = "Farmers") +
  scale_x_discrete(drop = FALSE) + #show levels with empty data to make charts consistent
  ylim(0, 11) + #make y-axis the same for all charts to show differences
  scale_fill_brewer(direction = -1, drop = FALSE) + #add color, fill specified in aes
  theme(legend.position = "right", plot.title=element_text(size=12), axis.text=element_text(size=8), 
        axis.text.x=element_blank(), axis.ticks.x=element_blank()) + #take out legend, unnecessary
  ggtitle("think about climate risks I face in a new way")

plot_AgreeAdapt.F <- 
  ggplot(data=Mindset_f, mapping = aes(x = AgreeAdapt.F, fill=AgreeAdapt.F)) +
  geom_bar() +
  labs(x = " ",
       y = "Farmers") +
  scale_x_discrete(drop = FALSE) +
  ylim(0, 11) +
  scale_fill_brewer(direction = -1, drop = FALSE) + 
  theme(legend.position = "none", plot.title=element_text(size=12), axis.text=element_text(size=8), 
        axis.text.x=element_blank(), axis.ticks.x=element_blank()) + 
  ggtitle("think about climate adaptations I could \nuse in a new way")

plot_AgreeConf.F <- 
  ggplot(data=Mindset_f, mapping = aes(x = AgreeConf.F, fill=AgreeConf.F)) +
  geom_bar() +
  labs(x = " ",
       y = "Farmers") +
  ylim(0, 11) +
  scale_fill_brewer(direction = -1, drop = FALSE) + 
  theme(legend.position = "none", plot.title=element_text(size=12), axis.text=element_text(size=8), 
        axis.text.x=element_blank(), axis.ticks.x=element_blank()) + 
  scale_x_discrete(drop = FALSE, guide = guide_axis(n.dodge=3)) + #avoid overlapping x-labels due to more missing data
  ggtitle("increased confidence to address climate change \non my farm")

####create plots for advisors' responses to these 3 questions about change in mindset

plot_AgreeRisk.A<- 
  ggplot(data=Mindset_aa, mapping = aes(x = AgreeRisk.A, fill=AgreeRisk.A)) +
  geom_bar() +
  labs(x = " ",
       y = "Advisors") +
  scale_x_discrete(drop = FALSE) + #show levels with empty data to make charts consistent
  ylim(0, 11) + #make y-axis the same for all charts to show differences
  scale_fill_brewer(direction = -1, drop = FALSE) + #add color, fill specified in aes
  theme(legend.position = "none", plot.title=element_text(size=12), axis.text=element_text(size=8), 
        axis.text.x=element_blank(), axis.ticks.x=element_blank()) + #take out legend, unnecessary
  ggtitle("think about how to address climate risks \nin my programs in a new way")

plot_AgreeAdapt.A <- 
  ggplot(data=Mindset_aa, mapping = aes(x = AgreeAdapt.A, fill=AgreeAdapt.A)) +
  geom_bar() +
  labs(x = " ",
       y = "Advisors") +
  scale_x_discrete(drop = FALSE) +
  ylim(0, 11) +
  scale_fill_brewer(direction = -1, drop = FALSE) + 
  theme(legend.position = "none", plot.title=element_text(size=12), axis.text=element_text(size=8), 
        axis.text.x=element_blank(), axis.ticks.x=element_blank()) + 
  ggtitle("think about how to address climate \nadaptations in my programs in a new way")

plot_AgreeConf.A <- 
  ggplot(data=Mindset_aa, mapping = aes(x = AgreeConf.A, fill=AgreeConf.A)) +
  geom_bar() +
  labs(x = " ",
       y = "Advisors") +
  ylim(0, 11) +
  scale_fill_brewer(direction = -1, drop = FALSE) + 
  theme(legend.position = "none", plot.title=element_text(size=12), axis.text=element_text(size=8), 
        axis.text.x=element_blank(), axis.ticks.x=element_blank()) + 
  scale_x_discrete(drop = FALSE, guide = guide_axis(n.dodge=3)) + #avoid overlapping x-labels due to more missing data
  ggtitle("increased confidence to incorporate climate \nissues into my programs")

#can use plot_grid to display multiple plots on a grid, need cowplot library
#need to make new versions of the plots that don't have long titles for it to work?
library(cowplot)
prow1 <- plot_grid(plot_AgreeRisk.F + theme(legend.position="none"), plot_AgreeAdapt.F, plot_AgreeConf.F, nrow=1, ncol=3)
prow2 <- plot_grid(plot_AgreeRisk.A, plot_AgreeAdapt.A, plot_AgreeConf.A, nrow=1, ncol=3)

legend <- get_legend (plot_AgreeRisk.F + theme(legend.box.margin = margin(0,0,0,4)))

#add the legend to prow2 and set widths so it displays nicely
prow2 <- plot_grid(plot_AgreeRisk.A, plot_AgreeAdapt.A, plot_AgreeConf.A, legend, nrow=1, ncol=4, rel_widths = c(1,1,1,.6))

#plot together! don't specify number of columns, since it is 3 on top and 4 on bottom
plot_grid(prow1, prow2, nrow=2)
