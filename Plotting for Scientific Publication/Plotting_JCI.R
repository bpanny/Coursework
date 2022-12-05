# This script generates plots of functional magnetic resonance imaging region of interest analysis for publication in a scientific journal

library(ggplot2)
library(dplyr)
library(gridExtra)
library(ggpubr)
library(ggrepel)

fil <- read.csv("/Users/pannyb/OneDrive - UPMC/Desktop/Ahmari Pilot/EMMs.csv", header=T)

names(fil)[1]<-paste("Group")

fil %>% glimpse()

# replace CR level with Compulsion-Related level
levels(fil$Image.Type)[match("CR",levels(fil$Image.Type))] <- "Compulsion-Related"

fil$Epoch <- factor(fil$Epoch, levels = c("Image Onset", "Button Press to Remove Image"))

# reorder factor levels
fil$region <- factor(fil$region, levels = c("bilatBA11", "R_Amyg","L_Amyg", "L_NucAcc", "R_NucAcc", "bilaVTA6mm"))

#Add new line to button press factor level
levels(fil$Epoch)[match("Button Press to Remove Image",levels(fil$Epoch))] <- "Button Press to 
Remove Image"

#replace region levels with names desired to be displayed in plot
levels(fil$region)[match("bilatBA11",levels(fil$region))] <- "A) Bilateral BA11"
levels(fil$region)[match("L_Amyg",levels(fil$region))] <- "C) Left Amygdala"
levels(fil$region)[match("R_Amyg",levels(fil$region))] <- "B) Right Amygdala"

# assign Right Amygdala plot to `ramyg`
ramyg <- fil %>% filter(region == "R_Amyg") %>%
  ggplot(mapping = aes(x = Epoch, y = mean, group=Group, color=Group, label=PWC)) +
  geom_point() +
  geom_line(size = 1, mapping = aes(linetype=Group)) + scale_linetype_manual(values=c("dashed","solid")) +
  geom_text_repel(size = 4.3, show.legend = FALSE, nudge_x = -.7, segment.colour = NA) +
  geom_errorbar(mapping = aes(ymin=mean-std..error, ymax=mean+std..error),
                width=.1, position=position_dodge(0.05)) +
  facet_wrap(~Image.Type) +
  labs(y = "Estimated Marginal Means", x = "", title = "") +
  scale_color_manual(values=c("red","blue")) +
  theme_grey(base_size = 15)

# assign Left Amygdala plot to `lamyg`
lamyg <- fil %>% filter(region == "L_Amyg") %>%
  ggplot(mapping = aes(x = Epoch, y = mean, group=Group, color=Group, label=PWC)) +
  geom_point() +
  geom_line(size = 1, mapping = aes(linetype=Group)) + scale_linetype_manual(values=c("dashed","solid")) +
  geom_text_repel(size = 4.3, show.legend = FALSE, nudge_x = -.7, segment.colour = NA) +
  geom_errorbar(mapping = aes(ymin=mean-std..error, ymax=mean+std..error),
                width=.1, position=position_dodge(0.05)) +
  facet_wrap(~Image.Type) +
  labs(y = "Estimated Marginal Means", x = "", title = "") +
  scale_color_manual(values=c("red","blue")) +
  theme_grey(base_size = 15)

# assign Three brain regions on the same facet_grid to `abc`
abc <- fil %>% filter(region == "A) Bilateral BA11" | region == "B) Right Amygdala" | region == "C) Left Amygdala", Group == "HC" | Group == "OCD") %>%
  ggplot(mapping = aes(x = Epoch, y = mean, color = Group, group = Group, label=PWC)) +
  geom_point() +
  geom_line(size = 1, mapping = aes(linetype=Group)) + scale_linetype_manual(values=c("dashed","solid")) +
  geom_text_repel(size = 4.3, show.legend = FALSE, nudge_x = -.9, segment.colour = NA) +
  geom_errorbar(mapping = aes(ymin=mean-std..error, ymax=mean+std..error), 
                width=.1, position=position_dodge(0.05)) +
  facet_grid(region ~ Image.Type) +
  labs(y = "Estimated Marginal Means", x = "Epoch", title = "Bilateral BA11, Right Amygdala, Left Amygdala") +
  scale_color_manual(values=c("red","blue")) +
  theme_grey(base_size = 15) 

# assign Left Nucleus Accumbens plot to `d`
d <- fil %>% filter(region == "L_NucAcc") %>%
  ggplot(mapping = aes(x = Epoch, y = mean, group=1, label=PWC)) +
  geom_point() +
  geom_line(size = 1) +
  geom_text_repel(size = 4.3, show.legend = FALSE, nudge_x = -.9, segment.colour = NA) +
  geom_errorbar(mapping = aes(ymin=mean-std..error, ymax=mean+std..error), 
                width=.1, position=position_dodge(0.05)) +
  labs(y = "Estimated Marginal Means", x = "Epoch", title = "Left{} Nucleus Accumbens") +
  theme_grey(base_size = 15)  

# assign Right Nucleus Accumbens plot to `e`
e <- fil %>% filter(region == "R_NucAcc") %>%
  ggplot(mapping = aes(x = Group, y = mean, group=Image.Type, color=Image.Type, label=PWC)) +
  geom_point() +
  geom_line(size = 1, mapping = aes(linetype=Image.Type),show.legend = FALSE) + scale_linetype_manual(values=c("dashed","solid", "dotted")) + 
  geom_text_repel(size = 4.3, show.legend = FALSE, nudge_x = -.5, segment.colour = NA) +
  geom_errorbar(mapping = aes(ymin=mean-std..error, ymax=mean+std..error), 
                width=.1, position=position_dodge(0.05)) +
  labs(y = "Estimated Marginal Means", x = "Group", title = "Right Nucleus Accumbens", color = "Image Type") +
  scale_color_manual(values=c("red","blue", "chocolate"))  +
  theme_grey(base_size = 15) + 
  theme(legend.key.size = unit(.15, 'cm'), #change legend key size
        legend.key.height = unit(.15, 'cm'), #change legend key height
        legend.key.width = unit(.15, 'cm'), #change legend key width
        legend.title = element_text(size=12), #change legend title font size
        legend.text = element_text(size=12),
        legend.justification = c(1, 0), legend.position = c(1, 0)) 

# assign Bilateral Venral Tegmental Area plot to `f`
f <- fil %>% filter(region == "bilaVTA6mm") %>%
  ggplot(mapping = aes(x = Epoch, y = mean, group=Group, color=Group, label=PWC)) +
  geom_point() +
  geom_line(size = 1, mapping = aes(linetype=Group),show.legend = FALSE) + scale_linetype_manual(values=c("dashed","solid")) + 
  geom_text_repel(size = 4.3, show.legend = FALSE, nudge_x = -.5, segment.colour = NA) +
  geom_errorbar(mapping = aes(ymin=mean-std..error, ymax=mean+std..error), 
                width=.1, position=position_dodge(0.05)) +
  labs(y = "Estimated Marginal Means", x = "Group", title = "Bilateral VTA") +
  scale_color_manual(values=c("red","blue")) +
  theme_grey(base_size = 15) +
  theme(legend.justification = c(1, 1), legend.position = c(1, 1))

# arrange abc, d, e, and f plots together
# abc takes the first row and is twice the height of the second row because it is a facet_grid plot
# d, e, and f are plotted in the second row and is half the height of the first row, since each are individual plots
# d, e, and f are plotted side by side and with their own unique labels.
ggarrange(abc, ggarrange(d, e, f,labels = c("D","E", "F"), nrow = 1), 
          labels=c("A - C"),
          nrow = 2,
          heights = c(2,1))
