library(ggtree)
library(tidyverse)
library(tidytree)
library(ape)
library(treeio)
library(readxl)
library(dplyr)
library(data.table)
library(shadowtext)

#load tree file
tree <- read.tree("data/new_tree_outliers_dropped.nwk")
ggtree(tree)

drop_tip <- c("Egypt/NRC-6657/2020-09-10",
              "Zambia/SP91/2021−02−14")

new_tree <- drop.tip(tree, drop_tip, trim.internal = TRUE)
ggtree(new_tree)

write.tree(new_tree, file="new_tree_2.nwk", append = FALSE)

#Read in metadata
metadata_df <- read_delim("data/mozambique_metadata_phylotypeIDs.tsv", 
                          delim = "\t", escape_double = FALSE, 
                          trim_ws = TRUE)  #mozambique_metadata_phylotypeIDs

metadata_df <- metadata_df %>% mutate(
  pango = case_when(
    pango_lineage %like% "B.1.351" ~ "Beta",
    pango_lineage %like% "B.1.1.7" ~ "Alpha",
    pango_lineage %like% "AY" ~ "Delta",
    pango_lineage == "B" ~ "B",
    pango_lineage == "B.1.1" ~ "B.1.1",
    pango_lineage == "B.1.1.375" ~ "B.1.1.375",
    pango_lineage == "C.1" ~ "C.1",
    pango_lineage == "BA.1" ~ "BA.1",
    pango_lineage == "BA.2" ~ "BA.2",
    pango_lineage == "BA.3" ~ "BA.3",
    TRUE ~ "Other"
  )
)

#tree with tips coloured by lineage 
p<-ggtree(tree, color='grey',size=0.15) %<+% metadata_df +
  geom_tippoint(aes(subset=(country=='Mozambique' & pango=='Other')),size=3,fill = '#9E9E9E', align=F, color='black',shape=21, stroke=0.2)+
  geom_tippoint(aes(subset=(country=='Mozambique' & pango=='B')),size=3,fill = 'darkseagreen3', align=F, color='black',shape=21, stroke=0.2)+
  geom_tippoint(aes(subset=(country=='Mozambique' & pango=='B.1.1')),size=3,fill = 'thistle1', align=F, color='black',shape=21, stroke=0.2)+
  geom_tippoint(aes(subset=(country=='Mozambique' & pango=='B.1.1.375')),size=3,fill = 'wheat', align=F, color='black',shape=21, stroke=0.2)+
  geom_tippoint(aes(subset=(country=='Mozambique' & pango=='C.1')),size=3,fill = 'coral4', align=F, color='black',shape=21, stroke=0.2)+
  geom_tippoint(aes(subset=(country=='Mozambique' & pango=='Beta')),size=3,fill = 'goldenrod2', align=F, color='black',shape=21, stroke=0.2)+
  geom_tippoint(aes(subset=(country=='Mozambique' & pango=='Alpha')),size=3,fill = 'burlywood4', align=F, color='black',shape=21, stroke=0.2)+
  geom_tippoint(aes(subset=(country=='Mozambique' & pango=='Delta')),size=3,fill = 'darkorange2', align=F, color='black',shape=21, stroke=0.2)+
  geom_tippoint(aes(subset=(country=='Mozambique' & pango=='BA.1')),size=3,fill = 'light blue', align=F, color='black',shape=21, stroke=0.2)+
  geom_tippoint(aes(subset=(country=='Mozambique' & pango=='BA.2')),size=3,fill = '#9FA8DA', align=F, color='black',shape=21, stroke=0.2)+
  geom_tippoint(aes(subset=(country=='Mozambique' & pango=='BA.3')),size=3,fill = '#CE93D8', align=F, color='black',shape=21, stroke=0.2) 

p

#group clades of interest

tree1 <- groupClade(tree,.node=c(19595, 33623, 29340, 29372, 29373, 26794, 21032, 21322, 22689, 23793))

p<-ggtree(tree1, aes(color=group),size=0.35) + 
  scale_colour_manual(values=c("darkseagreen3",'darkorange2','light blue', '#9FA8DA','#CE93D8','goldenrod2','#ea877c',"coral4",'#96938D','burlywood4'))

p

#source("offspring.R")
p1 <- p %<+% metadata_df + 
  geom_tippoint(aes(
  subset=(grepl('Mozambique',label,fixed=TRUE)==TRUE), fill=group),size=4, align=F, color='black',shape=21)+
  scale_fill_manual(values=c("darkseagreen3",'darkorange2','light blue', '#9FA8DA','#CE93D8','goldenrod2','#ea877c',"coral4",'#96938D','burlywood4'))+
  theme(legend.position = 'none')#+
  # geom_cladelab(node=26794, label="Beta", align=FALSE,barsize = 1, barcolour='goldenrod2',textcolour = 'goldenrod2', angle =90, fontface=2, hjust = 0.5, vjust = 1.2)+
  # geom_cladelab(node=29372, label="BA.2", align=FALSE,barsize = 1, barcolour='#9FA8DA',textcolour = '#9FA8DA', angle =90, fontface=2, hjust = 0.5, vjust = 1.2)+
  # geom_cladelab(node=29373, label="BA.3", align=FALSE,barsize = 0, barcolour='#CE93D8',textcolour = '#CE93D8', angle =90, fontface=2, hjust = 1.5, vjust = -1.5)+
  # #geom_cladelab(node=19595, label="B lineages", align=FALSE,barsize = 1.5, barcolour='darkseagreen3',textcolour = 'darkseagreen3', angle =90)+
  # geom_cladelab(node=33623, label="Delta", align=FALSE,barsize = 1, barcolour='darkorange2',textcolour = 'darkorange2', angle =90, fontface=2, hjust = 0.5, vjust = 1.2)+
  # geom_cladelab(node=30755, label="BA.1", align=FALSE,barsize = 1, barcolour='light blue',textcolour = 'light blue', angle =90, fontface=2, hjust = 0.5, vjust = 1.2)+
  # geom_cladelab(node=21032, label="B.1.1 lineages", align=FALSE,barsize = 0, barcolour='white',textcolour = '#ea877c', angle =90, offset.text= 0, fontface=2, hjust = 0.8, vjust = 3)+
  # geom_cladelab(node=22689, label="B.1.1.375", align=FALSE,barsize = 0, barcolour='white',textcolour = '#96938D', angle =90, offset.text= 0, fontface=2, hjust = 0.55, vjust = 4)+
  # geom_cladelab(node=23793, label="Alpha", align=FALSE,barsize = 1, barcolour='burlywood4',textcolour = 'burlywood4', angle =90, fontface=2, hjust = 0.5, vjust = 1.2)+
  # geom_cladelab(node=21332, label="C.1/C.1.1", align=FALSE,barsize = 0, barcolour='white',textcolour = 'coral4', angle =90, offset.text= 0, fontface=2, hjust = 0.5, vjust = 4)+
  # geom_cladelab(node=29371, label="Omicron", align=FALSE,barsize = 1, barcolour='black',textcolour = 'black', angle =90, offset = 0.00035, fontface=2, vjust = 1, hjust = 0.5)


p1

