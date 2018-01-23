##Code from week 1##

#5. working with dataframes

###subset all versicolor with a Sepal.Width between 3.0 and 3.5
versi = subset(iris, Species == 'versicolor' & Sepal.Width>= 3 & Sepal.Width <=3.5)

##I found the function between() from the dplyr package
##that does a similar job than the previous command
library(dplyr)
versi = subset(iris, Species == 'versicolor' & between(iris$Sepal.Width, 3, 3.5) )

#1. Probability_definition

## Function to draw randomly n times from an omega(Sample space)={1:6}
##simulating a die draw

chevalier = function(x,n){
  a = sample(x,n,replace = T)
  return(a)
}

#we need a loop that cycles m times using our sampling function, 
#and assing each result to either a true or false if we have a 6 in the draw

m = 1000 #how many times we are going to repeat the draw

a = character(m) #create an empty vector of m number of characters
mul = list() #create an empty list to get all our results
res = vector()
for (i in 1:m){
  res = chevalier(die,n)
  mul[[i]] = res
  if (6 %in% res){
    a[i] = "TRUE"
  }
  else {
    a[i] = "FALSE"
  }
  
}

##Find Frequencies
table(a)["TRUE"]/m

##4. Probability Operations

##Genetics exercise

##Generate random gene lists from http://www.molbiotools.com/randomgenesetgenerator.html
##Add them as a csv file or even a text file

##
setwd("/your/location/")
gene = read.csv(file = "Gene.list.csv", 
                header=TRUE, sep=",", stringsAsFactors = F)

##convert dataframe to list and get rid of empty cells
geneLS <- lapply(as.list(gene), function(x) x[x != ""])

##What genes are different from two conditions?
setdiff(geneLS$ConditionA, geneLS$ConditionB)

##Prepare file for venn diagrams
library(ggplot2)

VENN.LIST <- geneLS
venn.plot <- venn.diagram(VENN.LIST ,
                          NULL, fill=c("darkmagenta", "darkblue","darkgreen"), 
                          alpha=c(0.5,0.5,0.5), cex = 2, cat.fontface=4, 
                          category.names=c("A", "B","C"), main="Random Gene Lists")
grid.draw(venn.plot)
dev.off()

##Which genes are in common between two conditions
intersect(gene$ConditionA,gene$ConditionB)

##Vector of condition C
Condc = gene$ConditionC
##subset from condition C genes that start with letter A
genes_A =Condc[grepl("^A",Condc)]

##List with both conditions
Contained = list(Condc,genes_A)
names(Contained) = c("Full_genes","genes_A")

length(setdiff(Contained$Full_genes,Contained$genes_A))

##Ven diagram 
venn.plot2 <- venn.diagram(Contained , NULL, fill=c("darkmagenta", "darkblue"), alpha=c(0.5,0.5), cex = 2, cat.fontface=4, category.names=c("A", "B"), main="Random Gene Lists")
grid.draw(venn.plot2)
dev.off()

######-------------------------------------------#########
######-------------------------------------------#########