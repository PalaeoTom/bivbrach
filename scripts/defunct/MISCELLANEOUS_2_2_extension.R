## 50km
sgcs_50 <- unique(master_50$stage_cell)
sgcOCCS_50 <- c()
for(i in sgcs_50){
  ## get occs
  sgcOCCS_50 <- c(sgcOCCS_50,length(which(master_50$stage_cell %in% i)))
}

## Box plot
boxplot(sgcOCCS_50)

## How many have at least 50?
length(which(sgcOCCS_50>=50))
sum(sgcOCCS_50[which(sgcOCCS_50>=50)])

## 100km
sgcs_100 <- unique(master_100$stage_cell)
sgcOCCS_100 <- c()
for(i in sgcs_100){
  ## get occs
  sgcOCCS_100 <- c(sgcOCCS_100,length(which(master_100$stage_cell %in% i)))
}

## How many have at least 50?
length(which(sgcOCCS_100>=50))
sum(sgcOCCS_100[which(sgcOCCS_100>=50)])

## 200km
sgcs_200 <- unique(master_200$stage_cell)
sgcOCCS_200 <- c()
for(i in sgcs_200){
  ## get occs
  sgcOCCS_200 <- c(sgcOCCS_200,length(which(master_200$stage_cell %in% i)))
}

## How many have at least 100?
length(which(sgcOCCS_200>=100))
sum(sgcOCCS_200[which(sgcOCCS_200>=100)])

## Get grid cells over 5000 occurrences
over5000_50 <- which(sgcOCCS_50 > 5000)

## How many and what is there proportion?
length(over5000_50)
length(over5000_50)/length(sgcOCCS_50)

## How many of the occurrences do they account for
sum(sgcOCCS_50[over5000_50])
sum(sgcOCCS_50[over5000_50])/sum(sgcOCCS_50)

## Prune these out for histogram
sgcOCCS_50_pruned <- sgcOCCS_50[-over5000_50]

## Plot as histogram the grid cells under 500 occurrences. Set frequency limit to 100.
pdf("figures/final/supplemental/genera_50_occurrences_in_grid_cells.pdf")
hist(sgcOCCS_50_pruned, breaks = seq(0,20000,5), ylim = c(0,100), main = "50km grid cells", xlab = "Number of occurrences in grid cells")
dev.off()

## How many occurrences dropped with 25 occurrence cutoff, and what is the proportion
sum(sgcOCCS_50[which(sgcOCCS_50 < 25)])
sum(sgcOCCS_50[which(sgcOCCS_50 < 25)])/sum(sgcOCCS_50)
sum(sgcOCCS_50)-sum(sgcOCCS_50[which(sgcOCCS_50 < 25)])

## How many grid cells drop, and what is the proportion?
length(which(sgcOCCS_50 < 25))
length(which(sgcOCCS_50 < 25))/length(sgcOCCS_50)
length(sgcOCCS_50)-length(which(sgcOCCS_50 < 25))

## How many occurrences dropped with 20 occurrence cutoff, and what is the proportion
sum(sgcOCCS_50[which(sgcOCCS_50 < 20)])
sum(sgcOCCS_50[which(sgcOCCS_50 < 20)])/sum(sgcOCCS_50)
sum(sgcOCCS_50)-sum(sgcOCCS_50[which(sgcOCCS_50 < 20)])

## How many grid cells drop, and what is the proportion?
length(which(sgcOCCS_50 < 20))
length(which(sgcOCCS_50 < 20))/length(sgcOCCS_50)
length(sgcOCCS_50)-length(which(sgcOCCS_50 < 20))

## How many occurrences dropped with 15 occurrence cutoff, and what is the proportion
sum(sgcOCCS_50[which(sgcOCCS_50 < 15)])
sum(sgcOCCS_50[which(sgcOCCS_50 < 15)])/sum(sgcOCCS_50)
sum(sgcOCCS_50)-sum(sgcOCCS_50[which(sgcOCCS_50 < 15)])

## How many grid cells drop, and what is the proportion?
length(which(sgcOCCS_50 < 15))
length(which(sgcOCCS_50 < 15))/length(sgcOCCS_50)
length(sgcOCCS_50)-length(which(sgcOCCS_50 < 15))

## How many occurrences dropped with 10 occurrence cutoff, and what is the proportion
sum(sgcOCCS_50[which(sgcOCCS_50 < 10)])
sum(sgcOCCS_50[which(sgcOCCS_50 < 10)])/sum(sgcOCCS_50)
sum(sgcOCCS_50)-sum(sgcOCCS_50[which(sgcOCCS_50 < 10)])

## How many grid cells drop, and what is the proportion?
length(which(sgcOCCS_50 < 10))
length(which(sgcOCCS_50 < 10))/length(sgcOCCS_50)
length(sgcOCCS_50)-length(which(sgcOCCS_50 < 10))

## How many occurrences dropped with 5 occurrence cutoff, and what is the proportion
sum(sgcOCCS_50[which(sgcOCCS_50 < 5)])
sum(sgcOCCS_50[which(sgcOCCS_50 < 5)])/sum(sgcOCCS_50)
sum(sgcOCCS_50)-sum(sgcOCCS_50[which(sgcOCCS_50 < 5)])

## How many grid cells drop, and what is the proportion?
length(which(sgcOCCS_50 < 5))
length(which(sgcOCCS_50 < 5))/length(sgcOCCS_50)
length(sgcOCCS_50)-length(which(sgcOCCS_50 < 5))

## What would we be left with if we applied an upper limit of 5k and a lower limit of 20.
sgcOCCS_50_final <- sgcOCCS_50[-which(sgcOCCS_50 < 20)]
sgcOCCS_50_final <- sgcOCCS_50_final[-which(sgcOCCS_50_final > 1526)]
sum(sgcOCCS_50_final)
length(sgcOCCS_50_final)

## What is 5th and 95th quantile
quantile(sgcOCCS_50_final, c(0.05, 0.99))
sort(sgcOCCS_50_final,decreasing = T)

## Grid cells above 100 and less than 1000
above100 <- which(sgcOCCS_50 >= 100)
less100 <- which(sgcOCCS_50 <= 1000)
length(sgcOCCS_50[intersect(above100, less100)])
sum(sgcOCCS_50[intersect(above100, less100)])
