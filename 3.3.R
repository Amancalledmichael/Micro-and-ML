setwd("M:/Kurse/Micro and ML/Tutorial")

carseats <- read.csv(file = 'carseats.csv')

View(carseats)

#1. 
carseats$High <- ifelse(carseats$Sales > 8, 1, 0)

#2.
install.packages("tree")
library(tree)
install.packages("ISLR")
library(ISLR)

seat_tree = tree(Sales ~ ., data = carseats, minsize = 12)


#3.
plot(seat_tree)
text(seat_tree, pretty = 0)
title(main = "Unpruned Classification Tree")

#4.

summary(seat_tree)

#5.
dim(carseats)
set.seed(2)
seat_idx = sample(1:nrow(carseats), 200)
seat_trn = carseats[seat_idx,]
seat_tst = carseats[-seat_idx,]

#6.
seat_tree = tree(Sales ~ ., data = seat_trn, minsize = 12)

#7.
seat_trn_pred = predict(seat_tree, seat_trn, type = "class")
seat_tst_pred = predict(seat_tree, seat_tst, type = "class")

#8.
# train confusion
table(predicted = seat_trn_pred, actual = seat_trn$Sales)
# test confusion
table(predicted = seat_tst_pred, actual = seat_tst$Sales)

accuracy = function(actual, predicted) {
  mean(actual == predicted)
}

# train acc
accuracy(predicted = seat_trn_pred, actual = seat_trn$Sales)
#test acc
accuracy(predicted = seat_trn_pred, actual = seat_tst$Sales)

#9.
seat_tree_cv = cv.tree(seat_tree, FUN = prune.misclass)
min_idx = which.min(seat_tree_cv$dev)
seat_tree_cv$size[min_idx]
seat_tree_cv$size[9]
seat_tree_cv$size[14]
