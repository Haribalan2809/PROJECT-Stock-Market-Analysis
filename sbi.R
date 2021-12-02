sbi=read.csv(file.choose())
sbi
str(sbi)
#partition datainto Training and Valiadtion datasets
set.seed(1234)
pd=sample(2,nrow(sbi),replace=TRUE,prob = c(0.8,0.2))
train=sbi[pd==1,]
validate=sbi[pd==2,]

#Decision tree with Party
library(party)
tree=ctree(Action~Open.Price+ High.Price+Low.Price+Close.Price,data=train)
tree
plot(tree)

#predict
predict(tree,validate,type="prob")

#Decision Tree with rpart
library(rpart)
tree1=rpart(Action~Open.Price+ High.Price+Low.Price,train)
library(rpart.plot)
rpart.plot(tree1,extra=1)

#predict
predict(tree1,validate)


#MisClassification error for 'train' data
tab=table(predict(tree),train$Action)
print(tab)
1-sum(diag(tab))/sum(tab)

#MisClassification error for 'train' data
testPred=predict(tree,newdata=validate)
tab=table(testPred,validate$Action)
print(tab)
1-sum(diag(tab))/sum(tab)
