bootstrap <- function(formula, data, n=100, seed =1234){
    set.seed(seed)
    ## create table for confussion matrix
    table <- as.table(matrix(c(0,0,0,0),ncol=2))
    colnames(table)=c("0","1")
    row.names(table)=c("0","1")
    names(dimnames(table)) <- c("Prediction","Reference")
    C <- 0 ## store C score
    Acc <- 0 ## store accuracy
    for(i in 1:n){
        print(paste("[",i,"]"))
        trainIndex <- createDataPartition(data$Speaker, p = .8,
                                          list = FALSE,
                                          times = 1)
        data.train <- data[trainIndex,]
        data.test <- data[-trainIndex,]
        data.train.glmer <- glmer(formula,
            data <- data.train, family="binomial",
            control=glmerControl(optimizer="bobyqa"))
        data.test.pred <- predict(data.train.glmer,newdata=data.test, type="response")
        data.test.predicted <- ifelse(data.test.pred>0.5,1,0)
        first_level <- levels(data.test$Agreement)[1]
        data.test$Agreement1 <- ifelse(data.test$Agreement==first_level, 0,1)
        confM <- confusionMatrix(data.test.predicted, data.test$Agreement1)
        table <- table + confM$table
        Acc <- Acc + confM$overall["Accuracy"]
        C <- C + as.numeric(auc(data.test.predicted, data.test$Agreement1))
    }
    return(list("Matrix"=table/n,"Accuracy"=Acc/n,"C"=C/n))
}
