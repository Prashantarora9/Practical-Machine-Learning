#Coursera Practical ML assignment

library(doParallel)
registerDoParallel(cores=2)


#Importing the data
                   
training_init = read.csv("C:/R assingment/pml-training.csv", header = TRUE);

validation = read.csv("C:/R assingment/pml-testing.csv", header = TRUE);


# Creating training and testing set

set.seed(12345)
inTrain = createDataPartition(training_init$classe, p = 0.7)[[1]]
training = training_init[ inTrain,]
testing = training_init[-inTrain,]    


# Dropping the variables with near zero variance

 nsv <- nearZeroVar(training, saveMetrics = TRUE);
 
 training1<- training[,!nsv$nzv]
 
#Dropping variables which are non sensor based
 
 training2<- training1[,-c(1,2,3,4,5,6)]
 training2<- training2[,-1]
 
#Dropping colums with NA
 
training3 <- training2[ , colSums(is.na(training2)) == 0]


# Developing a random forest prediction model
 
 modFit<- train(training3$classe~., method= "rf", allowparallel= TRUE, data = training3);
                
 print(modFit$finalModel)      
 
 
 #Cross validation
 
 predictions <- predict(modFit, newdata = testing)
 pred <- data.frame(predictions, classe=testing$classe)
 correct <- nrow(pred[with( which(predictions==classe), data=pred ),])
 wrong <- nrow(pred[with( which(predictions != classe), data=pred ),])
 percent_correct = correct/nrow(pred)*100
 percent_correct #99.27
 
 
 percent_wrong = wrong/nrow(pred)*100
 percent_wrong #0.73

summary(train_dt$classe)
 
#out of sample erroe


out_of_sample_error <- 1/(length(predictions)) * sum( ( wrong )^2 )
out_of_sample_error # 0.314

# Generating final prediction on the intial test set

Final_predict <- predict(modFit,validation);


#function to create op files

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

#generating output files

pml_write_files(Final_predict)

 
 
 