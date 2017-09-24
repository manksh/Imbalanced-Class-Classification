# Clearing Workspace ---------------------------------------------------------
library(mise)
mise(vars = TRUE, figs = TRUE, console = TRUE, pkgs = FALSE)
# Install Packages -------------------------------------------------------
install.packages("caret", dependencies = T)
install.packages("DMwR", dependencies = T)
install.packages("pROC", dependencies = T)

# Load Packages -----------------------------------------------------------

library(DMwR)                     #SMOTE
library(caret)                    #Data Wrapping
library(pROC)                     #ROC and AUC


# Functions ---------------------------------------------------------------

Def_Wd_Data_Dv_Iv <- function(){ 
cat("Welcome, We will now Load your Data and define the Dependant and Independant Variables\n")
  # Choosing Working Directory 
  working_directory <- getwd()
  cat("Currently, the Working Directory is: ",getwd(),"\n")
  
  repeat  {        
           working_directory <- readline("If you want to change the Workiing Directory, please mention the address of the same, else input 0: ")
           if(length(working_directory) & working_directory != "" & 
              !is.na(working_directory)) 
             {break}
          }
  
  if (!dir.exists(working_directory))
    {
    working_directory <- getwd()
    cat("Invalid input!\nThe Working Directory is: ", getwd(),"\n")
    } 
  else {
        setwd(working_directory)
        cat("The Working Directory is: ",working_directory,"\n")
       } 
  # Choosing the Data File to Work on
  
  repeat  {
           data_dynamic_path <- readline("Mention the name of the data file (with extension): ") 
    
           if ( file.exists(data_dynamic_path) & 
                substr(data_dynamic_path,nchar(data_dynamic_path)-3,
                nchar(data_dynamic_path)) == ".csv"){break} else
                {cat("Invalid input!\nTry again. Check with the name or extension of the file\n")}
          }  
  
  original_data <- read.csv(file = paste0(working_directory,"/",data_dynamic_path)) 
  data_input <- as.data.frame(original_data)  
  data_input <- na.omit(data_input) 
  
  
  assign("working_directory", working_directory, envir=globalenv()) 
   
  
  # Defining Independent Variables
  print(colnames(data_input)) 
  
  repeat  {
    ind_var <- readline("Mention the names of the Independent variables from the data file (separated by comma(s)): ") 
    ind_var <- unlist(strsplit(gsub(" ","",ind_var), ","))
    
    
    if(all(ind_var %in% names(data_input))) {break} else
    {cat("Invalid input!\nTry again. 
         Ensure the name(s) of the variable(s) are correctly spelled.\n")}
  }  
  
  cat("Independent Variables you selected:\n")
  print(ind_var)
  
  # Defining Dependent Variable
  
  repeat  {
    dep_var <- readline("Specify the Dependent variable from the data file: ") 
    dep_var <- gsub(" ","",dep_var)
    dep<- gsub(" ","",dep_var)
    if(dep_var %in% names(data_input) & !(dep_var %in% ind_var)) {
      break
    } else if (dep_var %in% ind_var){
      cat("Invalid input!\nTry again. The Dependent variable cannot be one of the Independent variables specified.\n")
    } else {
      cat("Invalid input!\nTry again. Ensure the name of the variable is correctly spelled.\n")
    }
    
  }  
  
  cat("Dependent Variable you selected:\n")  
  print(dep_var) 
  
  assign("dep_var", dep_var, envir=globalenv())  
  assign("ind_var", ind_var, envir=globalenv())
  
  data_input <- data_input[,c(ind_var,dep_var)]  
  
  assign("data_input", data_input, envir=globalenv())   

  

  } # Defines working Directory, Dataset, Dependant Variables and Independant Variable

DataSplit <- function() { 
  cat("Now we will split the data into test and training set")
  # Splitting Data
  set.seed(1234)
 
    repeat{ 
      p <- readline("Proportion of samples in Test set(As Decimal, Default: 0.5): ")
      if( p < 1 && p > 0  ){break}
      else if(is.na(p)|!length(p)){p = 0.5}
      else{cat("Invalid input!\nTry again. Value of p should be between 0 and 1\n")}
    }
    
  p <- as.numeric(p)
  assign("p", p, envir=globalenv())
  
  data = sort(sample(nrow(data_input), nrow(data_input)*p))
  trainSplit<- data_input[data,]
  testSplit <- data_input[-data,]
  
  trainSplit[,dep_var] <- as.factor(trainSplit[,dep_var])
  trainSplit<- trainSplit[,c(dep_var,ind_var)]
  
  testSplit[,dep_var] <- as.factor(testSplit[,dep_var])
  testSplit<- testSplit[,c(dep_var,ind_var)]
  
  assign("trainSplit", trainSplit, envir=globalenv())
  assign("testSplit", testSplit, envir=globalenv())
  
}  # Splitting the Data into Training Set and Test Set

SMOTEResample <- function() {
  cat("We will Resample the data using the SMOTE algorithm")
  trainSplit$dep <- trainSplit[,dep_var]
  trainSplit[,dep_var] <- NULL
  
  repeat{
  o <- as.numeric(readline("Proportion of Oversampling to be done:  "))
   if(o <= 0 | is.na(o)) {cat("Invalid input!\nTry again. Proportion of oversampling should be greater than 0")}
   else{break}
   
  }
  assign("o", o, envir=globalenv())  
       repeat{
         u <- as.numeric(readline("Proportion of Undersampling to be done:  "))
         if(u <= 0 | is.na(u)){cat("Invalid input!\nTry again. Proportion of oversampling should be greater than 0")}
         else{break} 
          
       }
       assign("u", u, envir=globalenv())
       
  trainSplitSMOTE <- SMOTE(form = dep ~ .,data = trainSplit, perc.over = o , perc.under = u)
  assign("trainSplitSMOTE", trainSplitSMOTE, envir=globalenv())
  
} #SMOTING Data

Classification <- function(){
 cat("Classification using Logistic Regression")
  
  testSplit$dep <- testSplit[,dep_var]
  testSplit[,dep_var] <- NULL
  
  assign("testSplit", testSplit, envir=globalenv()) 
  
  
  glm.fit <- suppressWarnings(glm(dep ~. , data = trainSplitSMOTE, family = binomial))
  
  glm.probs = predict(glm.fit, newdata = testSplit, type = "response")
  
  repeat{
    cutoff <- as.numeric(readline("What Should be the probability cutoff[As Decimal, eg 0.5]?: "))
    if(cutoff < 0 |cutoff > 1| is.na(cutoff)| !length(cutoff)| is.character(a))
      {cat("Invalid Input, Enter a Number between 0 and 1")}
      
    else{
      break 
      }
  } 
  assign("cutoff", cutoff, envir=globalenv())
  glm.pred=ifelse(glm.probs>cutoff, "1", "0")
  
  assign("glm.pred", glm.pred, envir=globalenv()) 
  assign("glm.probs", glm.probs, envir=globalenv()) 
  cat("\n")
  

  } #Classification

Diagnostics <- function(){

  cmatrix <- table(as.numeric(glm.pred), testSplit$dep)
  Accuracy <- sum(diag(cmatrix)) / sum(cmatrix) 
  Accuracy
  Precision <- as.numeric(cmatrix[1,1]/sum(cmatrix[1,1:2]))
  Precision
  Recall<- as.numeric(cmatrix[1,1]/sum(cmatrix[1:2,1]))
  Recall
  Fscore <- as.numeric((2*Precision*Recall)/(Precision + Recall))
  Fscore
  cm <- confusionMatrix(as.numeric(glm.pred), testSplit$dep)
  auc <- roc(glm.pred,as.numeric(testSplit$dep))

  assign("auc", auc, envir=globalenv()) 
  
  Diagnostic_Parameters <- rbind(Accuracy,Precision,Recall,Fscore, Auc = auc$auc, Sensitivity = cm$byClass["Sensitivity"], 
                                Specificity = cm$byClass["Specificity"], Kappa = cm$overall["Kappa"])
  
  colnames(Diagnostic_Parameters) <- "Values"
  print(Diagnostic_Parameters)
  assign("Diagnostic_Parameters", Diagnostic_Parameters, envir=globalenv()) 
  
  write.csv(Diagnostic_Parameters, "SMOTE - Diagnostic_Parameters.csv")
  
  c <- readline("Would you like to append Predicted Classes and Class Probabilities with the Dataset?[1 = Yes, 0 = No]:")
      if(c == 1 ){c(testSplit$predictions <- glm.pred , testSplit$probabilities <- glm.probs)}
      assign("testSplit", testSplit, envir=globalenv()) 
      
      write.csv(testSplit, "Predicted Class Data - SMOTE.csv")
      
    jpeg(filename = "ROC Curve SMOTE.jpg")
    plot(auc, ylim=c(0,1),  print.thres=TRUE, main=paste('AUC:',round(auc$auc[[1]],2))) 
    abline(h=1,col='blue',lwd=2)    
    abline(h=0,col='red',lwd=2)
    dev.off()
  if(c == 1){cat("The Data set with predicted classes and class probabilities has been successfully exported!")}
  
  assign("cmatrix", cmatrix, envir=globalenv()) 
  assign("testSplit", testSplit, envir=globalenv())  
} # Diagnostic parameters

SMOTEModule <- function(){
  
  Def_Wd_Data_Dv_Iv()
  DataSplit()
  
  SMOTEResample()
  Classification()
  Diagnostics()
  
} #Calling all Functions

# Execution of Function ---------------------------------------------------
SMOTEModule()









 