# Clearing Workspace ---------------------------------------------------------
install.packages("mise", dependencies = T)
library(mise)
mise(vars = TRUE, figs = TRUE, console = TRUE, pkgs = FALSE)
# Install Packages -------------------------------------------------------
install.packages("caret", dependencies = T)
install.packages("DMwR", dependencies = T)
install.packages("pROC", dependencies = T)

# Load Packages -----------------------------------------------------------

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
CaseControl <- function() {
  
  cat("Case Control Sampling \n")
  
  data_input$dep <- data_input[,dep_var]
  data_input[,dep_var] <- NULL
  
  assign("data_input", data_input, envir=globalenv()) 
  
  data_input1 <- subset(data_input, data_input$dep == 1)
  data_input0 <- subset(data_input, data_input$dep == 0)
  
  assign("data_input1", data_input1, envir=globalenv()) 
  assign("data_input0", data_input0, envir=globalenv()) 
  
 cat("The number of Cases are : \n" )
 print(nrow(data_input1))
 cat("The number of Controls are: \n")
 print(nrow(data_input0))
  repeat{
         p <- as.numeric(readline("How many controls would you like: \n"))
         if(p >nrow(data_input0) || is.na(p)){cat("Invalid Input, Number of Controls should be below:\n")}
         else {break}
         print(nrow(data_input0))
        }
  assign("p", p, envir=globalenv()) 
  
  data_input_control <- data_input0[sample(nrow(data_input0), p), ]
  
  data_input_case_control <- rbind(data_input1, data_input_control)
  
  data_input_case_control <- as.data.frame(data_input_case_control)
  
  assign("data_input_case_control", data_input_case_control, envir=globalenv()) 
  assign("data_input_control", data_input_control, envir=globalenv()) 
  
  cat("The ratio of cases to controls is:\n")
  print(prop.table(table(data_input_case_control$dep)))
  


} #Case Control Sampled Data
Classification <- function(){
  cat("Classification using Logistic Regression")
  
  
  glm.fit <- suppressWarnings(glm(dep ~. , data = data_input_case_control, family = binomial))
  assign("glm.fit", glm.fit, envir=globalenv())
  
  m<- as.numeric(nrow(data_input1)/nrow(data_input0))
  n<- as.numeric(nrow(data_input1)/nrow(data_input_control))
  
  assign("m", m, envir=globalenv())
  assign("n", n, envir=globalenv())
  
  adjustment <- log(m/(1-m))  - log(n / ( 1 - n) ) 
  
  glm.fit$coefficients[1] <- glm.fit$coefficients[1] + adjustment
  
  assign("glm.fit", glm.fit, envir=globalenv())
  
  
  glm.probs = predict(glm.fit, newdata = data_input, type = "response")
  
  repeat{
  cutoff <- as.numeric(readline("What Should be the probability cutoff[As Decimal, eg 0.5]?: "))
       if(cutoff > 0 && cutoff < 1) {break}
        else{cat("Invalid Input, Enter a Number between 0 and 1")}
  }  
  assign("cutoff", cutoff, envir=globalenv())
  glm.pred=ifelse(glm.probs > cutoff, "1", "0")
  
  assign("glm.pred", glm.pred, envir=globalenv()) 
  assign("glm.probs", glm.probs, envir=globalenv()) 
  cat("\n")
  
  
} #Classification
Diagnostics <- function(){
  
  cmatrix <- table(as.numeric(glm.pred), data_input$dep)
  Accuracy <- sum(diag(cmatrix)) / sum(cmatrix) 
  Accuracy
  Precision <- as.numeric(cmatrix[1,1]/sum(cmatrix[1,1:2]))
  Precision
  Recall<- as.numeric(cmatrix[1,1]/sum(cmatrix[1:2,1]))
  Recall
  Fscore <- as.numeric((2*Precision*Recall)/(Precision + Recall))
  Fscore
  cm <- confusionMatrix(as.numeric(glm.pred), data_input$dep)
  auc <- roc(glm.pred,as.numeric(data_input$dep))
  
  assign("auc", auc, envir=globalenv()) 
  
  Diagnostic_Parameters <- rbind(Accuracy,Precision,Recall,Fscore, Auc = auc$auc, Sensitivity = cm$byClass["Sensitivity"], 
                                 Specificity = cm$byClass["Specificity"], Kappa = cm$overall["Kappa"])
  
  colnames(Diagnostic_Parameters) <- "Values"
  print(Diagnostic_Parameters)
  assign("Diagnostic_Parameters", Diagnostic_Parameters, envir=globalenv()) 
  
  write.csv(Diagnostic_Parameters, "Case Control - Diagnostic_Parameters.csv")
  
  c <- readline("Would you like to append Predicted Classes and Class Probabilities with the Dataset?[1 = Yes, 0 = No]:")
  if(c == 1 ){c(data_input$predictions <- glm.pred , data_input$probabilities <- glm.probs)}
  assign("data_input", data_input, envir=globalenv()) 
  
  write.csv(data_input_case_control, "Predicted Class Data - Case Control.csv")
  
  jpeg(filename = "ROC Curve Case Control.jpg")
  plot(auc, ylim=c(0,1),  print.thres=TRUE, main=paste('AUC:',round(auc$auc[[1]],2))) 
  abline(h=1,col='blue',lwd=2)    
  abline(h=0,col='red',lwd=2)
  dev.off()
  if(c == 1){cat("The Data set with predicted classes and class probabilities has been successfully exported!")}
  
  assign("cmatrix", cmatrix, envir=globalenv()) 
  assign("data_input_case_control", data_input_case_control, envir=globalenv())  
} # Diagnostic parameters
CaseControlModule <- function(){
  Def_Wd_Data_Dv_Iv()
  CaseControl()
  Classification()
  Diagnostics()
  
}#Calling all Functions

# Execution ---------------------------------------------------------------

CaseControlModule() 

