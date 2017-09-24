# Clearing Workspace ---------------------------------------------------------
library(mise)
mise(vars = TRUE, figs = TRUE, console = TRUE, pkgs = FALSE)
# Install Packages -------------------------------------------------------
install.packages("RWeka", dependencies = T)
install.packages("DMwR", dependencies = T)
install.packages("pROC", dependencies = T)

# Load Packages -----------------------------------------------------------


library(RWeka)                    #Cost Sensitive Classification
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
    p <- readline("Proportion of samples in Test set(As Decimal, eg: 0.5): ")
    if( p < 1 && p > 0  ){break}
    else if(is.na(p)){p == 0.5}
    else{cat("Invalid input!\nTry again. Value of p should be between 0 and 1\n")}
  }
  
  p <- as.numeric(p)
  data = sort(sample(nrow(data_input), nrow(data_input)*p))
  trainSplit<- data_input[data,]
  testSplit <- data_input[-data,]
  
  trainSplit[,dep_var] <- as.factor(trainSplit[,dep_var])
  trainSplit<- trainSplit[,c(dep_var,ind_var)]
  
  testSplit[,dep_var] <- as.factor(testSplit[,dep_var])
  testSplit<- testSplit[,c(dep_var,ind_var)]
  rerun <- 0
  
  assign("rerun", rerun, envir=globalenv())
  assign("trainSplit", trainSplit, envir=globalenv())
  assign("testSplit", testSplit, envir=globalenv())
  
}  # Splitting the Data into Training Set and Test Set

CostCla <- function(){ 
  cat("Cost Sensitive Classification  \n")
  
  if(rerun == 0){
    trainSplit$dep <- trainSplit[,dep_var]
    trainSplit[,dep_var] <- NULL
    
    testSplit$dep <- testSplit[,dep_var]
    testSplit[,dep_var] <- NULL
  }
  
  assign("trainSplit", trainSplit, envir=globalenv())
  assign("testSplit", testSplit, envir=globalenv())

  glm.fit <- Logistic(dep ~ .,data = trainSplit, control = Weka_control(),
                      options = NULL)
  assign("glm.fit", glm.fit, envir=globalenv())
  cat("Enter The Cost Matrix: \n")
  
  
  repeat{
    a <- as.numeric(readline(paste0("Enter Cost (Actual = ",levels(testSplit$dep)[1],
                                    ", Predicted = ",levels(testSplit$dep)[1],"): ")))
    if(a < 0 | is.na(a)){
      cat("Invalid Input, Cost must be a numeric value greater than or equal to 0. Please enter cost again:  ")
    } else {
      break
    }
  } #Entering Cost
  
  repeat{
    b <- as.numeric(readline(paste0("Enter Cost (Actual = ",levels(testSplit$dep)[1],
                                    ", Predicted = ",levels(testSplit$dep)[2],"): ")))
    if(b < 0 | is.na(b)){
      cat("Invalid Input, Cost must be a numeric value greater than or equal to 0. Please enter cost again:  ")
    } else {
      break
    }
  } #Entering Cost
  
  repeat{
    c <- as.numeric(readline(paste0("Enter Cost (Actual = ",levels(testSplit$dep)[2],
                                    ", Predicted = ",levels(testSplit$dep)[1],"): ")))
    if(c < 0 | is.na(c)){
      cat("Invalid Input, Cost must be a numeric value greater than or equal to 0. Please enter cost again:  ")
    } else {
      break
    }
  } #Entering Cost
  
  repeat{
    d <- as.numeric(readline(paste0("Enter Cost (Actual = ",levels(testSplit$dep)[2],
                                    ", Predicted = ",levels(testSplit$dep)[2],"): ")))
    if(d < 0 | is.na(d)){
      cat("Invalid Input, Cost must be a numeric value greater than or equal to 0. Please enter cost again:  ")
    } else {
      break
    }
  } #Entering Cost
  
  assign("a", a, envir=globalenv()) 
  assign("b", b, envir=globalenv()) 
  assign("c", c, envir=globalenv()) 
  assign("d", d, envir=globalenv()) 
  
  costmat <- matrix(c(a, c, b, d), ncol = 2)
  costmat <- as.matrix(costmat)
  rownames(costmat) <- c("Actual 0","Actual 1")
  colnames(costmat) <- c("Predicted 0","Predicted 1")
  assign("costmat", costmat, envir=globalenv())
  
  cat("Your chosen cost matrix is: \n")
  print(costmat)
  repeat{ 
    f <- readline("Would you like to go ahead with this cost matrix[Yes = 1, No = 0] ? \n")
    if(f == 1 | is.na(f)| !length(rerun)){
      break
    } else {
      
      cat("Enter The Cost Matrix: \n")
      repeat{
        a <- as.numeric(readline(paste0("Enter Cost (Actual = ",levels(testSplit$dep)[1],
                                        ", Predicted = ",levels(testSplit$dep)[1],"): ")))
        if(a < 0 | is.na(a)){
          cat("Invalid Input, Cost must be a numeric value greater than or equal to 0. Please enter cost again:  ")
        } else {
          break
      }
  }
      
      repeat{
        b <- as.numeric(readline(paste0("Enter Cost (Actual = ",levels(testSplit$dep)[1],
                                        ", Predicted = ",levels(testSplit$dep)[2],"): ")))
        if(b < 0 | is.na(b)){
          cat("Invalid Input, Cost must be a numeric value greater than or equal to 0. Please enter cost again:  ")
        } else {
          break
        }
      }
      
      repeat{
        c <- as.numeric(readline(paste0("Enter Cost (Actual = ",levels(testSplit$dep)[2],
                                        ", Predicted = ",levels(testSplit$dep)[1],"): ")))
        if(c < 0 | is.na(c)){
          cat("Invalid Input, Cost must be a numeric value greater than or equal to 0. Please enter cost again:  ")
        } else {
          break
        }
      }
      
      repeat{
        d <- as.numeric(readline(paste0("Enter Cost (Actual = ",levels(testSplit$dep)[2],
                                        ", Predicted = ",levels(testSplit$dep)[2],"): ")))
        if(c < 0 | is.na(c)){
          cat("Invalid Input, Cost must be a numeric value greater than or equal to 0. Please enter cost again:  ")
        } else {
          break
        }
      }
      
}
    
    assign("a", a, envir=globalenv()) 
    assign("b", b, envir=globalenv()) 
    assign("c", c, envir=globalenv()) 
    assign("d", d, envir=globalenv()) 
    
    costmat <- matrix(c(a, c, b, d), ncol = 2)
    costmat <- as.matrix(costmat)
    rownames(costmat) <- c("Actual 0","Actual 1")
    colnames(costmat) <- c("Predicted 0","Predicted 1")
    
    assign("costmat", costmat, envir=globalenv())
    
    cat("Your chosen cost matrix is: \n")
    print(costmat)
    
}
  
   
 costcla <-  suppressWarnings(evaluate_Weka_classifier(glm.fit, newdata= testSplit,  cost = costmat, class = T, numFolds = 3 ))


 assign("costcla", costcla, envir=globalenv())
  
}      # Cost Sensitive Classification

Diagnostics <- function(){ 
  
  print(costcla)
 k <- as.numeric(readline("Would you like to export this model[Yes = 1, No = 0]? :"))
 
     if(k == 1 | is.na(k)){
          sink(file = paste(" Cost Sensitive classification Model Summary.txt"))
          print(costcla) 
          sink() 
          cat("Your Model Summary has been exported as - 'Cost Sensitive classification Model Summary.txt' ")
     } else {
      rerun <- as.numeric(readline("Do you want to re-run the classification with different cost matrix [Yes = 1, No = 0]? : "))
      assign("rerun", rerun, envir=globalenv())
      if(rerun == 1 | is.na(rerun)| !length(rerun)){
        CostCla() 
        Diagnostics()
      }
       
     }
} # Diagnostic Parameters

CostModule <- function(){ 
Def_Wd_Data_Dv_Iv()
DataSplit()
CostCla() 
Diagnostics()
}

# Execution ---------------------------------------------------------------
CostModule()


