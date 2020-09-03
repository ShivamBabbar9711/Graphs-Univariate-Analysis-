Graphs <- function(data,m=1:ncol(data),folder='C:/Users/SHIVAM/Documents/R-New')#function Graphs accepting dataframe and a variable 'm
  #and folder argument accepts file saving loaction 
{
  setwd(folder)
  x = paste("Test",runif(1, min=0, max=100), Sys.Date(), sep = "_")
  dir.create(x)
  setwd(paste(folder,'\\',x,sep = ""))
  
  if(!is.data.frame(data))                      #checking if the object passed as argument in the function 
    #is dataframe or not
  {
    stop("The given object is not a data frame")#Printing message that object provided is not a data frame
  }
  for(i in 1:length(m))
  {
    if(is.numeric(data[,m[i]]))                 #if condition is placed as boxplot and histogram can be made
      #only for numerical variables.So if a variable in the dataframe 
      #is categorical for loop will not enter the if condition
    {
      png(paste(names(data)[i], ".png", sep=""))
      par(mfrow=c(2,1))                         #setting area into 2*1 array
      boxplot(data[,m[i]], main = paste("Boxplot of", names(data)[m[i]]), #Plotting Boxplot
              ylab = names(data)[m[i]], col = "maroon", border = "grey5", 
              horizontal = T)
      
      hist(data[,m[i]], main = paste("Histogram of", names(data)[m[i]]),  #Plotting Histogram
           xlab = names(data)[m[i]], ylab = "No. of Houses", col = "lightgreen", border=F)
      dev.off()
    }
    
    else                                        #if the variable is categorical it will enter else condition
    {
      print(i)
      png(paste(names(data)[i], ".png", sep="")) 
      
      par(mfrow=c(2,1))
      c.table <- table(data[,m[i]])
      barplot(c.table,main = paste("Barplot of", names(data)[m[i]]), #Plotting Barplot
              ylab = names(data)[m[i]], col = "maroon")
      pie(c.table,labels = round(c.table/sum(c.table)*100, 1),radius =1,main = paste("Piechart of", names(data)[m[i]])) #Plotting piechart
      dev.off()
    }
  }
}