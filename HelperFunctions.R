library(tseries)
library(Hmisc)
library(apt)

# Variable description: 
# numTick-> number of stocks taken from yahoo
# tick -> string of ticker symbols for search on yahoo
# date_start -> begining date for stock info
# date_end -> final date for stock info
# stock_data -> matrix of information taken from yahoo

get.stock_data<-function(tickOne,tickTwo){
  tick <-c(tickOne,tickTwo)# ticker names defined  
  numTick <- length(tick);  
  date_start <-"2010-12-30";
  date_end <-"2013-3-6" #start and end date  
  stock_data<-list()#empty list to fill in the data  
  for(i in 1:numTick) {  
    stock_data[[i]]<-holder<-get.hist.quote(instrument = tick[i], start=date_start, end=date_end, quote = c("Open", "High", "Low", "Close", "Volume", "Adj"),provider = "yahoo", compression = "d")  
  } 
  
  
  # Manipulate stock_data as following, stock_data[[1]] is the entire TS, use indexing method of stock_data[[1]][1] to get the first row.
  # To get a certain value within that row use matrix notation within second set of brackets, as follows: stock_data[[1]][1,2] would give you the High of the day
  # Following Column indexing: 
  # Column 1 -> Open
  # Column 2 -> high
  # Column 3 -> Low
  # Column 4 -> Close
  # Column 5 -> Volume
  # Column 6 -> AdjClose
  
  
  # Calculate trailing Standard Deviation
  
  
  # to make simple for now, 3 months will be # of trading days in a year divided by 4, and 6 months will be # of trading days in a year divided by 2
  # number of trading days in a year is 251
  
  trailingDays <- c(ceiling(251/4), ceiling(251/2), 251)
  
  #Lets take out the adjusted close values for 3 months 6 months and a year to create a vector of just the values we need
  
  threeMonth <- matrix(nrow=trailingDays[1],ncol=numTick)
  sixMonth<- matrix(nrow=trailingDays[2],ncol=numTick)
  year<- matrix(nrow=trailingDays[3],ncol=numTick)
  numOfTrades=length(stock_data[[1]])/6
  
  periods <- list(threeMonth, sixMonth, year)
  
  for(i in 1:numTick){
    for(n in 1:3){
      for(cnt in 1:trailingDays[n]){
        periods[[n]][cnt,i]=stock_data[[i]][numOfTrades-trailingDays[n] + cnt,6]
      }
    }
  }
  periods
}




metrics<-function(tickOne,tickTwo){
  periods = get.stock_data(tickOne,tickTwo)
  standardDev <- matrix(nrow=3, ncol=2)
  avgs <- matrix(nrow=3, ncol=2)
  vars <- matrix(nrow=3, ncol=2) 
  cors <- c() 
  
  #Populating said matrices with the respective information for future use
  # standardDev is a matrix of standard deviations of the stock for respective periods
  # avgs is a matrix of averages for each stock and respective timelines. Both matrices ar 3x2
  
  for(i in 1:3){
    for(n in 1:2){
      standardDev[i,n] = sd(periods[[i]][,n])
      avgs[i,n] = mean(periods[[i]][,n])
      vars[i,n] = var(periods[[i]][,n])
    }
  }
  
  for(i in 1:3){
    x=cor(periods[[i]])
    cors[i]=x[1,2]
  }
  
  dimnames(avgs)=list(c("Three Months","Six Months","Year"),c("stock1","stock2"))
  dimnames(standardDev)=list(c("Three Months","Six Months","Year"),c("stock1","stock2"))
  dimnames(vars)=list(c("Three Months","Six Months","Year"),c("stock1","stock2"))
  
  print(     "Averages")
  print(avgs)
  
  print(     "StandardDeviations")
  print(standardDev)
  
  print(     "Variances")
  print(standardDev)
  
  print(     "Correlations")
  print(cors)
  par(mfrow=c(2,1))
  plot(periods[[3]][,1], type="l", main=tickOne, xlab="Time- 1 Year",ylab="$")
  plot(periods[[3]][,2], type="l",main=tickTwo, xlab="Time- 1 Year",ylab="$")
}



# period {1,2,3} 
# 1 corresponds to 3 months
# 2 corresponds to 6 months
# 3 corresponds to 1 year
deltaMethod<-function(tickOne, tickTwo, periodNum, zScoreLimit, initialPortValue){
  periods = get.stock_data(tickOne,tickTwo)
  trailingDays <- c(ceiling(251/4), ceiling(251/2), 251)
  delta<-c(nrow=trailingDays[periodNum],ncol=1)
  
  for(i in 1:trailingDays[periodNum]){
    delta[i]=periods[[periodNum]][i,1]-periods[[periodNum]][i,2]
  }
  
  deltaSD=sd(delta)
  deltaMean=mean(delta)
  
  deltaZscore<-matrix(nrow=trailingDays[periodNum], ncol=1)
  
  for(i in 1:trailingDays[periodNum]){
    deltaZscore[i]=(delta[i]-deltaMean)/deltaSD
  }
  
  #Let us count the number of outliers by inspecting the vector of Zscore
  
  deltaOutlierCnt<-0;
  
  for(i in 1:length(deltaZscore)){
    if(deltaZscore[i] > zScoreLimit|| deltaZscore[i] < -zScoreLimit) {deltaOutlierCnt=deltaOutlierCnt+1 }
  }
  
  triggers<-tradeTrigger(deltaZscore, zScoreLimit, periodNum, periods)
  portfolio<-pNl(triggers, periods, periodNum, initialPortValue, deltaZscore)
  
  par(mfrow=c(2,2))
  plot(periods[[3]][,1], type="l", main=tickOne,ylab="$")
  plot(periods[[3]][,2], type="l",main=tickTwo,ylab="$")
  plot(deltaZscore, main="Z-Scores",type="l", ylab="# Standard Deviations" )
  plot(portfolio, type="l",main="portfolio")
  print("The number of outliers is: ")
  print(deltaOutlierCnt)
  print("The Mean is: ")
  print(deltaMean)
  triggers<-tradeTrigger(deltaZscore, zScoreLimit, periodNum, periods)
  portfolio<-pNl(triggers, periods, periodNum, initialPortValue, deltaZscore)
  print(portfolio)
  
  
}




#This method analyzes the ratio of the pair, Stock1/Stock2 to identify a trend
ratioMethod <- function(tickOne, tickTwo, periodNum, zScoreLimit, initialPortValue){
  periods = get.stock_data(tickOne,tickTwo)
  trailingDays <- c(ceiling(251/4), ceiling(251/2), 251)
  ratio <- matrix(nrow =trailingDays[periodNum], ncol=1)
  
  for(i in 1:trailingDays[periodNum]){
    ratio[i] = periods[[periodNum]][i,1]/periods[[periodNum]][i,2]
  }
  
  ratioZscore <- matrix(nrow=trailingDays[periodNum], ncol=1) 
  
  ratioSD=sd(ratio[,1])
  ratioMean=mean(ratio)
  
  for(i in 1:trailingDays[periodNum]){
    ratioZscore[i]=(ratio[i]-ratioMean)/ratioSD
  }
  
  ratioOutlierCnt<-0;
  
  for(i in 1:length(ratioZscore)){
    if(ratioZscore[i] >= 1.5 || ratioZscore[i] <= -1.5) {ratioOutlierCnt<-ratioOutlierCnt+1 }
  }
  
  print("The number of outliers is")
  print(ratioOutlierCnt)
  triggers<-tradeTrigger(ratioZscore, zScoreLimit, periodNum, periods)
  portfolio<-pNl(triggers, periods, periodNum, initialPortValue,ratioZscore)
  print(portfolio)
  
  par(mfrow=c(2,2))
  plot(periods[[3]][,1], type="l", main=tickOne, xlab="days", ylab="$")
  plot(periods[[3]][,2], type="l", main=tickTwo, xlab="days", ylab="$")
  plot(ratioZscore, main="Z-Scores", type="l", xlab="days" )
  plot(portfolio, type="l", main="Ratio Of The Pair")
  
}




#Conintegration analysis using general linear regression
#We will use log returns for this method to normalize the value
cointegration<-function(tickOne, tickTwo, periodNum){
  trailingDays <- c(ceiling(251/4), ceiling(251/2), 251)
  periods = get.stock_data(tickOne,tickTwo)
  logReturns<-matrix(nrow=trailingDays[periodNum]-1,ncol=2)
  logs <- matrix(nrow=trailingDays[periodNum]-1,ncol=2)
  for(i in 1:2){
    for(j in 1:(trailingDays[periodNum]-1)){
      logs [j,i] = log(periods[[periodNum]][j,i])
      logReturns[j,i]=log(periods[[periodNum]][j+1,i])-log(periods[[periodNum]][j,i])
    }
  }
  lmfit<-lm(logs[,1]~logs[,2]+0)
  print(lmfit)
  beta<-coef(lm(lmfit))
  plot(logs[,1]-beta*logs[,2],type="l")
  print("your Coefficient for the best linear fit:")
  print(beta)
  print(adf.test(logs[,1]-beta*logs[,2], alternative="stationary", k=0))
  #indep<-ts(logs[,1])
  #depen<-ts(logs[,2])
  #ecmSymFit(firstStock,secondStock)
  
}



#We will now begin creating portfolio and backtesting modules
# The function will have the following inputs-
# two tickers, time period to be tested, # standard deviations to be tested
# initial value willing to put into the strategy

tradeTrigger <- function(numOfStandardDeviations, zScoreLimit, periodNum, periods){
  trailingDays <- c(ceiling(251/4), ceiling(251/2), 251)
  portfolio <-rep(0,trailingDays[periodNum])
  
  for(i in 1:trailingDays[periodNum]){
    if(zScoreLimit < numOfStandardDeviations[i] ){
      portfolio[i]=1
    }
    else if(-zScoreLimit > numOfStandardDeviations[i]){
      portfolio[i]=-1
    }
    else{ 
      portfolio[i]=0 
    }
  }
  
  return(portfolio)
}


pNl <- function( triggers, periods, periodNum, initialPortValue, numOfStandardDeviations){
  trailingDays <- c(ceiling(251/4), ceiling(251/2), 251)
  portfolio <-rep(0,trailingDays[periodNum])
  portfolio[1]<-initialPortValue
  print(triggers)
  i<-1
  while(i<=trailingDays[periodNum]-1){
    
    if(triggers[i]==-1){
      if(i==1){
        initialPortValue=initialPortValue; #just coding what is essentially happening this line is not needed
      }
      else{
        initialPortValue<-portfolio[i-1]
      }
      
      repeat{
        portfolio[i]<-ceiling(initialPortValue/periods[[periodNum]][i,1])*periods[[periodNum]][i,1]-floor(initialPortValue/periods[[periodNum]][i,2])*periods[[periodNum]][i,2]+initialPortValue
        i<-i+1
        if(numOfStandardDeviations[i]<=0 || i==trailingDays[periodNum]-1){
          portfolio[i]=floor(initialPortValue/periods[[periodNum]][i,2])*periods[[periodNum]][i,2]-ceiling(initialPortValue/periods[[periodNum]][i,1])*periods[[periodNum]][i,1]+initialPortValue
          break
        }
      }
    }
    
    if(triggers[i]==1){
      if(i==1){
        initialPortValue=initialPortValue; #just coding what is essentially happening this line is not needed
      }
      else{
        initialPortValue<-portfolio[i-1]
      }
      repeat{
        portfolio[i]<-ceiling(initialPortValue/periods[[periodNum]][i,2])*periods[[periodNum]][i,2]-floor(initialPortValue/periods[[periodNum]][i,1])*periods[[periodNum]][i,1]+initialPortValue
        i<-i+1
        if(numOfStandardDeviations[i]>=0 || i==trailingDays[periodNum]){
          portfolio[i]=floor(initialPortValue/periods[[periodNum]][i,1])*periods[[periodNum]][i,1]-ceiling(initialPortValue/periods[[periodNum]][i,2])*periods[[periodNum]][i,2]+initialPortValue
          break
        }
      }
    }
    
    if(triggers[i]==0 && i==1){
      portfolio[i]<-initialPortValue
      i<-i+1
    }
    else if(triggers[i]==0){
      portfolio[i]<-portfolio[i-1]
      i<-i+1
    }
    
  }
  
  return(portfolio)
}



# number of trades (open and close is 1)
# Performance, portfolio