#x <- 3
square = 
    c(0,0,0,0,0,0,0,0,
      0,0,1,1,1,0,0,0,
      0,0,1,1,1,0,0,0,
      0,0,1,1,1,0,0,0,
      0,0,1,1,1,0,0,0,
      0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0)

expected = 
    c(0,0,0,0,0,0,0,0,
      0,0,1,1,1,0,0,0,
      0,0,1,0,1,0,0,0,
      0,0,1,0,1,0,0,0,
      0,0,1,1,1,0,0,0,
      0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0)

squareArray = array(square,dim = c(8,8))
expectedArray = array(expected,dim = c(8,8))

resultsTable = vector()

for(i in 2:7){
  for(j in 2:7){
    # it is running from top to down (strange)
    x_11 = squareArray[i-1,j-1]
    x_12 = squareArray[i-1,j]
    x_13 = squareArray[i-1,j+1]
    x_21 = squareArray[i,j-1]
    x_22 = squareArray[i,j]
    x_23 = squareArray[i,j+1]
    x_31 = squareArray[i+1,j-1]
    x_32 = squareArray[i+1,j]
    x_33 = squareArray[i+1,j+1]
    
    r_22 = expectedArray[i,j]
    
    test1 <- c(x_11,x_12,x_13,x_21,x_22,x_23,x_31,x_32,x_33)
    test2 <- c(x_11,x_12,x_13,x_21,abs(x_22 - 1),x_23,x_31,x_32,x_33)
    
    test3 <- c(r_22)
    collapsed = paste(test1, collapse=',')
    resultsTable<-c(resultsTable,c(collapsed,x_22,abs(x_22 - r_22)))
    resultsTable<-c(resultsTable,c(collapsed,x_22,abs(abs(x_22 - 1) - r_22)))
  }
}
results <- t(array(resultsTable, dim = c(3,30)))
print(results)


goodValues <- new.env(hash=TRUE)
for(i in 2:30){
  if(results[i,3] =="0"){
    print('added')
    goodValues[[results[i,1]]] = results[i,2]
  }
}


test = 
  
  c(0,0,0,0,0,0,0,0,
    0,0,1,1,1,0,0,0,
    0,0,1,1,1,0,0,0,
    0,0,1,1,1,0,0,0,
    0,0,1,1,1,0,0,0,
    0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0)

testArray = array(test,dim = c(8,8))

bits = vector()

for(i in 2:7){
  for(j in 2:7){
    # it is running from top to down (strange)
    x_11 = testArray[i-1,j-1]
    x_12 = testArray[i-1,j]
    x_13 = testArray[i-1,j+1]
    x_21 = testArray[i,j-1]
    x_22 = testArray[i,j]
    x_23 = testArray[i,j+1]
    x_31 = testArray[i+1,j-1]
    x_32 = testArray[i+1,j]
    x_33 = testArray[i+1,j+1]
    
    test1 <- c(x_11,x_12,x_13,x_21,x_22,x_23,x_31,x_32,x_33)
    collapsed = paste(test1, collapse=',')
    if(exists(collapsed,envir = goodValues)){
      bitResult = get(collapsed,envir = goodValues)  
    }else{
      print(collapsed)
      bitResult = "x"
    }
    bits<- c(bits,bitResult)
  }
}

print(bits)

#print(Sys.getenv(x=goodValues, unset = NA))
# how to print environment?
# missing something here. "x" should not appears since test and training are the same