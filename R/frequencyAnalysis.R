#
# Henry Samuelson 1/3/18
#
# Frequency Analysis for Ceasar Ciphers
#


asc <- function(x) { strtoi(charToRaw(x),16L) -96 } #to numeric

chr <- function(n) { #Same function used for the Viengere Cipher
  final <- character()
  for(i in 1:length(n)){
    if(n[i] != 0){
      final[i] <- rawToChar(as.raw(n[i]%%33 + 96))
    }
    else{
      final[i] <- "я"
    }
  }
  for(i in 1:length(final)){
    if(final[i] == "`"){
      final[i] <- "я"
    }
  }
  return(paste0(final, collapse = ""))
} #to charaters

ceasar.FrequencyAnalysis <- function(messageF){
  frequencyCount <- sort(table(strsplit(messageF, "")[[1]]))
  e <- tolower(names(frequencyCount[length(frequencyCount)]))

  #Shift forward until the letter is e
  return(ceasar.Decrypt(messageF, (asc(e) - 5)))
}
#Example tests
#ceasar.FrequencyAnalysis(ceasar.Encrypt("hellomynameishenryandiliektogotheparkandeatfoodbecasueithinkitisfun", 4))


#Two primes
p1 <- 6659707331
p2 <- 2274922729


#messageF <- "ПРОСТО ХОЧУ ВАС УСІХ ПРИВІТАТИ" #test input

subsutution.FrequencyAnalysis <- function(messageF){
  frequencyLetter <- c("О", "А", "Н", "І", "И", "В", "Р", "Т", "Е", "С", "К", "Л", "У", "Д", "М", "П", "Я", "З", "Ь", "Г", "Б", "Й", "Х", "Ч", "Ц", "Ї", "Ж", "Ш", "Ю", "Є", "Ф", "Щ", "Ґ")
  frequencyChance <- as.double(c(	9.28,
								8.34,	7.10,	6.23,	6.00,	5.50,	5.48,	4.77,	4.59,
                4.57,	4.00,	3.93,	3.38,	3.06,	3.02,	2.84,	2.16,
                2.10,	1.83,	1.59,	1.53,	1.24,	1.17,	1.15,	1.02,
                0.84,	0.71,	0.71,	0.70,	0.39,	0.35,	0.32,	0.01 ))

  frequencyTable <- cbind(frequencyLetter, frequencyChance)
  frequencyCount <- (sort(table(strsplit(messageF, "")[[1]])))
  #Issue here is often not all letters are used in a encrypted message
  #Which means we cannot run right through the list and find
  frequencysOfMessage <- frequencyCount/length(strsplit(messageF, "")[[1]])
  #Fill in the remaing area of the area to make 26 numbers so we can do matrix subtraction
  #frequencysOfMessage <- c(frequencysOfMessage ,rep(0,(26 -length(frequencysOfMessage))))

  newIndexs <- numeric()
  for(i in 1:length(frequencysOfMessage)){
    newIndexs[i] <- (which(abs(as.double(frequencyTable[,2])-frequencysOfMessage[i]*100)==min(abs(as.double(frequencyTable[,2])-frequencysOfMessage[i]*100))) )
  }
  letterList <- frequencyLetter[newIndexs] #Get guessed numbers in order
  officalGuesses <- cbind(names(frequencyCount), letterList) #pair them with the encrypted frequencys
  splittedMessage <- strsplit(messageF, "")[[1]]
  decrypedMessage <- numeric()
  for(i in 1:length(splittedMessage)){
    for(j in 1:length(officalGuesses[,1])){
      if(splittedMessage[i] == officalGuesses[j,1]){
        decrypedMessage[i] <- officalGuesses[j,2]
      }
    }
  }
  return(list(decrypedMessage, officalGuesses))
}

