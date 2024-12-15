## Advent 2024
## D1 p1
myin <- read.table("/Users/tfaits/Documents/Advent2024/d1.txt", header=FALSE)
t1 <- sort(myin[,1])
t2 <- sort(myin[,2])
t3 <- abs(t2-t1)
sum(t3)
#p2
simscore <- 0
for(i in 1:length(t1)){
  tmp <- t1[i]*sum(t2==t1[i])
  simscore <- simscore + tmp
  if(tmp > 0){
    print(tmp)
  }
}
print(simscore)

## D2 p1
myin <- read.table("/Users/tfaits/Documents/Advent2024/d2.txt", header=FALSE, fill=TRUE, col.names=paste("column",1:10, sep="_"))
checksafe <- function(inline){
  inline <- unlist(inline)
  inline <- inline[!is.na(inline)]
  if(length(inline)==1){
    return(TRUE)
  }
  if(!identical(inline,sort(inline)) & !identical(inline,sort(inline,decreasing = TRUE))){
    return(FALSE)
  }
  for(i in 2:length(inline)){
    if((abs(inline[i]-inline[i-1]) > 3) | (inline[i] == inline[i-1])){
      return(FALSE)
    }
  }
  return(TRUE)
}
sumSafe <- 0
for(i in 1:nrow(myin)){
  tmp <- checksafe(myin[i,])
  if(tmp){
    print(unname(unlist(myin[i,])))
    sumSafe <- sumSafe + 1
  }
}
print(sumSafe)

#p2
softSafe <- 0
for(i in 1:nrow(myin)){
  tmp <- unlist(myin[i,])
  tmp <- tmp[!is.na(tmp)]
  isOk <- FALSE
  for(j in 1:length(tmp)){
    if(checksafe(tmp[-j])){
      isOk <- TRUE
    }
  }
  softSafe <- softSafe + isOk
}
print(softSafe)

##D3 p1
library(readr)
myin <- read_file("/Users/tfaits/Documents/Advent2024/d3.txt")
tmp <- gregexpr("mul[(][[:digit:]]{1,3},[[:digit:]]{1,3})",myin)
tmp2 <- sapply(tmp, function(y) attr(y, "match.length"))[,1]
tmp <- tmp[[1]]
mul <- function(x,y){
  return(x*y)
}
myTot <- 0
for(i in 1:length(tmp)){
  myTot <- myTot + eval(parse(text=substr(myin, tmp[i],tmp[i]+tmp2[i]-1)))
}
##p2
modin <- paste(strsplit(myin,"don't()")[[1]][1], paste(sapply(strsplit(myin, "don't()")[[1]], function(x){paste(strsplit(x,"do()")[[1]][-1],collapse="!")}),collapse="!"),sep="!")
tmpb <- gregexpr("mul[(][[:digit:]]{1,3},[[:digit:]]{1,3})",modin)
tmp2b <- sapply(tmpb, function(y) attr(y, "match.length"))[,1]
tmpb <- tmpb[[1]]
myTotb <- 0
for(i in 1:length(tmpb)){
  myTotb <- myTotb + eval(parse(text=substr(modin, tmpb[i],tmpb[i]+tmp2b[i]-1)))
}
myTotb

##D4 p1
myin <- read.table("/Users/tfaits/Documents/Advent2024/d4.txt", header=F, sep="")
#myin <- data.frame(thing=c("MMMSXXMASM","MSAMXMSMSA","AMXSXMAAMM","MSAMASMSMX","XMASAMXAMM","XXAMMXXAMA","SMSMSASXSS","SAXAMASAAA","MAMMMXMMMM","MXMXAXMASX"))
ws <- data.frame(Fake1=rep("F",nrow(myin)+6),Fake2=rep("F",nrow(myin)+6),Fake3=rep("F",nrow(myin)+6))
for(i in 1:nrow(myin)){
  ws[[paste("Col",i,sep="_")]] <- c("F","F","F",strsplit(myin[i,],"")[[1]],"F","F","F")
}
ws$Fake4 <- rep("F",nrow(myin)+6)
ws$Fake5 <- rep("F",nrow(myin)+6)
ws$Fake6 <- rep("F",nrow(myin)+6)
ws <- t(ws)
xmas <- 0
#Check if each location is the start-point of XMAS in any direction and count it.
for(i in 4:(nrow(ws)-3)){
  for(j in 4:(ncol(ws)-3)){
    rt <- paste(ws[i,j:(j+3)],collapse="")
    lt <- paste(ws[i,j:(j-3)],collapse="")
    up <- paste(ws[i:(i-3),j],collapse="")
    dn <- paste(ws[i:(i+3),j],collapse="")
    ur <- paste0(ws[i,j],ws[i-1,j+1],ws[i-2,j+2],ws[i-3,j+3])
    ul <- paste0(ws[i,j],ws[i-1,j-1],ws[i-2,j-2],ws[i-3,j-3])
    dr <- paste0(ws[i,j],ws[i+1,j+1],ws[i+2,j+2],ws[i+3,j+3])
    dl <- paste0(ws[i,j],ws[i+1,j-1],ws[i+2,j-2],ws[i+3,j-3])
    xmas <- xmas + tmp
  }
}
print(xmas)
#p2
masx <- 0
for(i in 4:(nrow(ws)-3)){
  for(j in 4:(ncol(ws)-3)){
    if(ws[i,j]=="A"){
      if(sum(c(ws[i+1,j+1],ws[i+1,j-1],ws[i-1,j+1],ws[i-1,j-1]) %in% c("M","S"))==4 & ws[i+1,j+1]!=ws[i-1,j-1] & ws[i+1,j-1]!=ws[i-1,j+1]){
        masx <- masx+1
      }
    }
  }
}
print(masx)

##D5 p1
myin <- read.table("/Users/tfaits/Documents/Advent2024/d5a.txt", header=F, sep="|")
tmp <- strsplit(read_file("/Users/tfaits/Documents/Advent2024/d5b.txt"),"\n")[[1]]
tmp <- tmp[-1]
checkOrder <- function(instr){
  thing <- strsplit(instr, ",")[[1]]
  for(i in 1:(length(thing)-1)){
    for(j in (i+1):length(thing)){
      if(thing[j] %in% myin[myin[,2]==thing[i],1]){
        return(FALSE)
      }
    }
  }
  return(TRUE)
}
curTot <- 0
badones <- list()
for(myline in tmp){
  if(checkOrder(myline)){
    temp <- strsplit(myline,",")[[1]]
    curTot <- curTot + as.numeric(temp[ceiling(length(temp)/2)])
  }else{
    grrr <- as.numeric(strsplit(myline,",")[[1]])
    badones <- append(badones, list(grrr))
  }
}
print(curTot)
fixorder <- function(temp){
  idx <- 1
  while(idx < length(temp)-1){
    moveon <- TRUE
    for(j in (idx+1):length(temp)){
      if(temp[j] %in% myin[myin[,2]==temp[idx],1]){
        temp <- append(temp, temp[j], idx-1)
        temp <- temp[-(j+1)]
        idx <- idx - 1
        moveon <- FALSE
        break
      }
    }
    if(moveon){
      idx <- idx + 1
    }
  }
  return(temp)
}
newTot <- 0
for(i in badones){
  thing <- fixorder(i)
  newTot <- newTot + thing[ceiling(length(thing)/2)]
}
print(newTot)

##D6 p1
## X axis turns into -Y axis, Y axis turns into X axis (positive) to make a 90-degree right-hand turn!
myin <- read.table("/Users/tfaits/Documents/Advent2024/d6.txt", header=FALSE, comment.char = "")
# myin <- data.frame(V1=strsplit("....#.....
# .........#
# ..........
# ..#.......
# .......#..
# ..........
# .#..^.....
# ........#.
# #.........
# ......#...","\n")[[1]])
#nDim <- 10
nDim <- 130
mymat <- matrix(nrow=nDim, ncol=nDim)
for(i in 1:nDim){
  mymat[i,] <- strsplit(myin[i,1],"")[[1]]
}
## Starting point, found by checking which element=="^", is [74,42]. I'm hard-coding this, I guess.
mycoord <- c(74,42)
#mycoord <- c(7,5)
printMap <- function(){
  for(i in 1:nDim){
    print(paste(mymat[i,],collapse=""))
  }
}
while(mycoord[1] > 0){
  if(mymat[mycoord[1],mycoord[2]] == "#"){
    tmp1 <- mycoord[1]+1
    tmp2 <- nDim-mycoord[2]
    mycoord <- c(tmp2,tmp1)
    tempMat <- matrix(ncol=nDim, nrow=nDim)
    for(i in 1:nDim){
      tempMat[i,] <- mymat[,nDim+1-i]
    }
    mymat <- tempMat
    print("~~~~~~~~~~~~~~~~~~")
    print("~~~~~~~~~~~~~~~~~~")
    print("~~~~~~~~~~~~~~~~~~")
    printMap()
    #readline(prompt="Press [enter] to continue")
  }else{
    mymat[mycoord[1],mycoord[2]] <- "X"
    mycoord[1] <- mycoord[1]-1
  }
}
sum(mymat == "X")
#p2 ... not super proud of how I'm going to do this one, but I'm just going to try out every possible obstruction (only need to do where X's are)
#First, I want to put the map back in its original orientation (and save a copy so I have all of the possible X's)
## Ugly, very slow. Sorry! But it works.
for(i in 1:3){
  tempMat <- matrix(ncol=nDim, nrow=nDim)
  for(i in 1:nDim){
    tempMat[i,] <- mymat[,nDim+1-i]
    }
  mymat <- tempMat
}
origMap <- mymat
origMap[74,42] <- "."
posCoords1 <- c()
posCoords2 <- c()
for(i in 1:nDim){
  for(j in 1:nDim){
    if(origMap[i,j]=="X"){
      #If an obstacle can go here, reset the map and add it:
      mymat <- matrix(nrow=nDim, ncol=nDim)
      for(idx in 1:nDim){
        mymat[idx,] <- strsplit(myin[idx,1],"")[[1]]
      }
      mymat[i,j] <- "#"
      mycoord <- c(74,42)
      #Go through the whole rigamarole again, kill process if too many steps travelled
      curSteps <- 0
      while(mycoord[1] > 0){
        curSteps <- curSteps+1
        if(curSteps > nDim*nDim*4){
          posCoords1 <- c(posCoords1, i)
          posCoords2 <- c(posCoords2, j)
          print("Found an endless loop")
          break()
        }
        if(mymat[mycoord[1],mycoord[2]] == "#"){
          tmp1 <- mycoord[1]+1
          tmp2 <- nDim-mycoord[2]
          mycoord <- c(tmp2,tmp1)
          tempMat <- matrix(ncol=nDim, nrow=nDim)
          for(idx in 1:nDim){
            tempMat[idx,] <- mymat[,nDim+1-idx]
          }
          mymat <- tempMat
        }else{
          mymat[mycoord[1],mycoord[2]] <- "X"
          mycoord[1] <- mycoord[1]-1
        }
      }
      print(paste("Done with",i,j))
    }
  }
}

##D7 p1
myin <- read.table("/Users/tfaits/Documents/Advent2024/d7.txt", header=FALSE, sep="", fill=TRUE,col.names = paste("Column",1:15,sep="_"))
myin <- myin[,apply(myin,2,function(x){sum(is.na(x)) < length(x)})]
myin[,1] <- as.numeric(sapply(myin[,1],function(x){strsplit(x,":")[[1]][1]}))
checkPossible <- function(target, invec, isPartB=FALSE){
  ispos <- FALSE
  if(length(invec)==2){
    if(invec[1]+invec[2]==target | invec[1]*invec[2]==target | (as.numeric(paste0(invec[1],invec[2]))==target & isPartB)){
      return(TRUE)
    }else{
      return(FALSE)
    }
  }else{
    #Only test multiplication if the final value is a factor of the current target
    if(target %% invec[length(invec)] == 0){
      if(checkPossible(target=(target/invec[length(invec)]), invec=invec[-(length(invec))], isPartB=isPartB)){
        return(TRUE)
      }
    }
    #Only test catenation if the final value matches the last digits of the target!
    if(target %% (10^nchar(invec[length(invec)])) == invec[length(invec)] & isPartB){
      if(checkPossible(target=(floor(target/10^nchar(invec[length(invec)]))), invec=invec[-length(invec)], isPartB=isPartB)){
        return(TRUE)
      }
    }
    #Always check the addition option.
    if(checkPossible(target=(target-invec[length(invec)]), invec=invec[-(length(invec))], isPartB=isPartB)){
      return(TRUE)
    }
  }
  return(FALSE)
}
mytot <- 0
for(i in 1:nrow(myin)){
  if(checkPossible(target=myin[i,1], invec=myin[i,!is.na(myin[i,])][-1])){
    mytot <- mytot + myin[i,1]
  }
}
format(mytot, digits=nchar(mytot))
#p2 (Oh dip, this is of course set up in a way that it's a lot harder to just go back-to-front)
## Wait, no... we just lop off the end of "target" to work backwards (like when we divide for multiply or subtract for add!)
mytot <- 0
for(i in 1:nrow(myin)){
  if(checkPossible2(target=myin[i,1], invec=myin[i,!is.na(myin[i,])][-1], isPartB=TRUE)){
    mytot <- mytot + myin[i,1]
  }
}
format(mytot, digits=nchar(mytot))

##D8 p1
myin <- read.table("/Users/tfaits/Documents/Advent2024/d8.txt", header=FALSE, comment.char = "")
##The test data:
# myin <- data.frame(V1=c("............",
#           "........0...",
#           ".....0......",
#           ".......0....",
#           "....0.......",
#           "......A.....",
#           "............",
#           "............",
#           "........A...",
#           ".........A..",
#           "............",
#           "............"))
nDim <- nrow(myin)
mymat <- matrix(nrow=nDim, ncol=nchar(myin[1,1]))
for(i in 1:nDim){
  mymat[i,] <- strsplit(myin[i,1],"")[[1]]
}
mychars <- unique(as.vector(mymat))
mychars <- mychars[mychars != "."]
myAntinodes <- c()
for(mychar in mychars){#Loop through the frequencies
  print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
  print(paste("Working on",mychar))
  print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
  tmp <- which(mymat==mychar)#Pull the indexes of each tower that matches the current frequency
  if(length(tmp) > 1){#Need at least 2 towers to have any antinodes!
    for(idx1 in tmp){
      for(idx2 in tmp){#For each pair of towers
        if(idx1 != idx2){#A tower doesn't have an antinode with itself!
          coor1a <- (idx1-1) %% nDim + 1
          coor2a <- ceiling(idx1 / nDim)
          coor1b <- (idx2-1) %% nDim + 1
          coor2b <- ceiling(idx2 / nDim)
          anode <- c(2*coor1b-coor1a, 2*coor2b-coor2a)
          if(anode[1] < nDim+1 & anode[1] > 0 & anode[2] < nDim+1 & anode[2] > 0){
            myAntinodes <- c(myAntinodes, paste(anode, collapse=","))
            print(anode)
          }
        }
      }
    }
  }
  print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
  print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
}
print(length(unique(myAntinodes)))
#p2 Easy to just do multiples of the difference between stuff, but I need to make sure there aren't more antinodes between the whole steps:
#...in other words, I'll need to see if dX and dY have a common factor
myAntinodes <- c()
gcd <- function(x,y) {
  r <- x%%y;
  return(abs(ifelse(r, gcd(y, r), y)))
}#Output is abs() to avoid getting negative GCDs when inputs might be negative. This is to avoid flipping directions when moving along slopes below.
for(mychar in mychars){#Loop through the frequencies
  print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
  print(paste("Working on",mychar))
  print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
  tmp <- which(mymat==mychar)#Pull the indexes of each tower that matches the current frequency
  if(length(tmp) > 1){#Need at least 2 towers to have any antinodes!
    for(idx1 in tmp){
      for(idx2 in tmp){#For each pair of towers
        if(idx1 != idx2){#A tower doesn't have an antinode with itself!
          coor1a <- (idx1-1) %% nDim + 1
          coor2a <- ceiling(idx1 / nDim)
          coor1b <- (idx2-1) %% nDim + 1
          coor2b <- ceiling(idx2 / nDim)
          xdist <- coor1b - coor1a#Yeah, X/Y are flipped from normal conventions. I always get mixed up with how dataframes are subset by row,column, which means y,x instead of x,y
          ydist <- coor2b - coor2a
          distGCD <- gcd(xdist, ydist)
          xdist <- xdist/distGCD
          ydist <- ydist/distGCD
          curx <- coor1a
          cury <- coor2a
          while(curx <= nDim & curx > 0 & cury <= nDim & cury > 0){
            myAntinodes <- c(myAntinodes, paste(curx,cury, sep=","))
            print(paste(curx,cury, sep=","))
            curx <- curx + xdist
            cury <- cury + ydist
          }
        }
      }
    }
  }
  print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
  print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
}
print(length(unique(myAntinodes)))

##D9 p1
myin <- strsplit(read.table("/Users/tfaits/Documents/Advent2024/d9.txt", header=FALSE, colClasses = "character")[1,1],"")[[1]]
#myin <- strsplit("2333133121414131402","")[[1]]#Test case
compmap <- c()
for(i in 1:length(myin)){
  if(i%%2==1){
    compmap <- c(compmap, rep((i-1)/2, myin[i]))
  }else{
    compmap <- c(compmap, rep(NA, myin[i]))
  }
}#Set up the full map (dumb, I know, but I'm doing it!)
cleanMap <- function(){
  while(is.na(compmap[length(compmap)])){
    compmap <<- compmap[-length(compmap)]
  }
}#A way to remove trailing NAs
cleanMap()
i <- 1
while(i < length(compmap)){
  if(is.na(compmap[i])){
    compmap[i] <- compmap[length(compmap)]
    compmap <- compmap[-length(compmap)]
    cleanMap()
  }
  i <- i+1
}#Rearrange the file blocks
checkSum <- 0
for(i in 1:length(compmap)){
  checkSum <- checkSum + (i-1)*compmap[i]
}
print(format(checkSum, digits=nchar(checkSum)))
#p2 Oh gods, I have mad the most terrible brute-force solution here!
# Forgive me, for I have sinned, for this take over a full minute to run
compmap <- c()
for(i in 1:length(myin)){
  if(i%%2==1){
    compmap <- c(compmap, paste((i-1)/2, myin[i],sep="_"))
  }else{
    compmap <- c(compmap, as.numeric(myin[i]))
  }
}
for(i in ((length(myin)-1)/2):1){#Cycle through each file
  print(paste("Working on file #",i))
  startSpot <- grep(paste0("^",i,"_"),compmap)#Find where each file currently starts
  tmp <- as.numeric(strsplit(compmap[startSpot],"_")[[1]][2])#Get the file size
  for(j in 1:(startSpot-1)){
    if(!grepl("_", compmap[j])){#Scan through the current file map and look for empty spaces (files all have "_" in them)
      if(compmap[j] >= tmp){
        compmap[j] <- as.numeric(compmap[j]) - tmp
        compmap <- c(compmap[1:(j-1)], compmap[startSpot], compmap[j:(startSpot-1)],tmp,compmap[startSpot:length(compmap)][-1])
        break
      }
    }
  }
}
tmp <- c()
for(i in compmap){
  if(grepl("_",i)){
    temp <- strsplit(i,"_")[[1]]
    tmp <- c(tmp, rep(temp[1],temp[2]))
  }else{
    tmp <- c(tmp, rep(0,i))
  }
}
checkSum <- 0
for(i in 1:length(tmp)){
  checkSum <- checkSum + (i-1)*as.numeric(tmp[i])
}
print(format(checkSum, digits=nchar(checkSum)))

## D10, p1
myin <- read.table("/Users/tfaits/Documents/Advent2024/d10.txt", header=FALSE, comment.char = "", colClasses = "character")
# myin <- data.frame(V1=c("89010123",
#           "78121874",
#           "87430965",
#           "96549874",
#           "45678903",
#           "32019012",
#           "01329801",
#           "10456732"))
nDim <- nrow(myin)
#I'm going to add a border around the map with impossible-to-reach values (11) to make checking up/down/left/right less painful
mymat <- matrix(nrow=nDim+2, ncol=nchar(myin[1,1])+2)
for(i in 1:nDim){
  mymat[i+1,2:(nDim+1)] <- as.numeric(strsplit(myin[i,1],"")[[1]])
}
mymat[is.na(mymat)] <- 11
checkPath <- function(curloc){
  curAlt <- mymat[curloc]
  trailEnd <- c()
  if(curAlt==9){
    return(curloc)
  }else{
    for(i in c(curloc-ncol(mymat), curloc-1, curloc+1, curloc+ncol(mymat))){#Check left/up/down/right
      if(mymat[i] == curAlt+1){
        trailEnd <- c(trailEnd, checkPath(i))
      }
    }
    return(trailEnd)
  }
}
myPaths <- list()
for(location in which(mymat==0)){#Loop through each trailhead
  myPaths[[as.character(location)]] <- unique(checkPath(location))
}
print(sum(sapply(myPaths,length)))
#p2 Looks like I already solved this one. I just need to drop the "unique" call on the trail counts!
myPaths <- list()
for(location in which(mymat==0)){#Loop through each trailhead
  myPaths[[as.character(location)]] <- checkPath(location)
}
print(sum(sapply(myPaths,length)))

##D11 p1:
## Initial thoughts: this seems very straightforward to just *do*, but like it might balloon out into insane size/time/memory.
## So... is the trick to find the trick? I guess we'll see.
## One thing to consider is that neighboring rocks don't interract with each other, and they keep their order, so you can really process a single rock at a time.
myin <- c(5, 127, 680267, 39260, 0, 26, 3553, 5851995)
processRock <- function(rock){
  if(rock){
    if(nchar(rock) %% 2){
      return(rock*2024)
    }else{
      sp <- 10^(nchar(rock)/2)
      return(c(floor(rock/sp), rock %% sp))
    }
  }else{
    return(1)
  }
}
for(idx in 1:25){
  myout <- c()
  for(rock in myin){
    myout <- c(myout, processRock(rock))
  }
  myin <- myout
  #print(length(myin))
  print(idx)
  print(max(myin))
}

#p2
# Ok, yes, I was correct about the problem. The solution also seems straightforward: identify patterns of what types of numbers will produce numbers that split
##P1 is possible as-is, but takes forever. Definitely can't do 75 iterations that way. So! Let's crack numbers into *types* of numbers
##And find a function that will tell us what we get.
## I have a hunch that some values are going to keep coming up, as things loop a lot. To explore that, let's check:
length(myin)
length(unique(myin))## Only 54 unique values, despite 19778 rocks! That's... much easier to work with.
## So, I need to keep track of counts of values, rather than values. This would probably be more straightforward with a Python dictionary...
myin <- c(5, 127, 680267, 39260, 0, 26, 3553, 5851995)#Reset the starting values
myRocks <- list()
for(i in myin){
  myRocks[[as.character(i)]] <- 1#There are no repeated values in my input. This does assume that that's the case.
}
for(idx in 1:75){
  nextRound <- list()
  for(rock in names(myRocks)){
    tmp <- processRock(as.numeric(rock))
    for(newRock in tmp){
      if(newRock %in% names(nextRound)){
        nextRound[[as.character(newRock)]] <- nextRound[[as.character(newRock)]] + ifelse(rock %in% names(myRocks), myRocks[[as.character(rock)]], 1)
      }else{
        nextRound[[as.character(newRock)]] <- ifelse(rock %in% names(myRocks), myRocks[[as.character(rock)]], 1)
      }
    }
  }
  myRocks <- nextRound
}
tmp <- 0
for(i in names(myRocks)){
  tmp <- tmp + myRocks[[i]]
}
print(format(tmp, digits=nchar(tmp)))

## D12 p1
myin <- read.table("/Users/tfaits/Documents/Advent2024/d12.txt", header=FALSE, comment.char = "", colClasses = "character")
# myin <- data.frame(V1=c("RRRRIICCFF",
#                         "RRRRIICCCF",
#                         "VVRRRCCFFF",
#                         "VVRCCCJFFF",
#                         "VVVVCJJCFE",
#                         "VVIVCCJJEE",
#                         "VVIIICJJEE",
#                         "MIIIIIJJEE",
#                         "MIIISIJEEE",
#                         "MMMISSJEEE"))
nDim <- nrow(myin)
#I'm going to add a border around the map with non-crops to make checking up/down/left/right less painful
mymat <- matrix(nrow=nDim+2, ncol=nchar(myin[1,1])+2)
for(i in 1:nDim){
  mymat[i+1,2:(nDim+1)] <- strsplit(myin[i,1],"")[[1]]
}
mymat[is.na(mymat)] <- "#"
## I am currently imagining starting at a point, noting what crop is there, then changing that position to an active "."
## ...then propagate outward, filling in all adjascent spots of the same crop with more "."
## Couunting dots is easy for area. counting perimiter... means just checking each dot again?
## Let's start by identifying the region and finding the area. Maybe ideas for perimeter will come to me as I work.
findRegion <- function(curloc){
  curCrop <- mymat[curloc]
  mymat[curloc] <<- "."
  myPer <- 0
  for(i in c(curloc-nrow(mymat), curloc-1, curloc+1, curloc+nrow(mymat))){#Check left/up/down/right
    if(mymat[i] == curCrop){
      myPer <- myPer + findRegion(i)
    }
    if(!mymat[i] %in% c(".", curCrop)){
      myPer <- myPer + 1
    }
  }
  return(myPer)
}
measureArea <- function(){
  return(sum(mymat=="."))
}
myPrice <- 0
for(i in 1:length(mymat)){
  if(!mymat[i] %in% c(".","#")){
    curPerimeter <- findRegion(i)
    curArea <- measureArea()
    mymat[mymat=="."] <- "#"
    myPrice <- myPrice + (curPerimeter * curArea)
  }
}
myPrice
#p2
## This... is actually really easy? When I check each spot to see if it's an edge (where I add perimeter+1)...
## I just have to check its two neighbors to see if they're matching edges, and subtract 1/2 for each that is.
## I guess it's a little annoing to need to match direction? But not a big deal computationally.
mymat <- matrix(nrow=nDim+2, ncol=nchar(myin[1,1])+2)#Need to remake the map, since I destroy it each time... Not great planning on my part.
for(i in 1:nDim){
  mymat[i+1,2:(nDim+1)] <- strsplit(myin[i,1],"")[[1]]
}
mymat[is.na(mymat)] <- "#"
myDirs <- c(1, -nrow(mymat), -1, nrow(mymat), 1, -nrow(mymat))# down/left/up/right/down/left
findRegionB <- function(curloc){
  curCrop <- mymat[curloc]
  mymat[curloc] <<- "."
  #mymat[curloc] <- "."
  myPer <- 0
  for(i in 2:5){#Check left/up/right/down
    if(mymat[curloc+myDirs[i]] == curCrop){
      myPer <- myPer + findRegionB(curloc+myDirs[i])
    }
    if(!mymat[curloc+myDirs[i]] %in% c(".", curCrop)){
      myPer <- myPer + 1
      myPer <- myPer - (0.5)*(sum((!mymat[c(curloc + myDirs[i+1] + myDirs[i], curloc + myDirs[i-1] + myDirs[i])] %in% c(".",curCrop)) &
                                    (mymat[c(curloc + myDirs[i+1], curloc + myDirs[i-1])] %in% c(".",curCrop))))
    }
  }
  return(myPer)
}
myPrice <- 0
for(i in 1:length(mymat)){
  if(!mymat[i] %in% c(".","#")){
    curPerimeter <- findRegionB(i)
    curArea <- measureArea()
    mymat[mymat=="."] <- "#"
    myPrice <- myPrice + (curPerimeter * curArea)
  }
}
myPrice

##D13 p1:
##So... this is just math, right? The two buttons make two slopes. If you project those slopes out of the origin and back out of the prize...
##...they should intersect at 2 points (A->B or B->A). Those are the only two valid ways to get the prize, so then it's just a matter of minimizing the path
myin <- strsplit(read_file("/Users/tfaits/Documents/Advent2024/d13.txt"),"\n")[[1]]
AbuttX <- c()
AbuttY <- c()
BbuttX <- c()
BbuttY <- c()
PrizeX <- c()
PrizeY <- c()
idx <- 0
## Y=m*X-m*PrizeX+PrizeY
## Y=n*X
## n*X = m*X - m*PrizeX + PrizeY
## (n-m)*X = - m*PrizeX + PrizeY
## X = (- m*PrizeX + PrizeY)/(n-m)
## Y = n*X
##... Or the reverse. X=(n*PrizeX+PrizeY)/(m-n), and Y=m*X.
while(idx < length(myin)){
  idx <- idx + 1
  AbuttX <- c(AbuttX, as.numeric(strsplit(strsplit(myin[idx],"X+")[[1]][2],",")[[1]][1]))
  AbuttY <- c(AbuttY, as.numeric(strsplit(myin[idx],"Y+")[[1]][2]))
  idx <- idx + 1
  BbuttX <- c(BbuttX, as.numeric(strsplit(strsplit(myin[idx],"X+")[[1]][2],",")[[1]][1]))
  BbuttY <- c(BbuttY, as.numeric(strsplit(myin[idx],"Y+")[[1]][2]))
  idx <- idx + 1
  PrizeX <- c(PrizeX, as.numeric(strsplit(strsplit(myin[idx],"X=")[[1]][2],",")[[1]][1]))
  PrizeY <- c(PrizeY, as.numeric(strsplit(myin[idx],"Y=")[[1]][2]))
  idx <- idx + 1
}
findPoint <- function(idx){
  slopeA <- AbuttY[idx]/AbuttX[idx]
  slopeB <- BbuttY[idx]/BbuttX[idx]
  if(slopeA==slopeB){
    print("PROBLEM! Same Slopes, assumptions violated, need to rethink things for this case!")
  }
  point1 <- (PrizeY[idx] - slopeB*PrizeX[idx])/(slopeA - slopeB)#X coord of first intersect possibility
  if(point1 <= PrizeX[idx] & point1 >= 0){
    cost1 <- 3*point1/AbuttX[idx] + (PrizeX[idx]-point1)/BbuttX[idx]
    pressA <- point1/AbuttX[idx]
    pressB <- (PrizeX[idx]-point1)/BbuttX[idx]
  }else{#Otherwise it's impossible
    cost1 <- 0
    pressA <- 0
    pressB <- 0
  }
  tmp <- ifelse(abs(round(pressA)-pressA) < (1/AbuttX[idx]) & abs(round(pressB)-pressB) < (1/BbuttX[idx]), round(cost1), 0)## Account for rounding errors
  return(tmp)
}
myTokens <- 0
for(idx in 1:length(AbuttX)){
  myTokens <- myTokens + findPoint(idx)
}
myTokens
##p2 should be a piece of cake.
PrizeXa <- PrizeX
PrizeYa <- PrizeY
PrizeX <- PrizeXa + 10000000000000
PrizeY <- PrizeYa + 10000000000000
myTokensB <- 0
for(idx in 1:length(AbuttX)){
  myTokensB <- myTokensB + findPoint(idx)
}
format(myTokensB, digits=nchar(myTokensB))

##D14 p1
myin <- read.table("/Users/tfaits/Documents/Advent2024/d14.txt", header=FALSE)
robots <- data.frame(first=c(0,0,0,0))
endPos <- list()
myQuads <- c(0,0,0,0)
for(i in 1:nrow(myin)){
  robots[,i] <- c(as.numeric(strsplit(myin[i,1],"=|,")[[1]][2:3]), as.numeric(strsplit(myin[i,2],"=|,")[[1]][2:3]))
  endPos[[i]] <- c((robots[1,i] + 100*robots[3,i]) %% 101,(robots[2,i] + 100*robots[4,i]) %% 103)
  if(endPos[[i]][1] < 50 & endPos[[i]][2] < 51){
    myQuads[1] <- myQuads[1] + 1
  }
  if(endPos[[i]][1] > 50 & endPos[[i]][2] < 51){
    myQuads[2] <- myQuads[2] + 1
  }
  if(endPos[[i]][1] < 50 & endPos[[i]][2] > 51){
    myQuads[3] <- myQuads[3] + 1
  }
  if(endPos[[i]][1] > 50 & endPos[[i]][2] > 51){
    myQuads[4] <- myQuads[4] + 1
  }
}
prod(myQuads)
##p2
## After some individual checking of each step, I noticed clear patterns were emerging at regular intervals.
## They showed up every n*101+71 seconds, and n*103+16 seconds. Instead of checking *everything*, I just checked those beats:
for(j in 1:2000){
  i <- j*101+72
  mymat <- matrix(nrow=103, ncol=101, data=" ")
  tmp <- data.frame(X=unlist(robots[1,] + (i-1)*robots[3,]) %% 101,Y=unlist((robots[2,] + (i-1)*robots[4,]) %% 103))
  mymat[tmp[,2] + 103*tmp[,1] + 1] <- "#"
  print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
  print(paste("After", i-1, "seconds, the robots looks like this:"))
  for(myRow in 1:nrow(mymat)){
    print(paste(mymat[myRow,], collapse=""))
  }
  print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
  readline(prompt="Press [enter] to continue")
  i <- j*103+17
  mymat <- matrix(nrow=103, ncol=101, data=" ")
  tmp <- data.frame(X=unlist(robots[1,] + (i-1)*robots[3,]) %% 101,Y=unlist((robots[2,] + (i-1)*robots[4,]) %% 103))
  mymat[tmp[,2] + 103*tmp[,1] + 1] <- "#"
  print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
  print(paste("After", i-1, "seconds, the robots looks like this:"))
  for(myRow in 1:nrow(mymat)){
    print(paste(mymat[myRow,], collapse=""))
  }
  print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
  readline(prompt="Press [enter] to continue")
}
## Unnecessary for the problem, but I also wanted to be able to check any arbitrary point
checkMap <- function(i){
  mymat <- matrix(nrow=103, ncol=101, data=" ")
  tmp <- data.frame(X=unlist(robots[1,] + (i)*robots[3,]) %% 101,Y=unlist((robots[2,] + (i)*robots[4,]) %% 103))
  mymat[tmp[,2] + 103*tmp[,1] + 1] <- "#"
  print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
  print(paste("After", i, "seconds, the robots looks like this:"))
  for(myRow in 1:nrow(mymat)){
    print(paste(mymat[myRow,], collapse=""))
  }
  print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
}
checkMap(8050)#Found by examining the outputs of the above loop

##D15 p1:










