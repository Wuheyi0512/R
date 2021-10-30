library(readxl)
ennps <- read_excel("C:/Users/annie_wu/Desktop/nps/英语nps1011.xls")
View(ennps)
table(ennps$规划师)


#思维体验
trial <- read_excel("C:/Users/annie_wu/Desktop/nps/思维体验nps0927+1004.xls")
nrow(trial)
idtr <- trial$用户呱号
length(idtr)
batchtr <- as.data.frame(cbind(
  idtr,"vj","my9K5D1",idtr
)) 

#英语正价
formal <- read_excel("C:/Users/annie_wu/Desktop/nps/思维正价nps1011.xls")
nrow(formal)
idfm <- formal$用户呱号
length(idfm)
batchfm <- as.data.frame(cbind(
  idfm,"vj","OhPuVxe",idfm
))

#A—jq/101918105
idA <- ennps$呱号[ennps$规划师 == "A"]
length(idA)
batchA <- as.data.frame(cbind(idA,"jq","101918105",idA))

#C—jq/101927142
idc <- ennps$呱号[ennps$规划师 == "C"]
length(idc)
batchC <- as.data.frame(cbind(idc,"jq","101927142",idc))

#D—vj/YTw5yeo
idd <- ennps$呱号[ennps$规划师 == "D"]
length(idd)
batchD <- as.data.frame(cbind(idd,"vj","YTw5yeo",idd))

write.table(batchtr,file = "clipboard-1024",sep="\t",col.name=FALSE,row.names=FALSE)
write.table(batchfm,file = "clipboard-1024",sep="\t",col.name=FALSE,row.names=FALSE)
write.table(batchA,file = "clipboard-1024",sep="\t",col.name=FALSE,row.names=FALSE)
write.table(batchC,file = "clipboard-1024",sep="\t",col.name=FALSE,row.names=FALSE)
write.table(batchD,file = "clipboard-1024",sep="\t",col.name=FALSE,row.names=FALSE)

colnames(batchfm) <- c("para1","para2","para3","para4")
colnames(batchtr) <- c("para1","para2","para3","para4")
colnames(batchA) <- c("para1","para2","para3","para4")
colnames(batchC) <- c("para1","para2","para3","para4")
colnames(batchD) <- c("para1","para2","para3","para4")

inte <- rbind(batchtr,batchfm,batchA,batchC,batchD)
table(inte$para3)

write.table(inte,file = "clipboard-1024",sep="\t",col.name=FALSE,row.names=FALSE)

