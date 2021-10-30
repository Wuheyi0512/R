library(sqldf)
library(dplyr)
library(readr)
library(tidyverse)
library(readxl)
library(stringr)


# 设置工作区域
setwd("C:/Users/annie_wu/Desktop/思维课nps调研") 

# 表1：que问卷数据
# 表2：user用户数据

user0802 <- read_excel("C:/Users/annie_wu/Desktop/思维课nps调研/tru0802.xls")
View(user0802)
que0802 <- read_excel("C:/Users/annie_wu/Desktop/思维课nps调研/trq0802.xlsx")
View(que0802)


# 问卷发放 --------------------------------------------------------------------


# 用cut()分割序号列然后输出

# 1_加序号
user0802$id    <- seq(from=1, to=5330, by=1)                                  # to = 后面的值需要修改，=总观测数
# 2_分割成3批
user0802$batch <- cut(user0802$id,breaks = 2,right = FALSE,labels = c("A","B"))
# 3_分成3个表，形成4列：呱id、vj、当期问卷参数、呱id
user0802       <- transform(user0802,
                   var1 = "vj",
                   var2 = "QhxnEHz")                                          

attach(user0802)

fix(user0802)
batch123 <- unique(cbind(用户呱号,var1,var2,用户呱号,batch))
length(batch123[,1])

detach(user0802)

batch123 <- as.data.frame(batch123,label=true)

attach(batch123)

batch1   <- unique(cbind(用户呱号[which(batch == "1")],var1[which(batch == "1")],var2[which(batch == "1")],用户呱号[which(batch == "1")]))
batch2   <- unique(cbind(用户呱号[which(batch == "2")],var1[which(batch == "2")],var2[which(batch == "2")],用户呱号[which(batch == "2")]))
batch3   <- unique(cbind(用户呱号[which(batch == "3")],var1[which(batch == "3")],var2[which(batch == "3")],用户呱号[which(batch == "3")]))

detach(batch123)



# 4_把3表结果导出到剪贴板
write.table(batch1,file="clipboard-1024",sep="\t",col.name=FALSE,row.names=FALSE)
write.table(batch2,file="clipboard-1024",sep="\t",col.name=FALSE,row.names=FALSE)
write.table(batch3,file="clipboard-1024",sep="\t",col.name=FALSE,row.names=FALSE)

# 5_补发，挖掉已发送数据，形成新的名单
que <- read_excel("C:/Users/annie_wu/Desktop/0614que.xlsx")
rmd <- batch123[-which(batch123[,1] %in% que$V1),]
write.table(rmd,file="clipboard-1024",sep="\t",col.name=FALSE,row.names=FALSE)



# 处理用户数据 ------------------------------------------------------------------
# 1_提取 item_id 列，MATC之前的级别
trlv <- substr(sub("MATC.*","",user0802$item_id),-1,2)
trlv
user0802$trial_level <- trlv

# 2_提取 正价课思维包SKU 列，启蒙后的5位( "K3-K6(不含学习礼盒X)")，把+转化为-
fmlv <- substr(sub(".*启蒙","",user0802$正价课思维课包sku),1,5)
fmlv <- chartr("+","-",fmlv)
user0802$formal_level <- fmlv

# 3_null替换成空值
user0802[user0802 == "null"] <- NA                                             #在[]里的值为TRUE的时候赋值



# 处理问卷数据 ------------------------------------------------------------------
# 1_数据清洗
# 1.1_把 (跳过) 替换为空值
que0802[que0802 == "(跳过)"] <- NA                                             #在[]里的值为TRUE的时候赋值
# 1.2_把 。回T退订 删除
que0802$来源详情 <- str_replace(que0802$来源详情,"。回T退订","")
# 1.3_提取地址
str_split_fixed(que0802$来自IP,"-",2)[,2]                                      #提取到市，带右括号
que0802$IP       <- substr(sub(").*","",str_split_fixed(que0802$来自IP,"-",2)[,2]),-1,4)   #最后一个参数可能有变化，要看这期最长的市名是什么
# 1.4_替换一些值：极有可能推荐-10,不可能推荐-0,非常满意-10,非常不满意-1,一般-5
que0802[que0802 == "极有可能推荐"]   <- "10"
que0802[que0802 == "不可能推荐"]     <- "0"
que0802[que0802 == "非常满意"]       <- "10"
que0802[que0802 == "非常不满意"]     <- "1"
que0802[que0802 == "一般"]           <- "5"
#1.5_把文字变量转换成因子变量
que0802                              <- as.data.frame(que0802,stringAsFactors = FALSE)     #本步骤先不转因子型
que0802[,35]                         <- as.factor(que0802[,35])
que0802[,1]                          <- as.numeric(que0802[,1])
idx            <- which(names(que0802[,8:30]) %in% names(que0802))+7           #构建一个变量组，把其中变量转成因子型
              
idx                                                                            #可能要根据第一个数值变量的位置稍改，不确定
for(i in idx){
  que0802[,i]  <- as.numeric(que0802[,i])
}
#1.6_剔除答卷时间小于30的记录
que0802$所用时间 <- as.numeric(str_replace(que0802$所用时间,"秒",""))
bin              <- which(que0802$所用时间 <= 30)                              #返回的是序号
bin
que0802          <- que0802[-bin,]                                             #这一步不是必须

# 合并用户和问卷数据 ------------------------------------------------------------
# 按序号合并为X0802
X0802            <- merge(user0802,que0802,by.x="用户呱号",by.y="来源详情")
# 补充城市等级
citylevel        <- as.data.frame(read_excel("C:/Users/annie_wu/Desktop/思维课nps调研/城市线级列表.xlsx"))
trl0802          <- merge(X0802,citylevel[,c(2,3)],by.x = "IP",by.y = "city_name",all.x = TRUE)
trl0802$城市等级[which(is.na(trl0802$城市等级))]   <- trl0802$city_level[which(is.na(trl0802$城市等级))]
trl0802$numlow      <- NA
# 转换成数值
idx              <- which(names(trl0802[,21:43]) %in% names(trl0802))+20       #构建一个变量组，把其中变量转成因子型

idx                                                                            #可能要根据第一个数值变量的位置稍改，不确定
for(i in idx){
  trl0802[,i]       <- as.numeric(trl0802[,i])
}

# 输出新表
a    <- trl0802[,c(15,20,49,2,48,13,14,7,6)]
b.trls <- trl0802[,c(15,21,52,22:28,45)][trl0802[,21]<=6,][complete.cases(trl0802[,c(15,21:28)][trl0802[,21]<=6,]),]
b.trtc <- trl0802[,c(15,29,52,30:35,45)][trl0802[,29]<=6,][complete.cases(trl0802[,c(15,29:35)][trl0802[,29]<=6,]),]
b.trta <- trl0802[,c(15,36,52,37:41,45)][trl0802[,36]<=6,][complete.cases(trl0802[,c(15,36:41)][trl0802[,36]<=6,]),]
b.trfl <- trl0802[,c(15,42,45)][trl0802[,42]<=6,][complete.cases(trl0802[,c(15,42)][trl0802[,42]<=6,]),]

# 1_课程低分
trls          <- right_join(a,b.trls,by="序号")
idxtrls       <- c(1:nrow(trls))
for (i in idxtrls){
  trls[i,11]  <- length(which(trls[i,c(12:18)]<7))
}
# 2_班主任低分
trtc          <- right_join(a,b.trtc,by="序号")
idxtrtc       <- c(1:nrow(trtc))
for (i in idxtrtc){
  trtc[i,11]  <- length(which(trtc[i,c(12:17)]<7))
}
# 3_教具低分
trta          <- right_join(a,b.trta,by="序号")
idxtrta       <- c(1:nrow(trta))
for (i in idxtrta){
  trta[i,11]  <- length(which(trta[i,c(12:16)]<7))
}
# 4_流畅性低分
trfl          <- right_join(a,b.trfl,by="序号")


# 输出成符合跟踪报告的格式 ------------------------------------------------------
# 1_电访名单
write.table(trls,file="clipboard-1024",sep="\t",col.name=FALSE,row.names=FALSE)
write.table(trtc,file="clipboard-1024",sep="\t",col.name=FALSE,row.names=FALSE)
write.table(trta,file="clipboard-1024",sep="\t",col.name=FALSE,row.names=FALSE)
write.table(trfl,file="clipboard-1024",sep="\t",col.name=FALSE,row.names=FALSE)

# 2_客服反馈
fdbkfm  <- city[,c(2,46,45,49,7,48,49,41,44)][city[,41]<7,]
fdbkfm
fdbktrl <- trl0802[,c(2,49,48,13,14,7,6,43,45),][trl0802$`结合课程、服务、实体教具，您向亲朋好友推荐叽里呱啦【思维启蒙课程】的可能性有多大？`<=6,]
write.table(fdbkfm,file="clipboard-1024",sep="\t",col.name=FALSE,row.names=FALSE)
write.table(fdbktrl,file="clipboard-1024",sep="\t",col.name=FALSE,row.names=FALSE)





















