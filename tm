library(readr)
library(jiebaRD)
library(jiebaR)
library(dplyr)
library(tidyverse)
library(lubridate) #日期处理pkg
library(tidyr)
library(tmcn)
library(tidytext)
library(pacman)
library(tm)
library(wordcloud2)
library(ggplot2)

setwd("C:/Users/15878/Desktop/topic")
#import dtset
path <- "C:/Users/15878/Desktop/topic"
xlist <- list.files(path = path, pattern = "jianshen2021.csv")
xlist
jianshen <- map_dfr(xlist,read_csv)


# process -----------------------------------------------------------------



# 1-数据清理 --------------------------------------------------------------------

#保留所需字段
jianshen <- jianshen[,1:6]
jianshen <- jianshen[-which(jianshen$mid=="mid"),]

#按mid去重

#删除内容完全相同的wb (content in col4)
jianshen <- jianshen %>% distinct(content, .keep_all = TRUE)

#删除带有特定关键词的wb，如明星
jianshen <- jianshen %>% filter(!grepl('品牌',content))
jianshen <- jianshen %>% filter(!grepl('店$',user_name))
jianshen <- jianshen %>% filter(!grepl('公司',user_name))
jianshen <- jianshen %>% filter(!grepl('集团',user_name))
jianshen <- jianshen %>% filter(!grepl('连锁',user_name))
jianshen <- jianshen %>% filter(!grepl('网$',user_name))
jianshen <- jianshen %>% filter(!grepl('芭莎',user_name))
jianshen <- jianshen %>% filter(!grepl('网$',user_name))
jianshen <- jianshen %>% filter(!grepl('品牌',user_name))
jianshen <- jianshen %>% filter(!grepl('阚',user_name))
jianshen <- jianshen %>% filter(!grepl('租',user_name))
jianshen <- jianshen %>% filter(!grepl('牙齿',user_name))
jianshen <- jianshen %>% filter(!grepl('置业',user_name))
jianshen <- jianshen %>% filter(!grepl('卫浴',content))
jianshen <- jianshen %>% filter(!grepl('网页链接',content))
jianshen <- jianshen %>% filter(!grepl('网页',content))
jianshen <- jianshen %>% filter(!grepl('健康杯',content))
jianshen <- jianshen %>% filter(!grepl('代言',content))
jianshen <- jianshen %>% filter(!grepl('￥',content))
jianshen <- jianshen %>% filter(!grepl('隆胸',content))
jianshen <- jianshen %>% filter(!grepl('户型',content))
jianshen <- jianshen %>% filter(!grepl('开发商',content))
jianshen <- jianshen %>% filter(!grepl('民警',content))
jianshen <- jianshen %>% filter(!grepl('长歌行',content))
jianshen <- jianshen %>% filter(!grepl('任嘉伦',content))
jianshen <- jianshen %>% filter(!grepl('刘宇',content))
jianshen <- jianshen %>% filter(!grepl('职位描述',content))
jianshen <- jianshen %>% filter(!grepl('a股',content))
jianshen <- jianshen %>% filter(!grepl('A股',content))
jianshen <- jianshen %>% filter(!grepl('协同发展',content))
jianshen <- jianshen %>% filter(!grepl('正畸',content))
jianshen <- jianshen %>% filter(!grepl('填充',content))
jianshen <- jianshen %>% filter(!grepl('微博视频',content)) #待定
jianshen <- jianshen %>% filter(!grepl('正畸日记',content))
jianshen <- jianshen %>% filter(!grepl('转租',content))
jianshen <- jianshen %>% filter(!grepl('杜淳',content))
jianshen <- jianshen %>% filter(!grepl('租房',user_name))
jianshen <- jianshen %>% filter(!grepl('林陌',content))
jianshen <- jianshen %>% filter(!grepl('邓伦',content))
jianshen <- jianshen %>% filter(!grepl('李现',content))
jianshen <- jianshen %>% filter(!grepl('生图',content))
jianshen <- jianshen %>% filter(!grepl('辛芷蕾',content))
jianshen <- jianshen %>% filter(!grepl('黄晓明',content))

jianshen <- jianshen %>% filter(!grepl('朱一龙',content))
jianshen <- jianshen %>% filter(!grepl('王子异',content))
jianshen <- jianshen %>% filter(!grepl('丁程鑫',content))
jianshen <- jianshen %>% filter(!grepl('蔡徐坤',content))
jianshen <- jianshen %>% filter(!grepl('刘耀元',content))

jianshen <- jianshen %>% filter(!grepl('王一博',content))
jianshen <- jianshen %>% filter(!grepl('夏之光',content))
jianshen <- jianshen %>% filter(!grepl('肖战',content))
jianshen <- jianshen %>% filter(!grepl('暴走的萝莉',content))
jianshen <- jianshen %>% filter(!grepl('沈腾',content))
jianshen <- jianshen %>% filter(!grepl('关晓彤',content))
jianshen <- jianshen %>% filter(!grepl('雨花区',content))
jianshen <- jianshen %>% filter(!grepl('券后',content))
jianshen <- jianshen %>% filter(!grepl('优惠券',content))
jianshen <- jianshen %>% filter(!grepl('私房照',content))
jianshen <- jianshen %>% filter(!grepl('抖音直播',content))
jianshen <- jianshen %>% filter(!grepl('招聘',content))
jianshen <- jianshen %>% filter(!grepl('以上学历',content))

jianshen <- jianshen %>% filter(!grepl('郑希怡',content))
jianshen <- jianshen %>% filter(!grepl('华晨宇',content))
jianshen <- jianshen %>% filter(!grepl('刘耀元',content))
jianshen <- jianshen %>% filter(!grepl('2022',date))
jianshen <- jianshen %>% filter(!grepl('畊',content))

jianshen <- jianshen %>% filter(!grepl('媒婆',user_name))
jianshen <- jianshen %>% filter(!grepl('月老',user_name))
jianshen <- jianshen %>% filter(!grepl('交友',user_name))

jianshen <- jianshen %>% filter(!grepl('留学生',user_name))
jianshen <- jianshen %>% filter(!grepl('公寓',user_name))
jianshen <- jianshen %>% filter(!grepl('租',user_name))
jianshen <- jianshen %>% filter(!grepl('身边事$',user_name))
jianshen <- jianshen %>% filter(!grepl('报$',user_name))
jianshen <- jianshen %>% filter(!grepl('发布$',user_name))
jianshen <- jianshen %>% filter(!grepl('新闻$',user_name))
jianshen <- jianshen %>% filter(!grepl('家属索赔',content))
jianshen <- jianshen %>% filter(!grepl('猝死#$',content))

jianshen <- jianshen %>% filter(!grepl('委$',user_name))
jianshen <- jianshen %>% filter(!grepl('局$',user_name))
jianshen <- jianshen %>% filter(!grepl('共青团$',user_name))



#日期转化为年份
jianshen <- separate(jianshen,publish_time,
                     into = c("date","time"),
                     sep = " ")
jianshen <- jianshen %>% filter(!grepl('$2022',date))
jianshen$date <- ymd(jianshen$date)
class(jianshen$date)
jianshen <- jianshen[,-3]

#简繁
Encoding(jianshen$content)
jianshen$content <- toTrad(jianshen$content,rev = T)

#文本替换
jianshen$content <- gsub("hiit","HIIT",jianshen$content)
jianshen$content <- gsub("Hiit","HIIT",jianshen$content)
jianshen$content <- gsub("HIIT","高强度健身训练",jianshen$content)
jianshen$content <- gsub("健美操","健身操",jianshen$content)
jianshen$content <- gsub("跳操","跳健身操",jianshen$content)

jianshen$content <- gsub("tabata","TABATA",jianshen$content)
jianshen$content <- gsub("Tabata","TABATA",jianshen$content)
jianshen$content <- gsub("TABATA","踏巴塔",jianshen$content)

jianshen$content <- gsub("Pamela","帕梅拉",jianshen$content)
jianshen$content <- gsub("yoga","瑜伽",jianshen$content)
jianshen$content <- gsub("YOGA","瑜伽",jianshen$content)
jianshen$content <- gsub("Yoga","瑜伽",jianshen$content)
jianshen$content <- gsub("min","分钟",jianshen$content)
jianshen$content <- gsub("Min","分钟",jianshen$content)
jianshen$content <- gsub("MIN","分钟",jianshen$content)
jianshen$content <- gsub("舒胡","舒服",jianshen$content)

jianshen$content <- gsub("kcal","千卡",jianshen$content)
jianshen$content <- gsub("大卡","千卡",jianshen$content)
jianshen$content <- gsub("KCAL","千卡",jianshen$content)
jianshen$content <- gsub("Kcal","千卡",jianshen$content)
jianshen$content <- gsub("Kcal","千卡",jianshen$content)
jianshen$content <- gsub("0糖","零糖",jianshen$content)
jianshen$content <- gsub("0脂肪","零脂肪",jianshen$content)
jianshen$content <- gsub("0糖","零糖",jianshen$content)
jianshen$content <- gsub("0蔗糖","零蔗糖",jianshen$content)
jianshen$content <- gsub("无糖","零糖",jianshen$content)
jianshen$content <- gsub("零脂肪","零脂",jianshen$content)
jianshen$content <- gsub("脱脂","零脂",jianshen$content)

jianshen$content <- gsub("民警辅警","辅警",jianshen$content)

jianshen$content <- gsub("低脂肪","低脂",jianshen$content)
jianshen$content <- gsub("高脂肪","高脂",jianshen$content)
jianshen$content <- gsub("喜茶","奶茶",jianshen$content)
jianshen$content <- gsub("奈雪","奶茶",jianshen$content)
jianshen$content <- gsub("喜茶","奶茶",jianshen$content)
jianshen$content <- gsub("茶百道","奶茶",jianshen$content)
jianshen$content <- gsub("coco","奶茶",jianshen$content,ignore.case = TRUE)
jianshen$content <- gsub("高p","高P",jianshen$content,ignore.case = TRUE)
jianshen$content <- gsub("P图","P图",jianshen$content,ignore.case = TRUE)


jianshen$content <- gsub("bmi","体质指数",jianshen$content,ignore.case = TRUE)

jianshen$content <- gsub("茶颜悦色","奶茶",jianshen$content)
jianshen$content <- gsub("古茗","奶茶",jianshen$content)
jianshen$content <- gsub("番薯","红薯",jianshen$content)
jianshen$content <- gsub("西红柿","番茄",jianshen$content)
jianshen$content <- gsub("马铃薯","土豆",jianshen$content)



jianshen$content <- gsub("抑郁症","抑郁",jianshen$content)
jianshen$content <- gsub("cpa","会计师考试",jianshen$content,ignore.case = TRUE)

jianshen$content <- gsub("低卡","低热量",jianshen$content)
jianshen$content <- gsub("0卡","零热量",jianshen$content)
jianshen$content <- gsub("0热量","零热量",jianshen$content)
jianshen$content <- gsub("零度可乐","零热量可乐",jianshen$content)


jianshen$content <- gsub("体态大师","",jianshen$content)

jianshen$content <- gsub("gi","GI",jianshen$content)
jianshen$content <- gsub("Gi","GI",jianshen$content)
jianshen$content <- gsub("kg","公斤",jianshen$content)
jianshen$content <- gsub("KG","公斤",jianshen$content)
jianshen$content <- gsub("Kg","公斤",jianshen$content)
jianshen$content <- gsub("㎏","公斤",jianshen$content)
jianshen$content <- gsub("㎝","厘米",jianshen$content)
jianshen$content <- gsub("cm","厘米",jianshen$content)
jianshen$content <- gsub("CM","厘米",jianshen$content)
jianshen$content <- gsub("km","公里",jianshen$content,ignore.case = TRUE)
jianshen$content <- gsub("burpee","波比跳",jianshen$content,ignore.case = TRUE)
jianshen$content <- gsub("Lbs","磅",jianshen$content)
jianshen$content <- gsub("LBS","磅",jianshen$content)
jianshen$content <- gsub("x型","X型",jianshen$content)
jianshen$content <- gsub("o型","O型",jianshen$content)
jianshen$content <- gsub("xo","XO",jianshen$content)
jianshen$content <- gsub("x/o","XO",jianshen$content)
jianshen$content <- gsub("800m","八百米",jianshen$content)
jianshen$content <- gsub("800M","八百米",jianshen$content)
jianshen$content <- gsub("800米","八百米",jianshen$content)
jianshen$content <- gsub("switch","switch",jianshen$content,ignore.case = TRUE)
jianshen$content <- gsub("Nintendo","",jianshen$content,ignore.case = TRUE)
jianshen$content <- gsub("ootd","穿搭",jianshen$content,ignore.case = TRUE)
jianshen$content <- gsub("week","",jianshen$content,ignore.case = TRUE)
jianshen$content <- gsub("plank","平板支撑",jianshen$content,ignore.case = TRUE)
jianshen$content <- gsub("精致碳水","精制碳水",jianshen$content)
jianshen$content <- gsub("cheat","欺骗餐",jianshen$content,ignore.case = TRUE)
jianshen$content <- gsub("毕设","毕业论文",jianshen$content)
jianshen$content <- gsub("毕业设计","毕业论文",jianshen$content)
jianshen$content <- gsub("意大利面","意面",jianshen$content)

jianshen$content <- gsub("心理安慰","自我安慰",jianshen$content)
jianshen$content <- gsub("输给","败给",jianshen$content)
jianshen$content <- gsub("strong","强壮",jianshen$content,ignore.case = TRUE)
jianshen$content <- gsub("sexy","性感",jianshen$content,ignore.case = TRUE)
jianshen$content <- gsub("LBS","磅",jianshen$content,ignore.case = TRUE)
jianshen$content <- gsub("沙拉","色拉",jianshen$content)
jianshen$content <- gsub("健身环大冒险","健身环",jianshen$content)
jianshen$content <- gsub("月半","胖",jianshen$content)
jianshen$content <- gsub("keep","KEEP",jianshen$content,ignore.case = TRUE)
jianshen$content <- gsub("body shame","身材羞辱",jianshen$content,ignore.case = TRUE)
jianshen$content <- gsub("体脂率","体脂",jianshen$content,ignore.case = TRUE)
jianshen$content <- gsub("yygq","阴阳怪气",jianshen$content,ignore.case = TRUE)
jianshen$content <- gsub("lululemon","",jianshen$content,ignore.case = TRUE)
jianshen$content <- gsub("lulu lemon","",jianshen$content,ignore.case = TRUE)
jianshen$content <- gsub("lulu","",jianshen$content,ignore.case = TRUE)
jianshen$content <- gsub("emo","裔某",jianshen$content,ignore.case = TRUE)

jianshen$content <- gsub("转变","变化",jianshen$content,ignore.case = TRUE)
jianshen$content <- gsub("屁股","臀部",jianshen$content)
jianshen$content <- gsub("pp","臀部",jianshen$content,ignore.case = TRUE)
jianshen$content <- gsub("臀部肌肉","臀肌",jianshen$content,ignore.case = TRUE)

jianshen$content <- gsub("雪糕","冰淇淋",jianshen$content)
jianshen$content <- gsub("冰激凌","冰淇淋",jianshen$content)
jianshen$content <- gsub("冰棒","冰淇淋",jianshen$content)
jianshen$content <- gsub("冰激淋","冰淇淋",jianshen$content)
jianshen$content <- gsub("冰淇淋红茶","奶茶",jianshen$content)
jianshen$content <- gsub("红茶拿铁","茶拿铁",jianshen$content)
jianshen$content <- gsub("绿茶拿铁","茶拿铁",jianshen$content)
jianshen$content <- gsub("乌龙拿铁","茶拿铁",jianshen$content)
jianshen$content <- gsub("茉莉拿铁","茶拿铁",jianshen$content)
jianshen$content <- gsub("芝芝","甜食",jianshen$content)

jianshen$content <- gsub("奶霜","奶盖",jianshen$content)
jianshen$content <- gsub("指点","指引",jianshen$content)

jianshen$content <- gsub("养身","养生",jianshen$content)
jianshen$content <- gsub("芝心","芝士",jianshen$content)
jianshen$content <- gsub("奶酪","芝士",jianshen$content)
jianshen$content <- gsub("cheese","芝士",jianshen$content,ignore.case = TRUE)
jianshen$content <- gsub("burger","汉堡",jianshen$content,ignore.case = TRUE)
jianshen$content <- gsub("热狗","汉堡",jianshen$content)

jianshen$content <- gsub("周日","周末",jianshen$content)
jianshen$content <- gsub("周天","周末",jianshen$content)
jianshen$content <- gsub("礼拜天","周末",jianshen$content)
jianshen$content <- gsub("礼拜六","周末",jianshen$content)
jianshen$content <- gsub("周六","周末",jianshen$content)
jianshen$content <- gsub("拉屎","大便",jianshen$content)

jianshen$content <- gsub("甜品","甜食",jianshen$content)
jianshen$content <- gsub("甜点","甜食",jianshen$content)
jianshen$content <- gsub("燕麦","麦片",jianshen$content)
jianshen$content <- gsub("燕麦片","麦片",jianshen$content)
jianshen$content <- gsub("姨妈","例假",jianshen$content)
jianshen$content <- gsub("月经","例假",jianshen$content)
jianshen$content <- gsub("经期","例假",jianshen$content)


jianshen$content <- gsub("早饭","早餐",jianshen$content)
jianshen$content <- gsub("午饭","午餐",jianshen$content)
jianshen$content <- gsub("晚饭","晚餐",jianshen$content)
jianshen$content <- gsub("负罪感","罪恶感",jianshen$content)
jianshen$content <- gsub("罪恶感","罪恶",jianshen$content)
jianshen$content <- gsub("罪过罪过","罪恶",jianshen$content)
jianshen$content <- gsub("罪恶","罪恶",jianshen$content)

jianshen$content <- gsub("筋肉","瘦肉",jianshen$content)
jianshen$content <- gsub("精肉","瘦肉",jianshen$content)



# 2-分词 ----------------------------------------------------------------------
#混合模型(MixSegment):是四个分词引擎里面分词效果较好的类，结它合使用最大概率法和隐式马尔科夫模型。
#最大概率法(MPSegment) :负责根据Trie树构建有向无环图和进行动态规划算法，是分词算法的核心。
#隐式马尔科夫模型(HMMSegment):是根据基于人民日报等语料库构建的HMM模型来进行分词，主要算法思路是根据(B,E,M,S)四个状态来代表每个字的隐藏状态。 HMM模型由dict/hmm_model.utf8提供。分词算法即viterbi算法。
#索引模型(QuerySegment):先使用混合模型进行切词，再对于切出来的较长的词，枚举句子中所有可能成词的情况，找出词库里存在。
#标记模型(tag)
#Simhash模型(simhash)
#关键词模型(keywods)
splt <- worker(type = "mp",user = "C:/Users/15878/Desktop/topic/userdict.txt",
               stop_word = "C:/Users/15878/Desktop/stopword.txt")

raw_words <- segment(raw_words,splt)

freq <- jiebaR::freq(raw_words)


wds <- lapply(jianshen$content,segment,splt)
vocab <- wds %>% as.character %>% freq()

write.csv(freq,file="C:/Users/15878/Desktop/freq.csv",append = FALSE)



#语料库
write.table(jianshen$content,file="C:/Users/15878/Documents/R/win-library/4.0/tm/Corpus/ctt.txt",append = FALSE)
cps <- Corpus(DirSource("C:/Users/15878/Documents/R/win-library/4.0/tm/Corpus"))

inspect(cps)
#cleaning
require(tm)
cps <- tm_map(cps,stripWhitespace)
cnt <- tm_map(cps,removePunctuation)
cnt <- tm_map(cps,removeWords,stopwords("english")) #??
cnt <- tm_map(jianshen$content,removeNumbers)





# 文本降维 --------------------------------------------------------------------


#建立语料库，一行一个list
jianshen$id = 1:nrow(jianshen)
write.csv(jianshen[,c(7,4,5)],"C:/Users/15878/Desktop/genderpls.csv")

jianshen %>%
  mutate(words = map(jianshen$content,segment,jieba = splt)) %>%
  select(id,words) -> corpus
corpus #corpus的words是个list
class(corpus$words)

cps <- corpus$words

cps <- Corpus(VectorSource(cps))
dtm <- DocumentTermMatrix(cps,
                          control = list(wordLengths=c(7, Inf),
                                         bounds = list(global = c(1,Inf)),
                                         weighting = weightTf,
                                         encoding = "UTF-8"))

#


#tf-idf
corpus %>%
  unnest(cols = c(mid,words)) %>%
  count(mid,words)  -> n_table
n_table #mid+词+词频

n_table %>%
  bind_tf_idf(term = words,document = mid,n = n) -> tf_idf_table
tf_idf_table %>% 
  group_by(mid) %>% 
  top_n(5,tf_idf) %>% 
  ungroup() -> top5
 
top3 %>% 
  count(words) %>%
  top_n(200) %>%    #只显示出现次数最多的200个关键词
  wordcloud2(size = 2, fontFamily = "微软雅黑",
             color = "random-light", backgroundColor = "grey")
#PAM算法聚类
