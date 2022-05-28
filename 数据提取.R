
# 载入包 ---------------------------------------------------------------------
library(Rcpp)
library(tcltk)
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
library(NLP)


jianshen_raw <- read_csv("C:/Users/15878/Desktop/jianshen_com.csv",
                     col_types = cols(gd = col_factor(levels = c()), 
                                      id = col_character(), 
                                      publish_time = col_datetime(format = "%Y/%m/%d %H:%M"),
                                      yr = col_factor(levels = c())))
jianshen <- jianshen_raw


# 文本替换 --------------------------------------------------------------------

jianshen$content <- gsub("\\p{So}|\\p{Cn}","",jianshen$content,perl = TRUE)
jianshen$content <- gsub("","",jianshen$content)
jianshen$content <- gsub("#.+?#","",jianshen$content)
jianshen$content <- gsub("【.+?】","",jianshen$content)
jianshen$content <- gsub("[.+?]","",jianshen$content)

jianshen$content <- gsub("[:digit:].+?h","小时",jianshen$content)
jianshen$content <- gsub("第.*天","第X天",jianshen$content)
jianshen$content <- gsub("第.*周","第X周",jianshen$content)
jianshen$content <- gsub("第.*次","第X次",jianshen$content)
jianshen$content <- gsub(" .+?广场","",jianshen$content)
jianshen$content <- gsub("L.+? ","",jianshen$content)
jianshen$content <- gsub("@.+? ","",jianshen$content)
jianshen$content <- gsub("2.+? ","",jianshen$content)

jianshen$content <- gsub("我分享了","",jianshen$content)
jianshen$content <- gsub("分享图片|下载客户端|分享自|分享|新浪新闻|腾讯","",jianshen$content)

jianshen$content <- gsub("O打卡社区","",jianshen$content)
jianshen$content <- gsub("超话","",jianshen$content)
jianshen$content <- gsub("vc|维C|维他命C","维生素C",jianshen$content,ignore.case = TRUE)
jianshen$content <- gsub("vb|维B|维他命B","维生素B",jianshen$content,ignore.case = TRUE)
jianshen$content <- gsub("vA|维A|维他命A","维生素A",jianshen$content,ignore.case = TRUE)
jianshen$content <- gsub("vD|维D|维他命D","维生素D",jianshen$content,ignore.case = TRUE)
jianshen$content <- gsub("vE|维E|维他命E","维生素E",jianshen$content,ignore.case = TRUE)
jianshen$content <- gsub("vE|维E|维他命E","维生素E",jianshen$content,ignore.case = TRUE)
jianshen$content <- gsub("superMonkey","",jianshen$content,ignore.case = TRUE)
jianshen$content <- gsub("塌陷","凹陷",jianshen$content)
jianshen$content <- gsub("发表了博文","",jianshen$content)
jianshen$content <- gsub("秒拍","",jianshen$content)
jianshen$content <- gsub("视频|不可辜负的美食","",jianshen$content)

jianshen$content <- gsub("没去健身房","没有健身",jianshen$content)
jianshen$content <- gsub("没运动","没有健身",jianshen$content)
jianshen$content <- gsub("没去运动","没有健身",jianshen$content)
jianshen$content <- gsub("没有健身","没健身",jianshen$content)
jianshen$content <- gsub("没去健身","没健身",jianshen$content)
jianshen$content <- gsub("没去运动","没健身",jianshen$content)
jianshen$content <- gsub("没有健身","没健身",jianshen$content)

jianshen$content <- gsub("大腿肉","大腿",jianshen$content)
jianshen$content <- gsub("大雨天","雨天",jianshen$content)
jianshen$content <- gsub("大晴天","晴天",jianshen$content)
jianshen$content <- gsub("耽搁","耽误",jianshen$content)
jianshen$content <- gsub("自制力","自控力",jianshen$content)

jianshen$content <- gsub("早上吃了","早餐",jianshen$content)
jianshen$content <- gsub("中午吃了","午餐",jianshen$content)
jianshen$content <- gsub("晚上吃了","晚餐",jianshen$content)
jianshen$content <- gsub("不好吃","难吃",jianshen$content)
jianshen$content <- gsub("掉秤","减肥",jianshen$content)
jianshen$content <- gsub("减重","减肥",jianshen$content)
jianshen$content <- gsub("不好吃","难吃",jianshen$content)
jianshen$content <- gsub("不好吃","难吃",jianshen$content)

jianshen$content <- gsub("快乐肥宅水","可乐",jianshen$content)
jianshen$content <- gsub("肥宅水","可乐",jianshen$content)
jianshen$content <- gsub("肥宅快乐水","可乐",jianshen$content)
jianshen$content <- gsub("快乐水","可乐",jianshen$content)

jianshen$content <- gsub("金拱门","麦当劳",jianshen$content)
jianshen$content <- gsub("开封菜","肯德基",jianshen$content)
jianshen$content <- gsub("快快乐乐","快乐",jianshen$content)
jianshen$content <- gsub("开开心心","开心",jianshen$content)


jianshen$content <- gsub("不想再健身","不想健身",jianshen$content)
jianshen$content <- gsub("不想再去健身","不想健身",jianshen$content)
jianshen$content <- gsub("不想来健身","不想健身",jianshen$content)
jianshen$content <- gsub("不想去健身","没健身",jianshen$content)
jianshen$content <- gsub("不想去健身房","没健身",jianshen$content)
jianshen$content <- gsub("女孩子","女孩",jianshen$content)
jianshen$content <- gsub("girl","女孩",jianshen$content)

jianshen$content <- gsub("去健身房","健身",jianshen$content)

jianshen$content <- gsub("日常碎片","生活碎片",jianshen$content)

jianshen$content <- gsub("烧饭","做饭",jianshen$content)
jianshen$content <- gsub("嘴馋","馋",jianshen$content)
jianshen$content <- gsub("嘴巴馋","馋",jianshen$content)
jianshen$content <- gsub("嘴巴馋","想吃",jianshen$content)
jianshen$content <- gsub("无氧区","力量区",jianshen$content)

jianshen$content <- gsub("炸臀","练臀",jianshen$content)
jianshen$content <- gsub("炸腹","练腹",jianshen$content)
jianshen$content <- gsub("炸肩","练肩",jianshen$content)

jianshen$content <- gsub("带劲儿","带劲",jianshen$content)
jianshen$content <- gsub("得劲儿","带劲",jianshen$content)
jianshen$content <- gsub("得劲","带劲",jianshen$content)
jianshen$content <- gsub("担忧","担心",jianshen$content)

jianshen$content <- gsub("[[:digit:]]","",jianshen$content)
jianshen$content <- gsub("[[:punct:]]","",jianshen$content)

jianshen$content <- gsub("O绿洲","",jianshen$content)
jianshen$content <- gsub("O网页链接","",jianshen$content)
jianshen$content <- gsub("收起d","",jianshen$content)

jianshen$content <- gsub("workout","健身",jianshen$content,ignore.case = TRUE)
jianshen$content <- gsub("O抽奖详情","",jianshen$content,ignore.case = TRUE)

jianshen$content <- gsub("hiit","HIIT",jianshen$content)
jianshen$content <- gsub("Hiit","HIIT",jianshen$content)
jianshen$content <- gsub("HIIT","高强度间歇训练",jianshen$content)

jianshen$content <- gsub("zumba","尊巴",jianshen$content,ignore.case = TRUE)
jianshen$content <- gsub("头肌","头",jianshen$content,ignore.case = TRUE)
jianshen$content <- gsub("二头","二头肌",jianshen$content,ignore.case = TRUE)
jianshen$content <- gsub("三头","三头肌",jianshen$content,ignore.case = TRUE)

jianshen$content <- gsub("gym","健身房",jianshen$content,ignore.case = TRUE)
jianshen$content <- gsub("man","MAN",jianshen$content,ignore.case = TRUE)
jianshen$content <- gsub("powerful","有力量",jianshen$content,ignore.case = TRUE)

jianshen$content <- gsub("适应自己","适合自己",jianshen$content)

jianshen$content <- gsub("健美操","健身操",jianshen$content)
jianshen$content <- gsub("跳操","跳健身操",jianshen$content)

jianshen$content <- gsub("tabata","TABATA",jianshen$content)
jianshen$content <- gsub("Tabata","TABATA",jianshen$content)
jianshen$content <- gsub("insanity","INSANITY",jianshen$content)

jianshen$content <- gsub("Pamela","帕梅拉",jianshen$content)
jianshen$content <- gsub("yoga","瑜伽",jianshen$content)
jianshen$content <- gsub("YOGA","瑜伽",jianshen$content)
jianshen$content <- gsub("Yoga","瑜伽",jianshen$content)
jianshen$content <- gsub("min","分钟",jianshen$content)
jianshen$content <- gsub("Min","分钟",jianshen$content)
jianshen$content <- gsub("MIN","分钟",jianshen$content)
jianshen$content <- gsub("舒胡","舒服",jianshen$content)
jianshen$content <- gsub("掉磅","掉秤",jianshen$content)
jianshen$content <- gsub("掉秤","减重",jianshen$content)
jianshen$content <- gsub("轻了","减重",jianshen$content)

jianshen$content <- gsub("好久","很久",jianshen$content)

jianshen$content <- gsub("kcal","千卡",jianshen$content)
jianshen$content <- gsub("大卡","千卡",jianshen$content)
jianshen$content <- gsub("KCAL","千卡",jianshen$content)
jianshen$content <- gsub("Kcal","千卡",jianshen$content)
jianshen$content <- gsub("Kcal","千卡",jianshen$content)
jianshen$content <- gsub("0糖","零糖",jianshen$content)
jianshen$content <- gsub("不加糖","零糖",jianshen$content)
jianshen$content <- gsub("不添加糖","零糖",jianshen$content)

jianshen$content <- gsub("0脂肪","零脂肪",jianshen$content)
jianshen$content <- gsub("0糖","零糖",jianshen$content)
jianshen$content <- gsub("0蔗糖","零蔗糖",jianshen$content)
jianshen$content <- gsub("无糖","零糖",jianshen$content)
jianshen$content <- gsub("零脂肪","零脂",jianshen$content)
jianshen$content <- gsub("脱脂","零脂",jianshen$content)

jianshen$content <- gsub("低脂肪","低脂",jianshen$content)
jianshen$content <- gsub("高脂肪","高脂",jianshen$content)
jianshen$content <- gsub("喜茶","奶茶",jianshen$content)
jianshen$content <- gsub("奈雪","奶茶",jianshen$content)
jianshen$content <- gsub("喜茶","奶茶",jianshen$content)
jianshen$content <- gsub("茶百道","奶茶",jianshen$content)
jianshen$content <- gsub("coco","奶茶",jianshen$content,ignore.case = TRUE)
jianshen$content <- gsub("高p","高P",jianshen$content,ignore.case = TRUE)
jianshen$content <- gsub("P图","P图",jianshen$content,ignore.case = TRUE)
jianshen$content <- gsub("P图","修图",jianshen$content,ignore.case = TRUE)
jianshen$content <- gsub("美颜","修图",jianshen$content,ignore.case = TRUE)


jianshen$content <- gsub("bmi","体质指数",jianshen$content,ignore.case = TRUE)

jianshen$content <- gsub("茶颜悦色","奶茶",jianshen$content)
jianshen$content <- gsub("古茗","奶茶",jianshen$content)
jianshen$content <- gsub("番薯","红薯",jianshen$content)
jianshen$content <- gsub("西红柿","番茄",jianshen$content)
jianshen$content <- gsub("马铃薯","土豆",jianshen$content)



jianshen$content <- gsub("抑郁症","抑郁",jianshen$content)
jianshen$content <- gsub("痛经","生理期疼痛",jianshen$content)

jianshen$content <- gsub("cpa","会计师考试",jianshen$content,ignore.case = TRUE)

jianshen$content <- gsub("低卡","低热量",jianshen$content)
jianshen$content <- gsub("0卡","零热量",jianshen$content)
jianshen$content <- gsub("0热量","零热量",jianshen$content)
jianshen$content <- gsub("零度可乐","零热量可乐",jianshen$content)


jianshen$content <- gsub("gi","升糖指数",jianshen$content,ignore.case = TRUE)

jianshen$content <- gsub("kg","KG",jianshen$content)
jianshen$content <- gsub("KG","KG",jianshen$content)
jianshen$content <- gsub("Kg","KG",jianshen$content)
jianshen$content <- gsub("㎏","KG",jianshen$content)
jianshen$content <- gsub("KG","公斤",jianshen$content)

jianshen$content <- gsub("㎝","厘米",jianshen$content)
jianshen$content <- gsub("cm","厘米",jianshen$content)
jianshen$content <- gsub("CM","厘米",jianshen$content)
jianshen$content <- gsub("km","公里",jianshen$content,ignore.case = TRUE)
jianshen$content <- gsub("burpee","波比跳",jianshen$content,ignore.case = TRUE)
jianshen$content <- gsub("Lbs","磅",jianshen$content)
jianshen$content <- gsub("LBS","磅",jianshen$content)
jianshen$content <- gsub("x型","X型",jianshen$content)
jianshen$content <- gsub("o型","O型",jianshen$content)
jianshen$content <- gsub("Nintendo","",jianshen$content,ignore.case = TRUE)
jianshen$content <- gsub("ootd","穿搭",jianshen$content,ignore.case = TRUE)
jianshen$content <- gsub("week","",jianshen$content,ignore.case = TRUE)
jianshen$content <- gsub("plank","平板支撑",jianshen$content,ignore.case = TRUE)
jianshen$content <- gsub("精致碳水","精制碳水",jianshen$content)
jianshen$content <- gsub("cheat","欺骗餐",jianshen$content,ignore.case = TRUE)
jianshen$content <- gsub("毕设","毕业论文",jianshen$content)
jianshen$content <- gsub("毕业设计","毕业论文",jianshen$content)
jianshen$content <- gsub("意大利面","意面",jianshen$content)
jianshen$content <- gsub("办了卡","办健身卡",jianshen$content)
jianshen$content <- gsub("办了健身卡","办健身卡",jianshen$content)
jianshen$content <- gsub("办健身卡","办健身卡",jianshen$content)

jianshen$content <- gsub("心理安慰","自我安慰",jianshen$content)
jianshen$content <- gsub("输给","败给",jianshen$content)
jianshen$content <- gsub("strong","强壮",jianshen$content,ignore.case = TRUE)
jianshen$content <- gsub("sexy","性感",jianshen$content,ignore.case = TRUE)
jianshen$content <- gsub("LBS","磅",jianshen$content,ignore.case = TRUE)
jianshen$content <- gsub("沙拉","色拉",jianshen$content)
jianshen$content <- gsub("健身环大冒险","健身环",jianshen$content)
jianshen$content <- gsub("switch","健身环",jianshen$content,ignore.case = TRUE)

jianshen$content <- gsub("月半","胖",jianshen$content)
jianshen$content <- gsub("body shame","身材羞辱",jianshen$content,ignore.case = TRUE)
jianshen$content <- gsub("体脂率","体脂",jianshen$content,ignore.case = TRUE)
jianshen$content <- gsub("yygq","阴阳怪气",jianshen$content,ignore.case = TRUE)
jianshen$content <- gsub("lululemon","",jianshen$content,ignore.case = TRUE)
jianshen$content <- gsub("lulu lemon","",jianshen$content,ignore.case = TRUE)
jianshen$content <- gsub("lulu","",jianshen$content,ignore.case = TRUE)
jianshen$content <- gsub("emo","衣莫",jianshen$content,ignore.case = TRUE)

jianshen$content <- gsub("转变","变化",jianshen$content,ignore.case = TRUE)
jianshen$content <- gsub("屁股","臀部",jianshen$content)
jianshen$content <- gsub("pp","臀部",jianshen$content,ignore.case = TRUE)
jianshen$content <- gsub("臀部肌肉","臀肌",jianshen$content,ignore.case = TRUE)

jianshen$content <- gsub("雪糕","冰淇淋",jianshen$content)
jianshen$content <- gsub("冰激凌","冰淇淋",jianshen$content)
jianshen$content <- gsub("冰棒","冰淇淋",jianshen$content)
jianshen$content <- gsub("冰激淋","冰淇淋",jianshen$content)
jianshen$content <- gsub("冰淇淋红茶","奶茶",jianshen$content)
jianshen$content <- gsub("黑咖啡","美式",jianshen$content)
jianshen$content <- gsub("甜菊糖","代糖",jianshen$content)
jianshen$content <- gsub("阿斯巴甜","代糖",jianshen$content)
jianshen$content <- gsub("糖醇","代糖",jianshen$content)

jianshen$content <- gsub("红茶拿铁","奶茶",jianshen$content)
jianshen$content <- gsub("绿茶拿铁","奶茶",jianshen$content)
jianshen$content <- gsub("乌龙拿铁","奶茶",jianshen$content)
jianshen$content <- gsub("茉莉拿铁","奶茶",jianshen$content)
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
jianshen$content <- gsub("大姨妈","例假",jianshen$content)
jianshen$content <- gsub("姨妈","例假",jianshen$content)
jianshen$content <- gsub("月经","例假",jianshen$content)
jianshen$content <- gsub("麦麦","麦当劳",jianshen$content)
jianshen$content <- gsub("kfc","肯德基",jianshen$content,ignore.case = TRUE)
jianshen$content <- gsub("k记","肯德基",jianshen$content,ignore.case = TRUE)
jianshen$content <- gsub("m记","麦当劳",jianshen$content,ignore.case = TRUE)
jianshen$content <- gsub("自控","自我控制",jianshen$content,ignore.case = TRUE)

jianshen$content <- gsub("经期","例假",jianshen$content)
jianshen$content <- gsub("例假","生理期",jianshen$content)
jianshen$content <- gsub("我妈","妈妈",jianshen$content)
jianshen$content <- gsub("老妈","妈妈",jianshen$content)
jianshen$content <- gsub("母亲","妈妈",jianshen$content)


jianshen$content <- gsub("早饭","早餐",jianshen$content)
jianshen$content <- gsub("午饭","午餐",jianshen$content)
jianshen$content <- gsub("中饭","午餐",jianshen$content)
jianshen$content <- gsub("晚饭","晚餐",jianshen$content)
jianshen$content <- gsub("负罪感","罪恶感",jianshen$content)
jianshen$content <- gsub("罪恶感","罪恶",jianshen$content)
jianshen$content <- gsub("罪过罪过","罪恶",jianshen$content)
jianshen$content <- gsub("罪恶","罪恶",jianshen$content)

jianshen$content <- gsub("筋肉","瘦肉",jianshen$content)
jianshen$content <- gsub("精肉","瘦肉",jianshen$content)


# 删除空值 --------------------------------------------------------------------
jianshen[,4][jianshen$content == "",] <- NA
jianshen[,6][jianshen$gd == "NA",] <- NA
jianshen <- na.omit(jianshen)

jianshen[,1][jianshen$user_name == "一汀zoe" & jianshen$yr == "2018",] <- NA
jianshen[,1][jianshen$user_name == "每日沧州" & jianshen$yr == "2018",] <- NA

jianshen <- na.omit(jianshen)


xtabs(~yr+gd,data = jianshen)

js_f <- jianshen[jianshen$gd == "f",]

js_f <- js_f %>% rename(x=content)

write_excel_csv(js_f,file = "C:/Users/15878/Desktop/jscom.csv",
                append = FALSE)


# tfidf -------------------------------------------------------------------
#建立语料库，一行一个list
splt <- worker(type = "mp",user = "C:/Users/15878/Desktop/topic/userdict.txt",
               user_weight =  "max")
f15 <- js_f[js_f$yr == "2015",]
f18 <- js_f[js_f$yr == "2018",]
f21 <- js_f[js_f$yr == "2021",]

f15 %>%
  mutate(words = map(x,segment,jieba = splt)) %>%
  select(id,words) -> corpus

corpus %>%
  unnest(cols = c(id,words)) %>%
  count(id,words)  -> n_table

n_table %>%
  bind_tf_idf(term = words,document = id,n = n) -> tf_idf_table

write_excel_csv(tf_idf_table,file = "C:/Users/15878/Desktop/topic/tfidf.csv",append = FALSE)

tf_idf_table %>% 
  group_by(id) %>% 
  top_n(5,tf_idf) %>% 
  ungroup() -> top3

top3 %>% 
  count(words) %>%
  top_n(100) %>%    #只显示出现次数最多的200个关键词
  wordcloud2(size = 2, fontFamily = "微软雅黑",
             color = "random-light", backgroundColor = "white")


# 正文数据 --------------------------------------------------------------------

# 分年份导出 -------------------------------------------------------------------
jscom15 <- js_f[js_f$yr == "2015",]
jscom18 <- js_f[js_f$yr == "2018",]
jscom21 <- js_f[js_f$yr == "2021",]


write_excel_csv(jscom15,file = "C:/Users/15878/Desktop/jscom15.csv",
                append = FALSE)
write_excel_csv(jscom18,file = "C:/Users/15878/Desktop/jscom18.csv",
                append = FALSE)
write_excel_csv(jscom21,file = "C:/Users/15878/Desktop/jscom21.csv",
                append = FALSE)

# js_btf  美的认知：关键词——美、好看、漂亮、完美、理想、目标身材、想变成 ----------------------------------------------

js_btf <- dplyr::filter(js_f,grepl("美|好看|漂亮|目标|拥有|想变|想要|身材",content))

# 注意文件名！！！
write.csv(js_btf,file = "C:/Users/15878/Desktop/btf15.csv",
          quote = FALSE,
          row.names = FALSE)


# body part----------------------------------------------

js_pt1 <- dplyr::filter(js_f,grepl("臀|腿|屁股|胯",x))
xtabs(~yr,data = js_pt1)

js_pt2 <- dplyr::filter(js_f,grepl("腰|腹|人鱼线|马甲线|川字",x))
xtabs(~yr,data = js_pt2)

js_pt3 <- dplyr::filter(js_f,grepl("背|肩|臂|二头|三头|三角|拜拜肉|蝴蝶袖",x))
xtabs(~yr,data = js_pt3)

write_excel_csv(js_pt1[js_pt1$yr == "2018",],
                file = "C:/Users/15878/Desktop/topic/bodypart/tuntui.csv",
                append = FALSE)

write_excel_csv(js_pt2,file = "C:/Users/15878/Desktop/topic/bodypart/yaofu.csv",
                append = FALSE)

write_excel_csv(js_pt3,file = "C:/Users/15878/Desktop/topic/bodypart/shangzhi.csv",
                append = FALSE)


# 分词和词频 -------------------------------------------------------------------
splt <- worker(type = "mp",user = "C:/Users/15878/Desktop/topic/userdict.txt",
               stop_word = "C:/Users/15878/Desktop/stopword.txt",
               user_weight =  "max")
jscom15 <- js_f[js_f$yr == "2015",]
jscom18 <- js_f[js_f$yr == "2018",]
jscom21 <- js_f[js_f$yr == "2021",]

sp15 <- splt[jscom15$x]
freq <- freq(sp15) #输出词频表

sp18 <- splt[jscom18$x]
freq1 <- freq(sp18)

sp21 <- splt[jscom21$x]
freq2 <- freq(sp21)

spjs <- splt[js_f$x[js_f$yr == "2015"]]
freq <- jiebaR::freq(spjs) #输出词频表
freq <- freq[order(freq1$freq,decreasing = TRUE),]



# 变化情况 --------------------------------------------------------------------
js_wt <- dplyr::filter(js_f,grepl("减肥/减脂/胖/瘦/减重/掉秤/轻了/重了/增肌/臀围/腰围/腿围/臂围/围度",x))


# eating ------------------------------------------------------------------
js_et <- dplyr::filter(js_f,grepl("吃|喝|饮|食|摄入|餐|饭",x))

xtabs(~yr,data = js_et)

write_excel_csv(js_et[js_et$yr == "2018",],file = "C:/Users/15878/Desktop/topic/bodypart/et.csv",
                append = FALSE)

#词频
nutri <- worker(type = "mp",user = "C:/Users/15878/Desktop/topic/nutri.txt",
               stop_word = "C:/Users/15878/Desktop/stopword.txt",
               user_weight =  "max")
et <- nutri[js_et$x[js_et$yr == "2021"]]
freq <- jiebaR::freq(et) #输出词频表
freq <- freq[order(freq$freq,decreasing = TRUE),]



# 罪恶感叙事 -------------------------------------------------------------------

js_sin <- dplyr::filter(js_f,grepl("罪|控制|放纵|破戒|忍不住|报复性",x))



# julei -------------------------------------------------------------------

fit4 <- kmeans(mydata, 5)
clusplot(mydata, fit4$cluster, color=TRUE, shade=TRUE,labels=5, lines=0)




# senti ---------------------------------------------------------------------
pos <- senti[senti$情感三类2 == "积极",]
neg <- senti[senti$情感三类2 == "消极",]

sppos <- splt[pos$x]
frpos <- freq(sppos)

spneg <- splt[neg$x]
frneg <- freq(spneg)
