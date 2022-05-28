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


# 数据导入 --------------------------------------------------------------------

#import dtset
jianshen_raw <- read_csv("C:/Users/15878/Desktop/topic/jianshen2018.csv",
                         col_types = cols(mid = col_double(),
                                          publish_time = col_datetime(format = "%Y/%m/%d %H:%M")))

# 数据清理 --------------------------------------------------------------------

#保留所需字段
jianshen <- jianshen_raw[,c(2,3,4,5)]
jianshen$id <- 1:nrow(jianshen)
#简繁
Encoding(jianshen$content)
jianshen$content <- toTrad(jianshen$content,rev = T)

#删除内容完全相同的wb (content in col4)
jianshen <- jianshen %>% distinct(content, .keep_all = TRUE)

#匹配uid和性别(gd)
js_raw <- read_csv("C:/Users/15878/Desktop/topic/js18.csv",
                   col_types = cols(gd = col_factor(levels = c())))

js <- js_raw %>% distinct(uid, .keep_all = TRUE)

jianshen <- merge(jianshen,js,by="uid",all.x=TRUE)


# 删关键词微博 ------------------------------------------------------------------

#删除带有特定关键词的wb，如明星
jianshen <- jianshen %>% filter(!grepl('品牌',content))
jianshen <- jianshen %>% filter(!grepl('宣传月',content))
jianshen <- jianshen %>% filter(!grepl('宣传周',content))
jianshen <- jianshen %>% filter(!grepl('全覆盖',content))
jianshen <- jianshen %>% filter(!grepl('热瑜伽',content))
jianshen <- jianshen %>% filter(!grepl('宅男女神',content))
jianshen <- jianshen %>% filter(!grepl('好物推荐',content))
jianshen <- jianshen %>% filter(!grepl('冷笑话',content))

jianshen <- jianshen %>% filter(!grepl('配备',content))
jianshen <- jianshen %>% filter(!grepl('赵丽颖',content))
jianshen <- jianshen %>% filter(!grepl('冷门专业',content))
jianshen <- jianshen %>% filter(!grepl('广大市民',content))
jianshen <- jianshen %>% filter(!grepl('主办',content))
jianshen <- jianshen %>% filter(!grepl('景房',content))
jianshen <- jianshen %>% filter(!grepl('景区',content))
jianshen <- jianshen %>% filter(!grepl('课程',content))
jianshen <- jianshen %>% filter(!grepl('扫码',content))
jianshen <- jianshen %>% filter(!grepl('星译',content))
jianshen <- jianshen %>% filter(!grepl('转让',content))
jianshen <- jianshen %>% filter(!grepl('酒店',content))
jianshen <- jianshen %>% filter(!grepl('三天情侣',content))
jianshen <- jianshen %>% filter(!grepl('健身器材',content))
jianshen <- jianshen %>% filter(!grepl('檀健次',content))
jianshen <- jianshen %>% filter(!grepl('赵本山',content))
jianshen <- jianshen %>% filter(!grepl('西虹市首富|微博搞笑排行榜|李易峰|林清玄|美女健身合辑|美女写真',content))

jianshen <- jianshen %>% filter(!grepl('教授|好物推荐|白羊|野小兽|出道|ig|ins|运势|比劫|龚俊|张哲瀚|翟潇闻',content,ignore.case = TRUE))
jianshen <- jianshen %>% filter(!grepl('郭麒麟|孟佳|张雨绮|八字|好物|凝胶|胡歌|汶翰|吴尊|哥哥|男|白敬亭|陈晓',content,ignore.case = TRUE))
jianshen <- jianshen %>% filter(!grepl('阚清子|健身会所|卫健委|柳岩|悦莱美|吴磊|情感修复|创造营|许魏洲|米卡|阮经天',content,ignore.case = TRUE))
jianshen <- jianshen %>% filter(!grepl('郭麒麟|孟佳|张雨绮|八字|好物|凝胶|胡歌|汶翰|吴尊|哥哥|男|白敬亭|陈晓',content,ignore.case = TRUE))
jianshen <- jianshen %>% filter(!grepl('休闲健身|旅行地|院校|沈梦辰|李斯丹妮|徐正镔|骨科专家推荐的颈椎|张杰',content,ignore.case = TRUE))
jianshen <- jianshen %>% filter(!grepl('张新城|杨幂|张彬彬|探店|情感咨询|报道|详情页|健身设施|居老师|微整形',content,ignore.case = TRUE))
jianshen <- jianshen %>% filter(!grepl('高端|征婚|李湘|斛珠夫人',content,ignore.case = TRUE))
jianshen <- jianshen %>% filter(!grepl('娱乐圈|大S|欧阳娜娜',content,ignore.case = TRUE))
jianshen <- jianshen %>% filter(!grepl('为什么我不适合谈恋爱|令人心动的OFFER|茹茹减脂瘦身圈|毛晓彤|实在的一皮|小S|孙俪|a股|内娱',content,ignore.case = TRUE))
jianshen <- jianshen %>% filter(!grepl('张新城|杨幂|张彬彬|探店|情感咨询|报道|详情页|健身设施|居老师|微整形',content,ignore.case = TRUE))
jianshen <- jianshen %>% filter(!grepl('博君一肖|波士顿|寄宿考研|考研寄宿|金钟仁|面积',content,ignore.case = TRUE))

jianshen <- jianshen %>% filter(!grepl('爆料',content))
jianshen <- jianshen %>% filter(!grepl('党委',content))
jianshen <- jianshen %>% filter(!grepl('管理局',content))
jianshen <- jianshen %>% filter(!grepl('监局',content))
jianshen <- jianshen %>% filter(!grepl('曝光',content))
jianshen <- jianshen %>% filter(!grepl('追光吧哥哥',content))
jianshen <- jianshen %>% filter(!grepl('蒋欣',content))
jianshen <- jianshen %>% filter(!grepl('星座',content))
jianshen <- jianshen %>% filter(!grepl('星象',content))
jianshen <- jianshen %>% filter(!grepl('爱很美味',content))
jianshen <- jianshen %>% filter(!grepl('硬糖少女',content))
jianshen <- jianshen %>% filter(!grepl('张柏芝',content))
jianshen <- jianshen %>% filter(!grepl('袁姗姗',content))
jianshen <- jianshen %>% filter(!grepl('命盘',content))
jianshen <- jianshen %>% filter(!grepl('主星',content))
jianshen <- jianshen %>% filter(!grepl('张艺兴',content))
jianshen <- jianshen %>% filter(!grepl('塔罗牌',content))
jianshen <- jianshen %>% filter(!grepl('梅艳芳',content))
jianshen <- jianshen %>% filter(!grepl('法门',content))
jianshen <- jianshen %>% filter(!grepl('佛法',content))
jianshen <- jianshen %>% filter(!grepl('营销中心',content))
jianshen <- jianshen %>% filter(!grepl('人体胎盘',content))
jianshen <- jianshen %>% filter(!grepl('肌肉男牛蛙',content))
jianshen <- jianshen %>% filter(!grepl('仁兄能海涵',content))
jianshen <- jianshen %>% filter(!grepl('黄帝',content))
jianshen <- jianshen %>% filter(!grepl('批量加工',content))
jianshen <- jianshen %>% filter(!grepl('维权',content))
jianshen <- jianshen %>% filter(!grepl('首发',content))
jianshen <- jianshen %>% filter(!grepl('体育场馆',content))
jianshen <- jianshen %>% filter(!grepl('用户7564757120',user_name))
jianshen <- jianshen %>% filter(!grepl('钟汉良',content))
jianshen <- jianshen %>% filter(!grepl('彭昱畅',content))
jianshen <- jianshen %>% filter(!grepl('流年',content))
jianshen <- jianshen %>% filter(!grepl('命宫',content))
jianshen <- jianshen %>% filter(!grepl('阳光信用',content))
jianshen <- jianshen %>% filter(!grepl('法华',content))
jianshen <- jianshen %>% filter(!grepl('钟丽缇',content))
jianshen <- jianshen %>% filter(!grepl('发起',content))
jianshen <- jianshen %>% filter(!grepl('职话',user_name))
jianshen <- jianshen %>% filter(!grepl('考生',content))
jianshen <- jianshen %>% filter(!grepl('住宅',content))
jianshen <- jianshen %>% filter(!grepl('黄景瑜',content))


jianshen <- jianshen %>% filter(!grepl('两会',content))
jianshen <- jianshen %>% filter(!grepl('社会各界',content))
jianshen <- jianshen %>% filter(!grepl('举行',content))
jianshen <- jianshen %>% filter(!grepl('举办',content))
jianshen <- jianshen %>% filter(!grepl('开业',content))
jianshen <- jianshen %>% filter(!grepl('这猫是偷偷健身了吧',content))

jianshen <- jianshen %>% filter(!grepl('酬宾',content))
jianshen <- jianshen %>% filter(!grepl('邓紫棋',content))
jianshen <- jianshen %>% filter(!grepl('诚聘',content))
jianshen <- jianshen %>% filter(!grepl('招募',content))
jianshen <- jianshen %>% filter(!grepl('两会',content))
jianshen <- jianshen %>% filter(!grepl('开展',content))
jianshen <- jianshen %>% filter(!grepl('.9',content))
jianshen <- jianshen %>% filter(!grepl('娱',user_name))
jianshen <- jianshen %>% filter(!grepl('房产',user_name))
jianshen <- jianshen %>% filter(!grepl('输给了三个字|留学读研|爱情公寓|迪丽热巴|汪东城|吴宣仪|黄子韬|是一名|搞笑段子|快速减肥健康瘦身',user_name))

jianshen <- jianshen %>% filter(!grepl('小学',user_name))
jianshen <- jianshen %>% filter(!grepl('中学',user_name))
jianshen <- jianshen %>% filter(!grepl('大学',user_name))
jianshen <- jianshen %>% filter(!grepl('学院',user_name))
jianshen <- jianshen %>% filter(!grepl('政法',user_name))
jianshen <- jianshen %>% filter(!grepl('学院',user_name))
jianshen <- jianshen %>% filter(!grepl('整形',user_name))
jianshen <- jianshen %>% filter(!grepl('券',user_name))
jianshen <- jianshen %>% filter(!grepl('好看',user_name))
jianshen <- jianshen %>% filter(!grepl('推荐',user_name))
jianshen <- jianshen %>% filter(!grepl('入住',content))


jianshen <- jianshen %>% filter(!grepl('暨',content))
jianshen <- jianshen %>% filter(!grepl('爱问医生',user_name))
jianshen <- jianshen %>% filter(!grepl('店$',user_name))
jianshen <- jianshen %>% filter(!grepl('公司',user_name))
jianshen <- jianshen %>% filter(!grepl('集团',user_name))
jianshen <- jianshen %>% filter(!grepl('连锁',user_name))
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
jianshen <- jianshen %>% filter(!grepl('正畸日记',content))
jianshen <- jianshen %>% filter(!grepl('转租',content))
jianshen <- jianshen %>% filter(!grepl('杜淳',content))
jianshen <- jianshen %>% filter(!grepl('租房',user_name))
jianshen <- jianshen %>% filter(!grepl('出租',content))

jianshen <- jianshen %>% filter(!grepl('林陌',content))
jianshen <- jianshen %>% filter(!grepl('邓伦',content))
jianshen <- jianshen %>% filter(!grepl('李现',content))
jianshen <- jianshen %>% filter(!grepl('生图',content))
jianshen <- jianshen %>% filter(!grepl('辛芷蕾',content))
jianshen <- jianshen %>% filter(!grepl('黄晓明',content))
jianshen <- jianshen %>% filter(!grepl('全民',content))
jianshen <- jianshen %>% filter(!grepl('共创',content))
jianshen <- jianshen %>% filter(!grepl('构建',content))
jianshen <- jianshen %>% filter(!grepl('微问',content))
jianshen <- jianshen %>% filter(!grepl('社会各界',content))
jianshen <- jianshen %>% filter(!grepl('挑战者联盟',content))

jianshen <- jianshen %>% filter(!grepl('共同创造',content))
jianshen <- jianshen %>% filter(!grepl('共同建设',content))

jianshen <- jianshen %>% filter(!grepl('朱一龙',content))
jianshen <- jianshen %>% filter(!grepl('王子异',content))
jianshen <- jianshen %>% filter(!grepl('丁程鑫',content))
jianshen <- jianshen %>% filter(!grepl('蔡徐坤',content))
jianshen <- jianshen %>% filter(!grepl('刘耀元',content))
jianshen <- jianshen %>% filter(!grepl('肖宇梁',content))
jianshen <- jianshen %>% filter(!grepl('白敬亭',content))
jianshen <- jianshen %>% filter(!grepl('黄奕',content))

jianshen <- jianshen %>% filter(!grepl('王一博',content))
jianshen <- jianshen %>% filter(!grepl('夏之光',content))
jianshen <- jianshen %>% filter(!grepl('链接',content))
jianshen <- jianshen %>% filter(!grepl('基础设施',content))

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
jianshen <- jianshen %>% filter(!grepl('救命钱',content))
jianshen <- jianshen %>% filter(!grepl('受人之托',content))
jianshen <- jianshen %>% filter(!grepl('受友之托',content))

jianshen <- jianshen %>% filter(!grepl('郑希怡',content))
jianshen <- jianshen %>% filter(!grepl('华晨宇',content))
jianshen <- jianshen %>% filter(!grepl('刘耀元',content))
jianshen <- jianshen %>% filter(!grepl('畊',content))
jianshen <- jianshen %>% filter(!grepl('资讯',content))
jianshen <- jianshen %>% filter(!grepl('重磅',content))
jianshen <- jianshen %>% filter(!grepl('鸣谢',content))

jianshen <- jianshen %>% filter(!grepl('媒婆',user_name))
jianshen <- jianshen %>% filter(!grepl('月老',user_name))
jianshen <- jianshen %>% filter(!grepl('交友',user_name))
jianshen <- jianshen %>% filter(!grepl('型男',user_name))
jianshen <- jianshen %>% filter(!grepl('体育中心',user_name))
jianshen <- jianshen %>% filter(!grepl('系列',user_name))
jianshen <- jianshen %>% filter(!grepl('王珞丹',user_name))
jianshen <- jianshen %>% filter(!grepl('去处',user_name))

jianshen <- jianshen %>% filter(!grepl('留学生',user_name))
jianshen <- jianshen %>% filter(!grepl('公寓',user_name))
jianshen <- jianshen %>% filter(!grepl('租',user_name))
jianshen <- jianshen %>% filter(!grepl('身边事',user_name))
jianshen <- jianshen %>% filter(!grepl('新鲜事',user_name))

jianshen <- jianshen %>% filter(!grepl('报$',user_name))
jianshen <- jianshen %>% filter(!grepl('发布$',user_name))
jianshen <- jianshen %>% filter(!grepl('新闻$',user_name))
jianshen <- jianshen %>% filter(!grepl('家属索赔',content))
jianshen <- jianshen %>% filter(!grepl('猝死#$',content))
jianshen <- jianshen %>% filter(!grepl('汪峰',content))
jianshen <- jianshen %>% filter(!grepl('韩庚',content))
jianshen <- jianshen %>% filter(!grepl('能量月',content))

jianshen <- jianshen %>% filter(!grepl('职工',content))
jianshen <- jianshen %>% filter(!grepl('热销',content))

jianshen <- jianshen %>% filter(!grepl('委$',user_name))
jianshen <- jianshen %>% filter(!grepl('局$',user_name))
jianshen <- jianshen %>% filter(!grepl('共青团$',user_name))
jianshen <- jianshen %>% 
  filter(!grepl('白宇|谷嘉诚|胡一天|配套|苦瓜|扇贝打卡|那些能够让生活变得更好的小事|身边事|via|我就是演员|张俪|boy|noexcuses|短租|在售|紧邻|胜地|就有多美好 01|苏打Dinise|海王|二十几岁是最好的年纪|张钧甯|刘涛|夏目友人帐|延禧攻略|结肠癌|潘粤明|胃婆婆|三不争|件套|步道|嘉人|剪裁|半永久|微商|美国|市民|内美了自己，外养眼了别人|晒出|扬州·望月路|包邮|代购|美拍',content))



jianshen <- jianshen %>% distinct(content, .keep_all = TRUE)


# 文本替换 --------------------------------------------------------------------

jianshen$content <- gsub("\\p{So}|\\p{Cn}","",jianshen$content,perl = TRUE)
jianshen$content <- gsub("","",jianshen$content)
jianshen$content <- gsub("#.+?#]","",jianshen$content)
jianshen$content <- gsub("[:digit:].+?h","小时",jianshen$content)
jianshen$content <- gsub("第.*天","第X天",jianshen$content)
jianshen$content <- gsub("第.*周","第X周",jianshen$content)
jianshen$content <- gsub("第.*次","第X次",jianshen$content)
jianshen$content <- gsub("^ .+?广场","",jianshen$content)
jianshen$content <- gsub("^L.+? ","",jianshen$content)
jianshen$content <- gsub("^@.+? ","",jianshen$content)
jianshen$content <- gsub("我分享了","",jianshen$content)
jianshen$content <- gsub("分享图片|下载客户端|分享自","",jianshen$content)

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
jianshen$content <- gsub("去运动","没健身",jianshen$content)
jianshen$content <- gsub("大腿肉","大腿",jianshen$content)
jianshen$content <- gsub("大雨天","雨天",jianshen$content)
jianshen$content <- gsub("大晴天","晴天",jianshen$content)
jianshen$content <- gsub("耽搁","耽误",jianshen$content)
jianshen$content <- gsub("自制力","自控力",jianshen$content)


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

jianshen$content <- gsub("\\【.*】","",jianshen$content)
jianshen$content <- gsub("\\L.*视频","",jianshen$content)

jianshen$content <- gsub("\\@.* ","",jianshen$content)

jianshen$content <- gsub("\\<.*>","",jianshen$content)

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



# 导入大表



# 按性别分表 -------------------------------------------------------------------

aggregate(jianshen$id,by=list(mon=month(jianshen$publish_time)),length) #按月计数

jianshen <- na.omit(jianshen)
js_f <- jianshen[,c(2,5,4)][jianshen$gd == "f",]


# 整体词频 --------------------------------------------------------------------
splt <- worker(type = "mp",user = "C:/Users/15878/Desktop/topic/userdict.txt",
               stop_word = "C:/Users/15878/Desktop/stopword.txt")

jsf_fq <- splt[js_f$content]
freq <- jiebaR::freq(jsf_fq) #输出词频表
freq <- freq[order(freq$freq,decreasing = TRUE),]
freq1 <- freq[c(2:101),]

wordcloud2(freq1,size = 1,fontFamily = '微软雅黑',color = 'random-dark') #词云2画图
             

# tdidf 关键词提取+词云（暂时不用） --------------------------------------------------------------------

#建立语料库，一行一个list

js_f %>%
  mutate(words = map(js_f$x,segment,jieba = splt)) %>%
  select(id,words) -> corpus

corpus %>%
  unnest(cols = c(id,words)) %>%
  count(id,words)  -> n_table

n_table %>%
  bind_tf_idf(term = words,document = id,n = n) -> tf_idf_table

write.csv(tf_idf_table,file = "C:/Users/15878/Desktop/topic/tfidf.csv",quote = FALSE,row.names = FALSE)

tf_idf_table %>% 
  group_by(id) %>% 
  top_n(5,tf_idf) %>% 
  ungroup() -> top3

top3 %>% 
  count(words) %>%
  top_n(100) %>%    #只显示出现次数最多的200个关键词
  wordcloud2(size = 2, fontFamily = "微软雅黑",
             color = "random-light", backgroundColor = "white")



