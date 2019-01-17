setwd('D:\\租东东\\三方数据测试20181109\\百融\\租无忧数据测试结果\\01测试结果详情')
#----------------------------------------------
# ---读入数据-----
library(data.table);library(readxl);library(dplyr)
model_xy = readxl::read_xlsx('D:\\租东东\\三方数据测试20181109\\新颜\\新颜模型20181129\\model_xy_20181129.xlsx',
                             col_types = c(rep('guess',5),rep('text',3),rep('guess',8),rep('numeric',3),'guess',rep('numeric',5)))
model_xy$default_3 = model_xy$当期逾期天数 < 3
model_xy$default_5 = model_xy$当期逾期天数 < 5
model_xy$年龄段 <- ((model_xy$订单日期 %>% as.Date.character() - 
                    (model_xy$身份证号 %>% substr(7,15) %>% as.Date.character(format = '%Y%m%d'))) / 365) %>% 
  as.numeric() %>% cut(c(-Inf,23,Inf)) %>% as.character()

f_tuomin <- function(name,id){
  substring(id,15,16) <- '##'
  substring(id,18,18) = '#' # 加密 只用姓+身份证就可以
  substring(name,1,1) %>% paste(id,sep = '')
}
# ---百融脱敏---------
model_xy$idcard_jiami <- f_tuomin(model_xy$姓名,model_xy$身份证号)
# model_xy <- model_xy %>% mutate(idcard = idcard_jiami) %>% mutate(idcard_jiami = NULL)
#-----读取百融信息-------
# br_grzz <- readxl::read_xlsx("个人资质-基础版_PersonalCre.xlsx",sheet = 1,skip=1)
# br_jdxw <- readxl::read_xlsx("借贷行为验证_TotalLoan.xlsx",sheet = 1,skip=1);
br_jdyx <- readxl::read_xlsx("借贷意向验证_ApplyLoanStr.xlsx",sheet = 1,skip=1);
colnames(br_jdyx) <- str_replace_all(colnames(br_jdyx),'[、]|[，]|[-]','_') # 命名规范化
br_jdyx$id = paste(substring(br_jdyx$name,1,1),br_jdyx$id,sep = '') # 加密 只用姓+身份证就可以
# br_spxf <- readxl::read_xlsx("商品消费指数_Consumption_c.xlsx",sheet = 1,skip=1)
# br_smxx <- readxl::read_xlsx("实名信息验证_InfoRelation.xlsx",sheet = 1,skip=1)
# br_tsmd <- readxl::read_xlsx("特殊名单验证_v2.xlsx",sheet = 1,skip=1)
# 

br_jdyx <-
  model_xy %>% 
  left_join(br_jdyx,by = c("idcard_jiami" = "id"), suffix = c("", "_br_jdyx")) %>%  
  select(-one_of(c('cus_num','name','cell','user_date',
                   'sl_user_date','流水号','客户账号','返回码',
                   '借贷意向验证产品输出标识'))) %>% unique.data.frame() %>%
  select("姓名","身份证号","电话","default_5","在网时长","性别","年龄段","芝麻信用等级",contains('身份证')) %>% 
  unique.data.frame() 

# 计算IV
cols = colnames(br_jdyx)
# br_jdyx_iv = scorecard::iv(br_jdyx[,cols],y='default_5')  # or

# #----读入新颜 start----
# # 读入"新颜科技_信用雷达_全景雷达（回溯）_北京租无忧科技有限公司_20181109.xlsx"  # 新颜+百融联合建模
xyld_qjld_sq = readxl::read_xlsx("D:\\租东东\\三方数据测试20181109\\新颜\\北京租无忧科技有限公司 测试报告\\新颜科技_信用雷达_全景雷达（回溯）_北京租无忧科技有限公司_20181109.xlsx" ,sheet = 2,skip=2,na = '/')
xyld_qjld_xw = readxl::read_xlsx("D:\\租东东\\三方数据测试20181109\\新颜\\北京租无忧科技有限公司 测试报告\\新颜科技_信用雷达_全景雷达（回溯）_北京租无忧科技有限公司_20181109.xlsx" ,sheet = 3,skip=2,na = '/')
# xyld_qjld_xy = readxl::read_xlsx("D:\\租东东\\三方数据测试20181109\\新颜\\北京租无忧科技有限公司 测试报告\\新颜科技_信用雷达_全景雷达（回溯）_北京租无忧科技有限公司_20181109.xlsx" ,sheet = 4,skip=2,na = '/')
xyld_qjld <- 
  model_xy %>% 
  left_join(xyld_qjld_sq,by = c("身份证号" = "身份证号"), suffix = c("", "_xyld_qjld_sq")) %>%  
  select(-one_of(c('姓名','回溯时间','最近查询时间'))) %>% unique.data.frame() %>% 
  
  left_join(xyld_qjld_xw,by = c("身份证号" = "身份证号"), suffix = c("", "_xyld_qjld_xw")) %>%
  select(-one_of(c('姓名','回溯时间','最近一次贷款时间'))) %>% unique.data.frame() %>%
  # 
  # left_join(xyld_qjld_xy,by = c("身份证号" = "身份证号"), suffix = c("", "_xyld_qjld_xy")) %>%  
  # select(-one_of(c('姓名','回溯时间',"姓名_xyld_qjld_sq"))) %>%
  select(-one_of(c('姓名','回溯时间',"姓名_xyld_qjld_sq"))) %>%
  unique.data.frame()


xyld_qjld <- rename(xyld_qjld,"贷款逾期订单数M0plus" = "贷款逾期订单数（M0+）")  %>%
  `[`(.,c(6,26,28:52)) %>% unique.data.frame()
colnames(xyld_qjld) <- paste(colnames(xyld_qjld),'_xy',sep="")
iv_tables_for_all(xyld_qjld,x = colnames(xyld_qjld)[3:27],y='default_5_xy') -> iv_tabs_xy
monocols_xy <- woe_monotone_detect(iv_tabs_xy) %>% intersect(iv_tabs_xy$ivs[iv_tabs_xy$ivs > 0.2] %>% names())
iv_tables_for_all(br_jdyx,x = colnames(br_jdyx)[5:418],y='default_5') -> iv_tabs_br
monocols_br <- woe_monotone_detect(iv_tabs_br) %>% intersect(iv_tabs_br$ivs[iv_tabs_br$ivs > 0.2] %>% names())
monocols <- monocols_br %>% union(monocols_xy)
br_xy <- br_jdyx %>% left_join(xyld_qjld,by = c("身份证号" = "身份证号_xy")) %>% 
  select('身份证号','default_5','性别','年龄段',"在网时长" ,"芝麻信用等级" ,monocols) %>% unique.data.frame() 
br_xy <- br_xy[!duplicated(br_xy$身份证号),] 
# #----读入新颜 end----


#----读入同盾 start----
# 读入同盾测试
td_30 = readxl::read_xlsx("D:\\租东东\\三方数据测试20181109\\同盾\\北京租无忧科技有限公司_2018-11-27_信贷名单类测试脱敏结果\\北京租无忧科技有限公司_2018-11-27_信贷名单类测试脱敏结果.xlsx" ,sheet = '多平台结果_30',skip=1,na = '')
td_90 = readxl::read_xlsx("D:\\租东东\\三方数据测试20181109\\同盾\\北京租无忧科技有限公司_2018-11-27_信贷名单类测试脱敏结果\\北京租无忧科技有限公司_2018-11-27_信贷名单类测试脱敏结果.xlsx" ,sheet = '多平台结果_90',skip=1,na = '')
td_365 = readxl::read_xlsx("D:\\租东东\\三方数据测试20181109\\同盾\\北京租无忧科技有限公司_2018-11-27_信贷名单类测试脱敏结果\\北京租无忧科技有限公司_2018-11-27_信贷名单类测试脱敏结果.xlsx" ,sheet = '多平台结果_365',skip=1,na = '')
td_all_risklist = readxl::read_xlsx("D:\\租东东\\三方数据测试20181109\\同盾\\北京租无忧科技有限公司_2018-11-27_信贷名单类测试脱敏结果\\北京租无忧科技有限公司_2018-11-27_信贷名单类测试脱敏结果.xlsx" ,sheet = '全量_风险名单结果',skip=1,na = '')
td_all_focuslist = readxl::read_xlsx("D:\\租东东\\三方数据测试20181109\\同盾\\北京租无忧科技有限公司_2018-11-27_信贷名单类测试脱敏结果\\北京租无忧科技有限公司_2018-11-27_信贷名单类测试脱敏结果.xlsx" ,sheet = '全量_关注名单结果',skip=1,na = '')
td_30 <- td_30 %>% mutate(ID = paste(姓名,手机号,身份证,sep='')) %>% select(ID,contains('身份证'))
td_90 <- td_90 %>% mutate(ID = paste(姓名,手机号,身份证,sep='')) %>% select(ID,contains('身份证'))
td_365 <- td_365 %>% mutate(ID = paste(姓名,手机号,身份证,sep='')) %>% select(ID,contains('身份证'))
td_all_risklist <- td_all_risklist %>% mutate(ID = paste(姓名,手机号,身份证,sep='')) %>% select(ID,contains('身份证'))
td_all_focuslist <- td_all_focuslist %>% mutate(ID = paste(姓名,手机号,身份证,sep='')) %>% select(ID,contains('身份证'))

td <- td_30 %>% 
  left_join(td_90,by = c("ID" = "ID"), suffix = c("", "_td_90")) %>%  
  select(-one_of(c('','',''))) %>% unique.data.frame() %>%
  left_join(td_365,by = c("ID" = "ID"), suffix = c("", "_td_365")) %>%  
  select(-one_of(c('','',''))) %>% unique.data.frame() %>%
  left_join(td_all_risklist,by = c("ID" = "ID"), suffix = c("", "_risklist")) %>%  
  select(-one_of(c('','',''))) %>% unique.data.frame() %>%
  left_join(td_all_focuslist,by = c("ID" = "ID"), suffix = c("", "_focuslist")) %>%  
  select(-one_of(c('','',''))) %>% unique.data.frame()


jiami <- function(name,phone,id){
  substr(name,1,1) <- '*'
  # substr(phone,5,9) <- '******'
  phone <- paste(substr(phone,1,4),'******',substr(phone,10,11),sep = '')
  substr(id,7,16) <- '**********'
  paste(name,phone,id,sep='')
}

td_test <- model_xy %>% mutate(ID = jiami(姓名,电话,身份证号)) %>% 
  left_join(td_30,by = c("ID" = "ID"), suffix = c("", "_td_30")) %>%  
  select(-one_of(c('','',''))) %>% unique.data.frame() %>%
  left_join(td_90,by = c("ID" = "ID"), suffix = c("", "_td_90")) %>%  
  select(-one_of(c('','',''))) %>% unique.data.frame() %>%
  left_join(td_365,by = c("ID" = "ID"), suffix = c("", "_td_365")) %>%  
  select(-one_of(c('','',''))) %>% unique.data.frame() %>%
  left_join(td_all_risklist,by = c("ID" = "ID"), suffix = c("", "_risklist")) %>%  
  select(-one_of(c('','',''))) %>% unique.data.frame() %>%
  left_join(td_all_focuslist,by = c("ID" = "ID"), suffix = c("", "_focuslist")) %>%  
  select(-one_of(c('','',''))) %>% unique.data.frame()

# 转化为数值型
for(x in colnames(td_test)){
  if( x %>% str_detect('平台数')) td_test[[x]] = as.numeric(td_test[[x]])
}
# td_test %>% colnames()
td_test <- td_test[,c(6:8,28,27,10:12,20,30:78)] %>% unique.data.frame()
#----读入同盾 end----

td_br <- td_test %>% `[`(.,,c(2,4,10,11:58)) %>%
  left_join(br_jdyx,by=c('身份证号'='身份证号'), suffix = c("_td", "_br")) %>%
  unique.data.frame() %>% 
  # select monocols
  select(
    c("身份证号","idcard_jiami","身份证","default_5","性别","在网时长","年龄段","芝麻信用等级") %>% 
      union(
        intersect(iv_tabs$ivs[iv_tabs$ivs>0.3] %>% names(),monocols)
        )
    ) %>%
  unique.data.frame()


# iv_tables_for_all(td_br,x = colnames(td_br)[c(1:2,54,4:51,55:468)][-(1:3)],y='default_5') -> iv_tabs
# monocols <- woe_monotone_detect(woelist=iv_tabs$woe)
# monocols_td
# monocols_br


##################################################################################
#----贷前no芝麻分模型：百融-------
##################################################################################
# br_jd %>% colnames()
rm(train,test,rs,xy)
library(data.table);xy = setDT(br_jdyx)
setnames(xy,old = 'default_5','y')
xy[,.(sum(y),mean(y),.N)] #查看事件分布
rt = xy %>% split_df(.,0.80)
train = rt$train;
test  = rt$test;
train %>% nrow();test %>% nrow();test[,.(mean(y),sum(y),.N)];train[,.(mean(y),sum(y),.N)]
# 
# iv&woe for train;
cols = names(xy)
iv_tables_for_all(train,cols[-(1:4)],'y') -> iv_tabs
train_woe <- apply2woe(train,x = cols,iv_tables_list = iv_tabs) 
test_woe <- apply2woe(test,x= cols,iv_tables_list = iv_tabs)
monocols = woe_monotone_detect(iv_tabs)
sel_cols = iv_tabs$ivs[iv_tabs$ivs >0.2] %>% names() %>% intersect(monocols) # %>% c(c('芝麻信用等级','性别'))

#----------------------------model---------------------------------------------#
# #---------lasso--------
# monocols1 = woe_monotone_detect(iv_tabs$woe);iv_tabs$woe[monocols1];iv_tabs$ivs[monocols1] # 过滤掉WOE不单调的变量
# monocols_xy = woe_monotone_detect(iv_tabs$woe[xy_cols]);iv_tabs$woe[monocols_xy];iv_tabs$ivs[monocols_xy]
library(biglars)
lasso = biglars.fit(x=train_woe[,sel_cols,with = F] %>% as.matrix(),
                    y=train_woe$y,
                    type ='lasso',removeColumns =T)
coef(lasso)
(lasso$coefficients !=0) %>% apply(1,sum)
(lasso$coefficients[13,] != 0) %>% sum() # 选择15个变量

# cols =
#   c(which(lasso$coefficients[14,] != 0)  %>% names() %>%
#       '['(-1)) %>% c(c('zm_jianmian','real_mianya_ratio','近3个月总查询笔数'),.)

#----------------------------model---------------------------------------------#
rm(forms,GLM,GLM1,y_fit_test,wb,results)
forms <- c(which(lasso$coefficients[10,] != 0)  %>% names() %>% '['(-1)) %>% 
  c('性别','在网时长',.) %>%
  setdiff(c('芝麻信用等级','按身份证号查询_近6个月在非银机构_其他申请机构数__1',
  '按身份证号查询_近12个月申请线上现金分期的机构数')) %>%
  paste0(collapse = ' + ') %>% paste('y ~ ',.,sep='') %>% as.formula()
# forms <-
# y ~ 性别 + 在网时长 + 按身份证号查询_近12个月在非银机构_现金类分期申请机构数 + 
#   按身份证号查询_近12个月在非银机构_其他申请次数__1 + 按身份证号查询_近6个月在非银机构_其他申请机构数 + 
#   按身份证号查询_近12个月申请线下消费分期的机构数 + 年龄段 + 
#   按身份证号查询_近7天在非银机构_现金类分期机构申请次数 + 按身份证号查询_近7天申请线上小额现金贷的机构数
# glm begein
GLM1 <- glm(formula = forms,family = binomial(link = "logit"),data = train_woe);
GLM1 %>% car::vif();GLM1$coefficients
# GLM <- step(GLM1,direction = 'both');
GLM=GLM1
GLM %>% car::vif() # 先做lasso筛选变量，再做逐步回归效果好，剔除多重共线性，提高计算效率
GLM$coefficients;GLM$coefficients %>% length()
# GLM1$coefficients;
# GLM1 %>% car::vif();GLM1$coefficients %>% length()
InformationValue::AUROC(GLM$y,GLM$fitted.values)
InformationValue::Concordance(GLM$y,GLM$fitted.values)
InformationValue::ks_stat(GLM$y,GLM$fitted.values) 
InformationValue::ks_plot(GLM$y,GLM$fitted.values)
InformationValue::plotROC(GLM$y,GLM$fitted.values)
GLM$coefficients[-1] %>% names()
iv_tabs$woe[GLM$coefficients[-1] %>% names()]
# 测试集表现
# sqlscore::score_expression(GLM)
# sqlscore::score_expression(GLM)
# test_woe[,eval(sqlscore::score_expression(GLM))] -> y_fit_test_2

y_fit_test <- predict(GLM,test_woe,type = 'response')
InformationValue::ks_stat(test_woe$y,y_fit_test)
# save model
# zm_taochan_daizhong_m = GLM
# y_fit_test <- predict(zm_taochan_daizhong_m,test_woe,type = 'response')
# InformationValue::ks_stat(test_woe$y,y_fit_test)

# rm(score_train,score_test)
####################################
# 校准评分
# 600 20/1  650 40/1 pdo = 50
# 600 = A + B * log(30/1)
# 650 = A + B * log(60/1)

score_train = scorescale_f(GLM$fitted.values,base_odds = 20/1.0,pdo=50,base_points = 600)$score
score_test = scorescale_f(y_fit_test,base_odds = 20/1.0,pdo=50,base_points = 600)$score
strategy_f(test_woe$y,score_test,bins = 20)
strategy_f(train_woe$y,score_train,bins=20)
InformationValue::ks_stat(train_woe$y,score_train)
InformationValue::ks_stat(test_woe$y,score_test)
####################################
rm(results)
choosed_var = GLM$coefficients[-1] %>% names()
results = list()
results$xy = xy
results$woes = rbindlist(iv_tabs[['woe']][choosed_var])
results$train  = train
results$test   = test
results$train_stategy <- strategy_f(train_woe$y,score_train) 
results$train_score <- data.table(train_woe,score_train)
results$test_stategy <- strategy_f(test_woe$y,score_test)
# results$OOT_stategy <- strategy_f(OOT_woe$y,score_oot)
results$test_score <- data.table(test_woe,score_test)
# results$OOT_score <- data.table(OOT_woe,score_test)
results$test <- test
# results$OOT <- OOT
results$vif <- data.table(x = names(car::vif(GLM)),vif = car::vif(GLM))
results$iv <- data.table(x = names(iv_tabs$ivs[names(car::vif(GLM))]),iv = iv_tabs$ivs[names(car::vif(GLM))])
library(broom);tidy(GLM)
results$model <- data.table(tidy(GLM),vif=car::vif(GLM)[tidy(GLM)[['term']]])
results$KS_auc = data.table(train_ks = InformationValue::ks_stat(train$y,GLM$fitted.values),
                            test_ks = InformationValue::ks_stat(test_woe$y,score_test),
                            train_auc = InformationValue::AUROC(train$y,GLM$fitted.values),
                            test_auc = InformationValue::AUROC(test$y,score_test),
                            OOT = NA)
# 生成sql代码
iv_tabs$woe[GLM$coefficients %>% names() %>% '['(-1)] %>% woe2sql %>% 
  cat(file = 'D:\\租东东\\三方数据测试20181109\\百融\\租无忧数据测试结果\\01测试结果详情\\贷前反欺诈模型_brzm\\贷前反欺诈模型_brzm_woe2sql.txt')
sqlscore::score_expression(GLM) %>% capture.output() %>% paste0(collapse = ' \n') %>%
  cat(file = 'D:\\租东东\\三方数据测试20181109\\百融\\租无忧数据测试结果\\01测试结果详情\\贷前反欺诈模型_brzm\\贷前反欺诈模型_brzm_modelsql.txt')
iv_tabs$woe[GLM$coefficients %>% names() %>% '['(-1)] %>% score2sql(.,GLM) %>% 
  cat(file = 'D:\\租东东\\三方数据测试20181109\\百融\\租无忧数据测试结果\\01测试结果详情\\贷前反欺诈模型_brzm\\贷前反欺诈模型_brzm_scoresql.txt')
scorescale_f(GLM$fitted.values,base_odds = 20/1.0,pdo=50,base_points = 600)$scorescale2sql %>%
  cat(file = 'D:\\租东东\\三方数据测试20181109\\百融\\租无忧数据测试结果\\01测试结果详情\\贷前反欺诈模型_brzm\\贷前反欺诈模型_brzm_scorescale2sql.txt')
save(results,file = 'D:\\租东东\\三方数据测试20181109\\百融\\租无忧数据测试结果\\01测试结果详情\\贷前反欺诈模型_brzm\\贷前反欺诈模型_brzm.rdata')
# 中间结果输出到excel表

library(openxlsx)
rm(wb)
wb <- createWorkbook()
for(x in names(results)){
  print(x)
  addWorksheet(wb, x)
  writeData(wb,results[[x]],sheet = x,keepNA = T)
}
saveWorkbook(wb, "D:\\租东东\\三方数据测试20181109\\百融\\租无忧数据测试结果\\01测试结果详情\\贷前反欺诈模型_brzm\\贷前反欺诈模型_brzm.xlsx", overwrite = TRUE)



##################################################################################
#----贷前有芝麻分模型：百融+同盾-------
##################################################################################
#######################################################################
# br_jd %>% colnames()
rm(train,test,rs,xy)
# xy[,cols,with=F] %>% names()
library(data.table);xy = setDT(td_br)
setnames(xy,old = 'default_5','y')
xy[,.(sum(y),mean(y),.N)] #查看事件分布

# 拆分训练集，测试集，时间外测试集
xy <- copy(td_br);
rt = xy %>% split_df(.,0.80)
train = rt$train;
test  = rt$test;
train %>% nrow();test %>% nrow();test[,.(mean(y),sum(y),.N)];train[,.(mean(y),sum(y),.N)]
# 
# iv&woe for train;
cols = names(xy)
iv_tables_for_all(train,cols[-(1:4)],'y') -> iv_tabs
# iv_tabs$ivs %>% sort(decreasing = T) %>%  `[`(1:50)  %>% names -> cols
train_woe <- apply2woe(train,x = cols,iv_tables_list = iv_tabs) 
test_woe <- apply2woe(test,x= cols,iv_tables_list = iv_tabs) 

#----------------------------model---------------------------------------------#
# #---------lasso--------
# monocols1 = woe_monotone_detect(iv_tabs$woe);iv_tabs$woe[monocols1];iv_tabs$ivs[monocols1] # 过滤掉WOE不单调的变量
# monocols_xy = woe_monotone_detect(iv_tabs$woe[xy_cols]);iv_tabs$woe[monocols_xy];iv_tabs$ivs[monocols_xy]
# iv_tabs$ivs %>% sort(decreasing = T) %>%  `[`(1:50) %>% names -> cols
lar_cols =  cols[-(1:4)] %>% setdiff('芝麻信用等级')
library(biglars)
lasso = biglars.fit(x=train_woe[,lar_cols,with = F] %>% as.matrix(),
                    y=train_woe$y,
                    type ='lasso',removeColumns =T)
coef(lasso)
(lasso$coefficients !=0) %>% apply(1,sum)
(lasso$coefficients[20,] != 0) %>% sum() # 选择15个变量
# cols =
#   c(which(lasso$coefficients[14,] != 0)  %>% names() %>%
#       '['(-1)) %>% c(c('zm_jianmian','real_mianya_ratio','近3个月总查询笔数'),.)


##################################################################################
#----贷前有芝麻分模型：百融+同盾-------
##################################################################################
rm(forms,GLM,GLM1,y_fit_test,wb,results)
forms <- c(which(lasso$coefficients[10,] != 0)  %>% names() %>% '['(-1)) %>% 
  setdiff(c('芝麻信用等级','按身份证号查询_近12个月申请线上现金分期的次数','按身份证号查询_近12个月申请线上小额现金贷的机构数')) %>%
  paste0(collapse = ' + ') %>% paste('y ~ ',.,sep='') %>% as.formula()
# forms <- 
#   y ~ zmscore + sex + 按身份证号查询_近12个月申请线上小额现金贷的机构数 + 
#   按身份证号查询_近12个月在非银机构_现金类分期申请机构数 + 
#   按身份证号查询_近6个月在非银机构_其他申请机构数__1 + 
#   按身份证号查询_近12个月申请其他的次数 + 按身份证号查询_近12个月在非银机构_持牌融资租赁机构申请机构数 + 
#   按身份证号查询_近6个月在非银机构申请最小间隔天数 + 
#   按身份证号查询_近12个月在非银机构有申请记录月份数 + 按身份证号查询_近6个月在非银机构夜间申请机构数
# glm begein
GLM1 <- glm(formula = forms,family = binomial(link = "logit"),data = train_woe);
GLM1 %>% car::vif();GLM1$coefficients
# GLM <- step(GLM1,direction = 'both');
GLM=GLM1
GLM %>% car::vif() # 先做lasso筛选变量，再做逐步回归效果好，剔除多重共线性，提高计算效率
GLM$coefficients;GLM$coefficients %>% length()
# GLM1$coefficients;
# GLM1 %>% car::vif();GLM1$coefficients %>% length()
InformationValue::AUROC(GLM$y,GLM$fitted.values)
InformationValue::Concordance(GLM$y,GLM$fitted.values)
InformationValue::ks_stat(GLM$y,GLM$fitted.values) 
InformationValue::ks_plot(GLM$y,GLM$fitted.values)
InformationValue::plotROC(GLM$y,GLM$fitted.values)
GLM$coefficients[-1] %>% names()
iv_tabs$woe[GLM$coefficients[-1] %>% names()]
# 测试集表现
# sqlscore::score_expression(GLM)
# sqlscore::score_expression(GLM)
# test_woe[,eval(sqlscore::score_expression(GLM))] -> y_fit_test_2

y_fit_test <- predict(GLM,test_woe,type = 'response')
InformationValue::ks_stat(test_woe$y,y_fit_test)
# save model
# zm_taochan_daizhong_m = GLM
# y_fit_test <- predict(zm_taochan_daizhong_m,test_woe,type = 'response')
# InformationValue::ks_stat(test_woe$y,y_fit_test)

# rm(score_train,score_test)
####################################
# 校准评分
# 600 20/1  650 40/1 pdo = 50
# 600 = A + B * log(30/1)
# 650 = A + B * log(60/1)

score_train = scorescale_f(GLM$fitted.values,base_odds = 20/1.0,pdo=50,base_points = 600)$score
score_test = scorescale_f(y_fit_test,base_odds = 20/1.0,pdo=50,base_points = 600)$score
strategy_f(test_woe$y,score_test,bins = 20)
strategy_f(train_woe$y,score_train,bins=20)
InformationValue::ks_stat(train_woe$y,score_train)
InformationValue::ks_stat(test_woe$y,score_test)
####################################
rm(results)
choosed_var = GLM$coefficients[-1] %>% names()
results = list()
results$xy = xy
results$woes = rbindlist(iv_tabs[['woe']][choosed_var])
results$train  = train
results$test   = test
results$train_stategy <- strategy_f(train_woe$y,score_train) 
results$train_score <- data.table(train_woe,score_train)
results$test_stategy <- strategy_f(test_woe$y,score_test)
# results$OOT_stategy <- strategy_f(OOT_woe$y,score_oot)
results$test_score <- data.table(test_woe,score_test)
# results$OOT_score <- data.table(OOT_woe,score_test)
results$test <- test
# results$OOT <- OOT
results$vif <- data.table(x = names(car::vif(GLM)),vif = car::vif(GLM))
results$iv <- data.table(x = names(iv_tabs$ivs[names(car::vif(GLM))]),iv = iv_tabs$ivs[names(car::vif(GLM))])
library(broom);tidy(GLM)
results$model <- data.table(tidy(GLM),vif=car::vif(GLM)[tidy(GLM)[['term']]])
results$KS_auc = data.table(train_ks = InformationValue::ks_stat(train$y,GLM$fitted.values),
                            test_ks = InformationValue::ks_stat(test_woe$y,score_test),
                            train_auc = InformationValue::AUROC(train$y,GLM$fitted.values),
                            test_auc = InformationValue::AUROC(test$y,score_test),
                            OOT = NA)
# 生成sql代码
iv_tabs$woe[GLM$coefficients %>% names() %>% '['(-1)] %>% woe2sql %>% 
  cat(file = 'D:\\租东东\\三方数据测试20181109\\百融\\租无忧数据测试结果\\01测试结果详情\\贷前反欺诈模型_brzm\\贷前反欺诈模型_brzm_woe2sql.txt')
sqlscore::score_expression(GLM) %>% capture.output() %>% paste0(collapse = ' \n') %>%
  cat(file = 'D:\\租东东\\三方数据测试20181109\\百融\\租无忧数据测试结果\\01测试结果详情\\贷前反欺诈模型_brzm\\贷前反欺诈模型_brzm_modelsql.txt')
iv_tabs$woe[GLM$coefficients %>% names() %>% '['(-1)] %>% score2sql(.,GLM) %>% 
  cat(file = 'D:\\租东东\\三方数据测试20181109\\百融\\租无忧数据测试结果\\01测试结果详情\\贷前反欺诈模型_brzm\\贷前反欺诈模型_brzm_scoresql.txt')
scorescale_f(GLM$fitted.values,base_odds = 20/1.0,pdo=50,base_points = 600)$scorescale2sql %>%
  cat(file = 'D:\\租东东\\三方数据测试20181109\\百融\\租无忧数据测试结果\\01测试结果详情\\贷前反欺诈模型_brzm\\贷前反欺诈模型_brzm_scorescale2sql.txt')
save(results,file = 'D:\\租东东\\三方数据测试20181109\\百融\\租无忧数据测试结果\\01测试结果详情\\贷前反欺诈模型_brzm\\贷前反欺诈模型_brzm.rdata')
# 中间结果输出到excel表

library(openxlsx)
rm(wb)
wb <- createWorkbook()
for(x in names(results)){
  print(x)
  addWorksheet(wb, x)
  writeData(wb,results[[x]],sheet = x,keepNA = T)
}
saveWorkbook(wb, "D:\\租东东\\三方数据测试20181109\\百融\\租无忧数据测试结果\\01测试结果详情\\贷前反欺诈模型_brzm\\贷前反欺诈模型_brzm.xlsx", overwrite = TRUE)



##################################################################################
#----贷前有芝麻分模型：百融-------
##################################################################################
# br_jd %>% colnames()
rm(train,test,rs,xy)
library(data.table);xy = setDT(br_jdyx)
setnames(xy,old = 'default_5','y')
xy[,.(sum(y),mean(y),.N)] #查看事件分布
rt = xy %>% split_df(.,0.80)
train = rt$train;
test  = rt$test;
train %>% nrow();test %>% nrow();test[,.(mean(y),sum(y),.N)];train[,.(mean(y),sum(y),.N)]
# 
# iv&woe for train;
cols = names(xy)
iv_tables_for_all(train,cols[-(1:4)],'y') -> iv_tabs
train_woe <- apply2woe(train,x = cols,iv_tables_list = iv_tabs) 
test_woe <- apply2woe(test,x= cols,iv_tables_list = iv_tabs)
monocols = woe_monotone_detect(iv_tabs)
sel_cols = iv_tabs$ivs[iv_tabs$ivs >0.2] %>% names() %>% intersect(monocols) %>% c(c('芝麻信用等级','性别','在网时长'),.)

#----------------------------model---------------------------------------------#
# #---------lasso--------
# monocols1 = woe_monotone_detect(iv_tabs$woe);iv_tabs$woe[monocols1];iv_tabs$ivs[monocols1] # 过滤掉WOE不单调的变量
# monocols_xy = woe_monotone_detect(iv_tabs$woe[xy_cols]);iv_tabs$woe[monocols_xy];iv_tabs$ivs[monocols_xy]
library(biglars)
lasso = biglars.fit(x=train_woe[,sel_cols,with = F] %>% as.matrix(),
                    y=train_woe$y,
                    type ='lasso',removeColumns =T)
coef(lasso)
(lasso$coefficients !=0) %>% apply(1,sum)
(lasso$coefficients[13,] != 0) %>% sum() # 选择15个变量

# cols =
#   c(which(lasso$coefficients[14,] != 0)  %>% names() %>%
#       '['(-1)) %>% c(c('zm_jianmian','real_mianya_ratio','近3个月总查询笔数'),.)

#----------------------------model---------------------------------------------#
rm(forms,GLM,GLM1,y_fit_test,wb,results)
forms <- c(which(lasso$coefficients[8,] != 0)  %>% names() %>% '['(-1)) %>% 
  c('性别','',.) %>%
  setdiff(c('','',
            '')) %>%
  paste0(collapse = ' + ') %>% paste('y ~ ',.,sep='') %>% as.formula()
# forms <-
# y ~ 性别 + 芝麻信用等级 + 在网时长 + 按身份证号查询_近12个月在非银机构_现金类分期申请机构数 + 
#   按身份证号查询_近6个月在非银机构_其他申请机构数__1 + 按身份证号查询_近12个月申请线下消费分期的机构数 + 
#   年龄段 + 按身份证号查询_近7天在非银机构_现金类分期机构申请次数
# glm begein
GLM1 <- glm(formula = forms,family = binomial(link = "logit"),data = train_woe);
GLM1 %>% car::vif();GLM1$coefficients
# GLM <- step(GLM1,direction = 'both');
GLM=GLM1
GLM %>% car::vif() # 先做lasso筛选变量，再做逐步回归效果好，剔除多重共线性，提高计算效率
GLM$coefficients;GLM$coefficients %>% length()
# GLM1$coefficients;
# GLM1 %>% car::vif();GLM1$coefficients %>% length()
InformationValue::AUROC(GLM$y,GLM$fitted.values)
InformationValue::Concordance(GLM$y,GLM$fitted.values)
InformationValue::ks_stat(GLM$y,GLM$fitted.values) 
InformationValue::ks_plot(GLM$y,GLM$fitted.values)
InformationValue::plotROC(GLM$y,GLM$fitted.values)
GLM$coefficients[-1] %>% names()
iv_tabs$woe[GLM$coefficients[-1] %>% names()]
# 测试集表现
# sqlscore::score_expression(GLM)
# sqlscore::score_expression(GLM)
# test_woe[,eval(sqlscore::score_expression(GLM))] -> y_fit_test_2

y_fit_test <- predict(GLM,test_woe,type = 'response')
InformationValue::ks_stat(test_woe$y,y_fit_test)
# save model
# zm_taochan_daizhong_m = GLM
# y_fit_test <- predict(zm_taochan_daizhong_m,test_woe,type = 'response')
# InformationValue::ks_stat(test_woe$y,y_fit_test)

# rm(score_train,score_test)
####################################
# 校准评分
# 600 20/1  650 40/1 pdo = 50
# 600 = A + B * log(30/1)
# 650 = A + B * log(60/1)

score_train = scorescale_f(GLM$fitted.values,base_odds = 20/1.0,pdo=50,base_points = 600)$score
score_test = scorescale_f(y_fit_test,base_odds = 20/1.0,pdo=50,base_points = 600)$score
strategy_f(test_woe$y,score_test,bins = 20)
strategy_f(train_woe$y,score_train,bins=20)
InformationValue::ks_stat(train_woe$y,score_train)
InformationValue::ks_stat(test_woe$y,score_test)
####################################
rm(results)
choosed_var = GLM$coefficients[-1] %>% names()
results = list()
results$xy = xy
results$woes = rbindlist(iv_tabs[['woes']][choosed_var])
results$train  = train
results$test   = test
results$train_stategy <- strategy_f(train_woe$y,score_train) 
results$train_score <- data.table(train_woe,score_train)
results$test_stategy <- strategy_f(test_woe$y,score_test)
# results$OOT_stategy <- strategy_f(OOT_woe$y,score_oot)
results$test_score <- data.table(test_woe,score_test)
# results$OOT_score <- data.table(OOT_woe,score_test)
results$test <- test
# results$OOT <- OOT
results$vif <- data.table(x = names(car::vif(GLM)),vif = car::vif(GLM))
results$iv <- data.table(x = names(iv_tabs$ivs[names(car::vif(GLM))]),iv = iv_tabs$ivs[names(car::vif(GLM))])
library(broom);tidy(GLM)
results$model <- data.table(tidy(GLM),vif=car::vif(GLM)[tidy(GLM)[['term']]])
results$KS_auc = data.table(train_ks = InformationValue::ks_stat(train$y,GLM$fitted.values),
                            test_ks = InformationValue::ks_stat(test_woe$y,score_test),
                            train_auc = InformationValue::AUROC(train$y,GLM$fitted.values),
                            test_auc = InformationValue::AUROC(test$y,score_test),
                            OOT = NA)
# 生成sql代码
iv_tabs$woe[GLM$coefficients %>% names() %>% '['(-1)] %>% woe2sql %>% 
  cat(file = 'D:\\租东东\\三方数据测试20181109\\百融\\租无忧数据测试结果\\01测试结果详情\\贷前反欺诈模型_brzm\\贷前反欺诈模型_brzm_woe2sql.txt')
sqlscore::score_expression(GLM) %>% capture.output() %>% paste0(collapse = ' \n') %>%
  cat(file = 'D:\\租东东\\三方数据测试20181109\\百融\\租无忧数据测试结果\\01测试结果详情\\贷前反欺诈模型_brzm\\贷前反欺诈模型_brzm_modelsql.txt')
iv_tabs$woe[GLM$coefficients %>% names() %>% '['(-1)] %>% score2sql(.,GLM) %>% 
  cat(file = 'D:\\租东东\\三方数据测试20181109\\百融\\租无忧数据测试结果\\01测试结果详情\\贷前反欺诈模型_brzm\\贷前反欺诈模型_brzm_scoresql.txt')
scorescale_f(GLM$fitted.values,base_odds = 20/1.0,pdo=50,base_points = 600)$scorescale2sql %>%
  cat(file = 'D:\\租东东\\三方数据测试20181109\\百融\\租无忧数据测试结果\\01测试结果详情\\贷前反欺诈模型_brzm\\贷前反欺诈模型_brzm_scorescale2sql.txt')
iv_tabs$woe[GLM$coefficients %>% names() %>% '['(-1)] %>% 
  final_score2sql(.,GLM,table_name = "xxxxxx",base_odds = 20/1.0,pdo = 50,base_points = 600) %>% 
  cat(file = 'D:\\租东东\\三方数据测试20181109\\百融\\租无忧数据测试结果\\01测试结果详情\\贷前反欺诈模型_brzm\\贷前反欺诈模型_brzm_final_score2sql.txt')

save(results,file = 'D:\\租东东\\三方数据测试20181109\\百融\\租无忧数据测试结果\\01测试结果详情\\贷前反欺诈模型_brzm\\贷前反欺诈模型_brzm.rdata')
# 中间结果输出到excel表

library(openxlsx)
rm(wb)
wb <- createWorkbook()
for(x in names(results)){
  print(x)
  addWorksheet(wb, x)
  writeData(wb,results[[x]],sheet = x,keepNA = T)
}
saveWorkbook(wb, "D:\\租东东\\三方数据测试20181109\\百融\\租无忧数据测试结果\\01测试结果详情\\贷前反欺诈模型_brzm\\贷前反欺诈模型_brzm.xlsx", overwrite = TRUE)


##################################################################################
#-----贷前无芝麻分模型：百融-----
##################################################################################
# br_jd %>% colnames()
rm(train,test,rs,xy,forms,results,wb)
library(data.table);xy = setDT(br_xy)
# setnames(xy,old = 'default_5','y')
xy[,.(sum(y),mean(y),.N)] #查看事件分布
rt = xy %>% split_df(.,0.80)
train = rt$train;
test  = rt$test;
train %>% nrow();test %>% nrow();test[,.(mean(y),sum(y),.N)];train[,.(mean(y),sum(y),.N)]
# 
# iv&woe for train;
cols = names(xy)
iv_tables_for_all(train,cols[-c(1:2,6)],'y') -> iv_tabs
train_woe <- apply2woe(train,x = cols,iv_tables_list = iv_tabs) 
test_woe <- apply2woe(test,x= cols,iv_tables_list = iv_tabs)
monocols = woe_monotone_detect(iv_tabs)
sel_cols = iv_tabs$ivs[iv_tabs$ivs >0.2] %>% names() %>% intersect(monocols) %>%
  union(c('性别',"年龄段",'在网时长'),.)

#----------------------------model---------------------------------------------#
# #---------lasso--------
# monocols1 = woe_monotone_detect(iv_tabs$woe);iv_tabs$woe[monocols1];iv_tabs$ivs[monocols1] # 过滤掉WOE不单调的变量
# monocols_xy = woe_monotone_detect(iv_tabs$woe[xy_cols]);iv_tabs$woe[monocols_xy];iv_tabs$ivs[monocols_xy]
library(biglars)
lasso = biglars.fit(x=train_woe[,sel_cols,with = F] %>% as.matrix(),
                    y=train_woe$y,
                    type ='lasso',removeColumns =T)
coef(lasso)
(lasso$coefficients !=0) %>% apply(1,sum)
(lasso$coefficients[13,] != 0) %>% sum() # 选择15个变量

# cols =
#   c(which(lasso$coefficients[14,] != 0)  %>% names() %>%
#       '['(-1)) %>% c(c('zm_jianmian','real_mianya_ratio','近3个月总查询笔数'),.)

#----------------------------model---------------------------------------------#
rm(forms,GLM,GLM1,y_fit_test,wb,results)
forms <- c(which(lasso$coefficients[10,] != 0)  %>% names() %>% '['(-1)) %>% 
  # c('性别','在网时长',.) %>%
  # setdiff(c('芝麻信用等级','按身份证号查询_近6个月在非银机构_其他申请机构数__1',
  #           '按身份证号查询_近12个月申请线上现金分期的机构数')) %>%
  paste0(collapse = ' + ') %>% paste('y ~ ',.,sep='') %>% as.formula()
# y ~ 性别 + 年龄段 + 在网时长 + 查询机构数_xy + 近1个月贷款机构成功扣款笔数_xy + 
#   按身份证号查询_近12个月在非银机构_其他申请机构数__1 + 按身份证号查询_近12个月申请线上小额现金贷的机构数 + 
#   查询网络贷款类机构数_xy + 按身份证号查询_近12个月申请线下消费分期的机构数
# glm begein
GLM1 <- glm(formula = forms,family = binomial(link = "logit"),data = train_woe);
GLM1 %>% car::vif();GLM1$coefficients
# GLM <- step(GLM1,direction = 'both');
GLM=GLM1
GLM %>% car::vif() # 先做lasso筛选变量，再做逐步回归效果好，剔除多重共线性，提高计算效率
GLM$coefficients;GLM$coefficients %>% length()
# GLM1$coefficients;
# GLM1 %>% car::vif();GLM1$coefficients %>% length()
InformationValue::AUROC(GLM$y,GLM$fitted.values)
InformationValue::Concordance(GLM$y,GLM$fitted.values)
InformationValue::ks_stat(GLM$y,GLM$fitted.values) 
InformationValue::ks_plot(GLM$y,GLM$fitted.values)
InformationValue::plotROC(GLM$y,GLM$fitted.values)
GLM$coefficients[-1] %>% names()
iv_tabs$woe[GLM$coefficients[-1] %>% names()]
# 测试集表现
# sqlscore::score_expression(GLM)
# sqlscore::score_expression(GLM)
# test_woe[,eval(sqlscore::score_expression(GLM))] -> y_fit_test_2

y_fit_test <- predict(GLM,test_woe,type = 'response')
InformationValue::ks_stat(test_woe$y,y_fit_test)
# save model
# zm_taochan_daizhong_m = GLM
# y_fit_test <- predict(zm_taochan_daizhong_m,test_woe,type = 'response')
# InformationValue::ks_stat(test_woe$y,y_fit_test)

# rm(score_train,score_test)
####################################
# 校准评分
# 600 20/1  650 40/1 pdo = 50
# 600 = A + B * log(30/1)
# 650 = A + B * log(60/1)

score_train = scorescale_f(GLM$fitted.values,base_odds = 20/1.0,pdo=50,base_points = 600)$score
score_test = scorescale_f(y_fit_test,base_odds = 20/1.0,pdo=50,base_points = 600)$score
strategy_f(test_woe$y,score_test,bins = 20)
strategy_f(train_woe$y,score_train,bins=20)
InformationValue::ks_stat(train_woe$y,score_train)
InformationValue::ks_stat(test_woe$y,score_test)
####################################
rm(results)
choosed_var = GLM$coefficients[-1] %>% names()
results = list()
results$xy = xy
results$woes = rbindlist(iv_tabs[['woes']][choosed_var])
results$train  = train
results$test   = test
results$train_stategy <- strategy_f(train_woe$y,score_train) 
results$train_score <- data.table(train_woe,score_train)
results$test_stategy <- strategy_f(test_woe$y,score_test)
# results$OOT_stategy <- strategy_f(OOT_woe$y,score_oot)
results$test_score <- data.table(test_woe,score_test)
# results$OOT_score <- data.table(OOT_woe,score_test)
results$test <- test
# results$OOT <- OOT
results$vif <- data.table(x = names(car::vif(GLM)),vif = car::vif(GLM))
results$iv <- data.table(x = names(iv_tabs$ivs[names(car::vif(GLM))]),iv = iv_tabs$ivs[names(car::vif(GLM))])
library(broom);tidy(GLM)
results$model <- data.table(tidy(GLM),vif=car::vif(GLM)[tidy(GLM)[['term']]])
results$KS_auc = data.table(train_ks = InformationValue::ks_stat(train$y,GLM$fitted.values),
                            test_ks = InformationValue::ks_stat(test_woe$y,score_test),
                            train_auc = InformationValue::AUROC(train$y,GLM$fitted.values),
                            test_auc = InformationValue::AUROC(test$y,score_test),
                            OOT = NA)
# 生成sql代码
iv_tabs$woe[GLM$coefficients %>% names() %>% '['(-1)] %>% woe2sql %>% 
  cat(file = 'D:\\租东东\\三方数据测试20181109\\百融\\租无忧数据测试结果\\01测试结果详情\\贷前反欺诈模型_brxynozm\\贷前反欺诈模型_brxynozm_woe2sql.txt')
sqlscore::score_expression(GLM) %>% capture.output() %>% paste0(collapse = ' \n') %>%
  cat(file = 'D:\\租东东\\三方数据测试20181109\\百融\\租无忧数据测试结果\\01测试结果详情\\贷前反欺诈模型_brxynozm\\贷前反欺诈模型_brxynozm_modelsql.txt')
iv_tabs$woe[GLM$coefficients %>% names() %>% '['(-1)] %>% score2sql(.,GLM) %>% 
  cat(file = 'D:\\租东东\\三方数据测试20181109\\百融\\租无忧数据测试结果\\01测试结果详情\\贷前反欺诈模型_brxynozm\\贷前反欺诈模型_brxynozm_scoresql.txt')
scorescale_f(GLM$fitted.values,base_odds = 20/1.0,pdo=50,base_points = 600)$scorescale2sql %>%
  cat(file = 'D:\\租东东\\三方数据测试20181109\\百融\\租无忧数据测试结果\\01测试结果详情\\贷前反欺诈模型_brxynozm\\贷前反欺诈模型_brxynozm_scorescale2sql.txt')
iv_tabs$woe[GLM$coefficients %>% names() %>% '['(-1)] %>% 
  final_score2sql(.,GLM,table_name = "xxxxxx",base_odds = 20/1.0,pdo = 50,base_points = 600) %>% 
  cat(file = 'D:\\租东东\\三方数据测试20181109\\百融\\租无忧数据测试结果\\01测试结果详情\\贷前反欺诈模型_brxynozm\\贷前反欺诈模型_brxynozm_final_score2sql.txt')

save(results,file = 'D:\\租东东\\三方数据测试20181109\\百融\\租无忧数据测试结果\\01测试结果详情\\贷前反欺诈模型_brxynozm\\贷前反欺诈模型_brxynozm.rdata')
# 中间结果输出到excel表

library(openxlsx)
rm(wb)
wb <- createWorkbook()
for(x in names(results)){
  print(x)
  addWorksheet(wb, x)
  writeData(wb,results[[x]],sheet = x,keepNA = T)
}
saveWorkbook(wb, "D:\\租东东\\三方数据测试20181109\\百融\\租无忧数据测试结果\\01测试结果详情\\贷前反欺诈模型_brxynozm\\贷前反欺诈模型_brxynozm.xlsx", overwrite = TRUE)



##################################################################################
#----贷前有芝麻分模型：新颜+百融-----
##################################################################################

# lasso筛选变量后，逐步回归
# glm
# "y ~ zm_jianmian + zmscore + real_mianya_ratio + 
# 贷款行为分 + 近3个月总查询笔数  + 贷款行为置信度 + sex " %>% as.formula() -> forms
# paste0(cols_xybr_score_zm_dz,collapse = ' + ') %>% paste('y~',.,sep='')  %>%  as.formula() -> forms 
rm(forms,GLM,GLM1,y_fit_test,wb,results)
forms <-
  y ~ zmscore + 
  贷款行为分 + 贷款行为置信度 + 按身份证号查询_近12个月申请线上小额现金贷的机构数 + 
  按身份证号查询_近12个月在非银机构_持牌融资租赁机构申请机构数 + 
  按身份证号查询_近6个月在非银机构申请最小间隔天数 + 
  按身份证号查询_近6个月在非银机构夜间申请机构数
# glm begein
GLM1 <- glm(formula = forms,family = binomial(link = "logit"),data = train_woe);
GLM1 %>% car::vif()
# GLM <- step(GLM1,direction = 'both');
GLM=GLM1
GLM %>% car::vif() # 先做lasso筛选变量，再做逐步回归效果好，剔除多重共线性，提高计算效率
GLM$coefficients;GLM$coefficients %>% length()
# GLM1$coefficients;
# GLM1 %>% car::vif();GLM1$coefficients %>% length()
InformationValue::AUROC(GLM$y,GLM$fitted.values)
InformationValue::Concordance(GLM$y,GLM$fitted.values)
InformationValue::ks_stat(GLM$y,GLM$fitted.values) 
InformationValue::ks_plot(GLM$y,GLM$fitted.values)
InformationValue::plotROC(GLM$y,GLM$fitted.values)
GLM$coefficients[-1] %>% names()
iv_tabs$woe[GLM$coefficients[-1] %>% names()]
# 测试集表现
# sqlscore::score_expression(GLM)
# sqlscore::score_expression(GLM)
# test_woe[,eval(sqlscore::score_expression(GLM))] -> y_fit_test_2

y_fit_test <- predict(GLM,test_woe,type = 'response')
InformationValue::ks_stat(test_woe$y,y_fit_test)
# save model
# zm_taochan_daizhong_m = GLM
# y_fit_test <- predict(zm_taochan_daizhong_m,test_woe,type = 'response')
# InformationValue::ks_stat(test_woe$y,y_fit_test)

# rm(score_train,score_test)
####################################
# 校准评分
# 600 20/1  650 40/1 pdo = 50
# 600 = A + B * log(30/1)
# 650 = A + B * log(60/1)

score_train = scorescale_f(GLM$fitted.values,base_odds = 20/1.0,pdo=50,base_points = 600)$score
score_test = scorescale_f(y_fit_test,base_odds = 20/1.0,pdo=50,base_points = 600)$score
strategy_f(test_woe$y,score_test,bins = 20)
strategy_f(train_woe$y,score_train,bins=20)
InformationValue::ks_stat(train_woe$y,score_train)
InformationValue::ks_stat(test_woe$y,score_test)
####################################
rm(results)
choosed_var = GLM$coefficients[-1] %>% names()
results = list()
results$xy = xy
results$woes = rbindlist(iv_tabs[['woe']][choosed_var])
results$train  = train
results$test   = test
results$train_stategy <- strategy_f(train_woe$y,score_train) 
results$train_score <- data.table(train_woe,score_train)
results$test_stategy <- strategy_f(test_woe$y,score_test)
# results$OOT_stategy <- strategy_f(OOT_woe$y,score_oot)
results$test_score <- data.table(test_woe,score_test)
# results$OOT_score <- data.table(OOT_woe,score_test)
results$test <- test
# results$OOT <- OOT
results$vif <- data.table(x = names(car::vif(GLM)),vif = car::vif(GLM))
results$iv <- data.table(x = names(iv_tabs$ivs[names(car::vif(GLM))]),iv = iv_tabs$ivs[names(car::vif(GLM))])
library(broom);tidy(GLM)
results$model <- data.table(tidy(GLM),vif=car::vif(GLM)[tidy(GLM)[['term']]])
results$KS_auc = data.table(train_ks = InformationValue::ks_stat(train$y,GLM$fitted.values),
                            test_ks = InformationValue::ks_stat(test_woe$y,score_test),
                            train_auc = InformationValue::AUROC(train$y,GLM$fitted.values),
                            test_auc = InformationValue::AUROC(test$y,score_test),
                            OOT = NA)
# 生成sql代码
iv_tabs$woe[GLM$coefficients %>% names() %>% '['(-1)] %>% woe2sql %>% 
  cat(file = 'D:\\租东东\\三方数据测试20181109\\百融\\租无忧数据测试结果\\01测试结果详情\\贷前反欺诈模型_brxyzm\\贷前反欺诈模型_brxyzm_woe2sql.txt')
sqlscore::score_expression(GLM) %>% capture.output() %>% paste0(collapse = ' \n') %>%
  cat(file = 'D:\\租东东\\三方数据测试20181109\\百融\\租无忧数据测试结果\\01测试结果详情\\贷前反欺诈模型_brxyzm\\贷前反欺诈模型_brxyzm_modelsql.txt')
iv_tabs$woe[GLM$coefficients %>% names() %>% '['(-1)] %>% score2sql(.,GLM) %>% 
  cat(file = 'D:\\租东东\\三方数据测试20181109\\百融\\租无忧数据测试结果\\01测试结果详情\\贷前反欺诈模型_brxyzm\\贷前反欺诈模型_brxyzm_scoresql.txt')
scorescale2sql =   scorescale_f(GLM$fitted.values,base_odds = 20/1.0,pdo=50,base_points = 600)$scorescale2sql %>%
  cat(file = 'D:\\租东东\\三方数据测试20181109\\百融\\租无忧数据测试结果\\01测试结果详情\\贷前反欺诈模型_brxyzm\\贷前反欺诈模型_brxyzm_scorescale2sql.txt')

save(results,file = 'D:\\租东东\\三方数据测试20181109\\百融\\租无忧数据测试结果\\01测试结果详情\\贷前反欺诈模型_brxyzm\\贷前反欺诈模型_brxyzm.rdata')
# 中间结果输出到excel表

library(openxlsx)
rm(wb)
wb <- createWorkbook()
for(x in names(results)){
  print(x)
  addWorksheet(wb, x)
  writeData(wb,results[[x]],sheet = x,keepNA = T)
}
saveWorkbook(wb, "D:\\租东东\\三方数据测试20181109\\百融\\租无忧数据测试结果\\01测试结果详情\\贷前反欺诈模型_brxyzm\\贷前反欺诈模型_brxyzm.xlsx", overwrite = TRUE)


##################################################################################
#-------贷前无芝麻分模型：新颜+百融-----
##################################################################################

# br_jd %>% colnames()
rm(train,test,rs,xy)
library(data.table);xy = setDT(br_xy)
setnames(xy,old = 'default_5','y')
xy[,.(sum(y),mean(y),.N)] #查看事件分布
rt = xy %>% split_df(.,0.80)
train = rt$train;
test  = rt$test;
train %>% nrow();test %>% nrow();test[,.(mean(y),sum(y),.N)];train[,.(mean(y),sum(y),.N)]
# 
# iv&woe for train;
cols = names(xy)
iv_tables_for_all(train,cols[-c(1:2,6)],'y') -> iv_tabs
train_woe <- apply2woe(train,x = cols,iv_tables_list = iv_tabs) 
test_woe <- apply2woe(test,x= cols,iv_tables_list = iv_tabs)
monocols = woe_monotone_detect(iv_tabs)
sel_cols = iv_tabs$ivs[iv_tabs$ivs >0.2] %>% names() %>% intersect(monocols)  %>% union(c('年龄段','在网时长','性别'))

#----------------------------model---------------------------------------------#
# #---------lasso--------
# monocols1 = woe_monotone_detect(iv_tabs$woe);iv_tabs$woe[monocols1];iv_tabs$ivs[monocols1] # 过滤掉WOE不单调的变量
# monocols_xy = woe_monotone_detect(iv_tabs$woe[xy_cols]);iv_tabs$woe[monocols_xy];iv_tabs$ivs[monocols_xy]
library(biglars)
lasso = biglars.fit(x=train_woe[,sel_cols,with = F] %>% as.matrix(),
                    y=train_woe$y,
                    type ='lasso',removeColumns =T)
coef(lasso)
(lasso$coefficients !=0) %>% apply(1,sum)
(lasso$coefficients[13,] != 0) %>% sum() # 选择15个变量

# cols =
#   c(which(lasso$coefficients[14,] != 0)  %>% names() %>%
#       '['(-1)) %>% c(c('zm_jianmian','real_mianya_ratio','近3个月总查询笔数'),.)

#----------------------------model---------------------------------------------#
rm(forms,GLM,GLM1,y_fit_test,wb,results)
forms <- c(which(lasso$coefficients[10,] != 0)  %>% names() %>% '['(-1)) %>% 
  # c('性别','在网时长',.) %>%
  # setdiff(c('芝麻信用等级','按身份证号查询_近6个月在非银机构_其他申请机构数__1',
  #           '按身份证号查询_近12个月申请线上现金分期的机构数')) %>%
  paste0(collapse = ' + ') %>% paste('y ~ ',.,sep='') %>% as.formula()
# forms <-
# y ~ 性别 + 在网时长 + 按身份证号查询_近12个月在非银机构_现金类分期申请机构数 + 
#   按身份证号查询_近12个月在非银机构_其他申请次数__1 + 按身份证号查询_近6个月在非银机构_其他申请机构数 + 
#   按身份证号查询_近12个月申请线下消费分期的机构数 + 年龄段 + 
#   按身份证号查询_近7天在非银机构_现金类分期机构申请次数 + 按身份证号查询_近7天申请线上小额现金贷的机构数
# glm begein
GLM1 <- glm(formula = forms,family = binomial(link = "logit"),data = train_woe);
GLM1 %>% car::vif();GLM1$coefficients
# GLM <- step(GLM1,direction = 'both');
GLM=GLM1
GLM %>% car::vif() # 先做lasso筛选变量，再做逐步回归效果好，剔除多重共线性，提高计算效率
GLM$coefficients;GLM$coefficients %>% length()
# GLM1$coefficients;
# GLM1 %>% car::vif();GLM1$coefficients %>% length()
InformationValue::AUROC(GLM$y,GLM$fitted.values)
InformationValue::Concordance(GLM$y,GLM$fitted.values)
InformationValue::ks_stat(GLM$y,GLM$fitted.values) 
InformationValue::ks_plot(GLM$y,GLM$fitted.values)
InformationValue::plotROC(GLM$y,GLM$fitted.values)
GLM$coefficients[-1] %>% names()
iv_tabs$woe[GLM$coefficients[-1] %>% names()]
# 测试集表现
# sqlscore::score_expression(GLM)
# sqlscore::score_expression(GLM)
# test_woe[,eval(sqlscore::score_expression(GLM))] -> y_fit_test_2

y_fit_test <- predict(GLM,test_woe,type = 'response')
InformationValue::ks_stat(test_woe$y,y_fit_test)
# save model
# zm_taochan_daizhong_m = GLM
# y_fit_test <- predict(zm_taochan_daizhong_m,test_woe,type = 'response')
# InformationValue::ks_stat(test_woe$y,y_fit_test)

# rm(score_train,score_test)
####################################
# 校准评分
# 600 20/1  650 40/1 pdo = 50
# 600 = A + B * log(30/1)
# 650 = A + B * log(60/1)

score_train = scorescale_f(GLM$fitted.values,base_odds = 20/1.0,pdo=50,base_points = 600)$score
score_test = scorescale_f(y_fit_test,base_odds = 20/1.0,pdo=50,base_points = 600)$score
strategy_f(test_woe$y,score_test,bins = 20)
strategy_f(train_woe$y,score_train,bins=20)
InformationValue::ks_stat(train_woe$y,score_train)
InformationValue::ks_stat(test_woe$y,score_test)
####################################
rm(results)
choosed_var = GLM$coefficients[-1] %>% names()
results = list()
results$xy = xy
results$woes = rbindlist(iv_tabs[['woes']][choosed_var])
results$train  = train
results$test   = test
results$train_stategy <- strategy_f(train_woe$y,score_train) 
results$train_score <- data.table(train_woe,score_train)
results$test_stategy <- strategy_f(test_woe$y,score_test)
# results$OOT_stategy <- strategy_f(OOT_woe$y,score_oot)
results$test_score <- data.table(test_woe,score_test)
# results$OOT_score <- data.table(OOT_woe,score_test)
results$test <- test
# results$OOT <- OOT
results$vif <- data.table(x = names(car::vif(GLM)),vif = car::vif(GLM))
results$iv <- data.table(x = names(iv_tabs$ivs[names(car::vif(GLM))]),iv = iv_tabs$ivs[names(car::vif(GLM))])
library(broom);tidy(GLM)
results$model <- data.table(tidy(GLM),vif=car::vif(GLM)[tidy(GLM)[['term']]])
results$KS_auc = data.table(train_ks = InformationValue::ks_stat(train$y,GLM$fitted.values),
                            test_ks = InformationValue::ks_stat(test_woe$y,score_test),
                            train_auc = InformationValue::AUROC(train$y,GLM$fitted.values),
                            test_auc = InformationValue::AUROC(test$y,score_test),
                            OOT = NA)
# 生成sql代码
iv_tabs$woe[GLM$coefficients %>% names() %>% '['(-1)] %>% woe2sql %>% 
  cat(file = 'D:\\租东东\\三方数据测试20181109\\百融\\租无忧数据测试结果\\01测试结果详情\\贷前反欺诈模型_brxynozm\\贷前反欺诈模型_brxynozm_woe2sql.txt')
sqlscore::score_expression(GLM) %>% capture.output() %>% paste0(collapse = ' \n') %>%
  cat(file = 'D:\\租东东\\三方数据测试20181109\\百融\\租无忧数据测试结果\\01测试结果详情\\贷前反欺诈模型_brxynozm\\贷前反欺诈模型_brxynozm_modelsql.txt')
iv_tabs$woe[GLM$coefficients %>% names() %>% '['(-1)] %>% score2sql(.,GLM) %>% 
  cat(file = 'D:\\租东东\\三方数据测试20181109\\百融\\租无忧数据测试结果\\01测试结果详情\\贷前反欺诈模型_brxynozm\\贷前反欺诈模型_brxynozm_scoresql.txt')
scorescale_f(GLM$fitted.values,base_odds = 20/1.0,pdo=50,base_points = 600)$scorescale2sql %>%
  cat(file = 'D:\\租东东\\三方数据测试20181109\\百融\\租无忧数据测试结果\\01测试结果详情\\贷前反欺诈模型_brxynozm\\贷前反欺诈模型_brxynozm_scorescale2sql.txt')
iv_tabs$woe[GLM$coefficients %>% names() %>% '['(-1)] %>% 
  final_score2sql(.,GLM,table_name = "xxxxxx",base_odds = 20/1.0,pdo = 50,base_points = 600) %>% 
  cat(file = 'D:\\租东东\\三方数据测试20181109\\百融\\租无忧数据测试结果\\01测试结果详情\\贷前反欺诈模型_brxynozm\\贷前反欺诈模型_brxynozm_final_score2sql.txt')

save(results,file = 'D:\\租东东\\三方数据测试20181109\\百融\\租无忧数据测试结果\\01测试结果详情\\贷前反欺诈模型_brxynozm\\贷前反欺诈模型_brxynozm.rdata')
# 中间结果输出到excel表

library(openxlsx)
rm(wb)
wb <- createWorkbook()
for(x in names(results)){
  print(x)
  addWorksheet(wb, x)
  writeData(wb,results[[x]],sheet = x,keepNA = T)
}
saveWorkbook(wb, "D:\\租东东\\三方数据测试20181109\\百融\\租无忧数据测试结果\\01测试结果详情\\贷前反欺诈模型_brxynozm\\贷前反欺诈模型_brxynozm.xlsx", overwrite = TRUE)



##################################################################################
#----贷中有芝麻分模型：新颜+百融------
##################################################################################
rm(forms,GLM,GLM1,y_fit_test,wb,results)
# br_jd %>% colnames()
rm(train,test,rs,xy)
library(data.table);xy = setDT(br_xy)
setnames(xy,old = 'default_5','y')
xy[,.(sum(y),mean(y),.N)] #查看事件分布
rt = xy %>% split_df(.,0.80)
train = rt$train;
test  = rt$test;
train %>% nrow();test %>% nrow();test[,.(mean(y),sum(y),.N)];train[,.(mean(y),sum(y),.N)]
# 
# iv&woe for train;
cols = names(xy)
iv_tables_for_all(train,cols[-c(1:2)],'y') -> iv_tabs
train_woe <- apply2woe(train,x = cols,iv_tables_list = iv_tabs) 
test_woe <- apply2woe(test,x= cols,iv_tables_list = iv_tabs)
monocols = woe_monotone_detect(iv_tabs)
sel_cols = iv_tabs$ivs[iv_tabs$ivs >0.2] %>% names() %>% intersect(monocols)  %>% union(c('年龄段','在网时长','性别','芝麻信用等级'))

#----------------------------model---------------------------------------------#
# #---------lasso--------
# monocols1 = woe_monotone_detect(iv_tabs$woe);iv_tabs$woe[monocols1];iv_tabs$ivs[monocols1] # 过滤掉WOE不单调的变量
# monocols_xy = woe_monotone_detect(iv_tabs$woe[xy_cols]);iv_tabs$woe[monocols_xy];iv_tabs$ivs[monocols_xy]
library(biglars)
lasso = biglars.fit(x=train_woe[,sel_cols,with = F] %>% as.matrix(),
                    y=train_woe$y,
                    type ='lasso',removeColumns =T)
coef(lasso)
(lasso$coefficients !=0) %>% apply(1,sum)
(lasso$coefficients[13,] != 0) %>% sum() # 选择15个变量

# cols =
#   c(which(lasso$coefficients[14,] != 0)  %>% names() %>%
#       '['(-1)) %>% c(c('zm_jianmian','real_mianya_ratio','近3个月总查询笔数'),.)

#----------------------------model---------------------------------------------#
rm(forms,GLM,GLM1,y_fit_test,wb,results)
forms <- c(which(lasso$coefficients[8,] != 0)  %>% names() %>% '['(-1)) %>% 
  union(c('性别','在网时长'))  %>%
  # setdiff(c('芝麻信用等级','按身份证号查询_近6个月在非银机构_其他申请机构数__1',
  #           '按身份证号查询_近12个月申请线上现金分期的机构数')) %>%
  paste0(collapse = ' + ') %>% paste('y ~ ',.,sep='') %>% as.formula()
# forms <-
# y ~ 查询机构数_xy + 近1个月贷款机构成功扣款笔数_xy + 按身份证号查询_近12个月在非银机构_其他申请机构数__1 + 
#   按身份证号查询_近12个月申请线上小额现金贷的机构数 + 年龄段 + 
#   在网时长 + 芝麻信用等级 + 性别
# glm begein
GLM1 <- glm(formula = forms,family = binomial(link = "logit"),data = train_woe);
GLM1 %>% car::vif();GLM1$coefficients
# GLM <- step(GLM1,direction = 'both');
GLM=GLM1
GLM %>% car::vif() # 先做lasso筛选变量，再做逐步回归效果好，剔除多重共线性，提高计算效率
GLM$coefficients;GLM$coefficients %>% length()
# GLM1$coefficients;
# GLM1 %>% car::vif();GLM1$coefficients %>% length()
InformationValue::AUROC(GLM$y,GLM$fitted.values)
InformationValue::Concordance(GLM$y,GLM$fitted.values)
InformationValue::ks_stat(GLM$y,GLM$fitted.values) 
InformationValue::ks_plot(GLM$y,GLM$fitted.values)
InformationValue::plotROC(GLM$y,GLM$fitted.values)
GLM$coefficients[-1] %>% names()
iv_tabs$woe[GLM$coefficients[-1] %>% names()]
# 测试集表现
# sqlscore::score_expression(GLM)
# sqlscore::score_expression(GLM)
# test_woe[,eval(sqlscore::score_expression(GLM))] -> y_fit_test_2

y_fit_test <- predict(GLM,test_woe,type = 'response')
InformationValue::ks_stat(test_woe$y,y_fit_test)
# save model
# zm_taochan_daizhong_m = GLM
# y_fit_test <- predict(zm_taochan_daizhong_m,test_woe,type = 'response')
# InformationValue::ks_stat(test_woe$y,y_fit_test)

# rm(score_train,score_test)
####################################
# 校准评分
# 600 20/1  650 40/1 pdo = 50
# 600 = A + B * log(30/1)
# 650 = A + B * log(60/1)

score_train = scorescale_f(GLM$fitted.values,base_odds = 20/1.0,pdo=50,base_points = 600)$score
score_test = scorescale_f(y_fit_test,base_odds = 20/1.0,pdo=50,base_points = 600)$score
strategy_f(test_woe$y,score_test,bins = 20)
strategy_f(train_woe$y,score_train,bins=20)
InformationValue::ks_stat(train_woe$y,score_train)
InformationValue::ks_stat(test_woe$y,score_test)
####################################
rm(results)
choosed_var = GLM$coefficients[-1] %>% names()
results = list()
results$xy = xy
results$woes = rbindlist(iv_tabs[['woes']][choosed_var])
results$train  = train
results$test   = test
results$train_stategy <- strategy_f(train_woe$y,score_train) 
results$train_score <- data.table(train_woe,score_train)
results$test_stategy <- strategy_f(test_woe$y,score_test)
# results$OOT_stategy <- strategy_f(OOT_woe$y,score_oot)
results$test_score <- data.table(test_woe,score_test)
# results$OOT_score <- data.table(OOT_woe,score_test)
results$test <- test
# results$OOT <- OOT
results$vif <- data.table(x = names(car::vif(GLM)),vif = car::vif(GLM))
results$iv <- data.table(x = names(iv_tabs$ivs[names(car::vif(GLM))]),iv = iv_tabs$ivs[names(car::vif(GLM))])
library(broom);tidy(GLM)
results$model <- data.table(tidy(GLM),vif=car::vif(GLM)[tidy(GLM)[['term']]])
results$KS_auc = data.table(train_ks = InformationValue::ks_stat(train$y,GLM$fitted.values),
                            test_ks = InformationValue::ks_stat(test_woe$y,score_test),
                            train_auc = InformationValue::AUROC(train$y,GLM$fitted.values),
                            test_auc = InformationValue::AUROC(test$y,score_test),
                            OOT = NA)
# 生成sql代码
iv_tabs$woe[GLM$coefficients %>% names() %>% '['(-1)] %>% woe2sql %>% 
  cat(file = 'D:\\租东东\\三方数据测试20181109\\百融\\租无忧数据测试结果\\01测试结果详情\\贷前反欺诈模型_brxyzm\\贷前反欺诈模型_brxyzm_woe2sql.txt')
sqlscore::score_expression(GLM) %>% capture.output() %>% paste0(collapse = ' \n') %>%
  cat(file = 'D:\\租东东\\三方数据测试20181109\\百融\\租无忧数据测试结果\\01测试结果详情\\贷前反欺诈模型_brxyzm\\贷前反欺诈模型_brxyzm_modelsql.txt')
iv_tabs$woe[GLM$coefficients %>% names() %>% '['(-1)] %>% score2sql(.,GLM) %>% 
  cat(file = 'D:\\租东东\\三方数据测试20181109\\百融\\租无忧数据测试结果\\01测试结果详情\\贷前反欺诈模型_brxyzm\\贷前反欺诈模型_brxyzm_scoresql.txt')
scorescale_f(GLM$fitted.values,base_odds = 20/1.0,pdo=50,base_points = 600)$scorescale2sql %>%
  cat(file = 'D:\\租东东\\三方数据测试20181109\\百融\\租无忧数据测试结果\\01测试结果详情\\贷前反欺诈模型_brxyzm\\贷前反欺诈模型_brxyzm_scorescale2sql.txt')
iv_tabs$woe[GLM$coefficients %>% names() %>% '['(-1)] %>% 
  final_score2sql(.,GLM,table_name = "xxxxxx",base_odds = 20/1.0,pdo = 50,base_points = 600) %>% 
  cat(file = 'D:\\租东东\\三方数据测试20181109\\百融\\租无忧数据测试结果\\01测试结果详情\\贷前反欺诈模型_brxyzm\\贷前反欺诈模型_brxyzm_final_score2sql.txt')

save(results,file = 'D:\\租东东\\三方数据测试20181109\\百融\\租无忧数据测试结果\\01测试结果详情\\贷前反欺诈模型_brxyzm\\贷前反欺诈模型_brxyzm.rdata')
# 中间结果输出到excel表

library(openxlsx)
rm(wb)
wb <- createWorkbook()
for(x in names(results)){
  print(x)
  addWorksheet(wb, x)
  writeData(wb,results[[x]],sheet = x,keepNA = T)
}
saveWorkbook(wb, "D:\\租东东\\三方数据测试20181109\\百融\\租无忧数据测试结果\\01测试结果详情\\贷前反欺诈模型_brxyzm\\贷前反欺诈模型_brxyzm.xlsx", overwrite = TRUE)

