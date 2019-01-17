setwd('D:\\租东东\\三方数据测试20181109\\新颜\\新颜模型20181129\\')
# setwd('D:\\租东东\\三方数据测试20181109\\新颜\\北京租无忧科技有限公司 测试报告\\')
#----------------------------------------------
# 读入数据
library(data.table);library(readxl);library(dplyr);library(car)
# 读入自由数据XY
model_xy = readxl::read_xlsx('D:\\租东东\\三方数据测试20181109\\新颜\\新颜模型20181129\\model_xy_20181129.xlsx',
                             col_types = c(rep('guess',16),rep('numeric',3),'guess',rep('numeric',5)))
model_xy$default_3 = model_xy$当期逾期天数 < 3
model_xy$default_5 = model_xy$当期逾期天数 < 5
model_xy$年龄段 <- ((model_xy$订单日期 %>% as.Date.character() - 
       (model_xy$身份证号 %>% substr(7,15) %>% as.Date.character(format = '%Y%m%d'))) / 365) %>% 
  as.numeric() %>% cut(c(-Inf,23,Inf)) %>% as.character()


# # 读入新颜测试
# dir()
# # 读入"新颜科技_场景雷达_消费分期（回溯）_北京租无忧科技有限公司_20181109.xlsx"  
# cjld_xffq = readxl::read_xlsx('新颜科技_场景雷达_消费分期（回溯）_北京租无忧科技有限公司_20181109.xlsx',sheet = 2,skip=2,na = '/')
# # all = model_xy %>% left_join(cjld_xffq,by = c("idcard" = "身份证号"), suffix = c("", "_cjld_xffq"))
# # 读入"新颜科技_场景雷达_小额分期（回溯）_北京租无忧科技有限公司_20181109.xlsx"   
# cjld_xefq = readxl::read_xlsx("新颜科技_场景雷达_小额分期（回溯）_北京租无忧科技有限公司_20181109.xlsx",sheet = 2,skip=2,na = '/')
# # 读入"新颜科技_场景雷达_小额网贷（回溯）_北京租无忧科技有限公司_20181109.xlsx"       
# cjld_xewd = readxl::read_xlsx("新颜科技_场景雷达_小额网贷（回溯）_北京租无忧科技有限公司_20181109.xlsx",sheet = 2,skip=2,na = '/')
# # 读入"新颜科技_场景雷达_信用卡代偿（回溯）_北京租无忧科技有限公司_20181110.xlsx"     
# cjld_xykdc = readxl::read_xlsx("新颜科技_场景雷达_信用卡代偿（回溯）_北京租无忧科技有限公司_20181110.xlsx",sheet = 2,skip=2,na = '/')
# # 读入"新颜科技_场景雷达_中大额分期（回溯）_北京租无忧科技有限公司_20181110.xlsx"     
# cjld_zdefq = readxl::read_xlsx("新颜科技_场景雷达_中大额分期（回溯）_北京租无忧科技有限公司_20181110.xlsx",sheet = 2,skip=2,na = '/')
# # 读入"新颜科技_负面探针_全景探针（回溯）_北京租无忧科技有限公司_20181109.xlsx"       
# fmtz_qjtz = readxl::read_xlsx("新颜科技_负面探针_全景探针（回溯）_北京租无忧科技有限公司_20181109.xlsx",sheet = 2,skip=3,na = '-')
# # 读入"新颜科技_信用档案_贷后雷达_标准版（回溯）_北京租无忧科技有限公司_20181110.xlsx"
# xyda_dhld = readxl::read_xlsx("新颜科技_信用档案_贷后雷达_标准版（回溯）_北京租无忧科技有限公司_20181110.xlsx",sheet = 2,skip=2,na = '/')
# # 读入"新颜科技_信用雷达_全景档案（回溯）_北京租无忧科技有限公司_20181109.xlsx"       
# xyld_qjda_yq = readxl::read_xlsx("新颜科技_信用雷达_全景档案（回溯）_北京租无忧科技有限公司_20181109.xlsx",sheet = 2,skip=3,na = '/')
# xyld_qjda_gz = readxl::read_xlsx("新颜科技_信用雷达_全景档案（回溯）_北京租无忧科技有限公司_20181109.xlsx",sheet = 3,skip=3,na = '/')

# 读入"新颜科技_信用雷达_全景雷达（回溯）_北京租无忧科技有限公司_20181109.xlsx" 
xyld_qjld_sq = readxl::read_xlsx("D:\\租东东\\三方数据测试20181109\\新颜\\北京租无忧科技有限公司 测试报告\\新颜科技_信用雷达_全景雷达（回溯）_北京租无忧科技有限公司_20181109.xlsx" ,sheet = 2,skip=2,na = '/')
xyld_qjld_xw = readxl::read_xlsx("D:\\租东东\\三方数据测试20181109\\新颜\\北京租无忧科技有限公司 测试报告\\新颜科技_信用雷达_全景雷达（回溯）_北京租无忧科技有限公司_20181109.xlsx" ,sheet = 3,skip=2,na = '/')
xyld_qjld_xy = readxl::read_xlsx("D:\\租东东\\三方数据测试20181109\\新颜\\北京租无忧科技有限公司 测试报告\\新颜科技_信用雷达_全景雷达（回溯）_北京租无忧科技有限公司_20181109.xlsx" ,sheet = 4,skip=2,na = '/')


#######################################################################
#----------接信用雷达-全景雷达-------------------#
#######################################################################
xyld_qjld <- 
  model_xy %>% 
  left_join(xyld_qjld_sq,by = c("身份证号" = "身份证号"), suffix = c("", "_xyld_qjld_sq")) %>%  
  select(-one_of(c('姓名','回溯时间','最近查询时间'))) %>% unique.data.frame() %>% 
  
  left_join(xyld_qjld_xw,by = c("身份证号" = "身份证号"), suffix = c("", "_xyld_qjld_xw")) %>% 
  select(-one_of(c('姓名','回溯时间','最近一次贷款时间'))) %>% unique.data.frame() %>%
  
  left_join(xyld_qjld_xy,by = c("身份证号" = "身份证号"), suffix = c("", "_xyld_qjld_xy")) %>%  
  select(-one_of(c('姓名','回溯时间',"姓名_xyld_qjld_sq"))) %>%
  unique.data.frame()


xyld_qjld <- rename(xyld_qjld,"贷款逾期订单数M0plus" = "贷款逾期订单数（M0+）")  %>%
  `[`(.,c(5:6,26,8:11,16:23,27:63)) %>% unique.data.frame()

# xyld_qjld %>% colnames()
# xy[,cols,with=F] %>% names()
library(data.table);xyld_qjld = setDT(xyld_qjld)
setnames(xyld_qjld,'default_5','y') # 重命名目标变量default-->y
xyld_qjld[,.(sum(y),mean(y),.N)] #查看事件分布

# 拆分训练集，测试集，时间外测试集
xy <- copy(xyld_qjld);
rt = xy %>% split_df(.,0.80,seed = 10086)
train = rt$train;
test  = rt$test;
train %>% nrow();test %>% nrow();test[,.(mean(y),sum(y),.N)];train[,.(mean(y),sum(y),.N)]

# iv&woe for train;
cols = names(xy)
iv_tables_for_all(train,cols[-(1:3)],'y',zero_replace = 0.0001) -> iv_tabs
iv_tabs$ivs
train_woe <- apply2woe(train,x= iv_tabs$ivs %>% names(),iv_tables_list = iv_tabs) 
test_woe <- apply2woe(test,x= iv_tabs$ivs %>% names(),iv_tables_list = iv_tabs) 

# 筛选单调变量
monocols = woe_monotone_detect(iv_tabs$woe)
sel_cols = monocols %>% c("在网时长","性别","年龄段","芝麻信用等级",.) %>% setdiff(c('冻结金额','实际免押比例'))
# sel_cols = monocols %>% c("在网时长","性别","芝麻信用等级",.) %>% `[`(-(4:5))
iv_tabs$woe[sel_cols]
iv_tabs$ivs[sel_cols]
#----------------------------model---------------------------------------------#
#---------lasso--------
library(biglars)
lasso = biglars.fit(x=train_woe[,sel_cols,with = F] %>% as.matrix(),
                    y=train_woe$y,
                    type ='lasso',removeColumns =T)
coef(lasso)
(lasso$coefficients !=0) %>% apply(1,sum)
(lasso$coefficients[13,] != 0) %>% sum() # 选择15个变量
# cols =
#   c(which(lasso$coefficients[14,] != 0)  %>% names() %>% '['(-1)) # 选n个变量

#---------贷前模型有芝麻分-----------
#---------GLM--------
# lasso筛选变量后，逐步回归
# glm
# c(which(lasso$coefficients[10,] != 0)  %>% names() %>% '['(-1))  %>% 
#   paste0(collapse = ' + ') %>% paste('y~',.,sep='') %>% 
#   as.formula() -> forms
forms <- y ~ 在网时长 + 性别 + 年龄段 + 芝麻信用等级 + 总查询次数 + 网络贷款类机构数 + 
  历史贷款机构失败扣款笔数
# glm begein
GLM1 <- glm(formula = forms,family = binomial(link = "logit"),data = train_woe);
# GLM1 %>% car::vif()
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
# y_fit_test <- predict(GLM,test_woe,type = 'response')
# InformationValue::ks_stat(test_woe$y,y_fit_test)
# # save model
zm_taochan_daizhong_m = GLM
y_fit_test <- predict(zm_taochan_daizhong_m,test_woe,type = 'response')
InformationValue::ks_stat(test_woe$y,y_fit_test)


####################################
# 校准评分
# 600 30/1  650 60/1 pdo = 50
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
results$KS_auc = data.table(train_ks = InformationValue::ks_stat(GLM$y,GLM$fitted.values),
                            test_ks = InformationValue::ks_stat(test_woe$y,score_test),
                            train_auc = InformationValue::AUROC(GLM$y,GLM$fitted.values),
                            train_auc = InformationValue::AUROC(GLM$y,GLM$fitted.values),
                            OOT = NA)

# 生成sql代码
iv_tabs$woe[GLM$coefficients %>% names() %>% '['(-1)] %>% woe2sql %>% 
  cat(file = 'D:\\租东东\\三方数据测试20181109\\新颜\\新颜模型20181129\\贷前反欺诈模型20181129\\贷前反欺诈模型20181129_dq_zmxyzw_woe2sql.txt')
sqlscore::score_expression(GLM) %>% capture.output() %>% paste0(collapse = ' \n') %>%
  cat(file = 'D:\\租东东\\三方数据测试20181109\\新颜\\新颜模型20181129\\贷前反欺诈模型20181129\\贷前反欺诈模型20181129_dq_zmxyzw_modelsql.txt')
iv_tabs$woe[GLM$coefficients %>% names() %>% '['(-1)] %>% score2sql(.,GLM) %>% 
  cat(file = 'D:\\租东东\\三方数据测试20181109\\新颜\\新颜模型20181129\\贷前反欺诈模型20181129\\贷前反欺诈模型20181129_dq_zmxyzw_scoresql.txt')
scorescale2sql =   scorescale_f(GLM$fitted.values,base_odds = 20/1.0,pdo=50,base_points = 600)$scorescale2sql %>%
  cat(file = 'D:\\租东东\\三方数据测试20181109\\新颜\\新颜模型20181129\\贷前反欺诈模型20181129\\贷前反欺诈模型20181129_dq_zmxyzw_scorescale2sql.txt')
iv_tabs$woe[GLM$coefficients %>% names() %>% '['(-1)] %>% 
  final_score2sql(.,GLM,table_name = "xxxxxx",base_odds = 20/1.0,pdo = 50,base_points = 600) %>% 
  cat(file = 'D:\\租东东\\三方数据测试20181109\\新颜\\新颜模型20181129\\贷前反欺诈模型20181129\\贷前反欺诈模型20181129_dq_zmxyzw_final_score2sql.txt')

save(results,file = 'D:\\租东东\\三方数据测试20181109\\新颜\\新颜模型20181129\\贷前反欺诈模型20181129\\贷前反欺诈模型20181129_dq_zmxyzw.rdata')

library(openxlsx)
rm(wb)
wb <- createWorkbook()
for(x in names(results)){
  print(x)
  addWorksheet(wb, x)
  writeData(wb,results[[x]],sheet = x,keepNA = T)
}
saveWorkbook(wb, "D:\\租东东\\三方数据测试20181109\\新颜\\新颜模型20181129\\贷前反欺诈模型20181129\\贷前反欺诈模型20181129_dq_zmxyzw.xlsx", overwrite = TRUE)
# openxlsx::write.xlsx()



#---------贷前模型无芝麻分-----------
#---------GLM--------
# lasso筛选变量后，逐步回归
# glm
# c(which(lasso$coefficients[10,] != 0)  %>% names() %>% '['(-1))  %>% 
#   paste0(collapse = ' + ') %>% paste('y~',.,sep='') %>% 
#   as.formula() -> forms
forms <- y ~ 在网时长 + 性别 + 年龄段 + 芝麻信用等级 + 总查询次数 + 网络贷款类机构数 + 
  历史贷款机构失败扣款笔数
# glm begein
GLM1 <- glm(formula = forms,family = binomial(link = "logit"),data = train_woe);
# GLM1 %>% car::vif()
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
# y_fit_test <- predict(GLM,test_woe,type = 'response')
# InformationValue::ks_stat(test_woe$y,y_fit_test)
# # save model
zm_taochan_daizhong_m = GLM
y_fit_test <- predict(zm_taochan_daizhong_m,test_woe,type = 'response')
InformationValue::ks_stat(test_woe$y,y_fit_test)


####################################
# 校准评分
# 600 30/1  650 60/1 pdo = 50
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
results$KS_auc = data.table(train_ks = InformationValue::ks_stat(GLM$y,GLM$fitted.values),
                            test_ks = InformationValue::ks_stat(test_woe$y,score_test),
                            train_auc = InformationValue::AUROC(GLM$y,GLM$fitted.values),
                            train_auc = InformationValue::AUROC(GLM$y,GLM$fitted.values),
                            OOT = NA)

# 生成sql代码
iv_tabs$woe[GLM$coefficients %>% names() %>% '['(-1)] %>% woe2sql %>% 
  cat(file = 'D:\\租东东\\三方数据测试20181109\\新颜\\新颜模型20181129\\贷前反欺诈模型20181129\\贷前反欺诈模型20181129_dq_zmxyzw_woe2sql.txt')
sqlscore::score_expression(GLM) %>% capture.output() %>% paste0(collapse = ' \n') %>%
  cat(file = 'D:\\租东东\\三方数据测试20181109\\新颜\\新颜模型20181129\\贷前反欺诈模型20181129\\贷前反欺诈模型20181129_dq_zmxyzw_modelsql.txt')
iv_tabs$woe[GLM$coefficients %>% names() %>% '['(-1)] %>% score2sql(.,GLM) %>% 
  cat(file = 'D:\\租东东\\三方数据测试20181109\\新颜\\新颜模型20181129\\贷前反欺诈模型20181129\\贷前反欺诈模型20181129_dq_zmxyzw_scoresql.txt')
scorescale_f(GLM$fitted.values,base_odds = 20/1.0,pdo=50,base_points = 600)$scorescale2sql %>%
  cat(file = 'D:\\租东东\\三方数据测试20181109\\新颜\\新颜模型20181129\\贷前反欺诈模型20181129\\贷前反欺诈模型20181129_dq_zmxyzw_scorescale2sql.txt')
iv_tabs$woe[GLM$coefficients %>% names() %>% '['(-1)] %>% 
  final_score2sql(.,GLM,table_name = "xxxxxx",base_odds = 20/1.0,pdo = 50,base_points = 600) %>% 
  cat(file = 'D:\\租东东\\三方数据测试20181109\\新颜\\新颜模型20181129\\贷前反欺诈模型20181129\\贷前反欺诈模型20181129_dq_zmxyzw_final_score2sql.txt')

save(results,file = 'D:\\租东东\\三方数据测试20181109\\新颜\\新颜模型20181129\\贷前反欺诈模型20181129\\贷前反欺诈模型20181129_dq_zmxyzw.rdata')

library(openxlsx)
rm(wb)
wb <- createWorkbook()
for(x in names(results)){
  print(x)
  addWorksheet(wb, x)
  writeData(wb,results[[x]],sheet = x,keepNA = T)
}
saveWorkbook(wb, "D:\\租东东\\三方数据测试20181109\\新颜\\新颜模型20181129\\贷前反欺诈模型20181129\\贷前反欺诈模型20181129_dq_zmxyzw.xlsx", overwrite = TRUE)
# openxlsx::write.xlsx()
