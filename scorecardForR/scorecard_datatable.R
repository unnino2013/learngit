library(RSQLite);
library(DBI)
library(dplyr);library(magrittr)
library(broom);library(tidypredict);library(tidyr)
library(sqldf)
library(data.table)
library(openxlsx)
library(InformationValue)
library(car)
library(sqlscore)
library(openxlsx)
library(stringr)
# check_y
check_y <- function (dt, y) 
{
  dt = setDT(dt)
  if (!(y %in% names(dt))) 
    stop(paste0("Incorrect inputs;there is no \"", y, "\" column in dt."))
  if (length(y) != 1) 
    stop("Incorrect inputs; the length of \"", y, "\" != 1.")
  if (anyNA(dt[[y]])) {
    warning(paste0("There are NAs in ", y, ". The rows with NA in \"", 
                   y, "\" were removed from input data."))
    y_sel = !is.na(dt[[y]])
    dt = dt[y_sel]
  }
  if(!all.equal(sort(unique(dt[[y]])),c(0,1))) stop(paste0("\"", y, "\" must be binary, 0L or 1L."))
  return(dt)
}

# stratified sample
split_df <-
function (dt = NULL, ratio = 0.7, seed = 186) 
{
  rt = rn_train = rn_test = NULL
  dt = setDT(dt)
  set.seed(seed)
  rt = list(train = NULL, test = NULL)
  rn_sel = sample(nrow(dt), round(nrow(dt) * ratio))
  rn_train = rn_sel
  rn_test = setdiff(1:nrow(dt), rn_sel)
  rt$train = dt[rn_train]
  rt$test = dt[rn_test]
  return(rt)
}

x_variable <- function (dt, y, x) 
{
  x_all = setdiff(names(dt), y)
  if (is.null(x)) 
    x = x_all
  if (length(setdiff(x, x_all)) > 0) {
    warning(paste0("Incorrect inputs; the variables \n\"", 
                   paste0(setdiff(x, x_all), collapse = ","), "\"\n are not exist in input data, which are removed."))
    x = intersect(x, x_all)
  }
  return(x)
}

iv_xy <-
function (x, y) 
{ # x must be categorical
  . = DistrBad = DistrGood = bad = good = NULL
  data.table(x = x, y = y)[, .(good = sum(y), bad = sum(y == 0)), keyby = "x"
                           ][, `:=`((c("good", "bad")), lapply(.SD, function(x) ifelse(x == 0, 0.99, x))), .SDcols = c("good","bad")
                             ][, `:=`(DistrGood = good/sum(good), DistrBad = bad/sum(bad))
                               ][,sum((DistrGood - DistrBad) * log(DistrGood / DistrBad))]
  # dplyr版本
  # data.table(x = x, y = y) %>% group_by(x) %>% summarise(good =sum(y),bad = n()-sum(y)) %>% 
  #   mutate(DistrGood = good / sum(good),DistrBad = bad / sum(bad)) %>% 
  #   mutate(iv = (DistrGood - DistrBad) * log(DistrGood / DistrBad)) %>% summarize(sum(iv))
}

iv <- 
function (dt, y, x = NULL,  order = TRUE) 
{ # x must be catogerical.
  info_value = label = NULL
  dt = setDT(dt)
  dt = check_y(dt, y)
  x = x_variable(dt, y, x)
  dt = dt[, x, with = FALSE][, `:=`(rowid = .I, label = dt[[y]])]
  ivlist = dt[, sapply(.SD, iv_xy, label), .SDcols = x]
  ivlist = data.table(variable = names(ivlist), info_value = ivlist)
  if (order) 
    ivlist = ivlist[order(-info_value)]
  return(ivlist)
}


num2char <- function(x = NULL,cut_pts = 5){ # use for quick Ivs
  if(is.numeric(x)){
    cut_pts = c(-Inf,unique(quantile(x,probs = seq(0.2,0.8,0.2),na.rm = TRUE)),Inf)
    return(cut(x,breaks=cut_pts))
  }
  x
}

iv_for_numeric_ <- function(y,x,zero_replace = 0.0001,cut_pts = 5){
  if(sum(is.na(y)) > 0) stop('NAs not allowes in y.')
  if(!is.numeric(x)) stop('x must be numric.')
  x_rt = dt = agg_ = NULL
  #------function start-----
  num2char <- function(x = NULL,cut_pts = 5){ # use for quick Ivs
    if(is.numeric(x)){
      if(length(unique(x)) <=4) {
        cut_pts = c(-Inf,unique(x),Inf)
        return(cut(x,breaks=cut_pts))}
      else{
        cut_pts = c(-Inf,unique(quantile(x,probs = seq(1/cut_pts,1/cut_pts * (cut_pts-1),1/cut_pts),na.rm = TRUE)),Inf)
        return(cut(x,breaks=cut_pts))}
    }
    x
  }
  #------function end-----
  x_rt <- num2char(x,cut_pts)
  dt <- data.table(y,x_rt)
  agg_ <- dt[,.(good = sum(y),bad = sum(y==0),total = .N,good_rate = sum(y) / .N),keyby = "x_rt"]
  agg_[,`:=`(c('good_cum','bad_cum'),lapply(.SD,cumsum)),.SDcols = c('good','bad')
       ][,`:=`(c('good_dist','bad_dist'),lapply(.SD,function(x) x / sum(x))),.SDcols = c('good','bad')
         ][,`:=`('woe' = log(good_dist / bad_dist),'iv' = (good_dist - bad_dist) * log(good_dist / bad_dist))]
  # agg_ = agg_ %>% mutate(woe = ifelse(is.na(woe) | is.infinite(woe),0,woe), iv = ifelse(is.na(iv) | is.infinite(iv),0,iv)) 
  
  agg_ = agg_ %>% 
    mutate(woe = ifelse(is.na(woe) ,0,woe)) %>% 
    mutate(woe = ifelse(bad_dist ==0  | good_dist == 0,log((good_dist + zero_replace) / (bad_dist + zero_replace)), woe)) %>% 
    mutate(iv =  ifelse(bad_dist ==0  | good_dist == 0,(good_dist - bad_dist) * log((good_dist + zero_replace)  / (bad_dist + zero_replace)),iv))
  
  return(list(agg_ = agg_ %>% rename(x = x_rt),iv = agg_$iv %>% sum()))
}


woe_monotone_detect <- function(woelist=iv_tabs$woe){ # 检测woe单调性
  mono_cols = NULL
  if(class(woelist) == 'iv_tabs') woelist = woelist$woes
  if(class(woelist) == 'woe') {
    for(col in names(woelist)){
      # 判断数值类型
      if(woelist[[col]]$x %>% str_detect('Inf') %>% sum(.,na.rm = T)){
        woe_mono <- woelist[[col]][,c('x','woe')] %>% `[`(complete.cases(.),) %>% `[`(,'woe') %>% diff(lag = 1) %>% `>`(0) %>% unique() %>% length() %>% `==`(1)
        if(woe_mono) mono_cols <- c(mono_cols,col)
      }
    }
  }else stop("woelist must be class 'iv_tabs' or'woe'.")
  mono_cols
}

missing_identical_rate_of_x <- function(x =NULL){
  missing_rate = sum(is.na(x)) / length(x)
  if(is.numeric(x))
    identical_rate = sum(x == as.numeric(names(which.max(table(x)))),na.rm = TRUE) / length(x)
  if(is.character(x) || is.factor(x)){
    x <- as.character(x)
    identical_rate = sum(x == names(which.max(table(x))),na.rm = TRUE) / length(x)
  }
  return(list(missing_rate=missing_rate,identical_rate=identical_rate))
}

var_filter <- function(dt,y,x = NULL,missing_limit = 0.95, 
                       identical_limit = 0.95,iv_limit = 0.1, var_rm = NULL, 
                       var_kp = NULL, return_rm_reason = FALSE){
  . = info_value = variable = rt = rm_reason = NULL
  dt = setDT(dt)
  dt = check_y(dt, y)
  x = x_variable(dt, y, x)
  mi_id_rate <- dt[,lapply(.SD,missing_identical_rate_of_x),.SDcols = x]
  mi_id_rate <- t(mi_id_rate)
  mi_id_rate <- data.frame(mi_id_rate)
  mi_id_rate[,'variable'] = rownames(mi_id_rate)
  colnames(mi_id_rate)[1:2] <- c('missing_rate','identical_rate')
  mi_id_rate[,1:2] <- sapply(mi_id_rate[,1:2],as.numeric)
  mi_id_rate <- data.table(mi_id_rate)
  # 数值型转化为factor########################
  germancredit %>% select() %>% lapply(., num2char)
  #字符型，collaps levels
  ivs <- iv(dt,y,x,order=F)
  rt = merge(ivs,mi_id_rate,by='variable')
  library(dplyr);library(dtplyr)
  rt <- rt %>% mutate(rm_reason = ifelse(missing_rate > missing_limit,"> missing_limit",NA)) %>% 
    mutate(rm_reason = ifelse(identical_rate > identical_limit,"> identical_limit",rm_reason)) %>%
    mutate(rm_reason = ifelse(info_value < iv_limit,"< iv_limit",rm_reason)) %>%
    mutate(rm_reason = ifelse(variable %in% var_rm,"var_rm",rm_reason)) %>% 
    mutate(rm_reason = ifelse(variable %in% var_kp,NA,rm_reason))
  rt %>% arrange(-info_value) 
}
# y=sample(0:1,100,T);x= sample(LETTERS[1:15],100,replace = T);x[1:5]=NA_character_
split_lvs <- function(x) x %>% str_split(.,pattern = '::|&&',simplify = T) %>% ifelse(.=='NA',NA,.)

iv_for_character_ <- function(y,x,collapse_pct_limit = 0.05,k_levels = 5,zero_replace = 0.0001) {
  if(sum(is.na(y)) > 0) stop('NAs not allowes in y.')
  x <- as.character(x)
  tab <- table(x,useNA = 'always') / sum(table(x,useNA = 'always'))
  collapse_lvs = names(tab)[tab <= collapse_pct_limit]
  collapse_lvs_paste = paste0(collapse_lvs,collapse = "::")
  x_rt <- ifelse(x %in% collapse_lvs,collapse_lvs_paste,x) %>% as.factor()
  #-------------------function start----------------
  split_lvs <- function(x) x %>% str_split(.,pattern = '::|&&',simplify = T) %>% ifelse(.=='NA',NA,.)
  # str_split('B::C::E::NA&&D',pattern = '::|&&',simplify = T) %>% ifelse(.=='NA',NA,.)
  map_f <- function(x,map){
    map = setDF(map)
    cs = map$cluster
    x1 =NULL
    for(b in cs) {
      b_ = map[map$cluster == b,'x_rt']
      bins = split_lvs(b_)
      x = ifelse(x %in% bins,b_,x)
    }
    return(x)
  }
  #-------------------function end----------------
  if(length(tab) - length(collapse_lvs) + 1 > k_levels){ # levels clustered.
    dt <- data.table(y,x_rt)
    agg_ <- dt[,.(good = sum(y),bad = sum(y==0),total = .N,good_rate = sum(y) / .N),keyby = "x_rt"]
    agg_[,'cluster'] = agg_ %>% select(good_rate) %>% kmeans(centers = k_levels) %>% '[['('cluster')
    map = agg_[,lapply(.SD,function(x) paste0(x,collapse = '&&')),keyby='cluster',.SDcols = 'x_rt']
    x_rt = map_f(x,map)
    dt <- data.table(y,x_rt)
    agg_ <- dt[,.(good = sum(y),bad = sum(y==0),total = .N,good_rate = sum(y) / .N),keyby = "x_rt"]
    agg_[,`:=`(c('good_cum','bad_cum'),lapply(.SD,cumsum)),.SDcols = c('good','bad')
         ][,`:=`(c('good_dist','bad_dist'),lapply(.SD,function(x) x / sum(x))),.SDcols = c('good','bad')
           ][,`:=`('woe' = log(good_dist / bad_dist),'iv' = (good_dist - bad_dist) * log(good_dist / bad_dist))]
  }
  if(length(tab) - length(collapse_lvs) + 1 <= k_levels){
    new_levels = c(setdiff(names(tab),collapse_lvs),collapse_lvs_paste)
    map = data.table(cluster = 1:length(new_levels),x_rt = new_levels)
    x_rt = map_f(x,map)
    dt <- data.table(y,x_rt)
    agg_ <- dt[,.(good = sum(y),bad = sum(y==0),total = .N,good_rate = sum(y) / .N),keyby = "x_rt"]
    agg_[,`:=`(c('good_cum','bad_cum'),lapply(.SD,cumsum)),.SDcols = c('good','bad')
         ][,`:=`(c('good_dist','bad_dist'),lapply(.SD,function(x) x / sum(x))),.SDcols = c('good','bad')
           ][,`:=`('woe' = log(good_dist / bad_dist),'iv' = (good_dist - bad_dist) * log(good_dist / bad_dist))]
  }
  # agg_ = agg_ %>% 
  #   mutate(woe = ifelse(is.na(woe) | is.infinite(woe),0,woe), iv = ifelse(is.na(iv) | is.infinite(iv),0,iv)) 
  agg_ = agg_ %>% 
    mutate(woe = ifelse(is.na(woe) ,0,woe)) %>% 
    mutate(woe = ifelse(bad_dist ==0  | good_dist == 0,log((good_dist + zero_replace) / (bad_dist + zero_replace)), woe)) %>% 
    mutate(iv =  ifelse(bad_dist ==0  | good_dist == 0,(good_dist - bad_dist) * log((good_dist + zero_replace)  / (bad_dist + zero_replace)),iv))
  return(list(agg_ = agg_ %>% rename(x = x_rt),iv = agg_$iv %>% sum()))
}


iv_tables_for_all <- function(dt,x='x',y='y',zero_replace = 0.0001){
  if(!is.character(x)) stop('x must be a character vector with dt\'s colnames.')
  ivs = woe = NULL
  for(col in x){
    if(sum(!is.na(dt[[col]]))){
      if(is.numeric(dt[[col]])) {
        rt = iv_for_numeric_(y = dt[[y]],x = dt[[col]],zero_replace = zero_replace)
        ivs[[col]] = rt[['iv']]
        woe[[col]] = rt[['agg_']]
        woe[[col]][['x_name']] = col
        rt = NULL
      }
      if(is.character(dt[[col]]) || is.factor(dt[[col]])){
        rt = iv_for_character_(y = dt[[y]],x = dt[[col]],zero_replace = zero_replace)
        ivs[[col]] = rt[['iv']]
        woe[[col]] = rt[['agg_']]
        woe[[col]][['x_name']] = col
        rt =NULL
      }
    }
  }
  ivs = sort(ivs,decreasing = T);
  woes=woe[names(ivs)];
  class(ivs) = 'iv';class(woes) = 'woe';
  res = list(ivs=ivs,woes=woes);
  class(res) = 'iv_tabs'
  res
}

apply2woe <- function(dt,x=NULL,iv_tables_list = NULL){
  if(class(x) != 'character') stop("x must be 'character' class with column names.")
  if(class(iv_tables_list) != 'iv_tabs') stop("iv_tables_list must be 'iv_tabs' class.")
  library(stringr);library(dplyr);library(data.table)
  #--------function start-----------------
  split_lvs <- function(x) x %>% str_split(.,pattern = '::|&&',simplify = T) %>% ifelse(.=='NA',NA,.)
  map_f <- function(x=NULL,cut_points=NULL){
    for(b in cut_points) {
      bins = split_lvs(b)
      x = ifelse(x %in% bins,b,x)
    }
    return(x)
  }
  #--------function end-----------------
  for(col in x){
    if(is.numeric(dt[[col]])){
      cut_points = iv_tables_list[['woes']][[col]][['x']] %>% 
        as.character() %>% substr(.,2,nchar(.)-1) %>% strsplit(',') %>% unlist() %>% unique()
      dt[[col]] = dt[[col]] %>% cut(.,breaks = cut_points) %>% match(iv_tables_list[['woes']][[col]][['x']]) %>%
        `[`(iv_tables_list[['woes']][[col]][['woe']],.) 
      
    }
    if(is.character(dt[[col]]) || is.factor(dt[[col]])){
      cut_points = iv_tables_list[['woes']][[col]][['x']]
      dt[[col]] = dt[[col]] %>% map_f(.,cut_points) %>% match(iv_tables_list[['woes']][[col]][['x']]) %>%
        `[`(iv_tables_list[['woes']][[col]][['woe']],.) 
    }
  }
  dt
}

# woe vars filter for glm
features_filter_for_glm_by_lars <- function(x_mat = NULL, y_vec = NULL,features_sel_num = 10,force_keep_features = NULL){
  library(biglars);library(dplyr);library(magrittr)
  lasso = biglars.fit(x=x_mat,y=y_vec,type ='lasso',removeColumns =T)
  if(ncol(x_mat) <= features_sel_num) features_sel_num = ncol(x_mat)
  cols = c(which(lasso$coefficients[features_sel_num + 1,] != 0)  %>% names() %>% '['(-1),c('sex','province')) # 选n个变量
  names(cols) = paste('lars',1:length(cols),sep = '')
  force_keep_features = lubridate::setdiff(force_keep_features,cols)
  if(length(force_keep_features) > 0) names(force_keep_features) = paste('keep', 1:length(force_keep_features),sep = '')
  c(cols,force_keep_features)
}
# woe vars filter for glm
features_filter_for_glm_by_ivs_lars <- function(data = NULL,ivs = NULL,iv_cutoff = 0.2,
                                                features_sel_num = 10,force_keep_features = NULL){
  library(biglars);library(dplyr);library(magrittr)
  x_mat = data[,ivs %>% `[`(.,ivs %>% `>`(iv_cutoff)) %>% names(),with = F] %>% as.matrix()
  y_vec=data$y
  lasso = biglars.fit(x=x_mat,y=y_vec,type ='lasso',removeColumns =T)
  if(ncol(x_mat) <= features_sel_num) features_sel_num = ncol(x_mat)
  cols = c(which(lasso$coefficients[features_sel_num + 1,] != 0)  %>% names() %>% '['(-1),c('sex','province')) # 选n个变量
  names(cols) = paste('lars',1:length(cols),sep = '')
  force_keep_features = lubridate::setdiff(force_keep_features,cols)
  if(length(force_keep_features) > 0) names(force_keep_features) = paste('keep', 1:length(force_keep_features),sep = '')
  c(cols,force_keep_features)
}

# mysql and sqlite
woe2sql_char <- function(woe_table = NULL,if_NA = -9999,woe_precision = 16){
  # woe_table <- iv_tabs$woe$province
  mapply(paste0,
         ' when ',
         woe_table$x_name,
         " in ( '" , 
         {woe_table$x %>% str_replace_all("(::)|(&&)","','") %>% str_replace_all("NA","") },
         "' ) then ",
         round(woe_table$woe,woe_precision)) %>% 
    paste0(collapse = ' \n') %>% 
    paste('/******',unique(woe_table$x_name),'******/\n',
          'case \n',.,' \n else ',if_NA,' end as \n',unique(woe_table$x_name))  # %>% cat()
  }

woe2sql_num <- function(woe_table = NULL,if_NA = -9999,woe_precision = 16){
  # woe_table <- iv_tabs$woe$age
  pat = paste('(-Inf < ',woe_table$x_name %>% unique(),' and)|(and ',woe_table$x_name %>% unique(),' <= Inf)',sep='')
  woe_table$x %>% as.character() %>% {function(x) str_sub(x,2,nchar(x)-1)}() %>% 
    str_replace(',',woe_table$x_name %>% unique() %>% paste0(' < ',.,' and ',.," <=")) %>% 
    str_replace(pat,"") %>% # 改模式 "(-Inf < )|( <= Inf)"
    mapply(paste0,' when ', ., " then ", round(woe_table$woe,woe_precision)) %>% 
    paste0(collapse = ' \n') %>% 
    paste('/******',unique(woe_table$x_name),'******/\n',
          'case \n',.,' \n else ',if_NA,' end as \n',unique(woe_table$x_name)) %>% # %>% cat()
    str_replace(' NA ',paste0('  ',woe_table$x_name %>% unique(),' IS NULL ',collapse = '')) # 处理缺失值sql
  
}

woe2sql <- function(woe_list = NULL,table_name = "xxxxxx",...){
  sql_statement = NULL
  for(col in names(woe_list)){
    if(woe_list[[col]][["x"]] %>%  str_detect("-Inf") %>% ifelse(is.na(.),0,.) %>% sum()) {
      sql_statement = c(sql_statement,woe2sql_num(woe_list[[col]],...))
    }
    else sql_statement = c(sql_statement,woe2sql_char(woe_list[[col]],...))
  }
  sql_statement %>% 
    paste0(collapse = ' ,\n/******************************/\n') %>% 
    paste('select \n',.,' \n from ',table_name,' --you should modify the table name.')  # %>% cat()
}

score2sql <- function(woe_list = NULL,GLM = NULL,table_name = "xxxxxx",...){
  sql_statement = NULL # woesql
  for(col in names(woe_list)){
    if(woe_list[[col]][["x"]] %>%  str_detect("-Inf") %>% ifelse(is.na(.),0,.) %>% sum()) {
      sql_statement = c(sql_statement,woe2sql_num(woe_list[[col]],...))
    }
    else sql_statement = c(sql_statement,woe2sql_char(woe_list[[col]],...))
  }
  sql_score = sqlscore::score_expression(GLM) %>% capture.output() %>% paste0(collapse = '\n') %>% paste('\n as score \n') # %>% cat()
  sql_statement %>% 
    paste0(collapse = ' ,\n/******************************/\n') %>% 
    paste('select \n',.,
          '\n,  /* score statement.*/\n',
          sql_score,
          ' \n from ',table_name,' --you should modify the table name.')  # %>% cat()
}

final_score2sql <-function(woe_list = NULL,GLM = NULL,table_name = "xxxxxx",
                           base_odds = 20/1.0,pdo = 50,base_points = 600,...){
  sql_statement = NULL
  for(col in names(woe_list)){
    if(woe_list[[col]][["x"]] %>%  str_detect("-Inf") %>% ifelse(is.na(.),0,.) %>% sum()) {
      sql_statement = c(sql_statement,woe2sql_num(woe_list[[col]],...))
    }
    else sql_statement = c(sql_statement,woe2sql_char(woe_list[[col]],...))
  }
  sql_score = sqlscore::score_expression(GLM) %>% capture.output() %>% paste0(collapse = '\n') # %>% cat()
  B = pdo / log(2)
  A = base_points - B * log(base_odds)
  scorescale2sql = paste('\n--# base_odds = ',base_odds,
                         '\n--# pdo = ',pdo,
                         '\n--# base_points = ',base_points,
                         '\n--# A = ',A,'\n--# B = ',B,sep = '') %>% 
    paste('\n--#------------------scorescale2sql----------------------#\n',
          A," + ",B," * log(score_p / (1-score_p))",sep = ' ') # %>% cat()
  sql_statement %>% 
    paste0(collapse = ' ,\n/******************************/\n') %>% 
    paste('select \n',.,' \n from ',table_name,' --you should modify the table name.')  %>% 
    paste('select \n',sql_score,' as score_p,\n*',' \n from \n(',.,'\n)a') %>% 
    paste('select \n',scorescale2sql,' as score,\n*',' \n from \n(',.,'\n)b') 
}

# iv_tabs$woe[GLM$coefficients %>% names() %>% '['(2:13)] %>% score2sql(.,GLM) %>% cat()


scorescale_f <- function(y_fit,base_odds = 20/1.0,pdo = 50,base_points = 600){
  B = pdo / log(2)
  A = base_points - B * log(base_odds)
  score = A + B * log(y_fit / (1-y_fit))
  scorescale2sql = paste('\n--# base_odds = ',base_odds,
                         '\n--# pdo = ',pdo,
                         '\n--# base_points = ',base_points,
                         '\n--# A = ',A,'\n--# B = ',B,sep = '') %>% 
    paste('\n--#------------------scorescale2sql----------------------#\n',
          A," + ",B," * log(y_fit / (1-y_fit))",sep = ' ') # %>% cat()
  list(score = score,A=A,B=B,base_odds = base_odds,pdo=20,base_points = 600,scorescale2sql = scorescale2sql)
}
# score = score_f(y_fit)$score


# strategy_f <- function(y=NULL,score=NULL,bins = 30){
#   dt = data.table(score=score,y=y)
#   cut_pts = c(-Inf,unique(quantile(score,probs = seq(2:(bins-1))/bins,na.rm = TRUE)),Inf)
#   dt[,'score_bin' := .(cut(score,breaks = cut_pts))][
#     ,.(cnt = .N,good = sum(y),bad = .N-sum(y),
#        bad_rate = (.N-sum(y))/.N,odds = sum(y)/(.N-sum(y))),
#     keyby='score_bin'
#     ][,'pct':= list(cnt / sum(cnt))
#       ][,'cum_pct' := list(cumsum(pct))
#         ][,'apv_ratio' := list(1-cum_pct)][,'apv_ratio' := lag(apv_ratio,1)][]
# }
# strategy_f(OOT_woe$y,score)
strategy_f <- function(y=NULL,score=NULL,bins = 30){
  dt = data.table(score=score,y=y)
  cut_pts = c(-Inf,unique(quantile(score,probs = seq(2:(bins-1))/bins,na.rm = TRUE)),Inf)
  dt[,'score_bin' := .(cut(score,breaks = cut_pts))][
    ,.(cnt = .N,good = sum(y),bad = .N-sum(y),
       bad_rate = (.N-sum(y))/.N,odds = sum(y)/(.N-sum(y))),
    keyby='score_bin'
    ][,'pct':= list(cnt / sum(cnt))
      ][,'cum_pct' := list(cumsum(pct))
        ][,'apv_ratio' := list(1-cum_pct)][,'apv_ratio' := lag(apv_ratio,1)][
          ,'cum_bad_rate':= list(lag((sum(bad) - cumsum(bad)) / (sum(cnt) - cumsum(cnt)),1))
          ][]
}

ksplot <- function(target,pred){
  df = data.frame(target,pred)
  names(df) <- c('target','x1')
  # write.csv(df,'kyd_ks.csv',row.names=F)
  df$x0 <- 1-df$x1
  df=cbind(df,cut(df[,'x1'],100))
  df=df[order(df[,'x1']),] # 鎺掑簭
  
  library(sqldf)
  names(df)=c("target","x1","x0","bin")
  agg = sqldf("select count(*) as cnt,sum(target) as sum1,bin from df group by bin")
  agg$sum0=agg$cnt-agg$sum1
  agg$cum_sum1=cumsum(agg$sum1)/sum(agg$sum1)
  agg$cum_sum0=cumsum(agg$sum0)/sum(agg$sum0)
  k_s=max(agg$cum_sum0-agg$cum_sum1);k_s # write.csv(agg,'kyd_ks_agg.csv')
  # ks plot
  plot(agg$cum_sum1,type = 'l',col='green',main = 'K_S plot',ylab = 'Cum. rate%')
  lines(agg$cum_sum0,col='red')
  legend(x = 0,y=1,legend = c('BAD','GOOD'),col = c('red','green'),lty = c(1,1))
  k_s
}
# test 
# ksplot(target = PRED$dat1.target,pred = PRED$pred)


ks_and_auc <- function(y = NULL,y_fit = NULL){
  library(InformationValue)
  auc = InformationValue::AUROC(y,y_fit)
  c_stat =InformationValue::Concordance(y,y_fit)
  ks = InformationValue::ks_stat(y,y_fit)
  list(auc = auc,c_stat = c_stat,ks = ks)
}
ks_and_auc_plot <- function(y = NULL,y_fit = NULL){
  library(InformationValue)
  InformationValue::ks_plot(y,y_fit)
  InformationValue::plotROC(y,y_fit)
}

#----------------------------------------------
# 读入数据
# library(data.table)
# dt = data.table::fread('E:\\TIANSHENDAI\\2018第三方数据测试\\天创\\天创第二批测试回溯版\\天神贷第二批样本多头借贷高级版（回溯）测试报告.csv')
# names(dt)[c(1,4:32,34:38,41:45,48:58,65)]
# dt_y = data.table::fread('E:\\TIANSHENDAI\\2018第三方数据测试\\天创\\天创第二批测试回溯版\\sample_0514.txt')
# dt_a = merge(dt,dt_y,by.x = 'idcard',by.y = 'id_num')
# cols = names(dt_a)[c(1,4:32,34:38,41:45,48:58,65)]
# 
# dt_rt = copy(dt_a[,cols,with=F]) 
# dt_rt[,'y':= .(y==0)] # 原始0为好，重编码1为好
# dt_rt =setDF(dt_rt);dt_rt$y<- as.numeric(dt_rt$y)
# iv_tables_for_all(dt_rt,cols[-c(1:3,52)],'y') -> iv_tabs
# iv_tabs$ivs %>% names()
# apply2woe(dt_rt,x= iv_tabs$ivs %>% names(),iv_tables_list = iv_tabs) -> dt_rt_new



#######################################creat a logit model#########################################
# iv_tabs$ivs %>% `[`(.,iv_tabs$ivs %>% `>`(0.2)) %>% names() %>% paste0(collapse = ' + ') %>% 
#   paste('y','~',.,sep=' ') %>% as.formula() -> forms
# # glm begein
# GLM <- glm(formula = forms,family = binomial(link = "logit"),data = dt_rt_new)
# summary(GLM)
# GLM_step <- step(GLM,direction = 'both',trace=0)
# summary(GLM_step)
# forms = GLM_step %>% coef() %>% names %>% `[`(-1) %>% 
#   `[`(which(!(. %in% c('PTA0056','PTA0113','PTA0017','PTA0019','PTA0055','PTA0098','PTA0023','PTA0095','PTB0009','PTA0057')))) %>% paste0(collapse = ' + ') %>% 
#   paste('y','~',.,sep=' ') %>% as.formula()
# GLM <- glm(formula = forms,family = binomial(link = "logit"),data = dt_rt_new,x=T,y=T)
# GLM;car::vif(GLM);library(dbplyr);library(dplyr)
# drop1(GLM,scope= ~PTA0056) -> glm1
# GLM %>% vcov() %>% cov2cor()
# ksplot(target = GLM$y,GLM$fitted.values)
# res <- data.table(y = GLM$y,y_fit = GLM$fitted.values)
# InformationValue::AUROC(GLM$y,GLM$fitted.values)
# InformationValue::Concordance(GLM$y,GLM$fitted.values)
# InformationValue::ks_stat(GLM$y,GLM$fitted.values)
# InformationValue::ks_plot(GLM$y,GLM$fitted.values)
# InformationValue::plotROC(GLM$y,GLM$fitted.values)
# 
# #----speedglm---------
# library(speedglm) # the same result with glm,but with highest speed
# s_GLM <- speedglm(formula = forms,family = binomial(link = "logit"),data = dt_rt_new,y = TRUE,model=TRUE,fitted = TRUE)
# s_GLM;library(car);vif(s_GLM)
# s_GLM %>% vcov() %>% cov2cor()
# #------ bigglm------------
# library(biglm) # the same result with glm & speedglm
# b_GLM <- bigglm(formula = forms,family = binomial(link = "logit"),data = dt_rt_new)
# b_GLM;vif(b_GLM)
# b_GLM %>% vcov() %>% cov2cor()
# #---------lars--------
# library(biglars)
# lasso = biglars.fit(x=dt_rt_new[,iv_tabs$ivs %>% `[`(.,iv_tabs$ivs %>% `>`(0.2)) %>% names()] %>% as.matrix(),y=dt_rt_new$y)
# coef(lasso)
# lasso[[2]]
# lar = biglars.fit(x=dt_rt_new[,iv_tabs$ivs %>% `[`(.,iv_tabs$ivs %>% `>`(0.2)) %>% names()] %>% as.matrix(),y=dt_rt_new$y,type ='lar')
# coef(lar)
# lar[['rss']]
# lar = biglars.fit(x=dt_rt_new[,iv_tabs$ivs %>% `[`(.,iv_tabs$ivs %>% `>`(0.2)) %>% names()] %>% as.matrix(),y=dt_rt_new$y,type ='lar')
# coef(lar)
# lar[['rss']]
# # --------------h2o----------------
# 
# 








# found non-significant variables
# chos <- names(coefficients(GLM_step))[-c(1,4,5,8,10,12,20,21,24,27,28,30,18,19,11,13,14,22)]
# GLM_chos <- glm(formula = as.formula(paste('target','~',paste(chos,collapse = ' + '))),
#                 family = binomial(link = "logit"),data = dat1[,-1])
# summary(GLM_chos)
# # GLM_chos1 <- step(GLM_chos,direction = 'both')
# # summary(GLM_chos1)
# pred <- predict(GLM_chos,newdata = dat1,type = 'response')
# PRED <- data.frame(dat1$target,pred)
# 
# sapply(ivtabs, function(x) x$iv) %>% sort()
# 
# smbinning(dt_rt,y='y',x = 'PTA0040')$iv
# smbinning(dt_rt,y='y',x = 'PTA0083')$iv
# smbinning(dt_rt,y='y',x = 'PTA0097')$iv
# 
# 
# data("germancredit")
# dataset = data.table::as.data.table(germancredit)
# dataset$creditability <- dataset$creditability %>% recode(`good`=0,`bad`=1)
# target = 'creditability'
# vars_all = dataset %>% colnames %>% select_vars(-matches(`target`))


# 此后代码不用改
# # 拆分训练集测试集
# library(dtplyr)
# library(scorecard)
# dt_list = scorecard::split_df(dataset,ratio=0.6,seed = 10086)
# train = dt_list$train
# test = dt_list$test
# # compute IVs
# train %>% colnames
# IVs = iv(train, y= target, x = vars_all, positive = "bad|1", order = TRUE)
# # vars_pre_chosed <- IVs %>% filter(info_value >= 0.1) %>% `[[`('variable') # iv大于0.1的
# # 预筛选变量
# vars_pre_chosed <- 
#   var_filter(train, y= target, x = vars_all,
#              iv_limit = 0.1, missing_limit = 0.95,identical_limit = 0.95,
#              var_rm = NULL, var_kp = NULL,return_rm_reason = TRUE, 
#              positive = "bad|1")$dt %>% colnames() 
# 
# # bins & WOE & IV
# bins = woebin(train %>% select(vars_pre_chosed), target,no_cores=1,print_step=0)
# 


score_f <- function(y_fit,base_odds = 9/1.0,pdo=50,base_points = 600){
  B = pdo / log(2)
  A = 600 - B * log(base_odds)
  score = A + B * log(y_fit / (1-y_fit))
  list(score = score,A=A,B=B,base_odds = base_odds,pdo=20,base_points = 600)
}

strategy_f <- function(y=NULL,score=NULL,bins = 30){
  dt = data.table(score=score,y=y)
  cut_pts = c(-Inf,unique(quantile(score,probs = seq(2:(bins-1))/bins,na.rm = TRUE)),Inf)
  dt[,'score_bin' := .(cut(score,breaks = cut_pts))][
    ,.(cnt = .N,good = sum(y),bad = .N-sum(y),
       bad_rate = (.N-sum(y))/.N,odds = sum(y)/(.N-sum(y))),
    keyby='score_bin'
    ][,'pct':= list(cnt / sum(cnt))
      ][,'cum_pct' := list(cumsum(pct))
        ][,'apv_ratio' := list(1-cum_pct)][,'apv_ratio' := lag(apv_ratio,1)][]
}



lasso_glm_regression <- function(xy=data.table(),x=c('',''),y='y',var_nums = 15,
                                 force_keep_vars = NULL,force_drop_vars = NULL,
                                 split_limit=0.8,iv_limit = 0.2,stepglm=FALSE,cat_path = ''){
  rm(forms,GLM,GLM1,y_fit_test,wb,results);rm(train,test,rs,xy)
  cols <- xy %>% colnames()
  x = x;y=y
  x_in = intersect(x,cols); 
  if(length(setdiff(x,x_in))) warning(paste0(setdiff(x,x_in),collapse = ',') %>% paste("columns",.,'not in xy and removed!'))
  x = x_in
  library(data.table);xy = setDT(xy)
  rt = xy %>% split_df(.,split_limit)
  train = rt$train;
  test  = rt$test;
  
  # iv&woe for train;
  iv_tables_for_all(train,x,y) -> iv_tabs
  train_woe <- apply2woe(train,x,iv_tables_list = iv_tabs) 
  test_woe <- apply2woe(test,x,iv_tables_list = iv_tabs)
  monocols = woe_monotone_detect(iv_tabs);print(monocols)
  sel_cols = iv_tabs$ivs[iv_tabs$ivs > iv_limit] %>% names() %>% intersect(monocols)  %>% union(force_keep_vars)
  
  #----------------------------model---------------------------------------------#
  # #---------lasso--------
  x_mat <- train_woe[,sel_cols,with = F] %>% as.matrix();
  y_v <- train_woe[,y,with=F] %>% as.matrix() %>% `[`(,1)
  library(biglars)
  lasso = biglars.fit(x=x_mat,
                      y= y_v,
                      type ='lasso',removeColumns =T)
  
  # #----------------------------model---------------------------------------------#
  # rm(forms,GLM,GLM1,y_fit_test,wb,results)
  var_nums <- min(lasso$coefficients %>% nrow(),var_nums + 1)
  forms <- c(which(lasso$coefficients[var_nums,] != 0)  %>% names() %>% '['(-1)) %>% 
    union(force_keep_vars)  %>%
    setdiff(force_drop_vars) %>%
    paste0(collapse = ' + ') %>% paste(y,' ~ ',.,sep='') %>% as.formula()
  
  # glm begein
  GLM1 <- glm(formula = forms,family = binomial(link = "logit"),data = train_woe);
  GLM1 %>% car::vif() %>% print()
  GLM1$coefficients %>% print()
  if(stepglm){
    GLM = step(GLM1)
  }else GLM = GLM1
  
  InformationValue::AUROC(GLM$y,GLM$fitted.values)
  InformationValue::ks_stat(GLM$y,GLM$fitted.values) 
  InformationValue::ks_plot(GLM$y,GLM$fitted.values)
  InformationValue::plotROC(GLM$y,GLM$fitted.values)
  # 测试集表现
  y_fit_test <- predict(GLM,test_woe,type = 'response')
  InformationValue::ks_stat(test_woe$y,y_fit_test)
  
  
  ####################################
  # 校准评分
  # 600 30/1  650 60/1 pdo = 50
  # 600 = A + B * log(30/1)
  # 650 = A + B * log(60/1)
  score_train = scorescale_f(GLM$fitted.values,base_odds = 20/1.0,pdo=50,base_points = 600)$score
  score_test = scorescale_f(y_fit_test,base_odds = 20/1.0,pdo=50,base_points = 600)$score
  strategy_f(test_woe$y,score_test,bins = 20) %>% print()
  strategy_f(train_woe$y,score_train,bins=20) %>% print()
  InformationValue::ks_stat(train_woe$y,score_train) %>% print()
  InformationValue::ks_stat(test_woe$y,score_test) %>% print()
  
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
  results$ks_auc = data.table(train_ks = InformationValue::ks_stat(GLM$y,GLM$fitted.values),
                              test_ks = InformationValue::ks_stat(test_woe$y,score_test),
                              train_auc = InformationValue::AUROC(GLM$y,GLM$fitted.values),
                              train_auc = InformationValue::AUROC(GLM$y,GLM$fitted.values),
                              OOT_ks = NA,
                              OOT_auc = NA)
  
  # 生成sql代码
  iv_tabs$woes[GLM$coefficients %>% names() %>% '['(-1)] %>% woe2sql %>% 
    cat(file = '_woe2sql.txt' %>% paste(cat_path,Sys.Date(),.,sep=''))
  sqlscore::score_expression(GLM) %>% capture.output() %>% paste0(collapse = ' \n') %>%
    cat(file = '_modelsql.txt' %>% paste(cat_path,Sys.Date(),.,sep=''))
  iv_tabs$woes[GLM$coefficients %>% names() %>% '['(-1)] %>% score2sql(.,GLM) %>% 
    cat(file = '_scoresql.txt' %>% paste(cat_path,Sys.Date(),.,sep=''))
  scorescale_f(GLM$fitted.values,base_odds = 20/1.0,pdo=50,base_points = 600)$scorescale2sql %>%
    cat(file = '_scorescale2sql.txt' %>% paste(cat_path,Sys.Date(),.,sep=''))
  iv_tabs$woes[GLM$coefficients %>% names() %>% '['(-1)] %>% 
    final_score2sql(.,GLM,table_name = "xxxxxx",base_odds = 20/1.0,pdo = 50,base_points = 600) %>% 
    cat(file = '_final_score2sql.txt' %>% paste(cat_path,Sys.Date(),.,sep=''))
  
  save(results,file = '.rdata' %>% paste(cat_path,Sys.Date(),.,sep=''))
  
  library(openxlsx)
  rm(wb)
  wb <- createWorkbook()
  for(x in names(results)){
    print(x)
    addWorksheet(wb, x)
    writeData(wb,results[[x]],sheet = x,keepNA = T)
  }
  saveWorkbook(wb, file = ".xlsx" %>% paste(cat_path,Sys.Date(),.,sep=''), overwrite = TRUE)
  
  results$GLM =GLM
  results$forms = forms
  results
}

create_scorecard_sql <- function(iv_tabs,GLM,pdo = 50,base_points = 600,base_odds = 20 / 1.0){
  vars = GLM %>% coef() %>% names() # %>% `[`(-1)
  beta = GLM %>% coef()
  B = pdo / log(2)    
  A = base_points - B * log(base_odds)
  woes = iv_tabs$woes[vars[-1]]
  # woes$`(Intercept)`= data.frame(woe=1)
  for(x in vars[-1]){
    woes[[x]]$woe_score <- woes[[x]]$woe * beta[x] *  B
  }
  woe_score2sql(woes,intercept=beta['(Intercept)'],A=A,B=B) 
}
# create_scorecard_sql(iv_tabs,GLM) %>% cat(file='model20190605/scorecard0605.txt')
# score_dt = sqldf(sql_sscorecard) #  

woe_score2sql <- function(woe_list = NULL,intercept=-1.853679,A=383.9036,B=72.13475,table_name = "xxxxxx",...){
  sql_statement = NULL
  for(col in names(woe_list)){
    if(woe_list[[col]][["x"]] %>%  str_detect("-Inf") %>% ifelse(is.na(.),0,.) %>% sum()) {
      sql_statement = c(sql_statement,woe_score2sql_num(woe_list[[col]],...))
    }
    else sql_statement = c(sql_statement,woe_score2sql_char(woe_list[[col]],...))
  }
  base_score = paste(A,"+",intercept * B,"as base_score,\n",sep=" ")
  sql_statement %>% 
    paste0(collapse = ' ,\n/******************************/\n') %>% 
    paste('select \n',base_score,.,' \n from ',table_name,' --you should modify the table name.')  # %>% cat()
}

woe_score2sql_num <-
  function(woe_table = NULL,if_NA = -9999,woe_precision = 16){
    # woe_table <- iv_tabs$woe$age
    pat = paste('(-Inf < ',woe_table$x_name %>% unique(),' and)|(and ',woe_table$x_name %>% unique(),' <= Inf)',sep='')
    woe_table$x %>% as.character() %>% {function(x) str_sub(x,2,nchar(x)-1)}() %>% 
      str_replace(',',woe_table$x_name %>% unique() %>% paste0(' < ',.,' and ',.," <=")) %>% 
      str_replace(pat,"") %>% # 改模式 "(-Inf < )|( <= Inf)"
      mapply(paste0,' when ', ., " then ", round(woe_table$woe_score,woe_precision)) %>% 
      paste0(collapse = ' \n') %>% 
      paste('/******',unique(woe_table$x_name),'******/\n',
            'case \n',.,' \n else ',if_NA,' end as \n',unique(woe_table$x_name)) %>% # %>% cat()
      str_replace(' NA ',paste0('  ',woe_table$x_name %>% unique(),' IS NULL ',collapse = '')) # 处理缺失值sql
    
  }
woe_score2sql_char <- function(woe_table = NULL,if_NA = -9999,woe_precision = 16){
  # woe_table <- iv_tabs$woe$province
  mapply(paste0,
         ' when ',
         woe_table$x_name,
         " in ( '" , 
         {woe_table$x %>% str_replace_all("(::)|(&&)","','") %>% str_replace_all("NA","") },
         "' ) then ",
         round(woe_table$woe_score,woe_precision)) %>% 
    paste0(collapse = ' \n') %>% 
    paste('/******',unique(woe_table$x_name),'******/\n',
          'case \n',.,' \n else ',if_NA,' end as \n',unique(woe_table$x_name))  # %>% cat()
}


