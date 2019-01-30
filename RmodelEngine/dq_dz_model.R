
##############################################################################################
#------rules compute--                                                                              
##############################################################################################
# infos = json 
library(dplyr)
library(RSQLite)
library(DBI)
library(jsonlite)
library(stringr)
# library(car) 
library(magrittr)
library(lubridate)
library(hms)
# x %>% car_recode("0:600 = 'Z5';601:650 = 'Z4';651:700 = 'Z3';701:750 = 'Z2';751:Inf = 'Z1';else=NA")

check_num <- function(x){
  x <- as.numeric(x)
  ifelse(is.null(x),NA,x)
}
check_char <- function(x){
  x <- as.character(x)
  ifelse(is.null(x),NA,x)
}
check_date <- function(x){
  tryCatch({as.Date.character(x)},error = function(e) NA)
}
check_null_NA <- function(x) {is.null(x) || is.na(x)}
check_NA <- function(x){
  if(is.null(x)) x<-NA
  x
}

ruleset <- NULL
# base
ruleset$rule_age <- function(res=list(),age_limit = c(18,45)){
  tryCatch(
    {
      require(lubridate)
      require(magrittr)
      require(stringr)
      age <- res$baseInfo$id_card %>% str_sub(7,14) %>% as.Date.character(.,format = "%Y%m%d") %>% `-`(Sys.Date(),.) %>% `/`(ddays(365))
      if(check_null_NA(age)) return('ERROR')
      age_limit <- sort(age_limit)
      between(age,age_limit[1],age_limit[2]) %>% as.character()},
    error = function(e) 'ERROR'
  )
}
ruleset$rule_sanyaosu <- function(res){
  tryCatch(
    {
      sanyaoshu <- res$baseInfo$san %>% as.character() %>% str_to_upper()
      if(check_null_NA(sanyaoshu)) return('ERROR')
      sanyaoshu %in% c('1','TRUE') %>% as.character()
    },
    error = function(e) 'ERROR'
  )
}
ruleset$rule_zaiwang <- function(res){
  tryCatch(
    {
      zaiwang <- res$baseInfo$zaiwang %>% as.character() %>% str_to_upper()
      if(check_null_NA(zaiwang)) return('ERROR')
      zaiwang %in% c('2','3','4')  %>% as.character()
    },
    error = function(e) 'ERROR'
  )
}
# taobao
ruleset$rule_zmscore <- function(res){
  tryCatch(
    {
      # zhifubaomianya & taobao scpyder jianrong
      zm_zmscore = res$baseInfo$zmscore %>% check_char()
      taobao_zmscore = res$moxieInfo$taobaoReport$wealth_info$totalssets$taobao_zmscore %>% check_num()
      taobao_zmscore_grade = taobao_zmscore %>% car_recode("0:600 = 'Z5';601:650 = 'Z4';651:700 = 'Z3';701:750 = 'Z2';751:Inf = 'Z1';else=NA") %>% check_char()
      zmscore = ifelse(taobao_zmscore_grade %in% c('Z1','Z2','Z3','Z4','Z5'),taobao_zmscore_grade,zm_zmscore)
      if(check_null_NA(zmscore)) return('ERROR')
      zmscore %in% c('Z1','Z2','Z3','Z4')  %>% as.character()
    },
    error = function(e) 'ERROR'
  )
}
ruleset$rule_taobao_his_days <- function(res,taobao_his_days_limit = 90){
  tryCatch(
    {
      if(is.null(res$moxieInfo$taobaoReport)) return('ERROR')
      taobao_first_ordertime <- res$moxieInfo$taobaoReport$basic_info$user_and_account_basic_info$first_ordertime %>% check_NA()
      taobao_his_days <- taobao_first_ordertime %>% check_date() %>% `-`(Sys.Date(),.)  %>% `/`(ddays(1))  %>% check_NA()
      if(is.null(taobao_his_days) || is.na(taobao_his_days)) return('ERROR')
      (taobao_his_days > taobao_his_days_limit) %>% as.character()
    },
    error = function(e) 'ERROR'
  )
}
ruleset$rule_taobao_shiming <- function(res){
  name_check <-function(name,name_cases){
    if(is.null(name_cases)) name_cases <- ''
    name %in% name_cases
  }
  tel_check <- function(tel,tel_cases,tel_check_limit = 5){
    if(is.null(tel_cases)) tel_cases <- ''
    tel_cases %>% str_split('') %>% 
      sapply(function(x,y) sum(x==y,na.rm = T),
             y=tel %>% str_split('') %>% unlist() 
      ) %>% 
      max(na.rm = T) %>% `>=`(tel_check_limit)
  }
  shiming_grade_check <- function(sanyaosu,tel_check_status,name_check_status){
    (((str_to_lower(sanyaosu) %in% c('true','1') )  & name_check_status > 0) | 
       (tel_check_status > 0 & name_check_status > 0)) %>% 
      ifelse(is.na(.),0,.) %>% as.character()
  }
  tryCatch(
    {
      if(is.null(res$baseInfo$san)) return('ERROR')
      if(is.null(res$moxieInfo$taobaoInfo$userinfo$real_name)) return('ERROR')
      if(is.null(res$moxieInfo$taobaoInfo$userinfo$phone_number)) return('ERROR')
      sanyaoshu <- res$baseInfo$san
      tel <- res$baseInfo$tel
      taobao_namelist = res$moxieInfo$taobaoInfo$deliveraddress$name %>% c(res$moxieInfo$taobaoInfo$userinfo$real_name) %>% unique() %>% check_NA()
      taobao_tellist = res$moxieInfo$taobaoInfo$deliveraddress$phone_no %>% c(res$moxieInfo$taobaoInfo$userinfo$phone_number) %>% unique() %>% check_NA()
      name_check_status = name_check(res$baseInfo$realname,taobao_namelist) %>% check_NA()
      tel_check_status = tel_check(tel,taobao_tellist) %>% check_NA()
      shiming_grade_check(sanyaoshu,tel_check_status,name_check_status)
    },
    error = function(e) 'ERROR'
  )
}
# xuexin
ruleset$rule_xuexin_xueli_limit <- function(res){
  tryCatch(
    {
      if(is.null(res$moxieInfo$xuexinInfo)) return('ERROR')
      if(is.null(res$baseInfo$id_card) || is.null(res$moxieInfo$xuexinInfo$studentInfo_list$id_card)) return('ERROR')
      if(res$baseInfo$id_card != res$moxieInfo$xuexinInfo$studentInfo_list$id_card) return('FALSE') 
      edu_level_check <- function(edu_level) edu_level %in% c('专科','本科','硕士研究生','博士研究生') %>% sum(na.rm = TRUE) # consider mult. edu.
      edu_type_chenck <- function(edu_type) edu_type %in% c('普通','研究生','普通高等教育') %>% sum(na.rm = TRUE)
      edu_form_chenck <- function(edu_form) edu_form %in% c('全日制','普通全日制') %>% sum(na.rm = TRUE)
      edu_status_check <- function(edu_status) edu_status  %in% c('在籍注册学籍','在籍保留学籍') %>% sum(na.rm = TRUE)
      # edu_advice_amt_check <- function(edu_level) edu_level %>% car_recode("'专科' = 4500;'本科' = 5000;'硕士研究生' = 6000;'博士研究生' = 7000;else = 0") %>% max(na.rm = TRUE)
      # edu_level_max_level_check <- function(edu_level) edu_level %>% 
      #   car_recode("'专科' = 1;'本科' = 2;'硕士研究生' = 3;'博士研究生' = 4;else = 0") %>% max(na.rm = TRUE)
      # 
      edu_level = res$moxieInfo$xuexinInfo$studentInfo_list$level  %>% edu_level_check() %>% check_num()  # %in% c('专科','本科','硕士研究生','博士研究生') # 注意多学历无序列表
      edu_type = res$moxieInfo$xuexinInfo$studentInfo_list$edu_type  %>% edu_type_chenck() %>% check_num()  # %in% c('普通','研究生','普通高等教育')
      edu_form = res$moxieInfo$xuexinInfo$studentInfo_list$edu_form  %>% edu_form_chenck() %>% check_num()  # %in% c('全日制','普通全日制')
      edu_status = res$moxieInfo$xuexinInfo$studentInfo_list$status %>% str_remove_all("[（]|[(]|[)]|[）]")  %>% edu_status_check() %>% check_num()    # %in% c('在籍注册学籍','在籍保留学籍')
      # edu_advice_amt = res$moxieInfo$xuexinInfo$studentInfo_list$level %>% edu_advice_amt_check() %>% check_num()
      edu_student_status = (edu_level & edu_type & edu_form & edu_status) %>% as.character()
      edu_student_status
    },
    error = function(e) 'ERROR'
  )
}
ruleset$rule_xuexin_in_school_limit <- function(res,edu_leave_school_years = -1){
  tryCatch(
    {
      if(is.null(res$moxieInfo$xuexinInfo)) return('ERROR')
      if(is.null(res$baseInfo$id_card) || is.null(res$moxieInfo$xuexinInfo$studentInfo_list$id_card)) return('ERROR')
      if(res$baseInfo$id_card != res$moxieInfo$xuexinInfo$studentInfo_list$id_card) return('FALSE') 
      if(is.null(res$moxieInfo$xuexinInfo$studentInfo_list$leave_school_time) || 
         is.null(res$moxieInfo$xuexinInfo$studentInfo_list$enrollment_time)) return('ERROR')
      edu_level_max_level_check <- function(edu_level) {edu_level %>%
        car_recode("'专科' = 1;'本科' = 2;'硕士研究生' = 3;'博士研究生' = 4;else = 0") %>% max(na.rm = TRUE)}
      edu_level_num <- res$moxieInfo$xuexinInfo$studentInfo_list$level %>% edu_level_max_level_check() %>% check_NA() # xueli

      edu_leave_school_time <- res$moxieInfo$xuexinInfo$studentInfo_list$leave_school_time %>% check_date() %>% check_NA()
      edu_enrollment_time  <-  res$moxieInfo$xuexinInfo$studentInfo_list$enrollment_time %>% check_date() %>% check_NA()
      edu_grade <- Sys.Date() %>% check_date() %>% `-`(edu_enrollment_time) %>% `/`(ddays(365)) %>% ceiling() # 1234year # nianji 12345678
      edu_xuezhi <- edu_leave_school_time %>% check_date() %>% `-`(edu_enrollment_time) %>% `/`(ddays(365)) %>% ceiling() # xuezhi 1234
      
      rt  <- 
        ((edu_level_num %in% c(1,2)) && (edu_xuezhi >= 3) && (edu_grade <= (edu_xuezhi + edu_leave_school_years))) || # master below limit
        (edu_level_num > 2) # master above no limit
      rt %>% as.character()
    },
    error = function(e) 'ERROR'
  )
}
ruleset$rule_xuexin_xuezhi_limit <- function(res){
  tryCatch(
    {
      if(is.null(res$moxieInfo$xuexinInfo)) return('ERROR')
      if(is.null(res$baseInfo$id_card) || is.null(res$moxieInfo$xuexinInfo$studentInfo_list$id_card)) return('ERROR')
      if(res$baseInfo$id_card != res$moxieInfo$xuexinInfo$studentInfo_list$id_card) return('FALSE') 
      if(is.null(res$moxieInfo$xuexinInfo$studentInfo_list$leave_school_time) || 
         is.null(res$moxieInfo$xuexinInfo$studentInfo_list$enrollment_time)) return('ERROR')

      edu_leave_school_time <- res$moxieInfo$xuexinInfo$studentInfo_list$leave_school_time %>% check_date() %>% check_NA()
      edu_enrollment_time  <-  res$moxieInfo$xuexinInfo$studentInfo_list$enrollment_time %>% check_date() %>% check_NA()
      edu_xuezhi <- edu_leave_school_time %>% check_date() %>% `-`(edu_enrollment_time) %>% `/`(ddays(365)) %>% ceiling() # xuezhi 1234
     
      (edu_xuezhi >= 3)  %>% as.character()
    },
    error = function(e) 'ERROR'
  )
}

# yyx
ruleset$rule_yyx_shiming <- function(res){
  tryCatch(
    {
      if(is.null(res$baseInfo$san)) return('ERROR')
      if(is.null(res$moxieInfo$yunyingshangInfo)) return('ERROR')
      if(is.null(res$moxieInfo$yunyingshangInfo$name)) return('ERROR')
      if(length(res$baseInfo$realname) != length(res$moxieInfo$yunyingshangInfo$name)) return('ERROR')
      realname  <- res$baseInfo$realname %>% str_split(pattern = '') %>% unlist()
      yys_sm <- res$moxieInfo$yunyingshangInfo$name %>% str_split(pattern = '') %>% unlist() %>% `==`(realname) %>% any()
      res$baseInfo$san %>% str_to_lower() %>% `%in%`(c('1','true')) %>% `||`(yys_sm)
    },
    error = function(e) 'ERROR'
  )
}
ruleset$rule_yyx_his <- function(res,yys_his_days_limit = 180){
  tryCatch(
    {
      if(is.null(res$moxieInfo$yunyingshangInfo)) return('ERROR')
      if(is.null(res$moxieInfo$yunyingshangInfo$open_time)) return('ERROR')
      open_time <- res$moxieInfo$yunyingshangInfo$open_time
      yys_his_days <- open_time %>% check_date() %>% `-`(Sys.Date(),.)  %>% `/`(ddays(1))  %>% check_NA()
      if(is.null(yys_his_days) || is.na(yys_his_days)) return('ERROR')
      (open_time > yys_his_days_limit) %>% as.character()
    },
    error = function(e) 'ERROR'
  )
}
ruleset$rule_yyx_zaiwang_state <- function(res){
  tryCatch(
    {
      if(is.null(res$moxieInfo$yunyingshangInfo)) return('ERROR')
      if(is.null(res$moxieInfo$yunyingshangInfo$message) && is.null(res$moxieInfo$yunyingshangInfo$state)) return('ERROR')
      c(res$moxieInfo$yunyingshangInfo$message,res$moxieInfo$yunyingshangInfo$state) %>% `==`("正常") %>% any() %>% as.character()
    },
    error = function(e) 'ERROR'
  )
}

# yyx top calls in txl rule
ruleset$rule_yyx_call_last6m_topin_txl <- function(res,duration_limit = 6,top_num = 20,
                                           intersect_limit = 0){
  tryCatch(
    {
      txl = res$tongxunluInfo$tel %>% stringr::str_remove_all("(\\s)|-|(\\+86)")
      res$moxieInfo$yunyingshangInfo$calls$items %>% 
        do.call('rbind',.) -> tmp
      tmp %>% dplyr::filter(duration >= duration_limit, nchar(peer_number) == 11) %>% 
        group_by(peer_number) %>% summarise(cnt = n()) %>% 
        arrange(desc(cnt)) %>% head(top_num) %>% 
        `$`(peer_number) %>% intersect(txl) %>% length() %>% 
        `>`(intersect_limit) %>% as.character()
    },
    error = function(e) 'ERROR'
  )
} 
# top n calls concentration.
ruleset$rule_yyx_call_last6m_concentrate <- function(res,duration_limit = 6,top_num = 2,
                                                     concentrate = 0.8){
  tryCatch(
    {
      res$moxieInfo$yunyingshangInfo$calls$items %>% 
        do.call('rbind',.) -> tmp
      tmp %>% dplyr::filter(duration >= duration_limit, nchar(peer_number) == 11) %>% 
        group_by(peer_number) %>% summarise(cnt = n()) %>% 
        arrange(desc(cnt)) %>% mutate(ratio = cnt / sum(cnt)) %>% 
        head(top_num) %$% sum(ratio) %>%
        `<`(concentrate) %>% as.character()
    },
    error = function(e) 'ERROR'
  )
}

# rule silent days
ruleset$rule_yyx_call_last6m_Silent_days_n7_cnt <- function(res,silent_days = 7,
                                                            limit_cnt = 5){
  
  tryCatch(
    {
      res$moxieInfo$yunyingshangInfo$calls$items %>% 
        do.call('rbind',.) -> tmp
      tmp$time %>% as.POSIXct() %>% sort(decreasing = F) %>% 
        diff.POSIXt(lag = 1) %>% `/`(lubridate::dseconds(3600 * 24)) %>%
        `>`(silent_days) %>% sum(na.rm = T) %>% `<`(limit_cnt) %>% as.character()
    },
    error = function(e) 'ERROR'
  )
}
ruleset$rule_yyx_call_last6m_Silent_days_n5_cnt <- function(res,silent_days = 5,
                                                            limit_cnt = 10){
  
  tryCatch(
    {
      res$moxieInfo$yunyingshangInfo$calls$items %>% 
        do.call('rbind',.) -> tmp
      tmp$time %>% as.POSIXct() %>% sort(decreasing = F) %>% 
        diff.POSIXt(lag = 1) %>% `/`(lubridate::dseconds(3600 * 24)) %>%
        `>`(silent_days) %>% sum(na.rm = T) %>% `<`(limit_cnt) %>% as.character()
    },
    error = function(e) 'ERROR'
  )
}
ruleset$rule_yyx_call_last6m_Silent_days_n3_cnt <- function(res,silent_days = 3,
                                                            limit_cnt = 20){
  
  tryCatch(
    {
      res$moxieInfo$yunyingshangInfo$calls$items %>% 
        do.call('rbind',.) -> tmp
      tmp$time %>% as.POSIXct() %>% sort(decreasing = F) %>% 
        diff.POSIXt(lag = 1) %>% `/`(lubridate::dseconds(3600 * 24)) %>%
        `>`(silent_days) %>% sum(na.rm = T) %>% `<`(limit_cnt) %>% as.character()
    },
    error = function(e) 'ERROR'
  )
}

# rule dialed success ratio
ruleset$rule_yyx_call_last6m_dialed_succ_ratio <- function(res,duration_limit = 6,
                                                           last_days = 180,
                                                           dialed_succ_ratio = .3){
  tryCatch(
    {
      res$moxieInfo$yunyingshangInfo$calls$items %>% 
        do.call('rbind',.) -> tmp
      tmp %>% dplyr::filter(stringr::str_to_upper(dial_type) == "DIALED") %>% 
        mutate(call_date = as.Date(time),dialed_success = duration > duration_limit
               ,ym = stringr::str_sub(time,1,7)) %>% 
        dplyr::filter(call_date > (Sys.Date() - last_days)) %>%
        select(peer_number,duration,dial_type,call_date,dialed_success,ym) %>% 
        unique.data.frame() %>% 
        group_by(ym) %>%
        summarise(success = sum(dialed_success,na.rm = T),all_dialed = n(),succ_rate = success / all_dialed) %$%
        succ_rate %>% mean(na.rm = T) %>% `>`(dialed_succ_ratio) %>% as.character()
    },
    error = function(e) 'ERROR'
  )
}
ruleset$rule_yyx_call_last3m_dialed_succ_ratio <- function(res,duration_limit = 6,
                                                           last_days = 90,
                                                           dialed_succ_ratio = .3){
  tryCatch(
    {
      res$moxieInfo$yunyingshangInfo$calls$items %>% 
        do.call('rbind',.) -> tmp
      tmp %>% dplyr::filter(stringr::str_to_upper(dial_type) == "DIALED") %>% 
        mutate(call_date = as.Date(time),dialed_success = duration > duration_limit
               ,ym = stringr::str_sub(time,1,7)) %>% 
        dplyr::filter(call_date > (Sys.Date() - last_days)) %>%
        select(peer_number,duration,dial_type,call_date,dialed_success,ym) %>% 
        unique.data.frame() %>% 
        group_by(ym) %>%
        summarise(success = sum(dialed_success,na.rm = T),all_dialed = n(),succ_rate = success / all_dialed) %$%
        succ_rate %>% mean(na.rm = T) %>% `>`(dialed_succ_ratio) %>% as.character()
    },
    error = function(e) 'ERROR'
  )
}
ruleset$rule_yyx_call_last1m_dialed_succ_ratio <- function(res,duration_limit = 6,
                                                           last_days = 30,
                                                           dialed_succ_ratio = .3){
  tryCatch(
    {
      res$moxieInfo$yunyingshangInfo$calls$items %>% 
        do.call('rbind',.) -> tmp
      tmp %>% dplyr::filter(stringr::str_to_upper(dial_type) == "DIALED") %>% 
        mutate(call_date = as.Date(time),dialed_success = duration > duration_limit
               ,ym = stringr::str_sub(time,1,7)) %>% 
        dplyr::filter(call_date > (Sys.Date() - last_days)) %>%
        select(peer_number,duration,dial_type,call_date,dialed_success,ym) %>% 
        unique.data.frame() %>% 
        group_by(ym) %>%
        summarise(success = sum(dialed_success,na.rm = T),all_dialed = n(),succ_rate = success / all_dialed) %$%
        succ_rate %>% mean(na.rm = T) %>% `>`(dialed_succ_ratio) %>% as.character()
    },
    error = function(e) 'ERROR'
  )
}


# txl
ruleset$rule_txl <- function(res,tel = 'tel',name = 'name'){
  tryCatch(
    { require(magrittr);require(stringr)
      # 亲属联系人
      qinshu_dict <- c("爸","父","妈","母","爹","妹妹","姐姐","哥哥","弟弟","爷爷","奶奶","外公","外婆","姥姥","姥爷","老公","老婆","媳妇",
                       "爱人","丈夫","舅","叔","婶","姑","表妹","表弟","表姐","表哥","宝贝","堂哥","堂姐",
                       "堂妹","堂弟","儿子","女儿","姨","伯","乖乖","宝宝")
      
      
      # 敏感联系人包含关键字 
      black_dict <- c("套现","贷款","信用卡代办","中介","口子","借钱","黑户","贷","贷款","中介","黑户","白户","白条","信用卡","口子","分期",
                      "金服","金融","财富","理财","融资","套现","提额","网贷","还款","还钱","借钱","借款","代还","pos机","催收","同行","抵押",
                      "无抵押","高利贷","信用","二手车","垫养","呆账","欠款","欠钱","赌博")
      
      #-- configs start---
      validate_limit = 20;
      qingshu_limit = 1;
      black_limit = 20
      #-- configs end---
      if(is.null(res) || is.na(res) || (!is.list(res))) return("TRUE")
      txl <- res$tongxunluInfo
      if(is.null(txl) || is.na(txl) || (txl == '')) return('TRUE')
      
      txl[,tel] <- txl[,tel] %>% str_remove_all("(\\s)|-|(\\+86)")
      index = (nchar(txl[,tel]) == 11 & txl[,tel] %>% str_detect("^1")) & (txl$tel %>% duplicated() %>% `!`)  
      txl = txl[index,]
      validate_tels <- txl[,tel]
      validate_num <- txl[,tel] %>% length()
      qingshu_num <- txl[,name] %>% sapply(function(x) sapply(qinshu_dict, str_detect,string=x) %>% any(na.rm = T) %>% sum(na.rm = T)) %>% sum(na.rm = T)
      black_num <- txl[,name] %>% sapply(function(x) sapply(black_dict, str_detect,string=x) %>% any(na.rm = T) %>% sum(na.rm = T)) %>% sum(na.rm = T)
      (validate_num >= validate_limit & qingshu_num > qingshu_limit & black_num <= black_limit) %>% as.character() 
    },
    error = function(e) 'ERROR'
  )
}
# rules config


parse_json_2_rules <- function(json){
  if(jsonlite::validate(json)) {
    res = jsonlite::fromJSON(json)
  }else{
    cat(Sys.time() %>% as.character(),' - ','paras:',json,'\n',
        file=paste('modellog',Sys.Date(),'.log',sep=""),append = TRUE)
    res = NULL
  }
  res
}
ruleFun <- function(json,ruleset=list()){
  rule_society_state <- c("rule_age","rule_sanyaosu","rule_zaiwang","rule_zmscore","rule_taobao_his_days","rule_taobao_shiming","rule_txl",
                          "rule_yyx_call_last6m_topin_txl","rule_yyx_call_last6m_concentrate",
                          "rule_yyx_call_last6m_Silent_days_n7_cnt","rule_yyx_call_last6m_Silent_days_n5_cnt","rule_yyx_call_last6m_Silent_days_n3_cnt",
                          "rule_yyx_call_last6m_dialed_succ_ratio","rule_yyx_call_last3m_dialed_succ_ratio","rule_yyx_call_last1m_dialed_succ_ratio")
  
  rule_student_state <- c("rule_age","rule_zaiwang","rule_zmscore","rule_taobao_his_days","rule_taobao_shiming",
                          "rule_xuexin_xueli_limit","rule_xuexin_in_school_limit","rule_xuexin_xuezhi_limit","rule_txl")
  tryCatch(
    { res <- parse_json_2_rules(json)
      society_id = is.null(res$moxieInfo$xuexinInfo) || !(ruleset[["rule_xuexin_xueli_limit"]](res) %in% 'TRUE')
      rt = list()
      if(society_id){
        for(x in rule_society_state){
          if(class(ruleset[[x]]) != "function") next
          rt[[x]] <- ruleset[[x]](res)
        }
      }else{
        for(x in rule_student_state){
          if(class(ruleset[[x]]) != "function") next
          rt[[x]] <- ruleset[[x]](res)
        }
      }
      rt %>% jsonlite::toJSON(na='null') %>% 
        cat(Sys.time() %>% as.character(),'rules:',res$baseInfo$realname,res$baseInfo$id_card,society_id,.,'\n',sep = ' - ',
                                                 file=paste('modellog',Sys.Date(),'.log',sep=""),append = TRUE)
      mingzhong <- !(rt %in% 'TRUE')
      rt[mingzhong] %>% names() 
    }
    ,error = function(e) NULL
  )
}
 

edu_level_check <- function(edu_level) edu_level %in% c('专科','本科','硕士研究生','博士研究生') %>% sum(na.rm = TRUE) # consider mult. edu.
edu_type_chenck <- function(edu_type) edu_type %in% c('普通','研究生','普通高等教育') %>% sum(na.rm = TRUE)
edu_form_chenck <- function(edu_form) edu_form %in% c('全日制','普通全日制') %>% sum(na.rm = TRUE)
edu_status_check <- function(edu_status) edu_status  %in% c('在籍注册学籍','在籍保留学籍') %>% sum(na.rm = TRUE)
edu_advice_amt_check <- function(edu_level) edu_level %>% car_recode("'专科' = 4500;'本科' = 5000;'硕士研究生' = 6000;'博士研究生' = 7000;else = 0") %>% max(na.rm = TRUE)
edu_level_max_level_check <- function(edu_level) edu_level %>% car_recode("'专科' = 1;'本科' = 2;'硕士研究生' = 3;'博士研究生' = 4;else = 0") %>% max(na.rm = TRUE)

parse_json_2_scores <- function(json){
  if(jsonlite::validate(json)) {
    cat(Sys.time() %>% as.character(),' - ','success!:paras:',json,'\n',
        file=paste('modellog',Sys.Date(),'.log',sep=""),append = TRUE)
    res = jsonlite::fromJSON(json)
  }else{
    cat(Sys.time() %>% as.character(),' - ','faied!:paras:',json,'\n',
        file=paste('modellog',Sys.Date(),'.log',sep=""),append = TRUE)
    res = NULL
  }
  infos <- data_frame(
    # dq
    realname = res$baseInfo$realname %>% check_char(),
    id_card = res$baseInfo$id_card %>% check_char(),
    zm_zmscore = res$baseInfo$zmscore %>% check_char(),
    age = res$baseInfo$age  %>% check_num(),
    sex = res$baseInfo$sex %>% check_char(),
    zaiwang = res$baseInfo$zaiwang %>% check_char(),
    query_sum_count = res$xinyanInfo$data$result_detail$apply_report_detail$query_sum_count  %>% check_num(),
    loans_cash_count = res$xinyanInfo$data$result_detail$behavior_report_detail$loans_cash_count  %>% check_num(),
    history_fail_fee = res$xinyanInfo$data$result_detail$behavior_report_detail$history_fail_fee  %>% check_num(),
    # dz
    zm_jianmian = res$baseInfo$zm_jianmian  %>% check_num(),
    real_mianya_ratio = res$baseInfo$real_mianya_ratio  %>% check_num(),
    latest_three_month = res$xinyanInfo$data$result_detail$apply_report_detail$latest_three_month %>% check_num(),
    loans_score = res$xinyanInfo$data$result_detail$behavior_report_detail$loans_score %>% check_num(),
    loans_credibility = res$xinyanInfo$data$result_detail$behavior_report_detail$loans_credibility %>% check_num(),
    
    # taobao jiebei huabei zhimafen
    taobao_zmscore = res$moxieInfo$taobaoReport$wealth_info$totalssets$taobao_zmscore %>% check_num(),
    taobao_jiebei_amount = res$moxieInfo$taobaoReport$wealth_info$totalssets$taobao_jiebei_amount %>% check_num(), 
    taobao_jiebie_available_amount = res$moxieInfo$taobaoReport$wealth_info$totalssets$taobao_jiebie_available_amount %>% check_num(),
    huai_bei_limit = res$moxieInfo$taobaoReport$wealth_info$totalssets$huai_bei_limit %>% check_num(),
    huai_bei_can_use_limit = res$moxieInfo$taobaoReport$wealth_info$totalssets$huai_bei_can_use_limit %>% check_num(),
    # zhifubaomianya & taobao scpyder jianrong
    taobao_zmscore_grade = taobao_zmscore %>% car_recode("0:600 = 'Z5';601:650 = 'Z4';651:700 = 'Z3';701:750 = 'Z2';751:Inf = 'Z1';else=NA") %>% check_char(),
    zmscore = ifelse(taobao_zmscore_grade %in% c('Z1','Z2','Z3','Z4','Z5'),taobao_zmscore_grade,zm_zmscore),
    
    # taobao basic_info
    tao_score = res$moxieInfo$taobaoReport$basic_info$user_and_account_basic_info$tao_score  %>% check_num(),
    first_ordertime = res$moxieInfo$taobaoReport$basic_info$user_and_account_basic_info$first_ordertime  %>% check_char(),
    account_auth = res$moxieInfo$taobaoReport$basic_info$user_and_account_basic_info$account_auth  %>% check_char(),
    taobao_vip_level = res$moxieInfo$taobaoReport$basic_info$user_and_account_basic_info$taobao_vip_level  %>% check_char(),
    taobao_vip_count = res$moxieInfo$taobaoReport$basic_info$user_and_account_basic_info$taobao_vip_count    %>% check_char(),
    
    # check socitety
  )
  infos 
}

scoreFun = function(json,str_sql,str_amt,loan_amt_ratio = 1.0,score_threshold = 600,max_loan_limit = 10000,rules_check_set = ruleset){
  decision <- 
    tryCatch({
      infos <- parse_json_2_scores(json)
      cat(Sys.time() %>% as.character(),' - ','paras:',infos %>% jsonlite::toJSON(na='null'),'\n',
          file=paste('modellog',Sys.Date(),'.log',sep=""),append = TRUE)
      library(DBI)
      con <- dbConnect(RSQLite::SQLite(), ":memory:")
      RSQLite::dbWriteTable(con, "infos", infos,overwrite = TRUE)
      rs <- DBI::dbSendQuery(con,str_sql)
      infos_w <- dbFetch(rs)
      DBI::dbClearResult(rs)
      DBI::dbDisconnect(con);
      # compute society amt
      str_amt = sprintf(str_amt,score_threshold)
      con <- dbConnect(RSQLite::SQLite(), ":memory:")
      RSQLite::dbWriteTable(con, "infos_w", infos_w,overwrite = TRUE)
      rs <- DBI::dbSendQuery(con,str_amt)
      decision <- dbFetch(rs)
      DBI::dbClearResult(rs)
      DBI::dbDisconnect(con);
      
      # compute society 
      res <- jsonlite::fromJSON(json)
      society_id = is.null(res$moxieInfo$xuexinInfo) || !(rules_check_set[["rule_xuexin_xueli_limit"]](res) %in% 'TRUE')
      student_id = !society_id
      decision$student_id = student_id
      # compute edu amt
      edu_advice_amt_f <- function(json) {
        tryCatch(
          { res <- jsonlite::fromJSON(json)
          if(is.null(res$moxieInfo$xuexinInfo$studentInfo_list$level)) return(0)
          # compute amt
          edu_advice_amt_check <- function(edu_level) {edu_level %>% 
              car_recode("'专科' = 4500;'本科' = 5000;'硕士研究生' = 6000;'博士研究生' = 7000;else = 0") %>% max(na.rm = TRUE)}
          amt = res$moxieInfo$xuexinInfo$studentInfo_list$level %>% edu_advice_amt_check() %>% check_num()
          amt
          },
          error = function(e) 0
        )
      }
      decision$edu_advice_amt <- edu_advice_amt_f(json)
      # compute final_amt
      final_amt_check <- function(advice_amt,edu_advice_amt,edu_student_status,max_loan_limit){ 
        advice_amt <- advice_amt %>% car_recode("NA = 0")
        edu_advice_amt <- edu_advice_amt %>% car_recode("NA = 0") 
        edu_student_status <- edu_student_status %>% car_recode("NA = 0")
        ifelse(edu_student_status,edu_advice_amt,advice_amt) %>% min(max_loan_limit,na.rm = T)
      }
      decision$advice_amt <- round(decision$advice_amt * loan_amt_ratio / 100) * 100 + round(mod(infos$age,10) / 3) * 100 # rnd
      decision$edu_advice_amt <- round(decision$edu_advice_amt * loan_amt_ratio / 100) * 100 + round(mod(infos$age,10) / 3) * 100 # rnd
      decision$final_amt <- final_amt_check(decision$advice_amt,decision$edu_advice_amt,student_id,max_loan_limit)

      
      
      decision$score_float <- decision$score
      decision$score <- round(decision$score) + round(mod(infos$age,10) / 3) # rnd
      
      # compute rules
      rule_mingzhong <- ruleFun(json,rules_check_set)
      decision$rule_mingzhong_num <- rule_mingzhong %>% length() 
      decision$rule_mingzhong <- rule_mingzhong %>% jsonlite::toJSON(na = 'null')

      decision$score = ifelse(decision$rule_mingzhong_num,110,decision$score) # rules rescore
      decision$advice = (decision$score >= score_threshold) %>% as.numeric()
      
      
      list(infos = infos,infos_w = infos_w,decision = decision) %>% jsonlite::toJSON(na = 'null') %>% 
        cat(Sys.time() %>% as.character(),' - ','results:',.,'\n',
            file=paste('modellog',Sys.Date(),'.log',sep=""),append = TRUE)
      # if(Sys.Date() == "2019-3-10") {stop('should be upgraded!')}
      decision %>% select(score,advice,advice_amt,edu_advice_amt,final_amt,rule_mingzhong_num,rule_mingzhong) 
    }
    ,error = function(e){
      data_frame(score = 0,advice =0,advice_amt=0,edu_advice_amt=0,final_amt=0) 
    })
  decision %>% mutate(score = score %>% {function(x)ifelse(is.na(x) || is.null(x),0,x)}() %>% as.numeric(),
                      advice = advice %>% {function(x)ifelse(is.na(x) || is.null(x),0,x)}() %>% as.numeric()
                      ) %>% jsonlite::toJSON(na = 'null')
}










str_dq = "
/******case******/
/** 
infos = 
**/

select 
--#------------------scorescale2sql----------------------#
383.903595255632  +  72.1347520444482  * log(score_p / (1-score_p))  as score,
*  
from 
( select 
1/(1 + exp(-1 * (1 * 1.57124794845992 + zaiwang * 0.998620195318036 + 
sex * 0.708322586241409 + age * 0.945319075870239 + zmscore * 
0.920134447328887 + query_sum_count * 0.774599560824178 + loans_cash_count * 
0.0032585981654242 + history_fail_fee * 0.295654539546532)))  as score_p,
*  
from 
( select 
/****** zaiwang ******/
case 
when zaiwang in ( '1' ) then -1.90791154596408  
when zaiwang in ( '3' ) then 0.264145159665213 
when zaiwang in ( '4' ) then 0.556686098339944 
when zaiwang in ( '2' ) then -1.25732397982293 
when zaiwang in ( '' ) then -1.90791154596408  --2.89396786284092 adjust 1.90791154596408
else  -0.09999  end as 
zaiwang ,
/******************************/
/****** sex ******/
case 
when sex in ( 'female' ) then 0.813148891874145 
when sex in ( 'male' ) then -0.172317343210599  
else  -0.09999  end as 
sex ,
/******************************/
/****** age ******/
case 
when  age <=23 then 1.30280609120266 
when 23 < age  then -0.334857665904626  
else  -0.09999  end as 
age ,
/******************************/
/****** zmscore ******/
case 
when zmscore in ( 'Z1' ) then 5.93072313092869 
when zmscore in ( 'Z2' ) then 2.91513836166312 
when zmscore in ( 'Z3' ) then 1.06014972566487 
when zmscore in ( 'Z4' ) then 0.413249932602901 
when zmscore in ( 'Z5' ) then -2.76878148389683  
else  -0.09999  end as 
zmscore ,
/******************************/
/****** query_sum_count ******/
case 
when  query_sum_count IS NULL then 1.37898231642044 
when  query_sum_count <=2 then 1.49951638544871 
when 2 < query_sum_count and query_sum_count <=4 then 1.27339220599636 
when 4 < query_sum_count and query_sum_count <=8 then -0.500460984876878 
when 8 < query_sum_count and query_sum_count <=21 then -1.04601488615572 
when 21 < query_sum_count  then -1.72325428368103  
else  -0.09999  end as 
query_sum_count ,
/******************************/
/****** loans_cash_count ******/
case 
when  loans_cash_count IS NULL then 0.512129731890826 
when  loans_cash_count <=0 then 0.595060111221559 
when 0 < loans_cash_count and loans_cash_count <=1 then 0.172645444799622 
when 1 < loans_cash_count and loans_cash_count <=5 then -1.15934357146273 
when 5 < loans_cash_count  then -2.32776539152434  
else  -0.09999  end as 
loans_cash_count ,
/******************************/
/****** history_fail_fee ******/
case 
when  history_fail_fee IS NULL then 0.512129731890826 
when  history_fail_fee <=0 then 1.39943292689173 
when 0 < history_fail_fee and history_fail_fee <=1 then -0.0980870693383861 
when 1 < history_fail_fee and history_fail_fee <=3 then -0.328610727950218 
when 3 < history_fail_fee and history_fail_fee <=12 then -1.35576405263618 
when 12 < history_fail_fee  then -1.90168099621344  
else  -0.09999  end as 
history_fail_fee  
from  infos --you should modify the table name. 
)a 
)b"





#-----dz-------
str_dz = "
select
--# base_odds = 20
--# pdo = 50
--# base_points = 600
--# A = 383.903595255632
--# B = 72.1347520444482 
--#------------------scorescale2sql----------------------#
383.903595255632  +  72.1347520444482  * log(score_p / (1-score_p))  as score,
*  
from
(
  select
  1/(1 + exp(-1 * (1 * 1.57942619813914 + zm_jianmian * 0.294360478026009 +  
  zmscore * 0.739145652761913 + real_mianya_ratio * 0.176859245560491 +  
  loans_score * 0.394566878360799 + latest_three_month * 0.292217214470885 +  
  loans_credibility * 0.412137194494653 + sex * 0.472392098368664)))
  as score_p,
  *
  from
  (
  select 
  /****** zm_jianmian ******/
  case 
  when  zm_jianmian <=0 then -1.34998606941364 
  when 0 < zm_jianmian and zm_jianmian <=1e+03 then 0.701730924251337 
  when 1e+03 < zm_jianmian and zm_jianmian <=1.95e+03 then 2.3947555527937 
  when 1.95e+03 < zm_jianmian and zm_jianmian <=3.14e+03 then 1.4971569539317 
  when 3.14e+03 < zm_jianmian  then 2.40414529314354  
  else  -0.09999  end as 
  zm_jianmian ,
  /******************************/
  /****** zmscore ******/
  case 
  when zmscore in ( 'Z5' ) then -2.62285535503898 
  when zmscore in ( 'Z1' ) then 2.5  --edit:0-->2.5
  when zmscore in ( 'Z4' ) then 0.180883742024327 
  when zmscore in ( 'Z3' ) then 1.16236680349347 
  when zmscore in ( 'Z2' ) then 1.79175946922805  
  else  -0.09999  end as 
  zmscore ,
  /******************************/
  /****** real_mianya_ratio ******/
  case 
  when  real_mianya_ratio <=0.601 then -1.48698296341697 
  when 0.601 < real_mianya_ratio and real_mianya_ratio <=0.862 then -0.566872308753249 
  when 0.862 < real_mianya_ratio and real_mianya_ratio <=1 then 1.79488936223698  
  else  -0.09999  end as 
  real_mianya_ratio ,
  /******************************/
  /****** loans_score  ******/
  case 
  when  loans_score IS NULL then 0.540393474184554 
  when  loans_score <=494 then -2.17631022118735 
  when 494 < loans_score and loans_score <=577 then -0.847297860387204 
  when 577 < loans_score and loans_score <=600 then -0.510825623765991 
  when 600 < loans_score and loans_score <=610 then 1.06352096885684 
  when 610 < loans_score  then 2.11334309335552  
  else  -0.09999  end as 
  loans_score ,
  /******************************/
  /****** latest_three_month ******/
  case 
  when  latest_three_month IS NULL then 1.09284858395136 
  when  latest_three_month <=1 then 0.727048732235627 
  when 1 < latest_three_month and latest_three_month <=2 then -0.189241999638528 
  when 2 < latest_three_month and latest_three_month <=5 then 0.0136988443581619 
  when 5 < latest_three_month and latest_three_month <=14 then -0.779204943963243 
  when 14 < latest_three_month  then -1.43035435091392  
  else  -0.09999  end as 
  latest_three_month ,
  /******************************/
  /****** loans_credibility ******/
  case 
  when  loans_credibility IS NULL then 0.540393474184554 
  when  loans_credibility <=73 then -2.07452752687741 
  when 73 < loans_credibility and loans_credibility <=77 then -0.402816099936587 
  when 77 < loans_credibility and loans_credibility <=78 then -0.154150679827258 
  when 78 < loans_credibility and loans_credibility <=80 then 0.425943639451705 
  when 80 < loans_credibility  then 0.0984400728132525  
  else  -0.09999  end as 
  loans_credibility ,
  /******************************/
  /****** sex ******/
  case 
  when sex in ( 'female' ) then 0.794707380709441 
  when sex in ( 'male' ) then -0.152147875232288  
  else  -0.09999  end as 
  sex
  from  infos --you should modify the table name.
  )a 
)b"

  
  
  str_amt = "
  select 
  case 
  when score > %s then 1 else 0 
  end as advice
  ,
  case
  when	score	<=400	then	1000
  when	score	<=500	then	2000
  when	score	<=550	then	2500
  when	score	<=575	then	3000
  when	score	<=600	then	3500
  when	score	<=625	then	4000
  when	score	<=650	then	4500
  when	score	<=675	then	4800
  when	score	<=700	then	5000
  when	score	<=750	then	6000
  when	score	<=800	then	6500
  when	score	<=850	then	7000
  when	score	<=900	then	8000
  when	score	>900	then	9000
  else	0			
  end as	advice_amt			
  ,*
  from 
  infos_w
  "
  
  

  
  

    
  

  


# self-defined function
car_recode <-
  function (var, recodes, as.factor, as.numeric = TRUE, levels) 
  {
    squeezeBlanks <- function (text) 
    {
      gsub(" *", "", text)
    }
    lo <- -Inf
    hi <- Inf
    recodes <- gsub("\n|\t", " ", recodes)
    recode.list <- rev(strsplit(recodes, ";")[[1]])
    is.fac <- is.factor(var)
    if (missing(as.factor)) 
      as.factor <- is.fac
    if (is.fac) 
      var <- as.character(var)
    result <- var
    for (term in recode.list) {
      if (0 < length(grep(":", term))) {
        range <- strsplit(strsplit(term, "=")[[1]][1], ":")
        low <- try(eval(parse(text = range[[1]][1])), silent = TRUE)
        if (class(low) == "try-error") {
          stop("\n  in recode term: ", term, "\n  message: ", 
               low)
        }
        high <- try(eval(parse(text = range[[1]][2])), silent = TRUE)
        if (class(high) == "try-error") {
          stop("\n  in recode term: ", term, "\n  message: ", 
               high)
        }
        target <- try(eval(parse(text = strsplit(term, "=")[[1]][2])), 
                      silent = TRUE)
        if (class(target) == "try-error") {
          stop("\n  in recode term: ", term, "\n  message: ", 
               target)
        }
        result[(var >= low) & (var <= high)] <- target
      }
      else if (0 < length(grep("^else=", squeezeBlanks(term)))) {
        target <- try(eval(parse(text = strsplit(term, "=")[[1]][2])), 
                      silent = TRUE)
        if (class(target) == "try-error") {
          stop("\n  in recode term: ", term, "\n  message: ", 
               target)
        }
        result[1:length(var)] <- target
      }
      else {
        set <- try(eval(parse(text = strsplit(term, "=")[[1]][1])), 
                   silent = TRUE)
        if (class(set) == "try-error") {
          stop("\n  in recode term: ", term, "\n  message: ", 
               set)
        }
        target <- try(eval(parse(text = strsplit(term, "=")[[1]][2])), 
                      silent = TRUE)
        if (class(target) == "try-error") {
          stop("\n  in recode term: ", term, "\n  message: ", 
               target)
        }
        for (val in set) {
          if (is.na(val)) 
            result[is.na(var)] <- target
          else result[var == val] <- target
        }
      }
    }
    if (as.factor) {
      result <- if (!missing(levels)) 
        factor(result, levels = levels)
      else as.factor(result)
    }
    else if (as.numeric && (!is.numeric(result))) {
      result.valid <- na.omit(result)
      opt <- options(warn = -1)
      result.valid <- as.numeric(result.valid)
      options(opt)
      if (!any(is.na(result.valid))) 
        result <- as.numeric(result)
    }
    result
  }
