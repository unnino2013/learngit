
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
library(futile.logger)
# x %>% car_recode("1:599 = 'Z5';600:649 = 'Z4';650:699 = 'Z3';700:749 = 'Z2';750:Inf = 'Z1';else=NA")

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
ruleset$rule_region <- function(res){
  tryCatch(
    {
      id_card <- res$baseInfo$id_card
      if(is.null(id_card)) stop("error:id_card is NULL!")
      risk_list <- c('65','54','81','82','71') 
      risk_list1 <- c('5203','4502','4524','4508',
                      '6402','6403','6422','4527',
                      '5224','5223','4521','3508',
                      '3504','4526','4505','3506',
                      '4506','5225','3521','3509',
                      '5202','5226','4504','5001','4503')
      ret <- (str_sub(id_card,1,2) %in% risk_list) || (str_sub(id_card,1,4) %in% risk_list1)
      (!ret) %>% as.character()
    }
    ,error = function(e){
      flog.logger(name='ROOT',INFO,appender = appender.file(paste(Sys.Date(),'modellog.log')),
                  layout.format('[~l] [~t] [~n.~f]rule_region: ~m'));flog.error('%s',e)
      "ERROR"
    }
  )
}
ruleset$rule_age <- function(res=list(),age_limit = c(18,45)){
  tryCatch(
    {
      require(lubridate)
      require(magrittr)
      require(stringr)
      age <- res$baseInfo$id_card %>% str_sub(7,14) %>% as.Date.character(.,format = "%Y%m%d") %>% `-`(Sys.Date(),.) %>% `/`(ddays(365))
      if(check_null_NA(age)) stop('ERROR:res$baseInfo$id_card computed age is null.')
      age_limit <- sort(age_limit)
      between(age,age_limit[1],age_limit[2]) %>% as.character()},
    error = function(e){
      flog.logger(name='ROOT',INFO,appender = appender.file(paste(Sys.Date(),'modellog.log')),
                  layout.format('[~l] [~t] [~n.~f]rule_age: ~m'));flog.error('%s',e)
      "ERROR"
    }
  )
}
ruleset$rule_sanyaosu <- function(res){
  tryCatch(
    {
      sanyaoshu <- res$baseInfo$san %>% as.character() %>% str_to_upper()
      if(check_null_NA(sanyaoshu)) stop('ERROR:sanyaoshu is null.')
      sanyaoshu %in% c('1','TRUE') %>% as.character()
    },
    error = function(e){
      flog.logger(name='ROOT',INFO,appender = appender.file(paste(Sys.Date(),'modellog.log')),
                  layout.format('[~l] [~t] [~n.~f]rule_sanyaosu: ~m'));flog.error('%s',e)
      "ERROR"
    }
  )
}
ruleset$rule_zaiwang <- function(res){
  tryCatch(
    {
      zaiwang <- res$baseInfo$zaiwang %>% as.character() %>% str_to_upper()
      if(check_null_NA(zaiwang)) stop('ERROR:zaiwang is null or NA.')
      zaiwang %in% c('2','3','4')  %>% as.character()
    },
    error = function(e){
      flog.logger(name='ROOT',INFO,appender = appender.file(paste(Sys.Date(),'modellog.log')),
                  layout.format('[~l] [~t] [~n.~f]rule_zaiwang: ~m'));flog.error('%s',e)
      "ERROR"
    }
  )
}
# taobao
ruleset$rule_zmscore <- function(res){
  tryCatch(
    {
      # zhifubaomianya & taobao scpyder jianrong
      zm_zmscore = res$baseInfo$zmscore %>% check_char()
      taobao_zmscore = res$moxieInfo$taobaoReport$wealth_info$totalssets$taobao_zmscore %>% check_num()
      taobao_zmscore_grade = taobao_zmscore %>% car_recode("1:599 = 'Z5';600:649 = 'Z4';650:699 = 'Z3';700:749 = 'Z2';750:Inf = 'Z1';else=NA") %>% check_char()
      zmscore = ifelse(taobao_zmscore_grade %in% c('Z1','Z2','Z3','Z4','Z5'),taobao_zmscore_grade,zm_zmscore)
      if(check_null_NA(zmscore)) return('ERROR')
      zmscore %in% c('Z1','Z2','Z3','Z4')  %>% as.character()
    },
    error = function(e){
      flog.logger(name='ROOT',INFO,appender = appender.file(paste(Sys.Date(),'modellog.log')),
                  layout.format('[~l] [~t] [~n.~f]rule_zmscore: ~m'));flog.error('%s',e)
      "ERROR"
    }
  )
}
ruleset$rule_taobao_his_days <- function(res,taobao_his_days_limit = 180){
  tryCatch(
    {
      if(is.null(res$moxieInfo$taobaoReport)) stop('ERROR:res$moxieInfo$taobaoReport is null.')
      taobao_first_ordertime <- res$moxieInfo$taobaoReport$basic_info$user_and_account_basic_info$first_ordertime %>% check_NA()
      taobao_his_days <- taobao_first_ordertime %>% check_date() %>% `-`(Sys.Date(),.)  %>% `/`(ddays(1))  %>% check_NA()
      if(is.null(taobao_his_days) || is.na(taobao_his_days)) stop('ERROR:taobao_his_days is null.')
      (taobao_his_days > taobao_his_days_limit) %>% as.character()
    },
    error = function(e){
      flog.logger(name='ROOT',INFO,appender = appender.file(paste(Sys.Date(),'modellog.log')),
                  layout.format('[~l] [~t] [~n.~f]rule_taobao_his_days: ~m'));flog.error('%s',e)
      "ERROR"
    }
  )
}
ruleset$rule_taobao_alipay_his_days <- function(res,taobao_alipay_his_days_limit = 180){
  tryCatch(
    {
      if(is.null(res$moxieInfo$taobaoReport)) stop('ERROR:res$moxieInfo$taobaoReport is null.')
      alipay_register_time <- res$moxieInfo$alipayInfo$userinfo$register_time %>% check_NA()
      taobao_alipay_his_days <- alipay_register_time %>% check_date() %>% `-`(Sys.Date(),.)  %>% `/`(ddays(1))  %>% check_NA()
      if(is.null(taobao_alipay_his_days) || is.na(taobao_alipay_his_days)) stop('ERROR:taobao_alipay_his_days is null.')
      (taobao_alipay_his_days > taobao_alipay_his_days_limit) %>% as.character()
    },
    error = function(e){
      flog.logger(name='ROOT',INFO,appender = appender.file(paste(Sys.Date(),'modellog.log')),
                  layout.format('[~l] [~t] [~n.~f]rule_taobao_alipay_his_days: ~m'));flog.error('%s',e)
      "ERROR"
    }
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
      if(is.null(res$baseInfo$san)) stop('ERROR:res$baseInfo$san is null.')
      if(is.null(res$moxieInfo$taobaoInfo$userinfo$real_name)) stop('ERROR:res$moxieInfo$taobaoInfo$userinfo$real_name is null.')
      if(is.null(res$moxieInfo$taobaoInfo$userinfo$phone_number)) stop('ERROR:res$moxieInfo$taobaoInfo$userinfo$phone_number is null.')
      sanyaoshu <- res$baseInfo$san
      tel <- res$baseInfo$tel
      taobao_namelist = res$moxieInfo$taobaoInfo$deliveraddress$name %>% c(res$moxieInfo$taobaoInfo$userinfo$real_name) %>% unique() %>% check_NA()
      taobao_tellist = res$moxieInfo$taobaoInfo$deliveraddress$phone_no %>% c(res$moxieInfo$taobaoInfo$userinfo$phone_number) %>% unique() %>% check_NA()
      name_check_status = name_check(res$baseInfo$realname,taobao_namelist) %>% check_NA()
      tel_check_status = tel_check(tel,taobao_tellist) %>% check_NA()
      shiming_grade_check(sanyaoshu,tel_check_status,name_check_status)
    },
    error = function(e){
      flog.logger(name='ROOT',INFO,appender = appender.file(paste(Sys.Date(),'modellog.log')),
                  layout.format('[~l] [~t] [~n.~f]rule_taobao_shiming: ~m'));flog.error('%s',e)
      "ERROR"
    }
  )
}
# xuexin
ruleset$rule_xuexin_xueli_limit <- function(res){
  tryCatch(
    {
      if(is.null(res$moxieInfo$xuexinInfo)) stop('ERROR:res$moxieInfo$xuexinInfo is null.')
      if(is.null(res$baseInfo$id_card) || is.null(res$moxieInfo$xuexinInfo$studentInfo_list$id_card)) stop('ERROR:res$baseInfo$id_card or res$moxieInfo$xuexinInfo$studentInfo_list$id_card is null.')
      if(!res$baseInfo$id_card %in% res$moxieInfo$xuexinInfo$studentInfo_list$id_card) stop('ERROR:res$baseInfo$id_card != res$moxieInfo$xuexinInfo$studentInfo_list$id_card') 
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
    error = function(e){
      flog.logger(name='ROOT',INFO,appender = appender.file(paste(Sys.Date(),'modellog.log')),
                  layout.format('[~l] [~t] [~n.~f]rule_xuexin_xueli_limit: ~m'));flog.error('%s',e)
      "ERROR"
    }
  )
}
ruleset$rule_xuexin_in_school_limit <- function(res,edu_leave_school_years = 0){
  tryCatch(
    {
      if(is.null(res$moxieInfo$xuexinInfo)) stop('ERROR:res$moxieInfo$xuexinInfo is null.')
      if(is.null(res$baseInfo$id_card) || is.null(res$moxieInfo$xuexinInfo$studentInfo_list$id_card)) stop('ERROR:res$baseInfo$id_card or res$moxieInfo$xuexinInfo$studentInfo_list$id_card is null.')
      if(!res$baseInfo$id_card %in% res$moxieInfo$xuexinInfo$studentInfo_list$id_card) stop('ERROR:res$baseInfo$id_card != res$moxieInfo$xuexinInfo$studentInfo_list$id_card') 
      if(is.null(res$moxieInfo$xuexinInfo$studentInfo_list$leave_school_time) || 
         is.null(res$moxieInfo$xuexinInfo$studentInfo_list$enrollment_time)) stop('ERROR:res$moxieInfo$xuexinInfo$studentInfo_list$leave_school_time or res$moxieInfo$xuexinInfo$studentInfo_list$enrollment_time is null.')
      
      edu_level_max_level_check <- function(edu_level) {edu_level %>%
          car_recode("'专科' = 1;'本科' = 2;'硕士研究生' = 3;'博士研究生' = 4;else = 0") %>% max(na.rm = TRUE)}
      edu_level_num <- res$moxieInfo$xuexinInfo$studentInfo_list$level %>% edu_level_max_level_check() %>% check_NA() # xueli
      
      edu_leave_school_time <- res$moxieInfo$xuexinInfo$studentInfo_list$leave_school_time %>% check_date() %>% check_NA()
      edu_enrollment_time  <-  res$moxieInfo$xuexinInfo$studentInfo_list$enrollment_time %>% check_date() %>% check_NA()
      edu_grade <- Sys.Date() %>% check_date() %>% `-`(edu_enrollment_time) %>% `/`(ddays(365)) %>% ceiling() # 1234year # nianji 12345678
      edu_xuezhi <- edu_leave_school_time %>% check_date() %>% `-`(edu_enrollment_time) %>% `/`(ddays(365)) %>% ceiling() # xuezhi 1234
      
      rt  <- 
        ((edu_level_num %in% c(1,2)) && (edu_xuezhi >= 2) && (edu_grade <= (edu_xuezhi + edu_leave_school_years))) || # master below limit
        (edu_level_num > 2) # master above no limit
      rt %>% as.character()
    },
    error = function(e){
      flog.logger(name='ROOT',INFO,appender = appender.file(paste(Sys.Date(),'modellog.log')),
                  layout.format('[~l] [~t] [~n.~f]rule_xuexin_in_school_limit: ~m'));flog.error('%s',e)
      "ERROR"
    }
  )
}
ruleset$rule_xuexin_xuezhi_limit <- function(res){
  tryCatch(
    {
      if(is.null(res$moxieInfo$xuexinInfo)) stop('ERROR:res$moxieInfo$xuexinInfo is null.')
      if(is.null(res$baseInfo$id_card) || is.null(res$moxieInfo$xuexinInfo$studentInfo_list$id_card)) stop('ERROR:res$baseInfo$id_card or res$moxieInfo$xuexinInfo$studentInfo_list$id_card is null.')
      if(!res$baseInfo$id_card %in% res$moxieInfo$xuexinInfo$studentInfo_list$id_card) stop('ERROR:res$baseInfo$id_card != res$moxieInfo$xuexinInfo$studentInfo_list$id_card') 
      if(is.null(res$moxieInfo$xuexinInfo$studentInfo_list$leave_school_time) || 
         is.null(res$moxieInfo$xuexinInfo$studentInfo_list$enrollment_time)) stop('ERROR:res$moxieInfo$xuexinInfo$studentInfo_list$leave_school_time or res$moxieInfo$xuexinInfo$studentInfo_list$enrollment_time is null.')
      
      edu_leave_school_time <- res$moxieInfo$xuexinInfo$studentInfo_list$leave_school_time %>% check_date() %>% check_NA()
      edu_enrollment_time  <-  res$moxieInfo$xuexinInfo$studentInfo_list$enrollment_time %>% check_date() %>% check_NA()
      edu_xuezhi <- edu_leave_school_time %>% check_date() %>% `-`(edu_enrollment_time) %>% `/`(ddays(365)) %>% ceiling() # xuezhi 1234
      
      (edu_xuezhi >= 2) %>% any()  %>% as.character()
    },
    error = function(e){
      flog.logger(name='ROOT',INFO,appender = appender.file(paste(Sys.Date(),'modellog.log')),
                  layout.format('[~l] [~t] [~n.~f]rule_xuexin_xuezhi_limit: ~m'));flog.error('%s',e)
      "ERROR"
    }
  )
}

ruleset$rule_zmscore_edu <- function(res){
  tryCatch(
    {
      # zhifubaomianya & taobao scpyder jianrong
      zm_zmscore = res$baseInfo$zmscore %>% check_char()
      taobao_zmscore = res$moxieInfo$taobaoReport$wealth_info$totalssets$taobao_zmscore %>% check_num()
      taobao_zmscore_grade = taobao_zmscore %>% car_recode("1:599 = 'Z5';600:649 = 'Z4';650:699 = 'Z3';700:749 = 'Z2';750:Inf = 'Z1';else=NA") %>% check_char()
      zmscore = ifelse(taobao_zmscore_grade %in% c('Z1','Z2','Z3','Z4','Z5'),taobao_zmscore_grade,zm_zmscore)
      if(check_null_NA(zmscore)) return('TRUE')
      if(!zmscore %in% c('Z1','Z2','Z3','Z4','Z5')) return('TRUE')
      zmscore %in% c('Z1','Z2','Z3','Z4')  %>% as.character()
    },
    error = function(e){
      flog.logger(name='ROOT',INFO,appender = appender.file(paste(Sys.Date(),'modellog.log')),
                  layout.format('[~l] [~t] [~n.~f]rule_zmscore_edu: ~m'));flog.error('%s',e)
      "ERROR"
    }
  )
}

ruleset$rule_xinyan_edu <- function(res){
  tryCatch(
    {
      rt <- list(
        apply__query_sum_count = res$xinyanInfo$result_detail$apply_report_detail$query_sum_count %>% check_num() %>% `>`(15)    # 总查询次数 >21    
        ,apply__query_org_count = res$xinyanInfo$result_detail$apply_report_detail$query_org_count %>% check_num() %>% `>`(10)    # 查询机构数 >18   
        ,apply__latest_six_month =  res$xinyanInfo$result_detail$apply_report_detail$latest_six_month %>% check_num() %>% `>`(18)    # 近6个月总查询笔数 >18  
        ,apply__query_cash_count = res$xinyanInfo$result_detail$apply_report_detail$query_cash_count %>% check_num() %>% `>`(10)    # 查询网络贷款类机构数 >10  
        ,apply__query_finance_count = res$xinyanInfo$result_detail$apply_report_detail$query_finance_count %>% check_num() %>% `>`(5)    # 查询消费金融类机构数 > 2  
        ,apply__latest_three_month = res$xinyanInfo$result_detail$apply_report_detail$latest_three_month %>% check_num() %>% `>`(11)    # 近3个月总查询笔数>11  
        ,behavior__loans_org_count = res$xinyanInfo$result_detail$behavior_report_detail$loans_org_count %>% check_num() %>% `>`(2)    # 贷款机构数>8   
        ,behavior__loans_cash_count = res$xinyanInfo$result_detail$behavior_report_detail$loans_cash_count %>% check_num() %>% `>`(2)    # 网络贷款类机构数 > 5   
        ,behavior__history_fail_fee = res$xinyanInfo$result_detail$behavior_report_detail$history_fail_fee %>% check_num() %>% `>`(10)    # 历史贷款机构失败扣款笔数 > 12  
        ,current__loans_product_count = res$xinyanInfo$result_detail$current_report_detail$loans_product_count %>% check_num() %>% `>`(2)    # 网络贷款类产品数 > 5  
        ,behavior__loans_score = res$xinyanInfo$result_detail$behavior_report_detail$loans_score %>% check_num() %>% `<=`(493)    # 贷款行为分 <= 493  
        ,behavior__loans_count = res$xinyanInfo$result_detail$behavior_report_detail$loans_count %>% check_num() %>% `>`(5)    # 贷款放款总订单数 >21   
        ,behavior__loans_overdue_count = res$xinyanInfo$result_detail$behavior_report_detail$loans_overdue_count %>% check_num() %>% `>`(2)    # 贷款逾期订单数M0plus > 2  
        ,behavior__consfin_org_count = res$xinyanInfo$result_detail$behavior_report_detail$consfin_org_count %>% check_num() %>% `>`(2)    # 消费金融类机构数 > 4 
      )
      if(which(is.na(rt)) %>% length()){
        flog.logger(name='ROOT',INFO,appender = appender.file(paste(Sys.Date(),'modellog.log')),
                    layout.format('[~l] [~t] [~n.~f]rule_xinyan_edu: ~m'));flog.info('%s',rt %>% jsonlite::toJSON(na = "null"))
      }
      logic_rt <- !sum(rt %>% unlist(),na.rm = T)
      logic_rt %>% as.character()
      
    },
    error = function(e){
      flog.logger(name='ROOT',INFO,appender = appender.file(paste(Sys.Date(),'modellog.log')),
                  layout.format('[~l] [~t] [~n.~f]rule_xinyan_edu: ~m'));flog.error('%s',e)
      "ERROR"
    }
  )
  
}

ruleset$rule_taobao_his_days_edu <- function(res,taobao_his_days_limit = 30){
  tryCatch(
    {
      if(is.null(res$moxieInfo$taobaoReport)) stop('ERROR:res$moxieInfo$taobaoReport is null.')
      taobao_first_ordertime <- res$moxieInfo$taobaoReport$basic_info$user_and_account_basic_info$first_ordertime %>% check_NA()
      taobao_his_days <- taobao_first_ordertime %>% check_date() %>% `-`(Sys.Date(),.)  %>% `/`(ddays(1))  %>% check_NA()
      if(is.null(taobao_his_days) || is.na(taobao_his_days)) stop('ERROR:taobao_his_days is null.')
      (taobao_his_days > taobao_his_days_limit) %>% as.character()
    },
    error = function(e){
      flog.logger(name='ROOT',INFO,appender = appender.file(paste(Sys.Date(),'modellog.log')),
                  layout.format('[~l] [~t] [~n.~f]rule_taobao_his_days_edu: ~m'));flog.error('%s',e)
      "ERROR"
    }
  )
}
ruleset$rule_taobao_alipay_his_days_edu <- function(res,taobao_alipay_his_days_limit = 30){
  tryCatch(
    {
      if(is.null(res$moxieInfo$taobaoReport)) stop('ERROR:res$moxieInfo$taobaoReport is null.')
      alipay_register_time <- res$moxieInfo$alipayInfo$userinfo$register_time %>% check_NA()
      taobao_alipay_his_days <- alipay_register_time %>% check_date() %>% `-`(Sys.Date(),.)  %>% `/`(ddays(1))  %>% check_NA()
      if(is.null(taobao_alipay_his_days) || is.na(taobao_alipay_his_days)) stop('ERROR:taobao_alipay_his_days is null.')
      (taobao_alipay_his_days > taobao_alipay_his_days_limit) %>% as.character()
    },
    error = function(e){
      flog.logger(name='ROOT',INFO,appender = appender.file(paste(Sys.Date(),'modellog.log')),
                  layout.format('[~l] [~t] [~n.~f]rule_taobao_alipay_his_days_edu: ~m'));flog.error('%s',e)
      "ERROR"
    }
  )
}

ruleset$rule_taobao_address_edu <- function(res){
  tryCatch(
    {
      if(is.null(res$moxieInfo$taobaoInfo$recentdeliveraddress$deliver_address)) stop("error:res$moxieInfo$taobaoInfo$recentdeliveraddress$deliver_address is NULL!")
      if(is.null(res$moxieInfo$xuexinInfo$studentInfo_list$school_name)) stop("error:res$moxieInfo$xuexinInfo$studentInfo_list$school_name is NULL!")
      res$moxieInfo$taobaoInfo$recentdeliveraddress$deliver_address %>% unique() %>% str_detect(res$moxieInfo$xuexinInfo$studentInfo_list$school_name) %>% any() %>% as.character()
    },
    error = function(e){
      flog.logger(name='ROOT',INFO,appender = appender.file(paste(Sys.Date(),'modellog.log')),
                  layout.format('[~l] [~t] [~n.~f]rule_taobao_address_edu: ~m'));flog.error('%s',e)
      "ERROR"
    }
  )
}
ruleset$rule_taobao_huabei_amt_edu <- function(res,huabei_amt_limit = 20){
  tryCatch(
    { require(magrittr);require(stringr)
      # taobaoReport huabei unit yuan;and taobaoInfo huabei unit fen. version of moxie is taobaoxinxiV6 taobaobaogaoV4.
      if(is.null(res$moxieInfo$taobaoReport$wealth_info$totalssets$huai_bei_limit)) stop("error:res$moxieInfo$taobaoReport$wealth_info$totalssets$huai_bei_limit is NULL!")
      res$moxieInfo$taobaoReport$wealth_info$totalssets$huai_bei_limit %>% as.numeric() %>% `>`(huabei_amt_limit) %>% as.character() 
    },
    error = function(e){
      flog.logger(name='ROOT',INFO,appender = appender.file(paste(Sys.Date(),'modellog.log')),
                  layout.format('[~l] [~t] [~n.~f]rule_taobao_huabei_amt_edu: ~m'));flog.error('%s',e)
      "ERROR"
    }
  )
}
ruleset$rule_txl_edu <- function(res,tel = 'tel',name = 'name'){
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
      edu_dict <- c("辅导","老师","班主任","班长","同学")
      #-- configs start---
      validate_limit = 20;
      qingshu_limit = 3;
      black_limit = 3
      edu_limit = 2
      #-- configs end---
      # if(is.null(res) || is.na(res) || (!is.list(res))) return("TRUE")
      txl <- res$tongxunluInfo
      # if(is.null(txl) || is.na(txl) || (txl == '')) return('TRUE')
      
      txl[,tel] <- txl[,tel] %>% str_remove_all("(\\s)|-|(\\+86)")
      index = (nchar(txl[,tel]) == 11 & txl[,tel] %>% str_detect("^1")) & (txl$tel %>% duplicated() %>% `!`)  
      txl = txl[index,]
      validate_tels <- txl[,tel]
      validate_num <- txl[,tel] %>% length()
      qingshu_num <- txl[,name] %>% sapply(function(x) sapply(qinshu_dict, str_detect,string=x) %>% any(na.rm = T) %>% sum(na.rm = T)) %>% sum(na.rm = T)
      black_num <- txl[,name] %>% sapply(function(x) sapply(black_dict, str_detect,string=x) %>% any(na.rm = T) %>% sum(na.rm = T)) %>% sum(na.rm = T)
      edu_num <- txl[,name] %>% sapply(function(x) sapply(edu_dict, str_detect,string=x) %>% any(na.rm = T) %>% sum(na.rm = T)) %>% sum(na.rm = T)
      (validate_num >= validate_limit & qingshu_num >= qingshu_limit & black_num <= black_limit & edu_num >= edu_limit) %>% as.character() 
    },
    error = function(e){
      flog.logger(name='ROOT',INFO,appender = appender.file(paste(Sys.Date(),'modellog.log')),
                  layout.format('[~l] [~t] [~n.~f]rule_txl_edu: ~m'));flog.error('%s',e)
      "ERROR"
    }
  )
}
# yyx
ruleset$rule_yyx_shiming <- function(res){
  tryCatch(
    {
      if(is.null(res$baseInfo$san)) stop('ERROR:res$baseInfo$san is null.')
      if(is.null(res$moxieInfo$yunyingshangInfo)) stop('ERROR:res$moxieInfo$yunyingshangInfo is null')
      if(is.null(res$moxieInfo$yunyingshangInfo$name)) stop('ERROR:res$moxieInfo$yunyingshangInfo$name is null')
      if(length(res$baseInfo$realname) != length(res$moxieInfo$yunyingshangInfo$name)) stop('ERROR:res$baseInfo$realname != res$moxieInfo$yunyingshangInfo$name.')
      realname  <- res$baseInfo$realname %>% str_split(pattern = '') %>% unlist()
      yys_sm <- res$moxieInfo$yunyingshangInfo$name %>% str_split(pattern = '') %>% unlist() %>% `==`(realname) %>% any()
      res$baseInfo$san %>% str_to_lower() %>% `%in%`(c('1','true')) %>% `||`(yys_sm)
    },
    error = function(e){
      flog.logger(name='ROOT',INFO,appender = appender.file(paste(Sys.Date(),'modellog.log')),
                  layout.format('[~l] [~t] [~n.~f]rule_yyx_shiming: ~m'));flog.error('%s',e)
      "ERROR"
    }
  )
}
ruleset$rule_yyx_his <- function(res,yys_his_days_limit = 180){
  tryCatch(
    {
      if(is.null(res$moxieInfo$yunyingshangInfo)) stop('ERROR:res$moxieInfo$yunyingshangInfo is null.')
      if(is.null(res$moxieInfo$yunyingshangInfo$open_time)) stop('ERROR:res$moxieInfo$yunyingshangInfo$open_time is null.')
      open_time <- res$moxieInfo$yunyingshangInfo$open_time
      yys_his_days <- open_time %>% check_date() %>% `-`(Sys.Date(),.)  %>% `/`(ddays(1))  %>% check_NA()
      if(is.null(yys_his_days) || is.na(yys_his_days)) stop('ERROR:yys_his_days is null or NA.')
      (open_time > yys_his_days_limit) %>% as.character()
    },
    error = function(e){
      flog.logger(name='ROOT',INFO,appender = appender.file(paste(Sys.Date(),'modellog.log')),
                  layout.format('[~l] [~t] [~n.~f]rule_yyx_his: ~m'));flog.error('%s',e)
      "ERROR"
    }
  )
}
ruleset$rule_yyx_zaiwang_state <- function(res){
  tryCatch(
    {
      if(is.null(res$moxieInfo$yunyingshangInfo)) return('ERROR')
      if(is.null(res$moxieInfo$yunyingshangInfo$message) && is.null(res$moxieInfo$yunyingshangInfo$state)) return('ERROR')
      c(res$moxieInfo$yunyingshangInfo$message,res$moxieInfo$yunyingshangInfo$state) %>% `==`("正常") %>% any() %>% as.character()
    },
    error = function(e){
      flog.logger(name='ROOT',INFO,appender = appender.file(paste(Sys.Date(),'modellog.log')),
                  layout.format('[~l] [~t] [~n.~f]rule_yyx_zaiwang_state: ~m'));flog.error('%s',e)
      "ERROR"
    }
  )
}

# yyx top calls in txl rule
ruleset$rule_yyx_call_last6m_topin_txl <- function(res,duration_limit = 6,top_num = 20,
                                                   intersect_limit = 0){
  tryCatch(
    {
      if(is.null(res$moxieInfo$yunyingshangInfo$calls$items)) stop("error:res$moxieInfo$yunyingshangInfo$calls$items is NULL!")
      txl = res$tongxunluInfo$tel %>% stringr::str_remove_all("(\\s)|-|(\\+86)")
      res$moxieInfo$yunyingshangInfo$calls$items %>% 
        do.call('rbind',.) -> tmp
      tmp %>% dplyr::filter(duration >= duration_limit, nchar(peer_number) == 11) %>% 
        group_by(peer_number) %>% summarise(cnt = n()) %>% 
        arrange(desc(cnt)) %>% head(top_num) %>% 
        `$`(peer_number) %>% intersect(txl) %>% length() %>% 
        `>`(intersect_limit) %>% as.character()
    },
    error = function(e){
      flog.logger(name='ROOT',INFO,appender = appender.file(paste(Sys.Date(),'modellog.log')),
                  layout.format('[~l] [~t] [~n.~f]rule_yyx_call_last6m_topin_txl: ~m'));flog.error('%s',e)
      "ERROR"
    }
  )
} 
# top n calls concentration.
ruleset$rule_yyx_call_last6m_concentrate <- function(res,duration_limit = 6,top_num = 2,
                                                     concentrate = 0.8){
  tryCatch(
    {
      if(is.null(res$moxieInfo$yunyingshangInfo$calls$items)) stop("error:res$moxieInfo$yunyingshangInfo$calls$items is NULL!")
      res$moxieInfo$yunyingshangInfo$calls$items %>% 
        do.call('rbind',.) -> tmp
      tmp %>% dplyr::filter(duration >= duration_limit, nchar(peer_number) == 11) %>% 
        group_by(peer_number) %>% summarise(cnt = n()) %>% 
        arrange(desc(cnt)) %>% mutate(ratio = cnt / sum(cnt)) %>% 
        head(top_num) %$% sum(ratio) %>%
        `<`(concentrate) %>% as.character()
    },
    error = function(e){
      flog.logger(name='ROOT',INFO,appender = appender.file(paste(Sys.Date(),'modellog.log')),
                  layout.format('[~l] [~t] [~n.~f]rule_yyx_call_last6m_concentrate: ~m'));flog.error('%s',e)
      "ERROR"
    }
  )
}

# rule silent days
ruleset$rule_yyx_call_last6m_Silent_days_n7_cnt <- function(res,silent_days = 7,
                                                            limit_cnt = 10){
  
  tryCatch(
    {
      if(is.null(res$moxieInfo$yunyingshangInfo$calls$items)) stop("error:res$moxieInfo$yunyingshangInfo$calls$items is NULL!")
      res$moxieInfo$yunyingshangInfo$calls$items %>% 
        do.call('rbind',.) -> tmp
      tmp$time %>% as.POSIXct() %>% sort(decreasing = F) %>% 
        diff.POSIXt(lag = 1) %>% `/`(lubridate::dseconds(3600 * 24)) %>%
        `>`(silent_days) %>% sum(na.rm = T) %>% `<`(limit_cnt) %>% as.character()
    },
    error = function(e){
      flog.logger(name='ROOT',INFO,appender = appender.file(paste(Sys.Date(),'modellog.log')),
                  layout.format('[~l] [~t] [~n.~f]rule_yyx_call_last6m_Silent_days_n7_cnt: ~m'));flog.error('%s',e)
      "ERROR"
    }
  )
}
ruleset$rule_yyx_call_last6m_Silent_days_n5_cnt <- function(res,silent_days = 5,
                                                            limit_cnt = 20){
  
  tryCatch(
    {
      if(is.null(res$moxieInfo$yunyingshangInfo$calls$items)) stop("error:res$moxieInfo$yunyingshangInfo$calls$items is NULL!")
      res$moxieInfo$yunyingshangInfo$calls$items %>% 
        do.call('rbind',.) -> tmp
      tmp$time %>% as.POSIXct() %>% sort(decreasing = F) %>% 
        diff.POSIXt(lag = 1) %>% `/`(lubridate::dseconds(3600 * 24)) %>%
        `>`(silent_days) %>% sum(na.rm = T) %>% `<`(limit_cnt) %>% as.character()
    },
    error = function(e){
      flog.logger(name='ROOT',INFO,appender = appender.file(paste(Sys.Date(),'modellog.log')),
                  layout.format('[~l] [~t] [~n.~f]rule_yyx_call_last6m_Silent_days_n5_cnt: ~m'));flog.error('%s',e)
      "ERROR"
    }
  )
}
ruleset$rule_yyx_call_last6m_Silent_days_n3_cnt <- function(res,silent_days = 3,
                                                            limit_cnt = 30){
  
  tryCatch(
    {
      if(is.null(res$moxieInfo$yunyingshangInfo$calls$items)) stop("error:res$moxieInfo$yunyingshangInfo$calls$items is NULL!")
      res$moxieInfo$yunyingshangInfo$calls$items %>% 
        do.call('rbind',.) -> tmp
      tmp$time %>% as.POSIXct() %>% sort(decreasing = F) %>% 
        diff.POSIXt(lag = 1) %>% `/`(lubridate::dseconds(3600 * 24)) %>%
        `>`(silent_days) %>% sum(na.rm = T) %>% `<`(limit_cnt) %>% as.character()
    },
    error = function(e){
      flog.logger(name='ROOT',INFO,appender = appender.file(paste(Sys.Date(),'modellog.log')),
                  layout.format('[~l] [~t] [~n.~f]rule_yyx_call_last6m_Silent_days_n3_cnt: ~m'));flog.error('%s',e)
      "ERROR"
    }
  )
}

# rule dialed success ratio
ruleset$rule_yyx_call_last6m_dialed_succ_ratio <- function(res,duration_limit = 6,
                                                           last_days = 180,
                                                           dialed_succ_ratio = .2){
  tryCatch(
    {
      if(is.null(res$moxieInfo$yunyingshangInfo$calls$items)) stop("error:res$moxieInfo$yunyingshangInfo$calls$items is NULL!")
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
    error = function(e){
      flog.logger(name='ROOT',INFO,appender = appender.file(paste(Sys.Date(),'modellog.log')),
                  layout.format('[~l] [~t] [~n.~f]rule_yyx_call_last6m_dialed_succ_ratio: ~m'));flog.error('%s',e)
      "ERROR"
    }
  )
}
ruleset$rule_yyx_call_last3m_dialed_succ_ratio <- function(res,duration_limit = 6,
                                                           last_days = 90,
                                                           dialed_succ_ratio = .2){
  tryCatch(
    {
      if(is.null(res$moxieInfo$yunyingshangInfo$calls$items)) stop("error:res$moxieInfo$yunyingshangInfo$calls$items is NULL!")
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
    error = function(e){
      flog.logger(name='ROOT',INFO,appender = appender.file(paste(Sys.Date(),'modellog.log')),
                  layout.format('[~l] [~t] [~n.~f]rule_yyx_call_last3m_dialed_succ_ratio: ~m'));flog.error('%s',e)
      "ERROR"
    }
  )
}
ruleset$rule_yyx_call_last1m_dialed_succ_ratio <- function(res,duration_limit = 6,
                                                           last_days = 30,
                                                           dialed_succ_ratio = .2){
  tryCatch(
    {
      if(is.null(res$moxieInfo$yunyingshangInfo$calls$items)) stop("error:res$moxieInfo$yunyingshangInfo$calls$items is NULL!")
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
    error = function(e){
      flog.logger(name='ROOT',INFO,appender = appender.file(paste(Sys.Date(),'modellog.log')),
                  layout.format('[~l] [~t] [~n.~f]rule_yyx_call_last1m_dialed_succ_ratio: ~m'));flog.error('%s',e)
      "ERROR"
    }
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
      black_limit = 10
      #-- configs end---
      # if(is.null(res) || is.na(res) || (!is.list(res))) return("TRUE")
      txl <- res$tongxunluInfo
      # if(is.null(txl) || is.na(txl) || (txl == '')) return('TRUE')
      
      txl[,tel] <- txl[,tel] %>% str_remove_all("(\\s)|-|(\\+86)")
      index = (nchar(txl[,tel]) == 11 & txl[,tel] %>% str_detect("^1")) & (txl$tel %>% duplicated() %>% `!`)  
      txl = txl[index,]
      validate_tels <- txl[,tel]
      validate_num <- txl[,tel] %>% length()
      qingshu_num <- txl[,name] %>% sapply(function(x) sapply(qinshu_dict, str_detect,string=x) %>% any(na.rm = T) %>% sum(na.rm = T)) %>% sum(na.rm = T)
      black_num <- txl[,name] %>% sapply(function(x) sapply(black_dict, str_detect,string=x) %>% any(na.rm = T) %>% sum(na.rm = T)) %>% sum(na.rm = T)
      (validate_num >= validate_limit & qingshu_num >= qingshu_limit & black_num <= black_limit) %>% as.character() 
    },
    error = function(e){
      flog.logger(name='ROOT',INFO,appender = appender.file(paste(Sys.Date(),'modellog.log')),
                  layout.format('[~l] [~t] [~n.~f]rule_txl: ~m'));flog.error('%s',e)
      "ERROR"
    }
  )
}


#  trade orders' recentdeliveraddress, count for last 6 months.
ruleset$rule_taobao_order_succ_recentdeliveraddress_cnt <- 
  function(res,recent_address_limit = 0){
    tryCatch(
      { require(magrittr);require(stringr)
        # trade order successed. merge tradedetails and recentdeliveraddress dataframe.
        if(is.null(res$moxieInfo$taobaoInfo$tradedetails$trade_createtime)) stop("error:res$moxieInfo$taobaoInfo$tradedetails is NULL!")
        trade_ids <- res$moxieInfo$taobaoInfo$tradedetails %>% filter(trade_status == "TRADE_FINISHED") %$% trade_id
        res$moxieInfo$taobaoInfo$recentdeliveraddress %>% filter(trade_id %in% trade_ids) %$% deliver_address %>% length() %>% `>`(recent_address_limit) %>% as.character() 
      },
      error = function(e){
        flog.logger(name='ROOT',INFO,appender = appender.file(paste(Sys.Date(),'modellog.log')),
                    layout.format('[~l] [~t] [~n.~f]rule_taobao_order_succ_recentdeliveraddress_cnt: ~m'));flog.error('%s',e)
        "ERROR"
      }
    )
  }
# success tradedetails count for last 6 months.
ruleset$rule_taobao_order_succ_cnt <- 
  function(res,trade_order_cnt_limit = 3){
    tryCatch(
      { require(magrittr);require(stringr)
        if(is.null(res$moxieInfo$taobaoInfo$tradedetails$trade_createtime)) stop("error:res$moxieInfo$taobaoInfo$tradedetails is NULL!")
        res$moxieInfo$taobaoInfo$tradedetails %>% select(trade_createtime,trade_text,trade_status) %>% 
          filter(trade_status == "TRADE_FINISHED") %>% nrow() %>% `>=`(trade_order_cnt_limit) %>% as.character() 
      },
      error = function(e){
        flog.logger(name='ROOT',INFO,appender = appender.file(paste(Sys.Date(),'modellog.log')),
                    layout.format('[~l] [~t] [~n.~f]rule_taobao_order_succ_cnt: ~m'));flog.error('%s',e)
        "ERROR"
      }
    )
  }


# huabei limit
ruleset$rule_taobao_huabei_amt <- function(res,huabei_amt_limit = 200){
  tryCatch(
    { require(magrittr);require(stringr)
      # taobaoReport huabei unit yuan;and taobaoInfo huabei unit fen. version of moxie is taobaoxinxiV6 taobaobaogaoV4.
      if(is.null(res$moxieInfo$taobaoReport$wealth_info$totalssets$huai_bei_limit)) stop("error:res$moxieInfo$taobaoReport$wealth_info$totalssets$huai_bei_limit is NULL!")
      res$moxieInfo$taobaoReport$wealth_info$totalssets$huai_bei_limit %>% as.numeric() %>% `>`(huabei_amt_limit) %>% as.character() 
    },
    error = function(e){
      flog.logger(name='ROOT',INFO,appender = appender.file(paste(Sys.Date(),'modellog.log')),
                  layout.format('[~l] [~t] [~n.~f]rule_taobao_huabei_amt: ~m'));flog.error('%s',e)
      "ERROR"
    }
  )
}
# huabei can use limit 
ruleset$rule_taobao_huabei_amt_canuse <- function(res,huabei_amt_canuse_limit = 0){
  tryCatch(
    { require(magrittr);require(stringr)
      # taobaoReport huabei unit yuan;and taobaoInfo huabei unit fen. version of moxie is taobaoxinxiV6 taobaobaogaoV4.
      if(is.null(res$moxieInfo$taobaoReport$wealth_info$totalssets$huai_bei_limit)) stop("error:res$moxieInfo$taobaoReport$wealth_info$totalssets$huai_bei_limit is NULL!")
      res$moxieInfo$taobaoReport$wealth_info$totalssets$huai_bei_can_use_limit %>% as.numeric() %>% `>=`(huabei_amt_canuse_limit) %>% as.character() 
    },
    error = function(e){
      flog.logger(name='ROOT',INFO,appender = appender.file(paste(Sys.Date(),'modellog.log')),
                  layout.format('[~l] [~t] [~n.~f]rule_taobao_huabei_amt_canuse: ~m'));flog.error('%s',e)
      "ERROR"
    }
  )
}
# huabei use ratio
ruleset$rule_taobao_huabei_amt_use_ratio <- 
  function(res,huabei_use_ratio_limit = 1){
    tryCatch(
      { require(magrittr);require(stringr)
        # taobaoReport huabei unit yuan;and taobaoInfo huabei unit fen. version of moxie is taobaoxinxiV6 taobaobaogaoV4.
        if(is.null(res$moxieInfo$taobaoReport$wealth_info$totalssets$huai_bei_limit)) stop("error:res$moxieInfo$taobaoReport$wealth_info$totalssets$huai_bei_limit is NULL!")
        huai_bei_can_use_limit <- res$moxieInfo$taobaoReport$wealth_info$totalssets$huai_bei_can_use_limit %>% as.numeric()
        huai_bei_limit <- res$moxieInfo$taobaoReport$wealth_info$totalssets$huai_bei_limit %>% as.numeric()
        huabei_use_ratio <- 1 - ( huai_bei_can_use_limit / huai_bei_limit)
        huabei_use_ratio %>% `<=`(huabei_use_ratio_limit) %>% as.character() 
      },
      error = function(e){
        flog.logger(name='ROOT',INFO,appender = appender.file(paste(Sys.Date(),'modellog.log')),
                    layout.format('[~l] [~t] [~n.~f]rule_taobao_huabei_amt_use_ratio: ~m'));flog.error('%s',e)
        "ERROR"
      }
    )
  }

# skip through rules
apv_skip_through_of_zmscore_huabei <- function(res){
  tryCatch(
    {
      # res <- jsonlite::fromJSON(json)
      huabei_overdue <- res$moxieInfo$alipayInfo$wealth$huabei_overdue 
      huai_bei_limit <- res$moxieInfo$taobaoReport$wealth_info$totalssets$huai_bei_limit %>% check_num()
      # zhifubaomianya & taobao scpyder jianrong
      zm_zmscore = res$baseInfo$zmscore %>% check_char()
      taobao_zmscore = res$moxieInfo$taobaoReport$wealth_info$totalssets$taobao_zmscore %>% check_num()
      taobao_zmscore_grade = taobao_zmscore %>% car_recode("1:599 = 'Z5';600:649 = 'Z4';650:699 = 'Z3';700:749 = 'Z2';750:Inf = 'Z1';else=NA") %>% check_char()
      zmscore = ifelse(taobao_zmscore_grade %in% c('Z1','Z2','Z3','Z4','Z5'),taobao_zmscore_grade,zm_zmscore)
      zmscore_over_700 <- zmscore %in% c('Z1','Z2') 
      if(is.null(huabei_overdue) || is.na(huabei_overdue)) huabei_overdue <- FALSE
      if(is.null(huai_bei_limit) || is.na(huai_bei_limit)) huai_bei_limit <- 0
      if(is.null(zmscore_over_700) || is.na(zmscore_over_700)) zmscore_over_700 <- FALSE
      if(is.null(huabei_overdue)){
        (huai_bei_limit >= 10000 || zmscore_over_700) -> rt
      }else if(huabei_overdue %in% c(TRUE,FALSE)){
        ((huai_bei_limit >= 10000 || zmscore_over_700) && !huabei_overdue)  -> rt
      }else{
        (huai_bei_limit >= 10000 || zmscore_over_700)  -> rt 
      }
      if(is.null(rt) || is.na(rt)) stop("error:apv_skip_through_of_zmscore_huabei produce error,rt is NULL!")
      if(!rt %in% c(T,F)) stop("error:apv_skip_through_of_zmscore_huabei produce error,rt is not logic!")
      rt
      # alipay
    }
    ,error = function(e){
      flog.logger(name='ROOT',INFO,appender = appender.file(paste(Sys.Date(),'modellog.log')),
                  layout.format('[~l] [~t] [~n.~f]apv_skip_through_of_zmscore_huabei: ~m'));flog.error('%s',e)
      FALSE
    }
  )
}

ruleset$rule_apv_skip_through_of_zmscore_huabei <- function(res){
  tryCatch(
    {
      apv_skip_through_of_zmscore_huabei(res) %>% as.character()
    }
    ,error = function(e){
      flog.logger(name='ROOT',INFO,appender = appender.file(paste(Sys.Date(),'modellog.log')),
                  layout.format('[~l] [~t] [~n.~f]rule_apv_skip_through_of_zmscore_huabei: ~m'));flog.error('%s',e)
      "ERROR"
    }
  )
}
ruleset$rule_taobao_alipay_shiming <- function(res){
  tryCatch(
    {
      realname <- res$baseInfo$realname 
      # alipay_realname <- res$moxieInfo$alipayInfo$userinfo$user_name # nickname
      alipay_realname <- res$moxieInfo$alipayInfo$bankinfo$user_name %>% unique()
      # tel <- res$baseInfo$tel
      # alipay_tel <- res$moxieInfo$alipayInfo$userinfo$phone_number
      id_card <- res$baseInfo$id_card
      id_1_18 <- id_card %>% str_sub(1,1) %>% str_c(id_card %>% str_sub(18,18)) 
      alipay_id <- res$moxieInfo$alipayInfo$userinfo$idcard_number
      alipay_id_1_18 <- alipay_id %>% str_sub(1,1) %>% str_c(alipay_id %>% str_sub(18,18))
      rt <- (realname %in% alipay_realname) && (id_1_18 == alipay_id_1_18) 
      rt %>% as.character()
    }
    ,error = function(e){
      flog.logger(name='ROOT',INFO,appender = appender.file(paste(Sys.Date(),'modellog.log')),
                  layout.format('[~l] [~t] [~n.~f]rule_taobao_alipay_shiming: ~m'));flog.error('%s',e)
      "ERROR"
    }
  )
}
ruleset$rule_taobao_alipayinfo_bankcard_cnt <- function(res,bankcard_cnt_limit = 1){
  tryCatch(
    { require(magrittr);require(stringr)
      # trade order successed. merge tradedetails and recentdeliveraddress dataframe.
      if(is.null(res$moxieInfo$alipayInfo$bankinfo$card_number)) stop("error:res$moxieInfo$alipayInfo$bankinfo$card_number is NULL!")
      res$moxieInfo$alipayInfo$bankinfo$card_number %>% unique() %>% length()  %>% `>=`(bankcard_cnt_limit) %>% as.character() 
    },
    error = function(e){
      flog.logger(name='ROOT',INFO,appender = appender.file(paste(Sys.Date(),'modellog.log')),
                  layout.format('[~l] [~t] [~n.~f]rule_taobao_alipayinfo_bankcard_cnt: ~m'));flog.error('%s',e)
      "ERROR"
    }
  )
}

ruleset$rule_taobao_alipayinfo_deliver_tel <- function(res,tel_cnt_limit = 1){
  tryCatch(
    { require(magrittr);require(stringr)
      # trade order successed. merge tradedetails and recentdeliveraddress dataframe.
      if(is.null(res$moxieInfo$alipayInfo$alipaydeliveraddresses)) stop("error:res$moxieInfo$alipayInfo$alipaydeliveraddresses is NULL!")
      res$moxieInfo$alipayInfo$alipaydeliveraddresses$phone_number %>% unique() %>% length()  %>% `>=`(tel_cnt_limit) %>% as.character() 
    },
    error = function(e){
      flog.logger(name='ROOT',INFO,appender = appender.file(paste(Sys.Date(),'modellog.log')),
                  layout.format('[~l] [~t] [~n.~f]rule_taobao_alipayinfo_deliver_tel: ~m'));flog.error('%s',e)
      "ERROR"
    }
  )
}

ruleset$rule_taobao_alipayinfo_error <- function(res){
  tryCatch(
    {
      if(!length(res$moxieInfo$alipayInfo)) stop("error:res$moxieInfo$alipayInfo is NULL!")
      if(is.null(res$moxieInfo$alipayInfo$wealth)) stop("error:res$moxieInfo$alipayInfo$wealth is NULL")
      "TRUE"
    }
    ,error = function(e){
      flog.logger(name='ROOT',INFO,appender = appender.file(paste(Sys.Date(),'modellog.log')),
                  layout.format('[~l] [~t] [~n.~f]rule_taobao_alipayinfo_error: ~m'));flog.error('%s',e)
      "ERROR"
    }
  )
}

ruleset$rule_taobao_alipayinfo_huabei_overdue <- function(res){
  tryCatch(
    {
      res$moxieInfo$alipayInfo$wealth$huabei_overdue %>% `==`(FALSE) %>% as.character()
    }
    ,error = function(e){
      flog.logger(name='ROOT',INFO,appender = appender.file(paste(Sys.Date(),'modellog.log')),
                  layout.format('[~l] [~t] [~n.~f]rule_taobao_alipayinfo_huabei_overdue: ~m'));flog.error('%s',e)
      "ERROR"
    }
  )
}

# xinyan rule
ruleset$rule_xinyan_error <- function(res){
  tryCatch(
    { require(magrittr);require(stringr)
      #--jianrong res$xinyanInfo$data$result_detail and res$xinyanInfo$result_detail.--begin--#
      if(is.null(res$xinyanInfo) || (!length(res$xinyanInfo))) stop("error:res$xinyanInfo is NULL!")
      if(is.null(res$xinyanInfo$trans_id) && is.null(res$xinyanInfo$data$trans_id)) stop("error:res$xinyanInfo is NULL!")
      "TRUE"
    },
    error = function(e){
      flog.logger(name='ROOT',INFO,appender = appender.file(paste(Sys.Date(),'modellog.log')),
                  layout.format('[~l] [~t] [~n.~f]rule_xinyan_error: ~m'));flog.error('%s',e)
      "ERROR"
    }
  )
}
# suanhua rule
ruleset$rule_suanhua_error <- function(res){
    tryCatch(
      { 
        # suanhua
        if(is.null(res$suanhuaInfo) || (!length(res$suanhuaInfo))) stop("error:res$suanhuaInfo is NULL!")
        if(is.null(res$suanhuaInfo$STAN_FRD_LEVEL)) stop("error:res$suanhuaInfo is NULL!")
        "TRUE"
      },
      error = function(e){
        flog.logger(name='ROOT',INFO,appender = appender.file(paste(Sys.Date(),'modellog.log')),
                    layout.format('[~l] [~t] [~n.~f]rule_suanhua_error: ~m'));flog.error('%s',e)
        "ERROR"
      }
    )
  }

ruleset$rule_zrobot_idcard_in_blacklist <- function(res){
  tryCatch(
    {
      if(is.null(res$zrobotInfo$antiFraud$idcard_in_blacklist)){
        rt <- "TRUE"
      }else{
        rt <- res$zrobotInfo$antiFraud$idcard_in_blacklist %>% str_to_upper() %>% `==`("FALSE")
      }
      rt %>% as.character()
    }
    ,error = function(e){
      flog.logger(name='ROOT',INFO,appender = appender.file(paste(Sys.Date(),'modellog.log')),
                  layout.format('[~l] [~t] [~n.~f]rule_zrobot_idcard_in_blacklist: ~m'));flog.error('%s',e)
      "ERROR"
    }
  )
}
ruleset$rule_zrobot_sc_zrxy_zm <- function(res,limit = 450){
  tryCatch(
    {
      #-----zrxy------
      sql_zrxy = 
        "select 
      
      383.903595255632  +  72.1347520444482  * log(score_p / (1-score_p))  as score,
      *  
      from 
      ( select 
      1/(1 + exp(-1 * (1 * 1.29397150550726 + zr_score * 0.256757361337072 + 
      zaiwang * 0.95792277524138 + sex * 0.529841420870505 + 
      query_sum_count * 0.673237060561867 + zr_binding_phones * 0.392370779773745 + 
      loans_count * 0.0113640091299555 + loans_score * 0.0114134936412465 + 
      latest_one_month_suc * 0.421966488526195 + zr_overdue_status_12m * 
      0.214074602211157 + zr_sn_order1_blacklist_contacts_cnt * 0.570185720704065 + 
      zr_performance_index * 0.879091344076989 + zr_ecommerce_account_history * 
      1.26673675064716)))  as score_p,
      *  
      from 
      ( select 
      /****** zr_score ******/
      case 
      when  zr_score IS NULL then 0 
      when  zr_score <=-1 then 0.0979804083602036         --not finded is-1 range500-750
      when -1 < zr_score and zr_score <=588 then -1.22377543162212 
      when 588 < zr_score and zr_score <=638 then -0.1576724215193 
      when 638 < zr_score and zr_score <=651 then 0.393567660319098 
      when 651 < zr_score  then 2.47889734462416  
      else  -0.09999  end as 
      zr_score ,
      /******************************/
      /****** zaiwang ******/
      case 
      when zaiwang in ( '1' ) then -1.72887038067912 
      when zaiwang in ( '3' ) then 0.173263468780752 
      when zaiwang in ( '4' ) then 0.687664607038514 
      when zaiwang in ( '2' ) then -1.53284869405521 
      when zaiwang in ( '' ) then 0 
      else  -0.09999  end as 
      zaiwang ,
      /******************************/
      /****** sex ******/
      case 
      when sex in ( 'female' ) then 0.876285397260456 
      when sex in ( 'male' ) then -0.169334361267143  
      else  -0.09999  end as 
      sex ,
      /******************************/
      /****** query_sum_count ******/
      case 
      when  query_sum_count IS NULL then 1.52612187941559 
      when  query_sum_count <=2 then 1.87481422737158 
      when 2 < query_sum_count and query_sum_count <=4 then 1.47028852750535 
      when 4 < query_sum_count and query_sum_count <=9 then -0.518793793415168 
      when 9 < query_sum_count and query_sum_count <=26.4 then -1.08764325729754 
      when 26.4 < query_sum_count  then -1.78686948393386  
      else  -0.09999  end as 
      query_sum_count ,
      /******************************/
      /****** zr_binding_phones ******/
      case 
      when  zr_binding_phones IS NULL then 5.79239569074685 
      when  zr_binding_phones <=1 then 1.28339119482608 
      when 1 < zr_binding_phones and zr_binding_phones <=2 then -0.782475870504735 
      when 2 < zr_binding_phones  then -1.70620158086641  
      else  -0.09999  end as 
      zr_binding_phones ,
      /******************************/
      /****** loans_count ******/
      case 
      when  loans_count IS NULL then 0.713349887877465 
      when  loans_count <=2 then 0.968808766157601 
      when 2 < loans_count and loans_count <=3 then 0.162518929497775 
      when 3 < loans_count and loans_count <=8 then -0.286865412545225 
      when 8 < loans_count and loans_count <=29 then -1.08764325729754 
      when 29 < loans_count  then -2.79239134953596  
      else  -0.09999  end as 
      loans_count ,
      /******************************/
      /****** loans_score ******/
      case 
      when  loans_score IS NULL then 0.713349887877465 
      when  loans_score <=492 then -2.42774823594805 
      when 492 < loans_score and loans_score <=565 then -0.859553282048 
      when 565 < loans_score and loans_score <=592 then 0.0246491352746541 
      when 592 < loans_score and loans_score <=607 then 0.298651103822355 
      when 607 < loans_score  then 0.364609071614152  
      else  -0.09999  end as 
      loans_score ,
      /******************************/
      /****** latest_one_month_suc ******/
      case 
      when  latest_one_month_suc IS NULL then 0.713349887877465 
      when  latest_one_month_suc <=0 then -1.21685498877754 
      when 0 < latest_one_month_suc and latest_one_month_suc <=1 then 1.07880966137193 
      when 1 < latest_one_month_suc  then 2.28278246569787  
      else  -0.09999  end as 
      latest_one_month_suc ,
      /******************************/
      /****** zr_overdue_status_12m ******/
      case 
      when  zr_overdue_status_12m IS NULL then 0.428387425478326 
      when  zr_overdue_status_12m <=0 then -0.350044314166757 
      when 0 < zr_overdue_status_12m and zr_overdue_status_12m <=1 then -2.31055326264322  
      else  -0.09999  end as 
      zr_overdue_status_12m ,
      /******************************/
      /****** zr_sn_order1_blacklist_contacts_cnt ******/
      case 
      when  zr_sn_order1_blacklist_contacts_cnt IS NULL then -0.504548538279119 
      when  zr_sn_order1_blacklist_contacts_cnt <=0 then 0.598082068763219 
      when 0 < zr_sn_order1_blacklist_contacts_cnt and zr_sn_order1_blacklist_contacts_cnt <=1 then 0.361564929232924 
      when 1 < zr_sn_order1_blacklist_contacts_cnt  then -1.15231646763997  
      else  -0.09999  end as 
      zr_sn_order1_blacklist_contacts_cnt ,
      /******************************/
      /****** zr_performance_index ******/
      case 
      when  zr_performance_index IS NULL then -0.238741547328358 
      when  zr_performance_index <=50 then -0.0251359732715423 
      when 50 < zr_performance_index and zr_performance_index <=75 then 0.263060158719652 
      when 75 < zr_performance_index  then 2.84239825363329  
      else  -0.09999  end as 
      zr_performance_index ,
      /******************************/
      /****** zr_ecommerce_account_history ******/
      case 
      when  zr_ecommerce_account_history IS NULL then -0.238741547328358 
      when  zr_ecommerce_account_history <=4 then -0.307484699747961 
      when 4 < zr_ecommerce_account_history and zr_ecommerce_account_history <=5 then 0.877256742650088  
      else  -0.09999  end as 
      zr_ecommerce_account_history  
      from  infos  --you should modify the table name. 
      )a 
      )b
      "
      #-----zrxyzm-----
      sql_zrxyzm = 
        "select 
      
      383.903595255632  +  72.1347520444482  * log(score_p / (1-score_p))  as score,
      *  
      from 
      ( select 
      1/(1 + exp(-1 * (1 * 1.32935215045812 +  zr_score  * 0.213805435581071 + 
      sex * 0.374318005339816 + zmscore * 0.881719279429153 + 
      zaiwang * 0.855440485891169 + query_sum_count * 0.465731693999469 + 
      zr_binding_phones * 0.289537292194771 + loans_count * 0.332284713555046 + 
      latest_one_month_suc * 0.348129484493128 + zr_overdue_status_12m * 
      0.278782582653261 + zr_sn_order1_blacklist_contacts_cnt * 0.501361561780721 + 
      zr_performance_index * 0.649413376418309 + zr_ecommerce_account_history * 
      0.442851297537012)))  as score_p,
      *  
      from 
      ( select 
      /******  zr_score  ******/
      case 
      when   zr_score  IS NULL then 0 
      when   zr_score  <=-1 then 0.0979804083602036 
      when -1 <  zr_score  and  zr_score  <=588 then -1.22377543162212 
      when 588 <  zr_score  and  zr_score  <=638 then -0.1576724215193 
      when 638 <  zr_score  and  zr_score  <=651 then 0.393567660319098 
      when 651 <  zr_score   then 2.47889734462416  
      else  -0.09999  end as 
      zr_score ,
      /******************************/
      /****** sex ******/
      case 
      when sex in ( 'female' ) then 0.876285397260456 
      when sex in ( 'male' ) then -0.169334361267143  
      else  -0.09999  end as 
      sex ,
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
      /****** query_sum_count ******/
      case 
      when  query_sum_count IS NULL then 1.52612187941559 
      when  query_sum_count <=2 then 1.87481422737158 
      when 2 < query_sum_count and query_sum_count <=4 then 1.47028852750535 
      when 4 < query_sum_count and query_sum_count <=9 then -0.518793793415168 
      when 9 < query_sum_count and query_sum_count <=26.4 then -1.08764325729754 
      when 26.4 < query_sum_count  then -1.78686948393386  
      else  -0.09999  end as 
      query_sum_count ,
      /******************************/
      /****** zr_binding_phones ******/
      case 
      when  zr_binding_phones IS NULL then 5.79239569074685 
      when  zr_binding_phones <=1 then 1.28339119482608 
      when 1 < zr_binding_phones and zr_binding_phones <=2 then -0.782475870504735 
      when 2 < zr_binding_phones  then -1.70620158086641  
      else  -0.09999  end as 
      zr_binding_phones ,
      /******************************/
      /****** loans_count ******/
      case 
      when  loans_count IS NULL then 0.713349887877465 
      when  loans_count <=2 then 0.968808766157601 
      when 2 < loans_count and loans_count <=3 then 0.162518929497775 
      when 3 < loans_count and loans_count <=8 then -0.286865412545225 
      when 8 < loans_count and loans_count <=29 then -1.08764325729754 
      when 29 < loans_count  then -2.79239134953596  
      else  -0.09999  end as 
      loans_count ,
      /******************************/
      /****** latest_one_month_suc ******/
      case 
      when  latest_one_month_suc IS NULL then 0.713349887877465 
      when  latest_one_month_suc <=0 then -1.21685498877754 
      when 0 < latest_one_month_suc and latest_one_month_suc <=1 then 1.07880966137193 
      when 1 < latest_one_month_suc  then 2.28278246569787  
      else  -0.09999  end as 
      latest_one_month_suc ,
      /******************************/
      /****** zr_overdue_status_12m ******/
      case 
      when  zr_overdue_status_12m IS NULL then 0.428387425478326 
      when  zr_overdue_status_12m <=0 then -0.350044314166757 
      when 0 < zr_overdue_status_12m and zr_overdue_status_12m <=1 then -2.31055326264322  
      else  -0.09999  end as 
      zr_overdue_status_12m ,
      /******************************/
      /****** zr_sn_order1_blacklist_contacts_cnt ******/
      case 
      when  zr_sn_order1_blacklist_contacts_cnt IS NULL then -0.504548538279119 
      when  zr_sn_order1_blacklist_contacts_cnt <=0 then 0.598082068763219 
      when 0 < zr_sn_order1_blacklist_contacts_cnt and zr_sn_order1_blacklist_contacts_cnt <=1 then 0.361564929232924 
      when 1 < zr_sn_order1_blacklist_contacts_cnt  then -1.15231646763997  
      else  -0.09999  end as 
      zr_sn_order1_blacklist_contacts_cnt ,
      /******************************/
      /****** zr_performance_index ******/
      case 
      when  zr_performance_index IS NULL then -0.238741547328358 
      when  zr_performance_index <=50 then -0.0251359732715423 
      when 50 < zr_performance_index and zr_performance_index <=75 then 0.263060158719652 
      when 75 < zr_performance_index  then 2.84239825363329  
      else  -0.09999  end as 
      zr_performance_index ,
      /******************************/
      /****** zr_ecommerce_account_history ******/
      case 
      when  zr_ecommerce_account_history IS NULL then -0.238741547328358 
      when  zr_ecommerce_account_history <=4 then -0.307484699747961 
      when 4 < zr_ecommerce_account_history and zr_ecommerce_account_history <=5 then 0.877256742650088  
      else  -0.09999  end as 
      zr_ecommerce_account_history  
      from  infos  --you should modify the table name. 
      )a 
      )b
      "
      #-----------compute---------------------
      infos <- extract_scorefeatures_from_list(res)
      library(DBI);library(RSQLite)
      con <- dbConnect(RSQLite::SQLite(), ":memory:")
      RSQLite::dbWriteTable(con, "infos", infos,overwrite = TRUE)
      if(infos$zmscore %in% c('Z1','Z2','Z3','Z4','Z5')){
        rs <- DBI::dbSendQuery(con,sql_zrxyzm)
        flog.logger(name='ROOT',INFO,appender = appender.file(paste(Sys.Date(),'modellog.log')),
                    layout.format('[~l] [~t] [~n.~f]rule_zrobot_sc_zrxy_zm: ~m'));flog.info('%s',"walk through sql_zrxyzm model rule!")
      }else{
        rs <- DBI::dbSendQuery(con,sql_zrxy)
        flog.logger(name='ROOT',INFO,appender = appender.file(paste(Sys.Date(),'modellog.log')),
                    layout.format('[~l] [~t] [~n.~f]rule_zrobot_sc_zrxy_zm: ~m'));flog.info('%s',"walk through sql_zrxy  model rule!")
      }
      infos_w <- dbFetch(rs)
      DBI::dbClearResult(rs)
      DBI::dbDisconnect(con);
      flog.logger(name='ROOT',INFO,appender = appender.file(paste(Sys.Date(),'modellog.log')),
                  layout.format('[~l] [~t] [~n.~f]rule_zrobot_sc_zrxy_zm: ~m'));flog.info('%s',list(infos_w=infos_w,infos=infos) %>% jsonlite::toJSON(na = 'null'))      
      # score process
      score <- round(infos_w$score) + round(mod(infos$age,10) / 3) # rnd
      infos$score_p <- infos_w$score_p
      infos$score <- infos_w$score
      infos$score_adj <- score
      (score > limit) %>% as.character()
    }
    ,error = function(e){
      flog.logger(name='ROOT',INFO,appender = appender.file(paste(Sys.Date(),'modellog.log')),
                  layout.format('[~l] [~t] [~n.~f]rule_zrobot_sc_zrxy_zm: ~m'));flog.error('%s',e)
      "ERROR"
    }
  )
}
ruleset$rule_zrobot_sn_order1_blacklist_contacts_cnt <- function(res){
  tryCatch(
    {
      if(is.null(res$zrobotInfo$antiFraud$sn_order1_blacklist_contacts_cnt) || res$zrobotInfo$antiFraud$sn_order1_blacklist_contacts_cnt == ""){
        rt <- "TRUE"
      }else{
        rt <- res$zrobotInfo$antiFraud$sn_order1_blacklist_contacts_cnt %>% check_num() %>% `<=`(5)
      }
      rt %>% as.character()
    }
    ,error = function(e){
      flog.logger(name='ROOT',INFO,appender = appender.file(paste(Sys.Date(),'modellog.log')),
                  layout.format('[~l] [~t] [~n.~f]rule_zrobot_sn_order1_blacklist_contacts_cnt: ~m'));flog.error('%s',e)
      "ERROR"
    }
  )
}
ruleset$rule_suanhua_G1_payday <- function(res){
  tryCatch(
    {
      payday7_limit = 0
      # payday30_limit = 1
      if(is.null(res$suanhuaInfo$G1)) stop("error:res$suanhuaInfo$G1 is NULL!")
      res$suanhuaInfo$G1 %>% jsonlite::fromJSON() -> tmp
      logic_1 <- tmp$ID_ORG_ONL_PAYDAYLOAN_7 %>% check_num() %>% `<=`(payday7_limit) 
      # logic_2 <- tmp$ID_ORG_ONL_PAYDAYLOAN_30 %>% check_num() %>% `<=`(payday30_limit)
      logic_1  %>% as.character()
    },
    error = function(e){
      flog.logger(name='ROOT',INFO,appender = appender.file(paste(Sys.Date(),'modellog.log')),
                  layout.format('[~l] [~t] [~n.~f]rule_suanhua_G1_payday: ~m'));flog.error('%s',e)
      "ERROR"
    }
  )
}

ruleset$rule_xinyan <- function(res){
  tryCatch(
    {
      rt <- list(
        apply__query_sum_count = res$xinyanInfo$result_detail$apply_report_detail$query_sum_count %>% check_num() %>% `>`(35)    # 总查询次数 >21    
        ,apply__query_org_count = res$xinyanInfo$result_detail$apply_report_detail$query_org_count %>% check_num() %>% `>`(35)    # 查询机构数 >18   
        ,apply__latest_six_month =  res$xinyanInfo$result_detail$apply_report_detail$latest_six_month %>% check_num() %>% `>`(18)    # 近6个月总查询笔数 >18  
        ,apply__query_cash_count = res$xinyanInfo$result_detail$apply_report_detail$query_cash_count %>% check_num() %>% `>`(10)    # 查询网络贷款类机构数 >10  
        ,apply__query_finance_count = res$xinyanInfo$result_detail$apply_report_detail$query_finance_count %>% check_num() %>% `>`(10)    # 查询消费金融类机构数 > 2  
        ,apply__latest_three_month = res$xinyanInfo$result_detail$apply_report_detail$latest_three_month %>% check_num() %>% `>`(11)    # 近3个月总查询笔数>11  
        ,behavior__loans_org_count = res$xinyanInfo$result_detail$behavior_report_detail$loans_org_count %>% check_num() %>% `>`(8)    # 贷款机构数>8   
        ,behavior__loans_cash_count = res$xinyanInfo$result_detail$behavior_report_detail$loans_cash_count %>% check_num() %>% `>`(6)    # 网络贷款类机构数 > 5   
        ,behavior__history_fail_fee = res$xinyanInfo$result_detail$behavior_report_detail$history_fail_fee %>% check_num() %>% `>`(12)    # 历史贷款机构失败扣款笔数 > 12  
        ,current__loans_product_count = res$xinyanInfo$result_detail$current_report_detail$loans_product_count %>% check_num() %>% `>`(5)    # 网络贷款类产品数 > 5  
        ,behavior__loans_score = res$xinyanInfo$result_detail$behavior_report_detail$loans_score %>% check_num() %>% `<=`(493)    # 贷款行为分 <= 493  
        ,behavior__loans_count = res$xinyanInfo$result_detail$behavior_report_detail$loans_count %>% check_num() %>% `>`(21)    # 贷款放款总订单数 >21   
        ,behavior__loans_overdue_count = res$xinyanInfo$result_detail$behavior_report_detail$loans_overdue_count %>% check_num() %>% `>`(2)    # 贷款逾期订单数M0plus > 2  
        ,behavior__consfin_org_count = res$xinyanInfo$result_detail$behavior_report_detail$consfin_org_count %>% check_num() %>% `>`(4)    # 消费金融类机构数 > 4 
      )
      if(which(is.na(rt)) %>% length()){
        flog.logger(name='ROOT',INFO,appender = appender.file(paste(Sys.Date(),'modellog.log')),
                    layout.format('[~l] [~t] [~n.~f]rule_xinyan: ~m'));flog.info('%s',rt %>% jsonlite::toJSON(na = "null"))
      }
      logic_rt <- !sum(rt %>% unlist(),na.rm = T)
      logic_rt %>% as.character()
      
    },
    error = function(e){
      flog.logger(name='ROOT',INFO,appender = appender.file(paste(Sys.Date(),'modellog.log')),
                  layout.format('[~l] [~t] [~n.~f]rule_xinyan: ~m'));flog.error('%s',e)
      "ERROR"
    }
  )
  
}
edu_level_check <- function(edu_level) edu_level %in% c('专科','本科','硕士研究生','博士研究生') %>% sum(na.rm = TRUE) # consider mult. edu.
edu_type_chenck <- function(edu_type) edu_type %in% c('普通','研究生','普通高等教育') %>% sum(na.rm = TRUE)
edu_form_chenck <- function(edu_form) edu_form %in% c('全日制','普通全日制') %>% sum(na.rm = TRUE)
edu_status_check <- function(edu_status) edu_status  %in% c('在籍注册学籍','在籍保留学籍') %>% sum(na.rm = TRUE)
# edu_advice_amt_check <- function(edu_level) edu_level %>% car_recode("'专科' = 4500;'本科' = 5000;'硕士研究生' = 6000;'博士研究生' = 7000;else = 0") %>% max(na.rm = TRUE)
edu_level_max_level_check <- function(edu_level) edu_level %>% car_recode("'专科' = 1;'本科' = 2;'硕士研究生' = 3;'博士研究生' = 4;else = 0") %>% max(na.rm = TRUE)


extract_scorefeatures_from_list <- function(res){
  tryCatch(
    {
      if(!is.null(res$xinyanInfo$data$result_detail)){
        xy_result_detail <- res$xinyanInfo$data$result_detail
      }else if(!is.null(res$xinyanInfo$result_detail)){
        xy_result_detail <- res$xinyanInfo$result_detail
      }else{
        xy_result_detail <- NULL
      }
      #--jianrong res$xinyanInfo$data$result_detail and res$xinyanInfo$result_detail.--end--#
      infos <- data_frame(
        # dq
        realname = res$baseInfo$realname %>% check_char(),
        id_card = res$baseInfo$id_card %>% check_char(),
        zm_zmscore = res$baseInfo$zmscore %>% check_char(),
        age = res$baseInfo$age  %>% check_num(),
        sex = res$baseInfo$sex %>% check_char(),
        zaiwang = res$baseInfo$zaiwang %>% check_char(),
        # xy
        query_sum_count = xy_result_detail$apply_report_detail$query_sum_count  %>% check_num(), # 总查询次数
        loans_cash_count = xy_result_detail$behavior_report_detail$loans_cash_count  %>% check_num(), # 网络贷款类机构数
        history_fail_fee = xy_result_detail$behavior_report_detail$history_fail_fee  %>% check_num(), # 历史贷款机构失败扣款笔数
        
        latest_three_month = xy_result_detail$apply_report_detail$latest_three_month %>% check_num(),
        loans_credibility = xy_result_detail$behavior_report_detail$loans_credibility %>% check_num(),
        loans_score = xy_result_detail$behavior_report_detail$loans_score %>% check_num(), # 贷款行为分
        
        loans_count = xy_result_detail$behavior_report_detail$loans_count %>% check_num(), # 贷款放款总订单数 
        latest_one_month_suc = xy_result_detail$behavior_report_detail$latest_one_month_suc %>% check_num(), # 近1个月贷款机构成功扣款笔数
        # dz
        zm_jianmian = res$baseInfo$zm_jianmian  %>% check_num(),
        real_mianya_ratio = res$baseInfo$real_mianya_ratio  %>% check_num(),
        
        # taobao jiebei huabei zhimafen
        taobao_zmscore = res$moxieInfo$taobaoReport$wealth_info$totalssets$taobao_zmscore %>% check_num(),
        taobao_jiebei_amount = res$moxieInfo$taobaoReport$wealth_info$totalssets$taobao_jiebei_amount %>% check_num(), 
        taobao_jiebie_available_amount = res$moxieInfo$taobaoReport$wealth_info$totalssets$taobao_jiebie_available_amount %>% check_num(),
        huai_bei_limit = res$moxieInfo$taobaoReport$wealth_info$totalssets$huai_bei_limit %>% check_num(),
        huai_bei_can_use_limit = res$moxieInfo$taobaoReport$wealth_info$totalssets$huai_bei_can_use_limit %>% check_num(),
        # zhifubaomianya & taobao scpyder jianrong
        taobao_zmscore_grade = taobao_zmscore %>% car_recode("1:599 = 'Z5';600:649 = 'Z4';650:699 = 'Z3';700:749 = 'Z2';750:Inf = 'Z1';else=NA") %>% check_char(),
        zmscore = ifelse(taobao_zmscore_grade %in% c('Z1','Z2','Z3','Z4','Z5'),taobao_zmscore_grade,zm_zmscore),
        
        # taobao basic_info
        tao_score = res$moxieInfo$taobaoReport$basic_info$user_and_account_basic_info$tao_score  %>% check_num(),
        first_ordertime = res$moxieInfo$taobaoReport$basic_info$user_and_account_basic_info$first_ordertime  %>% check_char(),
        account_auth = res$moxieInfo$taobaoReport$basic_info$user_and_account_basic_info$account_auth  %>% check_char(),
        taobao_vip_level = res$moxieInfo$taobaoReport$basic_info$user_and_account_basic_info$taobao_vip_level  %>% check_char(),
        taobao_vip_count = res$moxieInfo$taobaoReport$basic_info$user_and_account_basic_info$taobao_vip_count    %>% check_char(),
        # ZR
        zr_score = res$zrobotInfo$zrobotCredit$score %>% check_num(),
        zr_binding_phones = res$zrobotInfo$antiFraud$binding_phones %>% check_num(),
        zr_overdue_status_12m = res$zrobotInfo$financalBehavior$overdue_status_12m %>% check_num() , 
        zr_sn_order1_blacklist_contacts_cnt = res$zrobotInfo$antiFraud$sn_order1_blacklist_contacts_cnt %>% check_num() , 
        zr_performance_index = res$zrobotInfo$zrobotCredit$performanceIndex %>% check_num() , 
        zr_ecommerce_account_history = res$zrobotInfo$zrobotCredit$ecommerceAccountHistory %>% check_num(),
        zr_org_cnt_recent_60_days = res$zrobotInfo$antiFraud$org_cnt_recent_60_days %>% check_num(),
        zr_overdue_status_3m = res$zrobotInfo$financalBehavior$overdue_status_3m %>% check_num(),
        zr_risk_period_consuming = res$zrobotInfo$zrobotCredit$riskPeriodConsuming %>% check_num()
      )
      infos 
    },
    error = function(e){
      flog.logger(name='ROOT',INFO,appender = appender.file(paste(Sys.Date(),'modellog.log')),
                  layout.format('[~l] [~t] [~n.~f]extract_scorefeatures_from_list: ~m'));flog.error('%s',e)
      NULL
    }
  )
}

parse_json_2_score_features <- function(json){
  tryCatch(
    {
      res = jsonlite::fromJSON(json)
      extract_scorefeatures_from_list(res)
    },
    error = function(e){
      flog.logger(name='ROOT',INFO,appender = appender.file(paste(Sys.Date(),'modellog.log')),
                  layout.format('[~l] [~t] [~n.~f]parse_json_2_score_features: ~m'));flog.error('%s',e)
      NULL
    }
  )
}
scoreFun_base  = function(json){
  #----str_sql-----
  str_sql = "
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
  
  str_sql_huabei = "
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
  sex * 0.708322586241409 + age * 0.945319075870239 + huai_bei_limit * 
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
  /****** huai_bei_limit ******/
  case 
  when huai_bei_limit is null then -2.76878148389683  
  when huai_bei_limit <= 0 then  -2.76878148389683  
  when 0 < huai_bei_limit and huai_bei_limit <= 199 then -0.48
  when 199 < huai_bei_limit and huai_bei_limit <= 499 then -0.37
  when 499 < huai_bei_limit and huai_bei_limit <= 999 then -0.11
  when 999 < huai_bei_limit and huai_bei_limit <= 2999 then 0.08
  when 2999 < huai_bei_limit and huai_bei_limit <= 3999 then 0.39
  when 3999 < huai_bei_limit and huai_bei_limit <= 5999 then 1.02
  when 5999 < huai_bei_limit and huai_bei_limit <= 9999 then 1.06014972566487 
  when 9999 < huai_bei_limit and huai_bei_limit <= 14999 then 1.18
  when 14999 < huai_bei_limit and huai_bei_limit <= 20000 then 2.91513836166312
  when 20000 < huai_bei_limit                              then 5.93072313092869
  else  -0.09999  end as 
  huai_bei_limit ,
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
#-------------
  tryCatch({
    infos <- parse_json_2_score_features(json)
    library(DBI);library(RSQLite)
    con <- dbConnect(RSQLite::SQLite(), ":memory:")
    RSQLite::dbWriteTable(con, "infos", infos,overwrite = TRUE)
    if(infos$zmscore %in% c('Z1','Z2','Z3','Z4','Z5')){
      rs <- DBI::dbSendQuery(con,str_sql)
      flog.logger(name='ROOT',INFO,appender = appender.file(paste(Sys.Date(),'modellog.log')),
                  layout.format('[~l] [~t] [~n.~f]scoreFun_base: ~m'));flog.info('%s',"walk through str_sql zm model!")
    }else{
      rs <- DBI::dbSendQuery(con,str_sql_huabei)
      flog.logger(name='ROOT',INFO,appender = appender.file(paste(Sys.Date(),'modellog.log')),
                  layout.format('[~l] [~t] [~n.~f]scoreFun_base: ~m'));flog.info('%s',"walk through str_sql_huabei huabei model!")
    }
    infos_w <- dbFetch(rs)
    DBI::dbClearResult(rs)
    DBI::dbDisconnect(con);
    
    # score process
    score <- round(infos_w$score) + round(mod(infos$age,10) / 3) # rnd
    infos$score_p <- infos_w$score_p
    infos$score <- infos_w$score
    infos$score_adj <- score
    score
  }
  ,error = function(e){
    flog.logger(name='ROOT',INFO,appender = appender.file(paste(Sys.Date(),'modellog.log')),
                layout.format('[~l] [~t] [~n.~f]scoreFun_base: ~m'));flog.error('%s',e)
    score = 0L 
    score
  })
}
scoreFun_custom <- function(json){
  tryCatch(
    {
      # score functions
      scoreFun_rent_app_edu <- scoreFun_base
      scoreFun_rent_app_soc <- scoreFun_base
      scoreFun_rent_alipay <- scoreFun_base
      scoreFun_cashloan <- scoreFun_base
      
      res <- jsonlite::fromJSON(json)
      # compute alipay id
      # zm_code <- paste('Z',1:5,sep='')
      alipay_id <- res$baseInfo$is_alipay %>% check_NA() %>% stringr::str_to_upper() %in% c("1","TRUE","True")
      # compute edu & society id
      society_id <- res$baseInfo$is_app_society %>% check_NA() %>%  stringr::str_to_upper() %in% c("1","TRUE","True")
      student_id <- res$baseInfo$is_app_student %>% check_NA() %>%  stringr::str_to_upper() %in% c("1","TRUE","True")
      # compute cashloan id
      cashloan_id <- res$baseInfo$is_cashloan  %>% check_NA() %>%  stringr::str_to_upper() %in% c("1","TRUE","True")
      
      # log
      ID_INFO <- list(realname = res$baseInfo$realname,id_card = res$baseInfo$id_card,tel = res$baseInfo$tel,cashloan_id = cashloan_id,alipay_id = alipay_id,society_id = society_id,student_id = student_id) %>% jsonlite::toJSON(null = 'null')
      flog.logger(name='ROOT',INFO,appender = appender.file(paste(Sys.Date(),'modellog.log')),
                  layout.format('[~l] [~t] [~n.~f]cust_status: ~m'));flog.info('%s',ID_INFO)
      # apv skip through check
      apv_skip_state <- apv_skip_through_of_zmscore_huabei(res)
      #---SCORE & RULE COMPUTE BEGIN ---#
      if(cashloan_id){
        #--- cashloan---#
        # score <- scoreFun_cashloan(json)
        if(apv_skip_state){
          rule_mingzhong <- ruleFun_custom(json,ruleset,product_type = c("apv_skip_throuth"))
        }else{
          rule_mingzhong <- ruleFun_custom(json,ruleset,product_type = c("cashloan"))
        }
        decision <- amtFun_cashloan(json, loan_amt_ratio = 1, score_threshold = 600, max_loan_limit = 2500)
        
      }else if(alipay_id && (society_id == FALSE && student_id == FALSE)) {
        #--- rent-alipay---#
        # score <- scoreFun_rent_alipay(json)
        if(apv_skip_state){
          rule_mingzhong <- ruleFun_custom(json,ruleset,product_type = c("alipay_apv_skip_throuth"))
        }else{
          rule_mingzhong <- ruleFun_custom(json,ruleset,product_type = c("rent_alipay"))
        }
        decision <- amtFun_rent_alipay(json, loan_amt_ratio = 1, score_threshold = 600, max_loan_limit = 9000)
      }else if(alipay_id && (society_id == TRUE || student_id == TRUE)) {
        #--- rent-alipay-app---#
        # score <- scoreFun_rent_alipay(json)
        if(apv_skip_state){
          rule_mingzhong <- ruleFun_custom(json,ruleset,product_type = c("alipay_apv_skip_throuth"))
        }else{
          rule_mingzhong <- ruleFun_custom(json,ruleset,product_type = c("rent_alipay_app"))
        }
        decision <- amtFun_rent_alipay(json, loan_amt_ratio = 1, score_threshold = 600, max_loan_limit = 9000)
      }else if((alipay_id == FALSE) && student_id){
        #--- rent edu---#
        # score <- scoreFun_rent_app_edu(json)
        if(apv_skip_state){
          rule_mingzhong <- ruleFun_custom(json,ruleset,product_type = c("apv_skip_throuth"))
        }else{
          rule_mingzhong <- ruleFun_custom(json,ruleset,product_type = c("rent_app_edu"))
        }
        decision <- amtFun_rent_app_edu(json, loan_amt_ratio = 1, score_threshold = 600, max_loan_limit = 7000)
      }else {
        #--- rent society---#
        # score <- scoreFun_rent_app_soc(json)
        if(apv_skip_state){
          rule_mingzhong <- ruleFun_custom(json,ruleset,product_type = c("apv_skip_throuth"))
        }else{
          rule_mingzhong <- ruleFun_custom(json,ruleset,product_type = c("rent_app_soc"))
        }
        decision <- amtFun_rent_app_soc(json, loan_amt_ratio = 1, score_threshold = 600, max_loan_limit = 10000)
      }
      #---SCORE & RULE COMPUTE END ---#
      
      #---SUMMARIZE SCORE&RULE BEGIN ---#
      rule_mingzhong_num <- rule_mingzhong %>% length() 
      decision$score_ori <- decision$score
      if(rule_mingzhong_num > 0) {
        decision$score <- 110
        decision$advice <- 0
      }
      decision$rule_mingzhong_num <- rule_mingzhong_num
      decision$rule_mingzhong <- rule_mingzhong %>% jsonlite::toJSON(na='null')
      #---SUMMARIZE SCORE&RULE END ---#
      # decision$rule_mingzhong <- rule_mingzhong %>% jsonlite::toJSON(na = 'null')
      list(decision = decision)
    },
    error = function(e){
      flog.logger(name='ROOT',INFO,appender = appender.file(paste(Sys.Date(),'modellog.log')),
                  layout.format('[~l] [~t] [~n.~f]scoreFun_custom: ~m'));flog.error('%s',e)
      decision <- data_frame(score = 0,advice =0,advice_amt=0,final_amt=0,advice_ori=0,final_amt_ori=0,error_status = 1) 
      list(decision = decision)
    }
  )
}

scoreFun = function(json,str_sql =NULL,str_amt=NULL){
  # str_sql str_amt nouse paras,jianrong last version.
  tryCatch({
    # log
    flog.logger(name='ROOT',INFO,appender = appender.file(paste(Sys.Date(),'modellog.log')),
                layout.format('[~l] [~t] [~n.~f]jsondata: ~m'));flog.info('%s',json)
    
    if(jsonlite::validate(json)){
      rt <- scoreFun_custom(json) 
      decision <- rt$decision
      res <- jsonlite::fromJSON(json);# print log
      #---- whitelist---begin---#
      if(file.exists('/usr/src/white_list.txt')){
        white_list <- readLines('/usr/src/white_list.txt')
        if(res$baseInfo$id_card %in% white_list){
          decision$score <- 111
          decision$advice <- 1;
          decision$final_amt <- 100;
          decision$white_status <- 1;
        }
      }
      #---- whitelist---end-----#
      #---- blacklist---begin---#
      if(file.exists('/usr/src/black_list.txt')){
        black_list <- readLines('/usr/src/black_list.txt')
        if(res$baseInfo$id_card %in% black_list){
          decision$score <- 119
          decision$advice <- 0;
          decision$final_amt <- 100;
          decision$black_status <- 1;
        }
      }
      #---- blacklist---end-----#

      #--anti-fraud--start-#
      decision$advice_ori <- decision$advice;
      decision$final_amt_ori <- decision$final_amt
      #--anti-fraud--end-#
      
      #-----all 100% yajin apv_skip---begin--#
      # anti_fraut take advice_ori
      real_mianya_ratio <- res$baseInfo$real_mianya_ratio %>% check_NA()
      if(!is.na(real_mianya_ratio) && real_mianya_ratio == 0){
        decision$advice_ori <- 1
        decision$final_amt <- 0
        decision$final_amt_ori <- 0
        decision$all_yajin_status <- 1
      }
      #-----all 100% yajin apv_skip---end----#
      
      # log
      cust_info <- list(baseInfo = res$baseInfo,decision = decision) %>% jsonlite::toJSON(null=NULL)
      flog.logger(name='ROOT',INFO,appender = appender.file(paste(Sys.Date(),'modellog.log')),
                  layout.format('[~l] [~t] [~n.~f]cust_info: ~m'));flog.info('%s',cust_info)
      decision %>% jsonlite::toJSON(na = 'null')
    }else{
      stop("error:customer infos data must be json!")
    }
  }
  ,error = function(e){
    decision <- data_frame(score = 0,advice =0,advice_amt=0,final_amt=0,advice_ori=0,final_amt_ori=0,error_status = 1) 
    
    flog.logger(name='ROOT',INFO,appender = appender.file(paste(Sys.Date(),'modellog.log')),
                layout.format('[~l] [~t] [~n.~f]scoreFun: ~m'));flog.error('%s',e)
    decision %>% jsonlite::toJSON(na = 'null')
  })
}


ruleFun_base <- function(json,rules_config='',ruleset = ruleset){
  # rules_config <- c("rule_age","rule_zaiwang","rule_zmscore",)
  tryCatch(
    { ruleset <- ruleset
    res = jsonlite::fromJSON(json)
    rt = list()
    for(x in rules_config){
      if(class(ruleset[[x]]) != "function") next
      rt[[x]] <- ruleset[[x]](res)
    }
    
    mingzhong <- !(rt %in% 'TRUE')
    rt[mingzhong] %>% names() 
    }
    ,error = function(e){
      flog.logger(name='ROOT',INFO,appender = appender.file(paste(Sys.Date(),'modellog.log')),
                  layout.format('[~l] [~t] [~n.~f]ruleFun_base: ~m'));flog.error('%s',e)
      NULL
    }
  )
}
ruleFun_custom <- function(json,ruleset,product_type = c("rent_app_edu","rent_app_soc","rent_alipay","rent_alipay_app","cashloan","apv_skip_throuth","alipay_apv_skip_throuth")){
  tryCatch(
    {
      rule_rent_app_student_state <-  c("rule_suanhua_G1_payday","rule_xinyan_edu","rule_xinyan_error","rule_suanhua_error","rule_age","rule_zaiwang"
                                        ,"rule_zmscore_edu","rule_taobao_his_days_edu","rule_taobao_shiming","rule_taobao_address_edu"
                                        ,"rule_xuexin_xueli_limit","rule_xuexin_in_school_limit","rule_xuexin_xuezhi_limit","rule_txl_edu"
                                        ,"rule_taobao_alipay_shiming","rule_taobao_alipayinfo_error","rule_taobao_alipayinfo_huabei_overdue","rule_taobao_alipay_his_days_edu","rule_taobao_alipayinfo_bankcard_cnt","rule_taobao_alipayinfo_deliver_tel"
                                        ,"rule_taobao_order_succ_recentdeliveraddress_cnt", "rule_taobao_order_succ_cnt", "rule_taobao_huabei_amt_edu", "rule_taobao_huabei_amt_canuse", "rule_taobao_huabei_amt_use_ratio"
                                        ,"rule_zrobot_idcard_in_blacklist","rule_zrobot_sn_order1_blacklist_contacts_cnt","rule_zrobot_sc_zrxy_zm")
      
      rule_rent_app_society_state <-  c("rule_suanhua_G1_payday","rule_xinyan","rule_xinyan_error","rule_suanhua_error","rule_region","rule_age","rule_zaiwang","rule_zmscore","rule_taobao_his_days","rule_taobao_shiming","rule_txl"
                                        ,"rule_taobao_alipay_shiming","rule_taobao_alipayinfo_error","rule_taobao_alipayinfo_huabei_overdue","rule_taobao_alipay_his_days","rule_taobao_alipayinfo_bankcard_cnt","rule_taobao_alipayinfo_deliver_tel"
                                        ,"rule_taobao_order_succ_recentdeliveraddress_cnt", "rule_taobao_order_succ_cnt", "rule_taobao_huabei_amt", "rule_taobao_huabei_amt_canuse", "rule_taobao_huabei_amt_use_ratio"
                                        ,"rule_zrobot_idcard_in_blacklist","rule_zrobot_sn_order1_blacklist_contacts_cnt","rule_zrobot_sc_zrxy_zm")
      
      rule_rent_alipay            <-  c("rule_suanhua_G1_payday","rule_xinyan","rule_xinyan_error","rule_suanhua_error","rule_region","rule_age","rule_zaiwang","rule_zmscore","rule_zrobot_sc_zrxy_zm")
      rule_rent_alipay_app        <-  c("rule_suanhua_G1_payday","rule_xinyan","rule_xinyan_error","rule_suanhua_error","rule_region","rule_age","rule_zaiwang","rule_zmscore","rule_txl","rule_zrobot_sc_zrxy_zm")
      
      rule_cashloan               <-  c("rule_xinyan_error","rule_suanhua_error","rule_region","rule_age","rule_sanyaosu","rule_zaiwang","rule_zmscore","rule_taobao_his_days","rule_taobao_shiming","rule_txl",
                                        "rule_yyx_call_last6m_topin_txl","rule_yyx_call_last6m_concentrate",
                                        "rule_yyx_call_last6m_Silent_days_n7_cnt","rule_yyx_call_last6m_Silent_days_n5_cnt","rule_yyx_call_last6m_Silent_days_n3_cnt",
                                        "rule_yyx_call_last6m_dialed_succ_ratio","rule_yyx_call_last3m_dialed_succ_ratio","rule_yyx_call_last1m_dialed_succ_ratio"
                                        ,"rule_taobao_alipay_shiming","rule_taobao_alipayinfo_error","rule_taobao_alipayinfo_huabei_overdue","rule_taobao_alipay_his_days"
                                        ,"rule_taobao_order_succ_recentdeliveraddress_cnt", "rule_taobao_order_succ_cnt", "rule_taobao_huabei_amt", "rule_taobao_huabei_amt_canuse", "rule_taobao_huabei_amt_use_ratio","rule_zrobot_sc_zrxy_zm")
      
      rule_apv_skip_throuth       <-   c("rule_suanhua_G1_payday","rule_xinyan","rule_xinyan_error","rule_suanhua_error","rule_apv_skip_through_of_zmscore_huabei"
                                         ,"rule_taobao_alipay_shiming","rule_taobao_alipayinfo_error","rule_taobao_alipayinfo_huabei_overdue"
                                         ,"rule_zrobot_idcard_in_blacklist","rule_zrobot_sn_order1_blacklist_contacts_cnt","rule_zrobot_sc_zrxy_zm")
      rule_alipay_apv_skip_throuth <-  c("rule_suanhua_G1_payday","rule_xinyan","rule_xinyan_error","rule_suanhua_error","rule_apv_skip_through_of_zmscore_huabei","rule_zrobot_sc_zrxy_zm")
      
      # ruleFun_list = ruleset$ruleFun_list
      if(product_type == "rent_app_edu"){
        ruleFun_base(json,rules_config = rule_rent_app_student_state,ruleset) -> rt
      }else if(product_type == "rent_app_soc"){
        ruleFun_base(json,rules_config = rule_rent_app_society_state,ruleset) -> rt
      }else if(product_type == "rent_alipay"){
        ruleFun_base(json,rules_config = rule_rent_alipay,ruleset) -> rt
      }else if(product_type == "rent_alipay_app"){
        ruleFun_base(json,rules_config = rule_rent_alipay_app,ruleset) -> rt
      }else if(product_type == "cashloan"){
        ruleFun_base(json,rules_config = rule_cashloan,ruleset) -> rt
      }else if(product_type == "apv_skip_throuth"){
        ruleFun_base(json,rules_config = rule_apv_skip_throuth,ruleset) -> rt
      }else if(product_type == "alipay_apv_skip_throuth"){
        ruleFun_base(json,rules_config = rule_alipay_apv_skip_throuth,ruleset) -> rt
      }else{
        NULL -> rt
      }
      flog.logger(name='ROOT',INFO,appender = appender.file(paste(Sys.Date(),'modellog.log')),
                  layout.format('[~l] [~t] [~n.~f]ruleFun_custom: ~m'));flog.info('product_type:%s',product_type)
    }
    ,error = function(e){
      flog.logger(name='ROOT',INFO,appender = appender.file(paste(Sys.Date(),'modellog.log')),
                  layout.format('[~l] [~t] [~n.~f]ruleFun_custom: ~m'));flog.error('%s',e)
      NULL
    }
  )
  rt
}

# define score functions
scoreFun_rent_app_edu <- scoreFun_base
scoreFun_rent_app_soc <- scoreFun_base
scoreFun_rent_alipay <- scoreFun_base
scoreFun_cashloan <- scoreFun_base

amtFun_rent_app_soc = function(json,loan_amt_ratio = 1.0,score_threshold = 600,max_loan_limit = 10000){
  tryCatch({
    infos <- parse_json_2_score_features(json)
    score <- scoreFun_rent_app_soc(json)
    score_df <- data.frame(score = score)
    #--------str_amt---------------------------
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
    score_df
    "
    # compute society amt
    str_amt = sprintf(str_amt,score_threshold)
    con <- dbConnect(RSQLite::SQLite(), ":memory:")
    RSQLite::dbWriteTable(con, "score_df", score_df,overwrite = TRUE)
    rs <- DBI::dbSendQuery(con,str_amt)
    decision <- dbFetch(rs)
    DBI::dbClearResult(rs)
    DBI::dbDisconnect(con);
    #-----------------------------------    
    decision$advice_amt <- round(decision$advice_amt * loan_amt_ratio / 100) * 100 + round(mod(infos$age,10) / 3) * 100 # rnd
    decision$final_amt <- decision$advice_amt %>% min(.,max_loan_limit)
    decision %>% select(score,advice,advice_amt,final_amt)
  }
  ,error = function(e){
    print('error!')
    flog.logger(name='ROOT',INFO,appender = appender.file(paste(Sys.Date(),'modellog.log')),
                layout.format('[~l] [~t] [~n.~f]amtFun_rent_app_soc: ~m'));flog.error('%s',e)
    decision <- data_frame(score = 0,advice =0,advice_amt=0,final_amt=0) 
    decision %>% select(score,advice,advice_amt,final_amt)
  })
}
amtFun_rent_app_edu = function(json,loan_amt_ratio = 1.0,score_threshold = 600,max_loan_limit = 8000){
  tryCatch({
    infos <- parse_json_2_score_features(json)
    score <- scoreFun_rent_app_edu(json)
    decision <- data.frame(infos,score = score)
    decision$advice <- ifelse(score > score_threshold,1,0)
    
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
    decision$edu_advice_amt <- round(decision$edu_advice_amt * loan_amt_ratio / 100) * 100 + round(mod(infos$age,10) / 3) * 100 # rnd
    decision$advice_amt <- decision$edu_advice_amt
    decision$final_amt <- decision$edu_advice_amt %>% min(.,max_loan_limit)
    decision %>% select(score,advice,advice_amt,final_amt)
  }
  ,error = function(e){
    flog.logger(name='ROOT',INFO,appender = appender.file(paste(Sys.Date(),'modellog.log')),
                layout.format('[~l] [~t] [~n.~f]amtFun_rent_app_edu: ~m'));flog.error('%s',e)
    data_frame(score = 0,advice =0,advice_amt=0,final_amt=0) 
    decision %>% select(score,advice,advice_amt,final_amt)
  })
  
}
amtFun_cashloan = function(json,loan_amt_ratio = 1.0,score_threshold = 600,max_loan_limit = 2000){
  tryCatch({
    infos <- parse_json_2_score_features(json)
    score <- scoreFun_cashloan(json)
    score_df <- data.frame(infos,score = score)
    #-----str_amt----------------------------------------------------
    str_amt = "
    select 
    case 
    when score > %s then 1 else 0 
    end as advice
    ,
    case
    when  huai_bei_limit is NULL  then  800
    when	huai_bei_limit <=	1000	then	800
    when	huai_bei_limit <=	3000 	then	1000
    when	huai_bei_limit <=	4000 	then	1200
    when	huai_bei_limit <=	6000 	then	1300
    when	huai_bei_limit <=	10000 then	1400
    when	huai_bei_limit <=	15000 then	1500
    when	huai_bei_limit >  15000 then 1600
    else	0			
    end as	advice_amt			
    ,*
    from 
    score_df
    "
    
    # compute society amt
    str_amt = sprintf(str_amt,score_threshold)
    con <- dbConnect(RSQLite::SQLite(), ":memory:")
    RSQLite::dbWriteTable(con, "score_df", score_df,overwrite = TRUE)
    rs <- DBI::dbSendQuery(con,str_amt)
    decision <- dbFetch(rs)
    DBI::dbClearResult(rs)
    DBI::dbDisconnect(con);
    #------------------------------------------------------------    
    decision$advice_amt <- round(decision$advice_amt * loan_amt_ratio / 100) * 100 + round(mod(infos$age,10) / 3) * 100 # rnd
    decision$final_amt <- decision$advice_amt %>% min(.,max_loan_limit)
    decision %>% select(score,advice,advice_amt,final_amt)
  }
  ,error = function(e){
    flog.logger(name='ROOT',INFO,appender = appender.file(paste(Sys.Date(),'modellog.log')),
                layout.format('[~l] [~t] [~n.~f]amtFun_cashloan: ~m'));flog.error('%s',e)
    decision <- data_frame(score = 0,advice =0,advice_amt=0,final_amt=0) 
    decision %>% select(score,advice,advice_amt,final_amt)
  })
  
}
amtFun_rent_alipay = function(json,loan_amt_ratio = 1.0,score_threshold = 600,max_loan_limit = 10000){
  tryCatch({
    infos <- parse_json_2_score_features(json)
    score <- scoreFun_rent_alipay(json)
    score_df <- data.frame(score = score)
    #--------str_amt---------------------------
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
    score_df
    "
    # compute society amt
    str_amt = sprintf(str_amt,score_threshold)
    con <- dbConnect(RSQLite::SQLite(), ":memory:")
    RSQLite::dbWriteTable(con, "score_df", score_df,overwrite = TRUE)
    rs <- DBI::dbSendQuery(con,str_amt)
    decision <- dbFetch(rs)
    DBI::dbClearResult(rs)
    DBI::dbDisconnect(con);
    #-----------------------------------    
    decision$advice_amt <- round(decision$advice_amt * loan_amt_ratio / 100) * 100 + round(mod(infos$age,10) / 3) * 100 # rnd
    decision$final_amt <- decision$advice_amt %>% min(.,max_loan_limit)
    decision %>% select(score,advice,advice_amt,final_amt)
  }
  ,error = function(e){
    flog.logger(name='ROOT',INFO,appender = appender.file(paste(Sys.Date(),'modellog.log')),
                layout.format('[~l] [~t] [~n.~f]amtFun_rent_alipay: ~m'));flog.error('%s',e)
    decision <- data_frame(score = 0,advice =0,advice_amt=0,final_amt=0) 
    decision %>% select(score,advice,advice_amt,final_amt)
  })
  
}

# self-defined function
car_recode <- function (var, recodes, as.factor, as.numeric = TRUE, levels) {
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






#-------------------no use & drop ----------------------------_#

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
  
  
  
  #---- now implement scoreFun(infos,str_dz,str_amt)-----#
  # re.assign("infos",infos);
  # log.info("Rpara infos = "+re.eval("infos").asString());
  # 
  # REXP x= new REXP();
  # if("loan".equals(type)) {
  #   x = re.eval("scoreFun(infos,str_dz,str_amt)");
  # }else {
  #   x = re.eval("scoreFun(infos,str_dq,str_amt)");
  # }
  # log.info("loantype= ["+type+"] R result="+ JSON.toJSONString(x));
  #------ now implement scoreFun(infos,str_dq,str_amt)-----#
  
  
  
  
  
  
  
  
  
  
  
  
  