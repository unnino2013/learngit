package com.zdd.risk.handler;

import com.zdd.risk.exception.BusinessException;
import com.zdd.risk.utils.APIResponse;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.ResponseBody;

/**
 * 统一异常处理
 * @author 租无忧科技有限公司
 * @date 2018-11-01.
 */
@ControllerAdvice
public class GlobalExceptionHandler {

    private static final Logger logger = LoggerFactory.getLogger(BusinessException.class);

    @ExceptionHandler(value = BusinessException.class)
    @ResponseBody
    public APIResponse businessException(Exception e){

        String msg = "请求错误";
        if (e instanceof BusinessException){
            msg = ((BusinessException) e).getErrorCode();
        }
        logger.error("find exception:e={}",e.getMessage());
        e.printStackTrace();
        return APIResponse.fail(msg);
    }
//
//    @ExceptionHandler(value = Exception.class)
//    public String exception(Exception e){
//        logger.error("find exception:e={}",e.getMessage());
//        e.printStackTrace();
//        return "error/400";
//    }

}
