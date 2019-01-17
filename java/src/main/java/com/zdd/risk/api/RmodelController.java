package com.zdd.risk.api;

import com.alibaba.fastjson.JSONObject;
import com.zdd.risk.dao.ICertificationDAO;
import com.zdd.risk.service.IRModelService;
import com.zdd.risk.service.IXinyanService;
import com.zdd.risk.utils.IdCardUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.io.*;
import java.math.BigDecimal;
import java.util.HashMap;
import java.util.Map;

/**
 * R model
 *
 * @author 租无忧科技有限公司
 * @date 2018-11-16.
 */
@RestController
@RequestMapping("/risk/Rmodel")
public class RmodelController {
    private static final Logger log = LoggerFactory.getLogger(RmodelController.class);

    @Autowired
    private IRModelService iRModelService;

    private static final int DEF_DIV_SCALE = 10;

    @RequestMapping(value = "/ceshishuju")
    public JSONObject ceshishuju(String params){
        try {
            //获取输入流
            FileReader fr = new FileReader("/usr/src/java/ceshishuju.txt");
            BufferedReader buff = new BufferedReader(fr);
            //获取输入流 CommonsMultipartFile 中可以直接得到文件的流
            while(buff.ready())
            {
                String strline = buff.readLine();
                log.info("strline="+strline);
                JSONObject  param = JSONObject.parseObject(strline);
                JSONObject result = iRModelService.callRJava(param,"loan");
            }

            fr.close();
            buff.close();
        } catch (Exception e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        return null;
    }

    @RequestMapping(value = "/callModel")
    public JSONObject callModel(String params){
        log.info("callModel接口入参 params= "+params);
//        String  infos="[{\"zm_jianmian\":1000,\"zmscore\":\"Z1( <600 )\",\"real_mianya_ratio\":0.6,\"loans_score\":199,\"latest_three_month\":2,\"loans_credibility\":90,\"sex\":\"male\",\"orderid\":\"123124124\",\"id_card\":\"450703199410121852\",\"apply_score\":189,\"apply_credibility\":84,\"q> ry_org_count\":7,\"query_sum_count\":13,\"loans_org_count\":5,\"latest_one_month_fail\":25,\"loans_credit_limit\":\"1400\",\"consfin_product_count\":\"5\",\"consfin_org_count\":3,\"consfin_max_limit\":5000}]";
        JSONObject  param = JSONObject.parseObject(params);
        JSONObject result = iRModelService.callRJava(param,"loan");
        log.info("callModel end");
        return result;
    }

    @PostMapping(value = "/calculate")
    public JSONObject calculate(@RequestBody String params){
        log.info("calculate接口入参 params= "+params);
        JSONObject result;

        result = iRModelService.calculateModleData(params,"loan");
        log.info("calculate 出参result=" + result.toJSONString());

        log.info("calculate end");
        return result;
    }

    @PostMapping(value = "/calculatePre")
    public JSONObject calculatePre(@RequestBody String params){
        log.info("calculate接口入参 params= "+params);
        JSONObject result;

        result = iRModelService.calculateModleData(params, "preloan");
        log.info("calculate 出参result=" + result.toJSONString());

        log.info("calculate end");
        return result;
    }

}
