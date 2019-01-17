package com.zdd.risk.service;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;
import com.zdd.risk.bean.Certification;
import com.zdd.risk.bean.CertificationExample;
import com.zdd.risk.dao.ICertificationDAO;
import com.zdd.risk.utils.HttpUtils;
import com.zdd.risk.utils.IdCardUtil;
import com.zdd.risk.utils.MD5Utils;
import com.zdd.risk.utils.SecurityUtil;
import com.zdd.risk.utils.rsa.RsaCodingUtil;
import org.rosuda.JRI.REXP;
import org.rosuda.JRI.Rengine;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.RequestMapping;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.UnsupportedEncodingException;
import java.math.BigDecimal;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author 租无忧科技有限公司
 * @createtime 2016年9月26日下午8:47:14
 */
@Service
public class RModelServiceImpl implements  IRModelService{
    private static final Logger log = LoggerFactory.getLogger(RModelServiceImpl.class);
    private static Rengine re = null;
    private static final String LEVEL_A="A";
    private static final String LEVEL_D="D";
    private static final String LEVEL_E="E";
    private static final int DEF_DIV_SCALE = 10;

    @Autowired
    private ICertificationDAO iCertificationDAO;
    @Autowired
    private IXinyanService xinyanService;

    static {
        re = new Rengine(new String[] { "--vanilla" }, false, null);

        if (!(re.waitForR())) {
            System.out.println("Cannot load R");
        } else {

            re.eval("Sys.setlocale('LC_CTYPE','de_DE.utf8')");
//            re.eval("load('/usr/src/score_dz.rdata')");
            re.eval("library(dplyr)");
            re.eval("library(dbplyr)");
            re.eval("library(magrittr)");
            re.eval("library(data.table)");
            re.eval("library(DBI)");
            re.eval("library(RJSONIO)");
            re.eval("library(jsonlite)");
            re.eval("library(RSQLite)");
            re.eval("library(log4r)");
            re.eval("source('/usr/src/dq_dz_model.R')");
        }
    }
    @Override
    public JSONObject callRJava(JSONObject paramJson, String type){
        JSONObject result = new JSONObject();
        String infos =new StringBuffer("").append(paramJson.toJSONString()).append("").toString();
        log.info("java.library.path====="+System.getProperty("java.library.path"));

        String version =re.eval("R.version.string").asString();
        log.info("version="+version);

        re.assign("infos",infos);
        log.info("Rpara infos = "+re.eval("infos").asString());

        REXP x= new REXP();
        if("loan".equals(type)) {
            x = re.eval("scoreFun(infos,str_dz,str_amt)");
        }else {
            x = re.eval("scoreFun(infos,str_dq,str_amt)");
        }
        log.info("loantype= ["+type+"] R result="+ JSON.toJSONString(x));

        if(x!=null && x.getContent()!=null){

            Certification record = new Certification();
            record.setIdCard(paramJson.getJSONObject("baseInfo").getString("id_card"));
            record.setMobile(paramJson.getJSONObject("baseInfo").getString("tel"));
            record.setCertificationType(String.valueOf(1));
            record.setCertificationItem(infos.length()<1000?infos:infos.substring(0,1000)+"...");
            record.setCertificationResult(JSON.toJSONString(x.getContent()));
            record.setCertificationLimit(new Date());
            record.setFlag(0);
            record.setCreatTime(new Date());

            iCertificationDAO.insert(record);
            log.info("result"+JSONObject.toJSON(x.getContent()).toString());
            result =JSON.parseArray(JSONObject.toJSON(x.getContent()).toString()).getJSONObject(0);

        }
        re.end();


        log.info("end");
        return result;
    }


    @Override
    public JSONObject calculateModleData(String params, String type){
        JSONObject result;
        JSONObject calculateInfo =new JSONObject();
        Map reMap = new HashMap<>();
        if(!StringUtils.isEmpty(params)) {

            JSONObject param = JSONObject.parseObject(params);
            String idcard= param.getString("idcard_no");
            String mobile= param.getString("tel")!=null?param.getString("tel"):"";

            param.put("id_card", idcard);
            if("loan".equals(type)) {
                BigDecimal market = param.getBigDecimal("market");
                if (market != null && market.compareTo(new BigDecimal(0)) >= 1) {
                    BigDecimal total_deposit = param.get("total_deposit") != null ? param.getBigDecimal("total_deposit") : new BigDecimal(0);
                    BigDecimal credit_cost = param.get("credit_cost") != null ? param.getBigDecimal("credit_cost") : new BigDecimal(0);
                    BigDecimal real_mianya_ratio = (market.subtract(total_deposit).add(credit_cost)).divide(market, DEF_DIV_SCALE, BigDecimal.ROUND_HALF_UP);
                    param.put("real_mianya_ratio", real_mianya_ratio.doubleValue());
                } else {
                    reMap.put("code", "100001");
                    reMap.put("codeMsg", "market为空");
                    log.info("calculate接口出参 reMap= " + new JSONObject(reMap).toString());
                    log.info("calculate end");
                    return new JSONObject(reMap);
                }
            }
            param.put("sex", IdCardUtil.getGenderByIdCard(param.getString("idcard_no")));

            calculateInfo.put("baseInfo",param);

            JSONObject xinyanInfo = xinyanService.getRModelInfoByXinyan(params);
            JSONObject suanhuaJson= getModleData(idcard,mobile,"3");
            JSONObject taobaoJson= getModleData(idcard,mobile,"6");
            JSONObject xuexinJson= getModleData(idcard,mobile,"7");
            JSONObject yunyingshangJson= getModleData(idcard,mobile,"8");
            JSONObject taobaoReportJson= getModleData(idcard,mobile,"9");
            JSONObject yunyingshangReportJson= getModleData(idcard,mobile,"10");
            calculateInfo.put("suanhuaInfo",suanhuaJson);
            calculateInfo.put("xinyanInfo",xinyanInfo);

            JSONObject moxieInfo =new JSONObject();
            moxieInfo.put("taobaoInfo",taobaoJson);
            moxieInfo.put("xuexinInfo",xuexinJson);
            moxieInfo.put("yunyingshangInfo",yunyingshangJson);
            moxieInfo.put("taobaoReport",taobaoReportJson);
            moxieInfo.put("yunyingshangReport",yunyingshangReportJson);
            calculateInfo.put("moxieInfo",moxieInfo);

            result = calculateModleData(calculateInfo,type);
            if (result != null) {
                log.info("calculate 出参result=" + result.toJSONString());
            }
        }else{
            reMap.put("code", "100002");
            reMap.put("codeMsg", "入参 params为空");
            return new JSONObject(reMap);
        }
        return result;
    }

    private JSONObject calculateModleData(JSONObject paramJson,String type){
        log.info("计算模型入参 param= "+paramJson.toJSONString());
        JSONObject result = callRJava(paramJson,type);
        Map para = new HashMap();
        Map reMap = new HashMap();
        reMap.put("code","100000");
        reMap.put("codeMsg","");
        if(result!=null && result.get("score")!= null) {
            Double final_amt = result.getDouble("final_amt");
            Integer advice = result.getInteger("advice");
            if(result.getDouble("score")>=600 && "1".equals(String.valueOf(advice))) {
                para = new HashMap();
                para.put("level", LEVEL_A);
                para.put("approveCredit", final_amt);
                para.put("recommend", "低风险客户");
                reMap.put("data", para);
            }else{
                para = new HashMap();
                para.put("level", LEVEL_E);
                para.put("approveCredit", final_amt);
                para.put("recommend", "高风险客户建议拒绝");
                reMap.put("data",para);
            }
        }else {
            para = new HashMap();
            para.put("level", LEVEL_D);
            para.put("recommend", "模型入参数据维度不足,建议人工");
            reMap.put("data",para);
        }
        return new JSONObject(reMap);
    }

    private JSONObject getModleData(String idcard,String mobile,String productId){
        CertificationExample example = new CertificationExample();
        CertificationExample.Criteria criteria = example.createCriteria();
        criteria.andIdCardEqualTo(idcard);
        criteria.andMobileEqualTo(mobile);
        criteria.andCertificationTypeEqualTo(productId);
        criteria.andFlagEqualTo(0);
        example.setOrderByClause("creatTime DESC");
        List<Certification> certificationList= iCertificationDAO.selectByExampleWithBLOBs(example);
        if(certificationList != null && certificationList.size()>0) {
            String detailDtoData = certificationList.get(0).getCertificationResult();
            return JSONObject.parseObject(detailDtoData);
        }
        return null;
    }
}
