package com.zdd.risk.service;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;
import com.zdd.risk.bean.Certification;
import com.zdd.risk.bean.CertificationExample;
import com.zdd.risk.dao.ICertificationDAO;
import com.zdd.risk.utils.*;
import com.zdd.risk.utils.rsa.RsaCodingUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.RequestBody;

import javax.servlet.*;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.*;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.Map.Entry;

/**
 * @author 租无忧科技有限公司
 * @createtime 2016年9月26日下午8:47:14
 */
@Service
public class XinyanServiceImpl implements  IXinyanService{
    private static final Logger log = LoggerFactory.getLogger(XinyanServiceImpl.class);

    @Value("${member.id}")
    private String memberId;
    @Value("${terminal.id}")
    private String terminalId;
    @Value("${pfx.name}")
    private String pfxName;
    @Value("${pfx.pwd}")
    private String pfxPwd;
    @Value("${radar.url}")
    private String radarUrl;
    @Autowired
    private ICertificationDAO iCertificationDAO;

    @Override
    public JSONObject getXinyanInfo(String param){
        log("getXinyanInfo start param:" + param);
        JSONObject inputParams = JSONObject.parseObject(param);

        /** 1、 商户号 **/
        String member_id = memberId;
        /** 2、终端号 **/
        String terminal_id =terminalId;
        /** 3、请求地址 **/
        String url;
        String PostString = null;
        Map<String, String> headers = new HashMap<String, String>();


        String urlType = "ZX-RadarUrl";
        String id_no = inputParams.getString("idcard_no");
        String id_name = inputParams.getString("realname");
        String phone_no = inputParams.getString("tel")!=null?inputParams.getString("tel"):"";
        String bankcard_no = inputParams.getString("bankcard_no")!=null?inputParams.getString("bankcard_no"):"";
        String versions = inputParams.getString("versions")!=null?inputParams.getString("versions"):"1.3.0";

        log(" 原始数据:id_no:" + id_no + ",id_name:" + id_name + ",phone_no:" + phone_no + ",bankcard_no:" + bankcard_no);

        id_no = MD5Utils.encryptMD5(id_no.trim());
        id_name = MD5Utils.encryptMD5(id_name.trim());
        bankcard_no = MD5Utils.encryptMD5(bankcard_no.trim());
        phone_no = MD5Utils.encryptMD5(phone_no.trim());

        log("32位小写MD5加密后数据:id_no:" + id_no + ",id_name:" + id_name + ",phone_no:" + phone_no + ",bankcard_no:"
                + bankcard_no);

        String trade_date = new SimpleDateFormat("yyyyMMddHHmmss").format(new Date());// 订单日期
        String trans_id = "" + System.currentTimeMillis();// 商户订单号

        String XmlOrJson = "";
        /** 组装参数 **/
        Map ArrayData = new HashMap<Object, Object>();
        ArrayData.put("member_id", member_id);
        ArrayData.put("terminal_id", terminal_id);
        ArrayData.put("trade_date", trade_date);
        ArrayData.put("trans_id", trans_id);
        ArrayData.put("phone_no", phone_no);
        ArrayData.put("bankcard_no", bankcard_no);
        ArrayData.put("versions", versions);
        ArrayData.put("industry_type", "A1");// 参照文档传自己公司对应的行业参数
        ArrayData.put("id_no", id_no);
        ArrayData.put("id_name", id_name);

        JSONObject jsonObjectFromMap = new JSONObject(ArrayData);
        XmlOrJson = jsonObjectFromMap.toString();
        log("====请求明文:" + XmlOrJson);

        /** base64 编码 **/
        String base64str = null;
        try {
            base64str = SecurityUtil.Base64Encode(XmlOrJson);
        } catch (UnsupportedEncodingException e) {
            e.printStackTrace();
        }
        log("base64str:" + base64str);
        /** rsa加密 **/
        String pfxpath = pfxName;// 商户私钥

        File pfxfile = new File(pfxpath);
        if (!pfxfile.exists()) {
            log("私钥文件不存在！");
            throw new RuntimeException("私钥文件不存在！");
        }
        String pfxpwd = pfxPwd;// 私钥密码

        String data_content = RsaCodingUtil.encryptByPriPfxFile(base64str, pfxpath, pfxpwd);// 加密数据
        log("====加密串:" + data_content);

        url = radarUrl;
        log("url:" + url);
        Map<String, Object> params = new HashMap<String, Object>();
        params.put("member_id", member_id);
        params.put("terminal_id", terminal_id);
        params.put("data_type", "json");
        params.put("data_content", data_content);

        PostString = HttpUtils.doPostByForm(url, headers, params);
        log("请求返回PostString：" + PostString);

        /** ================处理返回结果============= **/
        if (StringUtils.isEmpty(PostString)) {// 判断参数是否为空
            log("=====返回数据为空");
            throw new RuntimeException("返回数据为空");
        }
        log("getXinyanInfo end result:" + PostString);
        return JSON.parseObject(PostString);
    }

    public void log(String msg) {
        log.info(new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(new Date()) + "\t: " + msg);
    }

    @Override
    public JSONObject getRModelInfoByXinyan(String param){
        JSONObject outputJson = new JSONObject();
        JSONObject inputParams = JSONObject.parseObject(param);
        if(!StringUtils.isEmpty(param)) {
            String idcard= inputParams.getString("idcard_no");
            String mobile= inputParams.getString("tel")!=null?inputParams.getString("tel"):"";

            CertificationExample example = new CertificationExample();
            CertificationExample.Criteria criteria = example.createCriteria();
            criteria.andIdCardEqualTo(idcard);
            criteria.andMobileEqualTo(mobile);
            criteria.andCertificationTypeEqualTo(String.valueOf(4));
            criteria.andFlagEqualTo(0);
            example.setOrderByClause("creatTime DESC");
            List<Certification> certificationList= iCertificationDAO.selectByExampleWithBLOBs(example);
            if(certificationList != null && certificationList.size()>0) {
                String detailDtoData = certificationList.get(0).getCertificationResult();
                outputJson = JSONObject.parseObject(detailDtoData);
            }else {
                outputJson = getXinyanInfo(param);

                if (outputJson.getBoolean("success")) {
                    JSONObject data = outputJson.getJSONObject("data");
                    String code = data.getString("code");
                    if ("0".equals(code)) {
                        Certification record = new Certification();
                        record.setIdCard(idcard);
                        record.setMobile(mobile);
                        record.setCertificationType(String.valueOf(4));
                        record.setCertificationItem(param);
                        record.setCertificationResult(JSON.toJSONString(data));
                        record.setCertificationLimit(new Date());
                        record.setFlag(0);
                        record.setCreatTime(new Date());

                        iCertificationDAO.insert(record);
                    }
                } else {
                    log("新颜数据返回异常：" + outputJson.toJSONString());
                }
            }
        }
        return outputJson;
    }
}
