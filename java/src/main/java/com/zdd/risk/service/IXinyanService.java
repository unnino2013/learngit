package com.zdd.risk.service;

import com.alibaba.fastjson.JSONObject;
import com.zdd.risk.utils.FormatUtil;

import javax.servlet.*;
import java.io.IOException;
import java.util.Map;

/**
 * @author 租无忧科技有限公司
 */
public interface IXinyanService {

    JSONObject getXinyanInfo(String param);


    JSONObject getRModelInfoByXinyan(String param);
}
