package com.zdd.risk.service;

import com.alibaba.fastjson.JSONObject;

/**
 * Created by hyg on 2018-11-21.
 * Copyright by mofanghr
 */
public interface IRModelService {


    JSONObject callRJava(JSONObject paramJson, String type);

    JSONObject calculateModleData(String params, String type);

}
