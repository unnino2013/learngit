package com.zdd.risk.api;

import com.alibaba.fastjson.JSONObject;
import com.zdd.risk.service.IXinyanService;
import io.swagger.annotations.ApiOperation;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;



/**
 * @author 租无忧科技有限公司
 * @createtime 2016年9月26日下午8:58:24
 */
@RestController
@RequestMapping("/risk/model")
public class CreditRatingController{

	private static final Logger log = LoggerFactory.getLogger(CreditRatingController.class);

	@Autowired
	private IXinyanService xinyanService;


	@ApiOperation("新颜数据信息接口")
	@RequestMapping(value = "/xinyan")
	public JSONObject getXinyan(@RequestBody String param){
		log.info("新颜数据信息接口入参param="+param);
		JSONObject outParams = xinyanService.getXinyanInfo(param);
		log.info("新颜数据信息接口出参outParams= "+ outParams!=null?outParams.toString():"");
		return outParams;
	}

}
