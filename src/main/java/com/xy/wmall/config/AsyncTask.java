package com.xy.wmall.config;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.collections4.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Component;

import com.xy.wmall.common.WmallCache;
import com.xy.wmall.enums.TrueFalseStatusEnum;
import com.xy.wmall.model.LogisticsCompany;
import com.xy.wmall.model.Price;
import com.xy.wmall.service.LogisticsCompanyService;
import com.xy.wmall.service.PriceService;

/**
 * 异步任务
 * 
 * @author xiongyan
 * @date 2018年1月31日 下午4:35:09
 */
@Component
public class AsyncTask {

	@Autowired
	private PriceService priceService;
	
	@Autowired
	private LogisticsCompanyService logisticsCompanyService;
	
	/**
	 * 产品价格 缓存
	 */
	@Async
	public void priceCache() {
		Map<String, Object> map = new HashMap<>(1);
		map.put("isDelete", TrueFalseStatusEnum.FALSE.getValue());
		List<Price> prices = priceService.listPrice(map);
		if (CollectionUtils.isNotEmpty(prices)) {
			for (Price price : prices) {
				WmallCache.putPrice(price);
			}
		}
	}
	
	/**
	 * 物流公司 缓存
	 */
	@Async
	public void logisticsCompanyCache() {
		List<LogisticsCompany> logisticsCompanies = logisticsCompanyService.listLogisticsCompany();
		if (CollectionUtils.isNotEmpty(logisticsCompanies)) {
			for (LogisticsCompany logisticsCompany : logisticsCompanies) {
				WmallCache.putLogisticsCompany(logisticsCompany);
			}
		}
	}
	
	/**
	 * 角色权限 缓存
	 */
	@Async
	public void roleMenuCache() {
		Map<String, Object> map = new HashMap<>(1);
		map.put("isDelete", TrueFalseStatusEnum.FALSE.getValue());
		List<Price> prices = priceService.listPrice(map);
		if (CollectionUtils.isNotEmpty(prices)) {
			for (Price price : prices) {
				WmallCache.putPrice(price);
			}
		}
	}
}
