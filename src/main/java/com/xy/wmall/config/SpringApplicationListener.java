package com.xy.wmall.config;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.DisposableBean;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationListener;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.stereotype.Component;

import com.xy.wmall.common.utils.HttpClientUtils;

/**
 * 服务器启动初始化数据
 * 
 * @author xiongyan
 * @date 2017年10月27日 下午5:36:05
 */
@Component
public class SpringApplicationListener implements ApplicationListener<ContextRefreshedEvent>, DisposableBean {
	
	/**
	 * logger
	 */
	private static final Logger logger = LoggerFactory.getLogger(SpringApplicationListener.class);
	
	@Autowired
	private AsyncTask asyncTask;
	
	/**
	 * 当spring容器初始化完成后执行该方法
	 */
	@Override
	public void onApplicationEvent(ContextRefreshedEvent event) {
		// 价格缓存
		asyncTask.priceCache();
		// 物流公司缓存
		asyncTask.logisticsCompanyCache();
		// 角色权限缓存
		asyncTask.roleMenuCache();
		logger.info("数据初始化完成");
	}

	/**
	 * 销毁
	 */
	@Override
	public void destroy() throws Exception {
		// 关闭httpclient
		HttpClientUtils.getInstance().close();
	}

}
