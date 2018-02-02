package com.xy.wmall.config;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.task.AsyncTaskExecutor;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;

/**
 * 线程池
 * 
 * @author xiongyan
 * @date 2017年8月29日 上午11:39:06
 */
@Configuration
@EnableAsync
public class ThreadPoolConfiguration {
	
	/**
	 * 核心线程数
	 */
	@Value("${core.pool.size}")
	public int corePoolSize;
	
	/**
	 * 最大线程数
	 */
	@Value("${max.pool.size}")
	public int maxPoolSize;
	
	 /**
     * 自定义异步线程池
     * 
     * @return
     */
    @Bean
    public AsyncTaskExecutor taskExecutor() {  
        ThreadPoolTaskExecutor executor = new ThreadPoolTaskExecutor(); 
        executor.setCorePoolSize(corePoolSize);
        executor.setMaxPoolSize(maxPoolSize);
        executor.setThreadNamePrefix("wmall-thread-pool-");
        return executor;  
    } 
	
}
