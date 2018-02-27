package com.xy.wmall.service;

import java.util.List;
import java.util.Map;

import com.xy.wmall.model.ProxyLevel;

/**
 * Service 接口
 * 
 * @author admin
 * @date 2017年10月28日 上午08:54:24
 */
public interface ProxyLevelService extends BaseService<ProxyLevel> {

    /**
     * 查询代理级别
     * 
     * @param proxyIds
     * @return
     */
    Map<Integer, Integer> listLevelByProxy(List<Integer> proxyIds);
    
    /**
     * 查询代理级别
     * 
     * @param userIds
     * @return
     */
    Map<Integer, Integer> listLevelByUser(List<Integer> userIds);
    
}
