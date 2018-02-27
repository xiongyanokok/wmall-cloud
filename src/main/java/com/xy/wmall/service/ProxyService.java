package com.xy.wmall.service;

import java.util.Map;

import com.xy.wmall.model.Proxy;

/**
 * Service 接口
 * 
 * @author admin
 * @date 2017年10月28日 上午08:54:21
 */
public interface ProxyService extends BaseService<Proxy> {

    /**
     * 根据map查询
     * 
     * @param map
     * @return
     */
    Proxy getUserProxy(Map<String, Object> map);
    
}
