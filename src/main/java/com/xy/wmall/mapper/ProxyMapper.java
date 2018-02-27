package com.xy.wmall.mapper;

import java.util.Map;

import com.xy.wmall.model.Proxy;

/**
 * Mapper
 * 
 * @author admin
 * @date 2017年10月28日 上午08:54:21
 */
public interface ProxyMapper extends BaseMapper<Proxy> {

    /**
     * 根据map查询
     * 
     * @param map
     * @return
     */
    Proxy getUserProxy(Map<String, Object> map);
    
}
