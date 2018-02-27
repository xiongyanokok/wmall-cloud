package com.xy.wmall.mapper;

import java.util.List;
import java.util.Map;

import com.xy.wmall.model.ProxyLevel;

/**
 * Mapper
 * 
 * @author admin
 * @date 2017年10月28日 上午08:54:24
 */
public interface ProxyLevelMapper extends BaseMapper<ProxyLevel> {

    /**
     * 查询代理级别
     * 
     * @param map
     * @return
     */
    List<ProxyLevel> listLevelByProxy(Map<String, Object> map);
    
    /**
     * 查询代理级别
     * 
     * @param map
     * @return
     */
    List<ProxyLevel> listLevelByUser(Map<String, Object> map);
    
}
