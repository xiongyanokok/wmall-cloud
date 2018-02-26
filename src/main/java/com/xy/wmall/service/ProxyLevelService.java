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
public interface ProxyLevelService {

    /**
     * 保存数据
     *
     * @param proxyLevel
     */
    void save(ProxyLevel proxyLevel);

    /**
     * 修改数据
     *
     * @param proxyLevel
     */
    void update(ProxyLevel proxyLevel);
    
    /**
     * 删除数据
     * 
     * @param proxyLevel
     */
    void remove(ProxyLevel proxyLevel);
    
    /**
     * 根据ID查询
     *
     * @param id
     * @return
     */
    ProxyLevel getProxyLevelById(Integer id);
    
    /**
     * 根据map查询
     * 
     * @param map
     * @return
     */
    ProxyLevel getProxyLevel(Map<String, Object> map);
    
    /**
     * 根据map查询
     * 
     * @param map
     * @return
     */
    List<ProxyLevel> listProxyLevel(Map<String, Object> map);
    
    /**
     * 批量保存
     * 
     * @param list
     */
    void batchSave(List<ProxyLevel> list);
    
    /**
     * 批量更新
     * 
     * @param list
     */
    void batchUpdate(List<ProxyLevel> list);
    
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
