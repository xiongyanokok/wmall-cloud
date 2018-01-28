package com.xy.wmall.service;

import java.util.List;
import java.util.Map;

import com.xy.wmall.model.ProxyDeliver;

/**
 * Service 接口
 * 
 * @author admin
 * @date 2018年01月28日 上午11:34:02
 */
public interface ProxyDeliverService {

	/**
     * 根据主键查询
     *
     * @param id
     * @return
     */
    ProxyDeliver selectByPrimaryKey(Integer id);
    
    /**
     * 根据ID查询
     *
     * @param id
     * @return
     */
    ProxyDeliver getProxyDeliverById(Integer id);
    
    /**
     * 保存数据
     *
     * @param proxyDeliver
     */
    void save(ProxyDeliver proxyDeliver);

    /**
     * 修改数据
     *
     * @param proxyDeliver
     */
    void update(ProxyDeliver proxyDeliver);
    
    /**
     * 根据map查询
     * 
     * @param map
     * @return
     */
    ProxyDeliver getProxyDeliver(Map<String, Object> map);
    
    /**
     * 根据map查询
     * 
     * @param map
     * @return
     */
    List<ProxyDeliver> listProxyDeliver(Map<String, Object> map);
    
    /**
     * 批量保存
     * 
     * @param list
     */
    void batchSave(List<ProxyDeliver> list);
    
    /**
     * 批量更新
     * 
     * @param list
     */
    void batchUpdate(List<ProxyDeliver> list);
    
}
