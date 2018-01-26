package com.xy.wmall.mapper;

import java.util.List;
import java.util.Map;

import com.xy.wmall.model.ProxyDeliver;

/**
 * Mapper
 * 
 * @author admin
 * @date 2018年01月26日 下午02:19:05
 */
public interface ProxyDeliverMapper {

	/**
     * 根据主键查询
     *
     * @param id
     * @return
     */
    ProxyDeliver selectByPrimaryKey(Integer id);

    /**
     * 新增数据库记录
     *
     * @param proxyDeliver
     */
    void insert(ProxyDeliver proxyDeliver);

    /**
     * 更新数据库记录
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
    void batchInsert(List<ProxyDeliver> list);
    
    /**
     * 批量更新
     * 
     * @param list
     */
    void batchUpdate(List<ProxyDeliver> list);
    
}
