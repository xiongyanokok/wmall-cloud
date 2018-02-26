package com.xy.wmall.service;

import java.util.List;
import java.util.Map;

import com.xy.wmall.model.UserProxy;

/**
 * Service 接口
 * 
 * @author admin
 * @date 2018年01月30日 下午02:32:17
 */
public interface UserProxyService {

    /**
     * 保存数据
     *
     * @param userProxy
     */
    void save(UserProxy userProxy);

    /**
     * 修改数据
     *
     * @param userProxy
     */
    void update(UserProxy userProxy);
    
    /**
     * 删除数据
     * 
     * @param userProxy
     */
    void remove(UserProxy userProxy);
    
    /**
     * 根据ID查询
     *
     * @param id
     * @return
     */
    UserProxy getUserProxyById(Integer id);
    
    /**
     * 根据map查询
     * 
     * @param map
     * @return
     */
    UserProxy getUserProxy(Map<String, Object> map);
    
    /**
     * 根据map查询
     * 
     * @param map
     * @return
     */
    List<UserProxy> listUserProxy(Map<String, Object> map);
    
    /**
     * 批量保存
     * 
     * @param list
     */
    void batchSave(List<UserProxy> list);
    
    /**
     * 批量更新
     * 
     * @param list
     */
    void batchUpdate(List<UserProxy> list);
    
    /**
     * 查询代理用户
     * 
     * @param proxyIds
     * @return
     */
    Map<Integer, Integer> listUserByProxy(List<Integer> proxyIds);
    
}
