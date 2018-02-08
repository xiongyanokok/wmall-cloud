package com.xy.wmall.service;

import java.util.Date;
import java.util.List;
import java.util.Map;

import com.xy.wmall.model.ServiceFree;

/**
 * Service 接口
 * 
 * @author admin
 * @date 2018年02月07日 下午02:27:32
 */
public interface ServiceFreeService {

	/**
     * 根据主键查询
     *
     * @param id
     * @return
     */
    ServiceFree selectByPrimaryKey(Integer id);
    
    /**
     * 根据ID查询
     *
     * @param id
     * @return
     */
    ServiceFree getServiceFreeById(Integer id);
    
    /**
     * 保存数据
     *
     * @param serviceFree
     */
    void save(ServiceFree serviceFree);

    /**
     * 修改数据
     *
     * @param serviceFree
     */
    void update(ServiceFree serviceFree);
    
    /**
     * 删除数据
     * 
     * @param serviceFree
     */
    void remove(ServiceFree serviceFree);
    
    /**
     * 根据map查询
     * 
     * @param map
     * @return
     */
    ServiceFree getServiceFree(Map<String, Object> map);
    
    /**
     * 根据map查询
     * 
     * @param map
     * @return
     */
    List<ServiceFree> listServiceFree(Map<String, Object> map);
    
    /**
     * 批量保存
     * 
     * @param list
     */
    void batchSave(List<ServiceFree> list);
    
    /**
     * 批量更新
     * 
     * @param list
     */
    void batchUpdate(List<ServiceFree> list);
    
    /**
     * 用户服务有效期
     * 
     * @param userIds
     * @return
     */
    Map<Integer, Date> listServiceDate(List<Integer> userIds);
    
}
