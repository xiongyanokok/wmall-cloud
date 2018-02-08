package com.xy.wmall.mapper;

import java.util.List;
import java.util.Map;

import com.xy.wmall.model.ServiceFree;

/**
 * Mapper
 * 
 * @author admin
 * @date 2018年02月07日 下午02:27:32
 */
public interface ServiceFreeMapper {

	/**
     * 根据主键查询
     *
     * @param id
     * @return
     */
    ServiceFree selectByPrimaryKey(Integer id);

    /**
     * 新增数据库记录
     *
     * @param serviceFree
     */
    void insert(ServiceFree serviceFree);

    /**
     * 更新数据库记录
     *
     * @param serviceFree
     */
    void update(ServiceFree serviceFree);

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
    void batchInsert(List<ServiceFree> list);
    
    /**
     * 批量更新
     * 
     * @param list
     */
    void batchUpdate(List<ServiceFree> list);
    
    /**
     * 用户服务有效期
     * 
     * @param map
     * @return
     */
    List<ServiceFree> listServiceDate(Map<String, Object> map);
    
}
