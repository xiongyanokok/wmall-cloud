package com.xy.wmall.mapper;

import java.util.List;
import java.util.Map;

import com.xy.wmall.model.UserProxy;

/**
 * Mapper
 * 
 * @author admin
 * @date 2018年01月30日 下午02:32:17
 */
public interface UserProxyMapper {

	/**
     * 根据主键查询
     *
     * @param id
     * @return
     */
    UserProxy selectByPrimaryKey(Integer id);

    /**
     * 新增数据库记录
     *
     * @param userProxy
     */
    void insert(UserProxy userProxy);

    /**
     * 更新数据库记录
     *
     * @param userProxy
     */
    void update(UserProxy userProxy);

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
    void batchInsert(List<UserProxy> list);
    
    /**
     * 批量更新
     * 
     * @param list
     */
    void batchUpdate(List<UserProxy> list);
    
}
