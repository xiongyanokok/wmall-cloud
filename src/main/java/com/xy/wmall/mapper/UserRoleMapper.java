package com.xy.wmall.mapper;

import java.util.List;
import java.util.Map;

import com.xy.wmall.model.UserRole;

/**
 * Mapper
 * 
 * @author admin
 * @date 2018年01月29日 下午02:43:36
 */
public interface UserRoleMapper {

	/**
     * 根据主键查询
     *
     * @param id
     * @return
     */
    UserRole selectByPrimaryKey(Integer id);

    /**
     * 新增数据库记录
     *
     * @param userRole
     */
    void insert(UserRole userRole);

    /**
     * 更新数据库记录
     *
     * @param userRole
     */
    void update(UserRole userRole);

    /**
     * 根据map查询
     * 
     * @param map
     * @return
     */
    UserRole getUserRole(Map<String, Object> map);
    
    /**
     * 根据map查询
     * 
     * @param map
     * @return
     */
    List<UserRole> listUserRole(Map<String, Object> map);
    
    /**
     * 批量保存
     * 
     * @param list
     */
    void batchInsert(List<UserRole> list);
    
    /**
     * 批量更新
     * 
     * @param list
     */
    void batchUpdate(List<UserRole> list);
    
}
