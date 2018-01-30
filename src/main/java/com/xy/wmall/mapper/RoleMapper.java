package com.xy.wmall.mapper;

import java.util.List;
import java.util.Map;

import com.xy.wmall.model.Role;

/**
 * Mapper
 * 
 * @author admin
 * @date 2018年01月29日 下午02:43:26
 */
public interface RoleMapper {

	/**
     * 根据主键查询
     *
     * @param id
     * @return
     */
    Role selectByPrimaryKey(Integer id);

    /**
     * 新增数据库记录
     *
     * @param role
     */
    void insert(Role role);

    /**
     * 更新数据库记录
     *
     * @param role
     */
    void update(Role role);

    /**
     * 根据map查询
     * 
     * @param map
     * @return
     */
    Role getRole(Map<String, Object> map);
    
    /**
     * 根据map查询
     * 
     * @param map
     * @return
     */
    List<Role> listRole(Map<String, Object> map);
    
    /**
     * 批量保存
     * 
     * @param list
     */
    void batchInsert(List<Role> list);
    
    /**
     * 批量更新
     * 
     * @param list
     */
    void batchUpdate(List<Role> list);
    
}
