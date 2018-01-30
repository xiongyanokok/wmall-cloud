package com.xy.wmall.mapper;

import java.util.List;
import java.util.Map;

import com.xy.wmall.model.RoleMenu;

/**
 * Mapper
 * 
 * @author admin
 * @date 2018年01月29日 下午02:43:31
 */
public interface RoleMenuMapper {

	/**
     * 根据主键查询
     *
     * @param id
     * @return
     */
    RoleMenu selectByPrimaryKey(Integer id);

    /**
     * 新增数据库记录
     *
     * @param roleMenu
     */
    void insert(RoleMenu roleMenu);

    /**
     * 更新数据库记录
     *
     * @param roleMenu
     */
    void update(RoleMenu roleMenu);

    /**
     * 根据map查询
     * 
     * @param map
     * @return
     */
    RoleMenu getRoleMenu(Map<String, Object> map);
    
    /**
     * 根据map查询
     * 
     * @param map
     * @return
     */
    List<RoleMenu> listRoleMenu(Map<String, Object> map);
    
    /**
     * 批量保存
     * 
     * @param list
     */
    void batchInsert(List<RoleMenu> list);
    
    /**
     * 批量更新
     * 
     * @param list
     */
    void batchUpdate(List<RoleMenu> list);
    
}
