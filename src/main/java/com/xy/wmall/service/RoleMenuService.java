package com.xy.wmall.service;

import java.util.List;
import java.util.Map;

import com.xy.wmall.model.RoleMenu;

/**
 * Service 接口
 * 
 * @author admin
 * @date 2018年01月29日 下午02:43:31
 */
public interface RoleMenuService {

	/**
     * 根据主键查询
     *
     * @param id
     * @return
     */
    RoleMenu selectByPrimaryKey(Integer id);
    
    /**
     * 根据ID查询
     *
     * @param id
     * @return
     */
    RoleMenu getRoleMenuById(Integer id);
    
    /**
     * 保存数据
     *
     * @param roleMenu
     */
    void save(RoleMenu roleMenu);

    /**
     * 修改数据
     *
     * @param roleMenu
     */
    void update(RoleMenu roleMenu);
    
    /**
     * 删除数据
     * 
     * @param roleMenu
     */
    void remove(RoleMenu roleMenu);
    
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
    void batchSave(List<RoleMenu> list);
    
    /**
     * 批量更新
     * 
     * @param list
     */
    void batchUpdate(List<RoleMenu> list);
    
    /**
     * 删除角色权限
     * 
     * @param roleId
     */
    void delete(Integer roleId);
    
}
