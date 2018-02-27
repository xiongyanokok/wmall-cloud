package com.xy.wmall.mapper;

import java.util.List;

import com.xy.wmall.model.RoleMenu;

/**
 * Mapper
 * 
 * @author admin
 * @date 2018年01月29日 下午02:43:31
 */
public interface RoleMenuMapper extends BaseMapper<RoleMenu> {

    /**
     * 删除角色权限
     * 
     * @param roleId
     */
    void deleteByRoleId(Integer roleId);
    
    /**
     * 根据角色查询权限
     * 
     * @param roleId
     * @return
     */
    List<Integer> listMenuByRoleId(Integer roleId);
    
}
