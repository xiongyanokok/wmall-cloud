package com.xy.wmall.service;

import java.util.List;

import com.xy.wmall.model.RoleMenu;

/**
 * Service 接口
 * 
 * @author admin
 * @date 2018年01月29日 下午02:43:31
 */
public interface RoleMenuService extends BaseService<RoleMenu> {

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
