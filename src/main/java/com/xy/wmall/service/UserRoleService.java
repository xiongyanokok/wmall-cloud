package com.xy.wmall.service;

import com.xy.wmall.model.UserRole;

/**
 * Service 接口
 * 
 * @author admin
 * @date 2018年01月29日 下午02:43:36
 */
public interface UserRoleService extends BaseService<UserRole> {

    /**
     * 根据用户查询角色
     * 
     * @param userId
     * @return
     */
    UserRole getRoleByUserId(Integer userId);
    
}
