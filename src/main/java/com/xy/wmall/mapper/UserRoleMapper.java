package com.xy.wmall.mapper;

import com.xy.wmall.model.UserRole;

/**
 * Mapper
 * 
 * @author admin
 * @date 2018年01月29日 下午02:43:36
 */
public interface UserRoleMapper extends BaseMapper<UserRole> {

    /**
     * 根据用户查询角色
     * 
     * @param userId
     * @return
     */
    UserRole getRoleByUserId(Integer userId);
    
}
