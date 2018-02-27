package com.xy.wmall.service.impl;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.xy.wmall.common.Assert;
import com.xy.wmall.enums.ErrorCodeEnum;
import com.xy.wmall.exception.WmallException;
import com.xy.wmall.mapper.UserRoleMapper;
import com.xy.wmall.model.UserRole;
import com.xy.wmall.service.UserRoleService;

/**
 * Service 实现
 * 
 * @author admin
 * @date 2018年01月29日 下午02:43:36
 */
@Service
public class UserRoleServiceImpl extends BaseServiceImpl<UserRoleMapper, UserRole> implements UserRoleService {

    @Autowired
	private UserRoleMapper userRoleMapper;
	
    /**
     * 根据用户查询角色
     * 
     * @param userId
     * @return
     */
    @Override
    public UserRole getRoleByUserId(Integer userId) {
    	Assert.notNull(userId, "userId为空");
    	try {
	    	return userRoleMapper.getRoleByUserId(userId);
		} catch (Exception e) {
			throw new WmallException(ErrorCodeEnum.DB_SELECT_ERROR, "【" + userId + "】根据用户查询角色失败", e);
		}
    }
    
}
