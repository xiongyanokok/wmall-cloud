package com.xy.wmall.service.impl;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.xy.wmall.common.Assert;
import com.xy.wmall.enums.ErrorCodeEnum;
import com.xy.wmall.exception.WmallException;
import com.xy.wmall.mapper.RoleMenuMapper;
import com.xy.wmall.model.RoleMenu;
import com.xy.wmall.service.RoleMenuService;

/**
 * Service 实现
 * 
 * @author admin
 * @date 2018年01月29日 下午02:43:31
 */
@Service
public class RoleMenuServiceImpl extends BaseServiceImpl<RoleMenuMapper, RoleMenu> implements RoleMenuService {

    @Autowired
	private RoleMenuMapper roleMenuMapper;
    
    /**
     * 删除角色权限
     * 
     * @param roleId
     */
    @Override
    public void deleteByRoleId(Integer roleId) {
    	Assert.notNull(roleId, "roleId为空");
    	try {
	    	roleMenuMapper.deleteByRoleId(roleId);
		} catch (Exception e) {
			throw new WmallException(ErrorCodeEnum.DB_DELETE_ERROR, "【" + roleId + "】删除角色权限失败", e);
		}
    }
    
    /**
     * 根据角色查询权限
     * 
     * @param roleId
     * @return
     */
    @Override
    public List<Integer> listMenuByRoleId(Integer roleId) {
    	Assert.notNull(roleId, "roleId为空");
    	try {
	    	return roleMenuMapper.listMenuByRoleId(roleId);
		} catch (Exception e) {
			throw new WmallException(ErrorCodeEnum.DB_SELECT_ERROR, "【" + roleId + "】根据角色查询权限失败", e);
		}
    }
}
