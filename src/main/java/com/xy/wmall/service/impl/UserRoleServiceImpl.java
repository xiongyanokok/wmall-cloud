package com.xy.wmall.service.impl;

import java.util.List;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.xy.wmall.common.Assert;
import com.xy.wmall.common.utils.CommonUtils;
import com.xy.wmall.common.utils.ListPageUtils;
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
public class UserRoleServiceImpl implements UserRoleService {

    @Autowired
	private UserRoleMapper userRoleMapper;
	
	/**
     * 保存数据
     *
     * @param userRole
     * @throws WmallException
     */
    @Override
    public void save(UserRole userRole) {
    	Assert.notNull(userRole, "保存数据为空");
    	try {
			userRoleMapper.insert(userRole);
		} catch (Exception e) {
			throw new WmallException(ErrorCodeEnum.DB_INSERT_ERROR, "【" + userRole.toString() + "】保存失败", e);
		}
    }

    /**
     * 修改数据
     *
     * @param userRole
     * @throws WmallException
     */
    @Override
    public void update(UserRole userRole) {
    	Assert.notNull(userRole, "修改数据为空");
    	try {
    		userRoleMapper.update(userRole);
		} catch (Exception e) {
			throw new WmallException(ErrorCodeEnum.DB_UPDATE_ERROR, "【" + userRole.toString() + "】修改失败", e);
		}
    }
    
    /**
     * 删除数据
     * 
     * @param userRole
     * @throws WmallException
     */
    @Override
    public void remove(UserRole userRole) {
    	Assert.notNull(userRole, "删除数据为空");
		try {
    		UserRole deleteUserRole = new UserRole();
    		deleteUserRole.setId(userRole.getId());
    		userRoleMapper.update(deleteUserRole);
		} catch (Exception e) {
			throw new WmallException(ErrorCodeEnum.DB_DELETE_ERROR, "【" + userRole.toString() + "】删除失败", e);
    	}
    }
    
    /**
     * 根据ID查询
     *
     * @param id
     * @return
     * @throws WmallException
     */
    @Override
    public UserRole getUserRoleById(Integer id) {
    	Assert.notNull(id, "id为空");
    	try {
    		Map<String, Object> map = CommonUtils.defaultQueryMap();
    		map.put("id", id);
	    	return userRoleMapper.getUserRole(map);
		} catch (Exception e) {
			throw new WmallException(ErrorCodeEnum.DB_SELECT_ERROR, "【" + id + "】查询失败", e);
		}
    }
    
    /**
     * 根据map查询
     * 
     * @param map
     * @return
     * @throws WmallException
     */
    @Override
    public UserRole getUserRole(Map<String, Object> map) {
    	Assert.notEmpty(map, "查询数据为空");
    	try {
	    	return userRoleMapper.getUserRole(map);
		} catch (Exception e) {
			throw new WmallException(ErrorCodeEnum.DB_SELECT_ERROR, "【" + map + "】查询对象失败", e);
		}
    }
    
    /**
     * 根据map查询
     * 
     * @param map
     * @return
     * @throws WmallException
     */
    @Override
    public List<UserRole> listUserRole(Map<String, Object> map) {
   	 	Assert.notEmpty(map, "查询数据为空");
    	try {
	    	return userRoleMapper.listUserRole(map);
		} catch (Exception e) {
			throw new WmallException(ErrorCodeEnum.DB_SELECT_ERROR, "【" + map + "】查询列表失败", e);
		}
    }
    
    /**
     * 批量保存
     * 
     * @param list
     * @throws WmallException
     */
    @Override
    public void batchSave(List<UserRole> list) {
    	Assert.notEmpty(list, "批量保存数据为空");
    	try {
			List<List<UserRole>> pageList = ListPageUtils.listPage(list, 1000);
			for (List<UserRole> page : pageList) {
				userRoleMapper.batchInsert(page);
			}
		} catch (Exception e) {
			throw new WmallException(ErrorCodeEnum.DB_BATCH_ERROR, "批量保存失败", e);
		}
    }
    
    /**
     * 批量更新
     * 
     * @param list
     * @throws WmallException
     */
    @Override
    public void batchUpdate(List<UserRole> list) {
    	Assert.notEmpty(list, "批量修改数据为空");
    	try {
			List<List<UserRole>> pageList = ListPageUtils.listPage(list, 1000);
			for (List<UserRole> page : pageList) {
				userRoleMapper.batchUpdate(page);
			}
		} catch (Exception e) {
			throw new WmallException(ErrorCodeEnum.DB_BATCH_ERROR, "批量修改失败", e);
		}
    }
    
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
