package com.xy.wmall.service.impl;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.xy.wmall.common.Assert;
import com.xy.wmall.common.utils.ListPageUtils;
import com.xy.wmall.enums.ErrorCodeEnum;
import com.xy.wmall.enums.TrueFalseStatusEnum;
import com.xy.wmall.exception.WmallException;
import com.xy.wmall.mapper.RoleMapper;
import com.xy.wmall.model.Role;
import com.xy.wmall.service.RoleService;

/**
 * Service 实现
 * 
 * @author admin
 * @date 2018年01月26日 下午02:19:13
 */
@Service
public class RoleServiceImpl implements RoleService {

    @Autowired
	private RoleMapper roleMapper;
	
	/**
     * 根据主键查询
     *
     * @param id
     * @return
     * @throws WmallException
     */
    @Override
    public Role selectByPrimaryKey(Integer id) {
    	Assert.notNull(id, "id为空");
    	try {
	    	return roleMapper.selectByPrimaryKey(id);
		} catch (Exception e) {
			throw new WmallException(ErrorCodeEnum.DB_SELECT_ERROR, "【" + id + "】查询失败", e);
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
    public Role getRoleById(Integer id) {
    	Assert.notNull(id, "id为空");
    	try {
    		Map<String, Object> map = new HashMap<>(2);
    		map.put("id", id);
    		map.put("isDelete", TrueFalseStatusEnum.FALSE.getValue());
	    	return roleMapper.getRole(map);
		} catch (Exception e) {
			throw new WmallException(ErrorCodeEnum.DB_SELECT_ERROR, "【" + id + "】查询失败", e);
		}
    }
    
	/**
     * 保存数据
     *
     * @param role
     * @throws WmallException
     */
    @Override
    public void save(Role role) {
    	Assert.notNull(role, "保存数据为空");
    	try {
			roleMapper.insert(role);
		} catch (Exception e) {
			throw new WmallException(ErrorCodeEnum.DB_INSERT_ERROR, "【" + role.toString() + "】保存失败", e);
		}
    }

    /**
     * 修改数据
     *
     * @param role
     * @throws WmallException
     */
    @Override
    public void update(Role role) {
    	Assert.notNull(role, "修改数据为空");
    	try {
    		roleMapper.update(role);
		} catch (Exception e) {
			throw new WmallException(ErrorCodeEnum.DB_UPDATE_ERROR, "【" + role.toString() + "】修改失败", e);
		}
    }
    
    /**
     * 删除数据
     * 
     * @param role
     * @throws WmallException
     */
    @Override
    public void remove(Role role) {
    	Assert.notNull(role, "删除数据为空");
		try {
    		Role deleteRole = new Role();
    		deleteRole.setId(role.getId());
    		deleteRole.setIsDelete(TrueFalseStatusEnum.TRUE.getValue());
    		roleMapper.update(deleteRole);
		} catch (Exception e) {
			throw new WmallException(ErrorCodeEnum.DB_DELETE_ERROR, "【" + role.toString() + "】删除失败", e);
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
    public Role getRole(Map<String, Object> map) {
    	Assert.notEmpty(map, "查询数据为空");
    	try {
	    	return roleMapper.getRole(map);
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
    public List<Role> listRole(Map<String, Object> map) {
   	 	Assert.notEmpty(map, "查询数据为空");
    	try {
	    	return roleMapper.listRole(map);
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
    public void batchSave(List<Role> list) {
    	Assert.notEmpty(list, "批量保存数据为空");
    	try {
			List<List<Role>> pageList = ListPageUtils.listPage(list, 1000);
			for (List<Role> page : pageList) {
				roleMapper.batchInsert(page);
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
    public void batchUpdate(List<Role> list) {
    	Assert.notEmpty(list, "批量修改数据为空");
    	try {
			List<List<Role>> pageList = ListPageUtils.listPage(list, 1000);
			for (List<Role> page : pageList) {
				roleMapper.batchUpdate(page);
			}
		} catch (Exception e) {
			throw new WmallException(ErrorCodeEnum.DB_BATCH_ERROR, "批量修改失败", e);
		}
    }
    
}
