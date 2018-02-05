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
public class RoleMenuServiceImpl implements RoleMenuService {

    @Autowired
	private RoleMenuMapper roleMenuMapper;
	
	/**
     * 根据主键查询
     *
     * @param id
     * @return
     * @throws WmallException
     */
    @Override
    public RoleMenu selectByPrimaryKey(Integer id) {
    	Assert.notNull(id, "id为空");
    	try {
	    	return roleMenuMapper.selectByPrimaryKey(id);
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
    public RoleMenu getRoleMenuById(Integer id) {
    	Assert.notNull(id, "id为空");
    	try {
    		Map<String, Object> map = CommonUtils.defaultQueryMap();
    		map.put("id", id);
	    	return roleMenuMapper.getRoleMenu(map);
		} catch (Exception e) {
			throw new WmallException(ErrorCodeEnum.DB_SELECT_ERROR, "【" + id + "】查询失败", e);
		}
    }
    
	/**
     * 保存数据
     *
     * @param roleMenu
     * @throws WmallException
     */
    @Override
    public void save(RoleMenu roleMenu) {
    	Assert.notNull(roleMenu, "保存数据为空");
    	try {
			roleMenuMapper.insert(roleMenu);
		} catch (Exception e) {
			throw new WmallException(ErrorCodeEnum.DB_INSERT_ERROR, "【" + roleMenu.toString() + "】保存失败", e);
		}
    }

    /**
     * 修改数据
     *
     * @param roleMenu
     * @throws WmallException
     */
    @Override
    public void update(RoleMenu roleMenu) {
    	Assert.notNull(roleMenu, "修改数据为空");
    	try {
    		roleMenuMapper.update(roleMenu);
		} catch (Exception e) {
			throw new WmallException(ErrorCodeEnum.DB_UPDATE_ERROR, "【" + roleMenu.toString() + "】修改失败", e);
		}
    }
    
    /**
     * 删除数据
     * 
     * @param roleMenu
     * @throws WmallException
     */
    @Override
    public void remove(RoleMenu roleMenu) {
    	Assert.notNull(roleMenu, "删除数据为空");
		try {
    		RoleMenu deleteRoleMenu = new RoleMenu();
    		deleteRoleMenu.setId(roleMenu.getId());
    		roleMenuMapper.update(deleteRoleMenu);
		} catch (Exception e) {
			throw new WmallException(ErrorCodeEnum.DB_DELETE_ERROR, "【" + roleMenu.toString() + "】删除失败", e);
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
    public RoleMenu getRoleMenu(Map<String, Object> map) {
    	Assert.notEmpty(map, "查询数据为空");
    	try {
	    	return roleMenuMapper.getRoleMenu(map);
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
    public List<RoleMenu> listRoleMenu(Map<String, Object> map) {
   	 	Assert.notEmpty(map, "查询数据为空");
    	try {
	    	return roleMenuMapper.listRoleMenu(map);
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
    public void batchSave(List<RoleMenu> list) {
    	Assert.notEmpty(list, "批量保存数据为空");
    	try {
			List<List<RoleMenu>> pageList = ListPageUtils.listPage(list, 1000);
			for (List<RoleMenu> page : pageList) {
				roleMenuMapper.batchInsert(page);
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
    public void batchUpdate(List<RoleMenu> list) {
    	Assert.notEmpty(list, "批量修改数据为空");
    	try {
			List<List<RoleMenu>> pageList = ListPageUtils.listPage(list, 1000);
			for (List<RoleMenu> page : pageList) {
				roleMenuMapper.batchUpdate(page);
			}
		} catch (Exception e) {
			throw new WmallException(ErrorCodeEnum.DB_BATCH_ERROR, "批量修改失败", e);
		}
    }
    
    /**
     * 删除角色权限
     * 
     * @param roleId
     */
    @Override
    public void delete(Integer roleId) {
    	Assert.notNull(roleId, "roleId为空");
    	try {
	    	roleMenuMapper.delete(roleId);
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
    public List<Integer> listMenuByRole(Integer roleId) {
    	Assert.notNull(roleId, "roleId为空");
    	try {
	    	return roleMenuMapper.listMenuByRole(roleId);
		} catch (Exception e) {
			throw new WmallException(ErrorCodeEnum.DB_SELECT_ERROR, "【" + roleId + "】根据角色查询权限失败", e);
		}
    }
}
