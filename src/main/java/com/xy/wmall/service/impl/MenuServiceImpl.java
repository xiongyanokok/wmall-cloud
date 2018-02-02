package com.xy.wmall.service.impl;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.xy.wmall.enums.ErrorCodeEnum;
import com.xy.wmall.enums.TrueFalseStatusEnum;
import com.xy.wmall.common.Assert;
import com.xy.wmall.exception.WmallException;
import com.xy.wmall.mapper.MenuMapper;
import com.xy.wmall.model.Menu;
import com.xy.wmall.service.MenuService;
import com.xy.wmall.common.utils.ListPageUtils;

/**
 * Service 实现
 * 
 * @author admin
 * @date 2018年01月29日 下午02:43:20
 */
@Service
public class MenuServiceImpl implements MenuService {

    @Autowired
	private MenuMapper menuMapper;
	
	/**
     * 根据主键查询
     *
     * @param id
     * @return
     * @throws WmallException
     */
    @Override
    public Menu selectByPrimaryKey(Integer id) {
    	Assert.notNull(id, "id为空");
    	try {
	    	return menuMapper.selectByPrimaryKey(id);
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
    public Menu getMenuById(Integer id) {
    	Assert.notNull(id, "id为空");
    	try {
    		Map<String, Object> map = new HashMap<>(2);
    		map.put("id", id);
    		map.put("isDelete", TrueFalseStatusEnum.FALSE.getValue());
	    	return menuMapper.getMenu(map);
		} catch (Exception e) {
			throw new WmallException(ErrorCodeEnum.DB_SELECT_ERROR, "【" + id + "】查询失败", e);
		}
    }
    
	/**
     * 保存数据
     *
     * @param menu
     * @throws WmallException
     */
    @Override
    public void save(Menu menu) {
    	Assert.notNull(menu, "保存数据为空");
    	try {
			menuMapper.insert(menu);
		} catch (Exception e) {
			throw new WmallException(ErrorCodeEnum.DB_INSERT_ERROR, "【" + menu.toString() + "】保存失败", e);
		}
    }

    /**
     * 修改数据
     *
     * @param menu
     * @throws WmallException
     */
    @Override
    public void update(Menu menu) {
    	Assert.notNull(menu, "修改数据为空");
    	try {
    		menuMapper.update(menu);
		} catch (Exception e) {
			throw new WmallException(ErrorCodeEnum.DB_UPDATE_ERROR, "【" + menu.toString() + "】修改失败", e);
		}
    }
    
    /**
     * 删除数据
     * 
     * @param menu
     * @throws WmallException
     */
    @Override
    public void remove(Menu menu) {
    	Assert.notNull(menu, "删除数据为空");
		try {
    		Menu deleteMenu = new Menu();
    		deleteMenu.setId(menu.getId());
    		deleteMenu.setIsDelete(TrueFalseStatusEnum.TRUE.getValue());
    		menuMapper.update(deleteMenu);
		} catch (Exception e) {
			throw new WmallException(ErrorCodeEnum.DB_DELETE_ERROR, "【" + menu.toString() + "】删除失败", e);
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
    public Menu getMenu(Map<String, Object> map) {
    	Assert.notEmpty(map, "查询数据为空");
    	try {
	    	return menuMapper.getMenu(map);
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
    public List<Menu> listMenu(Map<String, Object> map) {
   	 	Assert.notEmpty(map, "查询数据为空");
    	try {
	    	return menuMapper.listMenu(map);
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
    public void batchSave(List<Menu> list) {
    	Assert.notEmpty(list, "批量保存数据为空");
    	try {
			List<List<Menu>> pageList = ListPageUtils.listPage(list, 1000);
			for (List<Menu> page : pageList) {
				menuMapper.batchInsert(page);
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
    public void batchUpdate(List<Menu> list) {
    	Assert.notEmpty(list, "批量修改数据为空");
    	try {
			List<List<Menu>> pageList = ListPageUtils.listPage(list, 1000);
			for (List<Menu> page : pageList) {
				menuMapper.batchUpdate(page);
			}
		} catch (Exception e) {
			throw new WmallException(ErrorCodeEnum.DB_BATCH_ERROR, "批量修改失败", e);
		}
    }
    
    /**
     * 根据用户查询权限菜单
     * 
     * @param userId
     * @return
     */
    @Override
    public List<Menu> listMenuByUser(Integer userId) {
    	Assert.notNull(userId, "userId为空");
    	try {
	    	return menuMapper.listMenuByUser(userId);
		} catch (Exception e) {
			throw new WmallException(ErrorCodeEnum.DB_SELECT_ERROR, "【" + userId + "】根据用户查询权限菜单失败", e);
		}
    }
    
}
