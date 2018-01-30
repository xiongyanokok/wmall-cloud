package com.xy.wmall.service;

import java.util.List;
import java.util.Map;

import com.xy.wmall.model.Menu;

/**
 * Service 接口
 * 
 * @author admin
 * @date 2018年01月29日 下午02:43:20
 */
public interface MenuService {

	/**
     * 根据主键查询
     *
     * @param id
     * @return
     */
    Menu selectByPrimaryKey(Integer id);
    
    /**
     * 根据ID查询
     *
     * @param id
     * @return
     */
    Menu getMenuById(Integer id);
    
    /**
     * 保存数据
     *
     * @param menu
     */
    void save(Menu menu);

    /**
     * 修改数据
     *
     * @param menu
     */
    void update(Menu menu);
    
    /**
     * 删除数据
     * 
     * @param menu
     */
    void remove(Menu menu);
    
    /**
     * 根据map查询
     * 
     * @param map
     * @return
     */
    Menu getMenu(Map<String, Object> map);
    
    /**
     * 根据map查询
     * 
     * @param map
     * @return
     */
    List<Menu> listMenu(Map<String, Object> map);
    
    /**
     * 批量保存
     * 
     * @param list
     */
    void batchSave(List<Menu> list);
    
    /**
     * 批量更新
     * 
     * @param list
     */
    void batchUpdate(List<Menu> list);
    
    /**
     * 根据map查询
     * 
     * @param map
     * @return
     */
    List<Menu> listUserRoleMenu(Map<String, Object> map);
    
}
