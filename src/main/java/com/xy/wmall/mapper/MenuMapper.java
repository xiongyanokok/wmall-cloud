package com.xy.wmall.mapper;

import java.util.List;
import java.util.Map;

import com.xy.wmall.model.Menu;

/**
 * Mapper
 * 
 * @author admin
 * @date 2018年01月26日 下午02:19:01
 */
public interface MenuMapper {

	/**
     * 根据主键查询
     *
     * @param id
     * @return
     */
    Menu selectByPrimaryKey(Integer id);

    /**
     * 新增数据库记录
     *
     * @param menu
     */
    void insert(Menu menu);

    /**
     * 更新数据库记录
     *
     * @param menu
     */
    void update(Menu menu);

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
    void batchInsert(List<Menu> list);
    
    /**
     * 批量更新
     * 
     * @param list
     */
    void batchUpdate(List<Menu> list);
    
}