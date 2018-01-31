package com.xy.wmall.mapper;

import java.util.List;
import java.util.Map;

import com.xy.wmall.model.User;

/**
 * Mapper
 * 
 * @author admin
 * @date 2018年01月29日 下午02:43:39
 */
public interface UserMapper {

	/**
     * 根据主键查询
     *
     * @param id
     * @return
     */
    User selectByPrimaryKey(Integer id);

    /**
     * 新增数据库记录
     *
     * @param user
     */
    void insert(User user);

    /**
     * 更新数据库记录
     *
     * @param user
     */
    void update(User user);

    /**
     * 根据map查询
     * 
     * @param map
     * @return
     */
    User getUser(Map<String, Object> map);
    
    /**
     * 根据map查询
     * 
     * @param map
     * @return
     */
    List<User> listUser(Map<String, Object> map);
    
    /**
     * 批量保存
     * 
     * @param list
     */
    void batchInsert(List<User> list);
    
    /**
     * 批量更新
     * 
     * @param list
     */
    void batchUpdate(List<User> list);
    
    /**
     * 根据map查询
     * 
     * @param map
     * @return
     */
    List<User> listUserRole(Map<String, Object> map);
    
}
