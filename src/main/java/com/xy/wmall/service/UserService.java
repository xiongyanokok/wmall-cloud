package com.xy.wmall.service;

import java.util.List;
import java.util.Map;

import com.xy.wmall.model.User;

/**
 * Service 接口
 * 
 * @author admin
 * @date 2018年01月29日 下午02:43:39
 */
public interface UserService {

    /**
     * 保存数据
     *
     * @param user
     */
    void save(User user);

    /**
     * 修改数据
     *
     * @param user
     */
    void update(User user);
    
    /**
     * 删除数据
     * 
     * @param user
     */
    void remove(User user);
    
    /**
     * 根据ID查询
     *
     * @param id
     * @return
     */
    User getUserById(Integer id);
    
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
    void batchSave(List<User> list);
    
    /**
     * 批量更新
     * 
     * @param list
     */
    void batchUpdate(List<User> list);
    
    /**
     * 根据用户
     * 
     * @param username
     * @return
     */
    User getUserByUsername(String username);
    
    /**
     * 根据map查询
     * 
     * @param map
     * @return
     */
    List<User> listUserRole(Map<String, Object> map);
    
}
