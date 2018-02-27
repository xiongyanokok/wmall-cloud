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
public interface UserService extends BaseService<User> {

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
