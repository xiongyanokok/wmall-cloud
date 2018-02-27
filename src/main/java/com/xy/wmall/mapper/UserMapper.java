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
public interface UserMapper extends BaseMapper<User> {

    /**
     * 根据map查询
     * 
     * @param map
     * @return
     */
    List<User> listUserRole(Map<String, Object> map);
    
}
