package com.xy.wmall.service;

import java.util.List;
import java.util.Map;

import com.xy.wmall.model.UserRole;

/**
 * Service 接口
 * 
 * @author admin
 * @date 2018年01月29日 下午02:43:36
 */
public interface UserRoleService {

	/**
     * 根据主键查询
     *
     * @param id
     * @return
     */
    UserRole selectByPrimaryKey(Integer id);
    
    /**
     * 根据ID查询
     *
     * @param id
     * @return
     */
    UserRole getUserRoleById(Integer id);
    
    /**
     * 保存数据
     *
     * @param userRole
     */
    void save(UserRole userRole);

    /**
     * 修改数据
     *
     * @param userRole
     */
    void update(UserRole userRole);
    
    /**
     * 删除数据
     * 
     * @param userRole
     */
    void remove(UserRole userRole);
    
    /**
     * 根据map查询
     * 
     * @param map
     * @return
     */
    UserRole getUserRole(Map<String, Object> map);
    
    /**
     * 根据map查询
     * 
     * @param map
     * @return
     */
    List<UserRole> listUserRole(Map<String, Object> map);
    
    /**
     * 批量保存
     * 
     * @param list
     */
    void batchSave(List<UserRole> list);
    
    /**
     * 批量更新
     * 
     * @param list
     */
    void batchUpdate(List<UserRole> list);
    
}
