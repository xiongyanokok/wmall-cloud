package com.xy.wmall.service;

import java.util.List;
import java.util.Map;

import com.xy.wmall.model.Role;

/**
 * Service 接口
 * 
 * @author admin
 * @date 2018年01月26日 下午02:19:13
 */
public interface RoleService {

	/**
     * 根据主键查询
     *
     * @param id
     * @return
     */
    Role selectByPrimaryKey(Integer id);
    
    /**
     * 根据ID查询
     *
     * @param id
     * @return
     */
    Role getRoleById(Integer id);
    
    /**
     * 保存数据
     *
     * @param role
     */
    void save(Role role);

    /**
     * 修改数据
     *
     * @param role
     */
    void update(Role role);
    
    /**
     * 删除数据
     * 
     * @param role
     */
    void remove(Role role);
    
    /**
     * 根据map查询
     * 
     * @param map
     * @return
     */
    Role getRole(Map<String, Object> map);
    
    /**
     * 根据map查询
     * 
     * @param map
     * @return
     */
    List<Role> listRole(Map<String, Object> map);
    
    /**
     * 批量保存
     * 
     * @param list
     */
    void batchSave(List<Role> list);
    
    /**
     * 批量更新
     * 
     * @param list
     */
    void batchUpdate(List<Role> list);
    
}
