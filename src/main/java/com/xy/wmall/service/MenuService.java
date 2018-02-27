package com.xy.wmall.service;

import java.util.List;

import com.xy.wmall.model.Menu;

/**
 * Service 接口
 * 
 * @author admin
 * @date 2018年01月29日 下午02:43:20
 */
public interface MenuService extends BaseService<Menu> {

    /**
     * 根据用户查询权限菜单
     * 
     * @param userId
     * @return
     */
    List<Menu> listMenuByUserId(Integer userId);
    
}
