package com.xy.wmall.mapper;

import java.util.List;

import com.xy.wmall.model.Menu;

/**
 * Mapper
 * 
 * @author admin
 * @date 2018年01月29日 下午02:43:20
 */
public interface MenuMapper extends BaseMapper<Menu> {

    /**
     * 根据用户查询权限菜单
     * 
     * @param userId
     * @return
     */
    List<Menu> listMenuByUserId(Integer userId);
    
}
