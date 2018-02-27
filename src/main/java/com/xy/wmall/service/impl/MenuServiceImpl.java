package com.xy.wmall.service.impl;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.xy.wmall.common.Assert;
import com.xy.wmall.enums.ErrorCodeEnum;
import com.xy.wmall.exception.WmallException;
import com.xy.wmall.mapper.MenuMapper;
import com.xy.wmall.model.Menu;
import com.xy.wmall.service.MenuService;

/**
 * Service 实现
 * 
 * @author admin
 * @date 2018年01月29日 下午02:43:20
 */
@Service
public class MenuServiceImpl extends BaseServiceImpl<MenuMapper, Menu> implements MenuService {

    @Autowired
	private MenuMapper menuMapper;
	
    /**
     * 根据用户查询权限菜单
     * 
     * @param userId
     * @return
     */
    @Override
    public List<Menu> listMenuByUserId(Integer userId) {
    	Assert.notNull(userId, "userId为空");
    	try {
	    	return menuMapper.listMenuByUserId(userId);
		} catch (Exception e) {
			throw new WmallException(ErrorCodeEnum.DB_SELECT_ERROR, "【" + userId + "】根据用户查询权限菜单失败", e);
		}
    }
    
}
