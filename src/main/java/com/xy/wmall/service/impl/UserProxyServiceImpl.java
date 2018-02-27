package com.xy.wmall.service.impl;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.collections4.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.xy.wmall.common.Assert;
import com.xy.wmall.enums.ErrorCodeEnum;
import com.xy.wmall.exception.WmallException;
import com.xy.wmall.mapper.UserProxyMapper;
import com.xy.wmall.model.UserProxy;
import com.xy.wmall.service.UserProxyService;

/**
 * Service 实现
 * 
 * @author admin
 * @date 2018年01月30日 下午02:32:17
 */
@Service
public class UserProxyServiceImpl extends BaseServiceImpl<UserProxyMapper, UserProxy> implements UserProxyService {

    @Autowired
	private UserProxyMapper userProxyMapper;
	
    /**
     * 查询代理用户
     * 
     * @param proxyIds
     * @return
     */
    @Override
    public Map<Integer, Integer> listUserByProxy(List<Integer> proxyIds) {
    	Assert.notEmpty(proxyIds, "proxyIds为空");
    	try {
    		Map<String, Object> map = new HashMap<>(1);
    		map.put("proxyIds", proxyIds);
	    	List<UserProxy> userProxies = userProxyMapper.listByMap(map);
	    	if (CollectionUtils.isEmpty(userProxies)) {
	    		return Collections.emptyMap();
	    	}
	    	Map<Integer, Integer> userProxyMap = new HashMap<>(userProxies.size());
	    	for (UserProxy userProxy : userProxies) {
	    		userProxyMap.put(userProxy.getProxyId(), userProxy.getUserId());
	    	}
	    	return userProxyMap;
		} catch (Exception e) {
			throw new WmallException(ErrorCodeEnum.DB_SELECT_ERROR, "【" + proxyIds + "】查询代理用户失败", e);
		}
    }
    
}
