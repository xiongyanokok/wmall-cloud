package com.xy.wmall.service.impl;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.collections4.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.xy.wmall.common.Assert;
import com.xy.wmall.common.utils.CommonUtils;
import com.xy.wmall.enums.ErrorCodeEnum;
import com.xy.wmall.exception.WmallException;
import com.xy.wmall.mapper.ProxyLevelMapper;
import com.xy.wmall.model.ProxyLevel;
import com.xy.wmall.service.ProxyLevelService;

/**
 * Service 实现
 * 
 * @author admin
 * @date 2017年10月28日 上午08:54:24
 */
@Service
public class ProxyLevelServiceImpl extends BaseServiceImpl<ProxyLevelMapper, ProxyLevel> implements ProxyLevelService {

    @Autowired
	private ProxyLevelMapper proxyLevelMapper;
    
    /**
     * 查询代理级别
     * 
     * @param proxyIds
     * @return
     */
    @Override
    public Map<Integer, Integer> listLevelByProxy(List<Integer> proxyIds) {
    	Assert.notEmpty(proxyIds, "proxyIds为空");
    	try {
    		Map<String, Object> map = CommonUtils.defaultQueryMap();
    		map.put("proxyIds", proxyIds);
	    	List<ProxyLevel> proxyLevels = proxyLevelMapper.listLevelByProxy(map);
	    	if (CollectionUtils.isEmpty(proxyLevels)) {
	    		return Collections.emptyMap();
	    	}
	    	Map<Integer, Integer> proxyLevelMap = new HashMap<>(proxyLevels.size());
	    	for (ProxyLevel proxyLevel : proxyLevels) {
	    		proxyLevelMap.put(proxyLevel.getProxyId(), proxyLevel.getLevel());
	    	}
	    	return proxyLevelMap;
		} catch (Exception e) {
			throw new WmallException(ErrorCodeEnum.DB_SELECT_ERROR, "【" + proxyIds + "】查询代理级别失败", e);
		}
    }
    
    /**
     * 查询代理级别
     * 
     * @param userIds
     * @return
     */
    @Override
    public Map<Integer, Integer> listLevelByUser(List<Integer> userIds) {
    	Assert.notEmpty(userIds, "userIds为空");
    	try {
    		Map<String, Object> map = CommonUtils.defaultQueryMap();
    		map.put("userIds", userIds);
	    	List<ProxyLevel> proxyLevels = proxyLevelMapper.listLevelByUser(map);
	    	if (CollectionUtils.isEmpty(proxyLevels)) {
	    		return Collections.emptyMap();
	    	}
	    	Map<Integer, Integer> proxyLevelMap = new HashMap<>(proxyLevels.size());
	    	for (ProxyLevel proxyLevel : proxyLevels) {
	    		proxyLevelMap.put(proxyLevel.getProxyId(), proxyLevel.getLevel());
	    	}
	    	return proxyLevelMap;
		} catch (Exception e) {
			throw new WmallException(ErrorCodeEnum.DB_SELECT_ERROR, "【" + userIds + "】查询代理级别失败", e);
		}
    }
    
}
