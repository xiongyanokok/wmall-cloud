package com.xy.wmall.service.impl;

import java.util.Collections;
import java.util.Date;
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
import com.xy.wmall.mapper.ServiceFreeMapper;
import com.xy.wmall.model.ServiceFree;
import com.xy.wmall.service.ServiceFreeService;

/**
 * Service 实现
 * 
 * @author admin
 * @date 2018年02月07日 下午02:27:32
 */
@Service
public class ServiceFreeServiceImpl extends BaseServiceImpl<ServiceFreeMapper, ServiceFree> implements ServiceFreeService {

    @Autowired
	private ServiceFreeMapper serviceFreeMapper;
    
    /**
     * 用户服务有效期
     * 
     * @param userIds
     * @return
     */
    @Override
    public Map<Integer, Date> listServiceDate(List<Integer> userIds){
    	Assert.notEmpty(userIds, "userIds为空");
    	try {
    		Map<String, Object> map = CommonUtils.defaultQueryMap();
    		map.put("userIds", userIds);
    		List<ServiceFree> serviceFrees = serviceFreeMapper.listServiceDate(map);
    		if (CollectionUtils.isEmpty(serviceFrees)) {
    			return Collections.emptyMap();
    		}
    		Map<Integer, Date> userServiceMap = new HashMap<>(serviceFrees.size());
    		for (ServiceFree serviceFree : serviceFrees) {
    			userServiceMap.put(serviceFree.getUserId(), serviceFree.getEndDate());
    		}
    		return userServiceMap;
		} catch (Exception e) {
			throw new WmallException(ErrorCodeEnum.DB_SELECT_ERROR, "【" + userIds + "】查询用户服务有效期失败", e);
		}
    }
    
}
