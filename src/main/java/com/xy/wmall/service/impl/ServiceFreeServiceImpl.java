package com.xy.wmall.service.impl;

import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.collections4.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.xy.wmall.enums.ErrorCodeEnum;
import com.xy.wmall.enums.TrueFalseStatusEnum;
import com.xy.wmall.common.Assert;
import com.xy.wmall.exception.WmallException;
import com.xy.wmall.mapper.ServiceFreeMapper;
import com.xy.wmall.model.ServiceFree;
import com.xy.wmall.service.ServiceFreeService;
import com.xy.wmall.common.utils.CommonUtils;
import com.xy.wmall.common.utils.ListPageUtils;

/**
 * Service 实现
 * 
 * @author admin
 * @date 2018年02月07日 下午02:27:32
 */
@Service
public class ServiceFreeServiceImpl implements ServiceFreeService {

    @Autowired
	private ServiceFreeMapper serviceFreeMapper;
	
	/**
     * 保存数据
     *
     * @param serviceFree
     * @throws WmallException
     */
    @Override
    public void save(ServiceFree serviceFree) {
    	Assert.notNull(serviceFree, "保存数据为空");
    	try {
			serviceFreeMapper.insert(serviceFree);
		} catch (Exception e) {
			throw new WmallException(ErrorCodeEnum.DB_INSERT_ERROR, "【" + serviceFree.toString() + "】保存失败", e);
		}
    }

    /**
     * 修改数据
     *
     * @param serviceFree
     * @throws WmallException
     */
    @Override
    public void update(ServiceFree serviceFree) {
    	Assert.notNull(serviceFree, "修改数据为空");
    	try {
    		serviceFreeMapper.update(serviceFree);
		} catch (Exception e) {
			throw new WmallException(ErrorCodeEnum.DB_UPDATE_ERROR, "【" + serviceFree.toString() + "】修改失败", e);
		}
    }
    
    /**
     * 删除数据
     * 
     * @param serviceFree
     * @throws WmallException
     */
    @Override
    public void remove(ServiceFree serviceFree) {
    	Assert.notNull(serviceFree, "删除数据为空");
		try {
    		ServiceFree deleteServiceFree = new ServiceFree();
    		deleteServiceFree.setId(serviceFree.getId());
    		deleteServiceFree.setIsDelete(TrueFalseStatusEnum.TRUE.getValue());
    		serviceFreeMapper.update(deleteServiceFree);
		} catch (Exception e) {
			throw new WmallException(ErrorCodeEnum.DB_DELETE_ERROR, "【" + serviceFree.toString() + "】删除失败", e);
    	}
    }
    
    /**
     * 根据ID查询
     *
     * @param id
     * @return
     * @throws WmallException
     */
    @Override
    public ServiceFree getServiceFreeById(Integer id) {
    	Assert.notNull(id, "id为空");
    	try {
    		Map<String, Object> map = CommonUtils.defaultQueryMap();
    		map.put("id", id);
	    	return serviceFreeMapper.getServiceFree(map);
		} catch (Exception e) {
			throw new WmallException(ErrorCodeEnum.DB_SELECT_ERROR, "【" + id + "】查询失败", e);
		}
    }
    
    /**
     * 根据map查询
     * 
     * @param map
     * @return
     * @throws WmallException
     */
    @Override
    public ServiceFree getServiceFree(Map<String, Object> map) {
    	Assert.notEmpty(map, "查询数据为空");
    	try {
	    	return serviceFreeMapper.getServiceFree(map);
		} catch (Exception e) {
			throw new WmallException(ErrorCodeEnum.DB_SELECT_ERROR, "【" + map + "】查询对象失败", e);
		}
    }
    
    /**
     * 根据map查询
     * 
     * @param map
     * @return
     * @throws WmallException
     */
    @Override
    public List<ServiceFree> listServiceFree(Map<String, Object> map) {
   	 	Assert.notEmpty(map, "查询数据为空");
    	try {
	    	return serviceFreeMapper.listServiceFree(map);
		} catch (Exception e) {
			throw new WmallException(ErrorCodeEnum.DB_SELECT_ERROR, "【" + map + "】查询列表失败", e);
		}
    }
    
    /**
     * 批量保存
     * 
     * @param list
     * @throws WmallException
     */
    @Override
    public void batchSave(List<ServiceFree> list) {
    	Assert.notEmpty(list, "批量保存数据为空");
    	try {
			List<List<ServiceFree>> pageList = ListPageUtils.listPage(list, 1000);
			for (List<ServiceFree> page : pageList) {
				serviceFreeMapper.batchInsert(page);
			}
		} catch (Exception e) {
			throw new WmallException(ErrorCodeEnum.DB_BATCH_ERROR, "批量保存失败", e);
		}
    }
    
    /**
     * 批量更新
     * 
     * @param list
     * @throws WmallException
     */
    @Override
    public void batchUpdate(List<ServiceFree> list) {
    	Assert.notEmpty(list, "批量修改数据为空");
    	try {
			List<List<ServiceFree>> pageList = ListPageUtils.listPage(list, 1000);
			for (List<ServiceFree> page : pageList) {
				serviceFreeMapper.batchUpdate(page);
			}
		} catch (Exception e) {
			throw new WmallException(ErrorCodeEnum.DB_BATCH_ERROR, "批量修改失败", e);
		}
    }
    
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
