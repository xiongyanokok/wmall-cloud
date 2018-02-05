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
import com.xy.wmall.common.utils.ListPageUtils;
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
public class UserProxyServiceImpl implements UserProxyService {

    @Autowired
	private UserProxyMapper userProxyMapper;
	
	/**
     * 根据主键查询
     *
     * @param id
     * @return
     * @throws WmallException
     */
    @Override
    public UserProxy selectByPrimaryKey(Integer id) {
    	Assert.notNull(id, "id为空");
    	try {
	    	return userProxyMapper.selectByPrimaryKey(id);
		} catch (Exception e) {
			throw new WmallException(ErrorCodeEnum.DB_SELECT_ERROR, "【" + id + "】查询失败", e);
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
    public UserProxy getUserProxyById(Integer id) {
    	Assert.notNull(id, "id为空");
    	try {
    		Map<String, Object> map = CommonUtils.defaultQueryMap();
    		map.put("id", id);
	    	return userProxyMapper.getUserProxy(map);
		} catch (Exception e) {
			throw new WmallException(ErrorCodeEnum.DB_SELECT_ERROR, "【" + id + "】查询失败", e);
		}
    }
    
	/**
     * 保存数据
     *
     * @param userProxy
     * @throws WmallException
     */
    @Override
    public void save(UserProxy userProxy) {
    	Assert.notNull(userProxy, "保存数据为空");
    	try {
			userProxyMapper.insert(userProxy);
		} catch (Exception e) {
			throw new WmallException(ErrorCodeEnum.DB_INSERT_ERROR, "【" + userProxy.toString() + "】保存失败", e);
		}
    }

    /**
     * 修改数据
     *
     * @param userProxy
     * @throws WmallException
     */
    @Override
    public void update(UserProxy userProxy) {
    	Assert.notNull(userProxy, "修改数据为空");
    	try {
    		userProxyMapper.update(userProxy);
		} catch (Exception e) {
			throw new WmallException(ErrorCodeEnum.DB_UPDATE_ERROR, "【" + userProxy.toString() + "】修改失败", e);
		}
    }
    
    /**
     * 删除数据
     * 
     * @param userProxy
     * @throws WmallException
     */
    @Override
    public void remove(UserProxy userProxy) {
    	Assert.notNull(userProxy, "删除数据为空");
		try {
    		UserProxy deleteUserProxy = new UserProxy();
    		deleteUserProxy.setId(userProxy.getId());
    		userProxyMapper.update(deleteUserProxy);
		} catch (Exception e) {
			throw new WmallException(ErrorCodeEnum.DB_DELETE_ERROR, "【" + userProxy.toString() + "】删除失败", e);
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
    public UserProxy getUserProxy(Map<String, Object> map) {
    	Assert.notEmpty(map, "查询数据为空");
    	try {
	    	return userProxyMapper.getUserProxy(map);
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
    public List<UserProxy> listUserProxy(Map<String, Object> map) {
   	 	Assert.notEmpty(map, "查询数据为空");
    	try {
	    	return userProxyMapper.listUserProxy(map);
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
    public void batchSave(List<UserProxy> list) {
    	Assert.notEmpty(list, "批量保存数据为空");
    	try {
			List<List<UserProxy>> pageList = ListPageUtils.listPage(list, 1000);
			for (List<UserProxy> page : pageList) {
				userProxyMapper.batchInsert(page);
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
    public void batchUpdate(List<UserProxy> list) {
    	Assert.notEmpty(list, "批量修改数据为空");
    	try {
			List<List<UserProxy>> pageList = ListPageUtils.listPage(list, 1000);
			for (List<UserProxy> page : pageList) {
				userProxyMapper.batchUpdate(page);
			}
		} catch (Exception e) {
			throw new WmallException(ErrorCodeEnum.DB_BATCH_ERROR, "批量修改失败", e);
		}
    }
    
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
	    	List<UserProxy> userProxies = userProxyMapper.listUserProxy(map);
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
