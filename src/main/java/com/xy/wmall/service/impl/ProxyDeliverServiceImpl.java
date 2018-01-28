package com.xy.wmall.service.impl;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.xy.wmall.enums.ErrorCodeEnum;
import com.xy.wmall.enums.TrueFalseStatusEnum;
import com.xy.wmall.common.Assert;
import com.xy.wmall.exception.WmallException;
import com.xy.wmall.mapper.ProxyDeliverMapper;
import com.xy.wmall.model.ProxyDeliver;
import com.xy.wmall.service.ProxyDeliverService;
import com.xy.wmall.common.utils.ListPageUtils;

/**
 * Service 实现
 * 
 * @author admin
 * @date 2018年01月28日 上午11:34:02
 */
@Service
public class ProxyDeliverServiceImpl implements ProxyDeliverService {

    @Autowired
	private ProxyDeliverMapper proxyDeliverMapper;
	
	/**
     * 根据主键查询
     *
     * @param id
     * @return
     * @throws WmallException
     */
    @Override
    public ProxyDeliver selectByPrimaryKey(Integer id) {
    	Assert.notNull(id, "id为空");
    	try {
	    	return proxyDeliverMapper.selectByPrimaryKey(id);
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
    public ProxyDeliver getProxyDeliverById(Integer id) {
    	Assert.notNull(id, "id为空");
    	try {
    		Map<String, Object> map = new HashMap<>(2);
    		map.put("id", id);
    		map.put("isDelete", TrueFalseStatusEnum.FALSE.getValue());
	    	return proxyDeliverMapper.getProxyDeliver(map);
		} catch (Exception e) {
			throw new WmallException(ErrorCodeEnum.DB_SELECT_ERROR, "【" + id + "】查询失败", e);
		}
    }
    
	/**
     * 保存数据
     *
     * @param proxyDeliver
     * @throws WmallException
     */
    @Override
    public void save(ProxyDeliver proxyDeliver) {
    	Assert.notNull(proxyDeliver, "保存数据为空");
    	try {
			proxyDeliverMapper.insert(proxyDeliver);
		} catch (Exception e) {
			throw new WmallException(ErrorCodeEnum.DB_INSERT_ERROR, "【" + proxyDeliver.toString() + "】保存失败", e);
		}
    }

    /**
     * 修改数据
     *
     * @param proxyDeliver
     * @throws WmallException
     */
    @Override
    public void update(ProxyDeliver proxyDeliver) {
    	Assert.notNull(proxyDeliver, "修改数据为空");
    	try {
    		proxyDeliverMapper.update(proxyDeliver);
		} catch (Exception e) {
			throw new WmallException(ErrorCodeEnum.DB_UPDATE_ERROR, "【" + proxyDeliver.toString() + "】修改失败", e);
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
    public ProxyDeliver getProxyDeliver(Map<String, Object> map) {
    	Assert.notEmpty(map, "查询数据为空");
    	try {
	    	return proxyDeliverMapper.getProxyDeliver(map);
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
    public List<ProxyDeliver> listProxyDeliver(Map<String, Object> map) {
   	 	Assert.notEmpty(map, "查询数据为空");
    	try {
	    	return proxyDeliverMapper.listProxyDeliver(map);
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
    public void batchSave(List<ProxyDeliver> list) {
    	Assert.notEmpty(list, "批量保存数据为空");
    	try {
			List<List<ProxyDeliver>> pageList = ListPageUtils.listPage(list, 1000);
			for (List<ProxyDeliver> page : pageList) {
				proxyDeliverMapper.batchInsert(page);
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
    public void batchUpdate(List<ProxyDeliver> list) {
    	Assert.notEmpty(list, "批量修改数据为空");
    	try {
			List<List<ProxyDeliver>> pageList = ListPageUtils.listPage(list, 1000);
			for (List<ProxyDeliver> page : pageList) {
				proxyDeliverMapper.batchUpdate(page);
			}
		} catch (Exception e) {
			throw new WmallException(ErrorCodeEnum.DB_BATCH_ERROR, "批量修改失败", e);
		}
    }
    
}
