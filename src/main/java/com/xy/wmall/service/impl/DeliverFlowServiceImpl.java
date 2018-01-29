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
import com.xy.wmall.mapper.DeliverFlowMapper;
import com.xy.wmall.model.DeliverFlow;
import com.xy.wmall.service.DeliverFlowService;
import com.xy.wmall.common.utils.ListPageUtils;

/**
 * Service 实现
 * 
 * @author admin
 * @date 2018年01月28日 上午11:34:02
 */
@Service
public class DeliverFlowServiceImpl implements DeliverFlowService {

    @Autowired
	private DeliverFlowMapper deliverFlowMapper;
	
	/**
     * 根据主键查询
     *
     * @param id
     * @return
     * @throws WmallException
     */
    @Override
    public DeliverFlow selectByPrimaryKey(Integer id) {
    	Assert.notNull(id, "id为空");
    	try {
	    	return deliverFlowMapper.selectByPrimaryKey(id);
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
    public DeliverFlow getDeliverFlowById(Integer id) {
    	Assert.notNull(id, "id为空");
    	try {
    		Map<String, Object> map = new HashMap<>(2);
    		map.put("id", id);
    		map.put("isDelete", TrueFalseStatusEnum.FALSE.getValue());
	    	return deliverFlowMapper.getDeliverFlow(map);
		} catch (Exception e) {
			throw new WmallException(ErrorCodeEnum.DB_SELECT_ERROR, "【" + id + "】查询失败", e);
		}
    }
    
	/**
     * 保存数据
     *
     * @param deliverFlow
     * @throws WmallException
     */
    @Override
    public void save(DeliverFlow deliverFlow) {
    	Assert.notNull(deliverFlow, "保存数据为空");
    	try {
			deliverFlowMapper.insert(deliverFlow);
		} catch (Exception e) {
			throw new WmallException(ErrorCodeEnum.DB_INSERT_ERROR, "【" + deliverFlow.toString() + "】保存失败", e);
		}
    }

    /**
     * 修改数据
     *
     * @param deliverFlow
     * @throws WmallException
     */
    @Override
    public void update(DeliverFlow deliverFlow) {
    	Assert.notNull(deliverFlow, "修改数据为空");
    	try {
    		deliverFlowMapper.update(deliverFlow);
		} catch (Exception e) {
			throw new WmallException(ErrorCodeEnum.DB_UPDATE_ERROR, "【" + deliverFlow.toString() + "】修改失败", e);
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
    public DeliverFlow getDeliverFlow(Map<String, Object> map) {
    	Assert.notEmpty(map, "查询数据为空");
    	try {
	    	return deliverFlowMapper.getDeliverFlow(map);
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
    public List<DeliverFlow> listDeliverFlow(Map<String, Object> map) {
   	 	Assert.notEmpty(map, "查询数据为空");
    	try {
	    	return deliverFlowMapper.listDeliverFlow(map);
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
    public void batchSave(List<DeliverFlow> list) {
    	Assert.notEmpty(list, "批量保存数据为空");
    	try {
			List<List<DeliverFlow>> pageList = ListPageUtils.listPage(list, 1000);
			for (List<DeliverFlow> page : pageList) {
				deliverFlowMapper.batchInsert(page);
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
    public void batchUpdate(List<DeliverFlow> list) {
    	Assert.notEmpty(list, "批量修改数据为空");
    	try {
			List<List<DeliverFlow>> pageList = ListPageUtils.listPage(list, 1000);
			for (List<DeliverFlow> page : pageList) {
				deliverFlowMapper.batchUpdate(page);
			}
		} catch (Exception e) {
			throw new WmallException(ErrorCodeEnum.DB_BATCH_ERROR, "批量修改失败", e);
		}
    }
    
}
