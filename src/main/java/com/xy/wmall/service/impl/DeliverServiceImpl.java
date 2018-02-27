package com.xy.wmall.service.impl;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.xy.wmall.common.Assert;
import com.xy.wmall.enums.ErrorCodeEnum;
import com.xy.wmall.enums.FlowStatusEnum;
import com.xy.wmall.exception.WmallException;
import com.xy.wmall.mapper.DeliverDetailMapper;
import com.xy.wmall.mapper.DeliverFlowMapper;
import com.xy.wmall.mapper.DeliverMapper;
import com.xy.wmall.model.Deliver;
import com.xy.wmall.model.DeliverDetail;
import com.xy.wmall.model.DeliverFlow;
import com.xy.wmall.pojo.Statistics;
import com.xy.wmall.service.DeliverService;

/**
 * Service 实现
 * 
 * @author admin
 * @date 2017年10月28日 上午08:53:59
 */
@Service
public class DeliverServiceImpl extends BaseServiceImpl<DeliverMapper, Deliver> implements DeliverService {

    @Autowired
	private DeliverMapper deliverMapper;
    
    @Autowired
    private DeliverDetailMapper deliverDetailMapper;
    
    @Autowired
    private DeliverFlowMapper deliverFlowMapper;
	
	/**
     * 保存数据
     *
     * @param deliver
     * @throws WmallException
     */
    @Override
    public void save(Deliver deliver) {
    	Assert.notNull(deliver, "保存数据为空");
    	try {
    		// 保存发货信息
			deliverMapper.insert(deliver);
			
			// 保存发货流程
			DeliverFlow deliverFlow = new DeliverFlow();
			deliverFlow.setDeliverId(deliver.getId());
			deliverFlow.setProxyId(deliver.getProxyId());
			deliverFlow.setParentProxyId(deliver.getParentProxyId());
			deliverFlow.setFlowStatus(FlowStatusEnum.START.getValue());
			deliverFlow.setCreateTime(new Date());
			deliverFlowMapper.insert(deliverFlow);
			
			// 产品id
			Integer[] productId = deliver.getProductId();
			// 数量
			Integer[] amount = deliver.getAmount();
			// 保存发货详情信息
	    	List<DeliverDetail> deliverDetails = new ArrayList<>(productId.length);
			for (int i=0; i<productId.length; i++) {
				DeliverDetail deliverDetail = new DeliverDetail();
				deliverDetail.setDeliverId(deliver.getId());
				deliverDetail.setProductId(productId[i]);
				deliverDetail.setAmount(amount[i]);
				deliverDetails.add(deliverDetail);
			}
			deliverDetailMapper.batchInsert(deliverDetails);
		} catch (Exception e) {
			throw new WmallException(ErrorCodeEnum.DB_INSERT_ERROR, "【" + deliver.toString() + "】保存失败", e);
		}
    }

    /**
     * 修改数据
     *
     * @param deliver
     * @throws WmallException
     */
    @Override
    public void update(Deliver deliver) {
    	Assert.notNull(deliver, "修改数据为空");
    	try {
    		// 修改发货单
    		deliverMapper.update(deliver);
    		
    		// 删除发货单详情
    		deliverDetailMapper.deleteByDeliverId(deliver.getId());
    		
    		// 产品id
			Integer[] productId = deliver.getProductId();
			// 数量
			Integer[] amount = deliver.getAmount();
			// 保存发货详情信息
	    	List<DeliverDetail> deliverDetails = new ArrayList<>(productId.length);
			for (int i=0; i<productId.length; i++) {
				DeliverDetail deliverDetail = new DeliverDetail();
				deliverDetail.setDeliverId(deliver.getId());
				deliverDetail.setProductId(productId[i]);
				deliverDetail.setAmount(amount[i]);
				deliverDetails.add(deliverDetail);
			}
			deliverDetailMapper.batchInsert(deliverDetails);
		} catch (Exception e) {
			throw new WmallException(ErrorCodeEnum.DB_UPDATE_ERROR, "【" + deliver.toString() + "】修改失败", e);
		}
    }
    
    /**
     * 发货状态
     * 
     * @param deliver
     */
    @Override
    public void deliverStatus(Deliver deliver) {
    	Assert.notNull(deliver, "修改数据为空");
    	try {
    		deliverMapper.update(deliver);
		} catch (Exception e) {
			throw new WmallException(ErrorCodeEnum.DB_UPDATE_ERROR, "【" + deliver.toString() + "】发货失败", e);
		}
    }
    
    /**
     * 发货统计
     * 
     * @param map
     * @return
     */
    @Override
    public List<Statistics> deliverStatistics(Map<String, Object> map) {
    	Assert.notEmpty(map, "查询数据为空");
    	try {
	    	return deliverMapper.deliverStatistics(map);
		} catch (Exception e) {
			throw new WmallException(ErrorCodeEnum.DB_SELECT_ERROR, "【" + map + "】查询统计失败", e);
		}
    }
    
    /**
     * 待发货数量
     * 
     * @param parentProxyId
     * @return
     */
    @Override
    public Integer countWaitDeliver(Integer proxyId) {
    	try {
	    	return deliverMapper.countWaitDeliver(proxyId);
		} catch (Exception e) {
			throw new WmallException(ErrorCodeEnum.DB_SELECT_ERROR, "【" + proxyId + "】查询待发货失败", e);
		}
    }
    
}
