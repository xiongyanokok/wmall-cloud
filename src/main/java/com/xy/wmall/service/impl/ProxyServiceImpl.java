package com.xy.wmall.service.impl;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.xy.wmall.common.Assert;
import com.xy.wmall.common.utils.DateUtils;
import com.xy.wmall.enums.ErrorCodeEnum;
import com.xy.wmall.enums.FlowStatusEnum;
import com.xy.wmall.enums.OrderStatusEnum;
import com.xy.wmall.enums.OrderTypeEnum;
import com.xy.wmall.enums.TrueFalseStatusEnum;
import com.xy.wmall.exception.WmallException;
import com.xy.wmall.mapper.DeliverDetailMapper;
import com.xy.wmall.mapper.DeliverFlowMapper;
import com.xy.wmall.mapper.DeliverMapper;
import com.xy.wmall.mapper.OrderDetailMapper;
import com.xy.wmall.mapper.OrderMapper;
import com.xy.wmall.mapper.ProxyMapper;
import com.xy.wmall.model.Deliver;
import com.xy.wmall.model.DeliverDetail;
import com.xy.wmall.model.DeliverFlow;
import com.xy.wmall.model.Order;
import com.xy.wmall.model.OrderDetail;
import com.xy.wmall.model.Proxy;
import com.xy.wmall.service.ProxyService;

/**
 * Service 实现
 * 
 * @author admin
 * @date 2017年10月28日 上午08:54:21
 */
@Service
public class ProxyServiceImpl extends BaseServiceImpl<ProxyMapper, Proxy> implements ProxyService {

    @Autowired
	private ProxyMapper proxyMapper;
    
    @Autowired
    private OrderMapper orderMapper;
    
    @Autowired
    private OrderDetailMapper orderDetailMapper;
    
    @Autowired
    private DeliverMapper deliverMapper;
    
    @Autowired
    private DeliverDetailMapper deliverDetailMapper;
    
    @Autowired
    private DeliverFlowMapper deliverFlowMapper;
	
	/**
     * 保存数据
     *
     * @param proxy
     * @throws WmallException
     */
    @Override
    public void save(Proxy proxy) {
    	Assert.notNull(proxy, "保存数据为空");
    	try {
    		// 保存代理基本信息
			proxyMapper.insert(proxy);

			// 老代理，不下单，不发货
			if (proxy.getIsOldProxy()) {
				return;
			}
		    
			// 保存订单信息
	    	Order order = new Order();
			order.setProxyId(proxy.getId());
			order.setParentProxyId(proxy.getParentId());
			order.setOrderType(OrderTypeEnum.PROXY_ORDER.getValue());
			order.setOrderPrice(proxy.getProxyPrice());
			order.setPreferentialPrice(0);
			order.setIsAccumulate(TrueFalseStatusEnum.FALSE.getValue());
			order.setNatureMonth(DateUtils.natureMonth());
			order.setOrderStatus(OrderStatusEnum.ORDER_SUCCESS.getValue());
			order.setCreateUserId(proxy.getCreateUserId());
			order.setCreateTime(new Date());
			order.setUpdateUserId(proxy.getCreateUserId());
			order.setUpdateTime(new Date());
			order.setIsDelete(TrueFalseStatusEnum.FALSE.getValue());
			orderMapper.insert(order);
			
			// 产品id
			Integer[] productId = proxy.getProductId();
			// 数量
			Integer[] amount = proxy.getAmount();
			// 单价
		    BigDecimal[] unitPrice = proxy.getUnitPrice();
		    // 总价
		    Integer[] totalPrice = proxy.getTotalPrice();
			// 保存订单详情信息
	    	List<OrderDetail> orderDetails = new ArrayList<>(productId.length);
			for (int i=0; i<productId.length; i++) {
				OrderDetail orderDetail = new OrderDetail();
				orderDetail.setOrderId(order.getId());
				orderDetail.setProductId(productId[i]);
				orderDetail.setAmount(amount[i]);
				orderDetail.setUnitPrice(unitPrice[i]);
				orderDetail.setTotalPrice(totalPrice[i]);
				orderDetail.setGive(0);
				orderDetails.add(orderDetail);
			}
			orderDetailMapper.batchInsert(orderDetails);
			
			// 是否发货
			if (!proxy.getIsDeliver()) {
				return;
			}
			
			// 保存发货单信息
			Deliver deliver = new Deliver();
			deliver.setReceiveName(proxy.getName());
			deliver.setReceivePhone(proxy.getPhone());
			deliver.setReceiveAddress(proxy.getAddress());
			deliver.setCourierPrice(proxy.getCourierPrice());
			deliver.setDeliverStatus(TrueFalseStatusEnum.FALSE.getValue());
			deliver.setCreateUserId(proxy.getCreateUserId());
			deliver.setCreateTime(new Date());
			deliver.setUpdateUserId(proxy.getCreateUserId());
			deliver.setUpdateTime(new Date());
			deliver.setIsDelete(TrueFalseStatusEnum.FALSE.getValue());
			deliverMapper.insert(deliver);
			
			// 保存发货流程
			DeliverFlow deliverFlow = new DeliverFlow();
			deliverFlow.setDeliverId(deliver.getId());
			deliverFlow.setProxyId(proxy.getId());
			deliverFlow.setParentProxyId(proxy.getParentId());
			deliverFlow.setFlowStatus(FlowStatusEnum.START.getValue());
			deliverFlow.setCreateTime(new Date());
			deliverFlowMapper.insert(deliverFlow);
			
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
			throw new WmallException(ErrorCodeEnum.DB_INSERT_ERROR, "【" + proxy.toString() + "】保存失败", e);
		}
    }

    /**
     * 根据map查询
     * 
     * @param map
     * @return
     */
    @Override
    public Proxy getUserProxy(Map<String, Object> map) {
    	Assert.notEmpty(map, "查询数据为空");
    	try {
	    	return proxyMapper.getUserProxy(map);
		} catch (Exception e) {
			throw new WmallException(ErrorCodeEnum.DB_SELECT_ERROR, "【" + map + "】查询对象失败", e);
		}
    }
    
}
