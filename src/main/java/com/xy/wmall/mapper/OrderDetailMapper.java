package com.xy.wmall.mapper;

import java.util.List;
import java.util.Map;

import com.xy.wmall.model.OrderDetail;

/**
 * Mapper
 * 
 * @author admin
 * @date 2017年10月28日 上午08:54:14
 */
public interface OrderDetailMapper {

    /**
     * 新增数据库记录
     *
     * @param orderDetail
     */
    void insert(OrderDetail orderDetail);

    /**
     * 更新数据库记录
     *
     * @param orderDetail
     */
    void update(OrderDetail orderDetail);
    
    /**
     * 根据map查询
     * 
     * @param map
     * @return
     */
    OrderDetail getOrderDetail(Map<String, Object> map);
    
    /**
     * 根据map查询
     * 
     * @param map
     * @return
     */
    List<OrderDetail> listOrderDetail(Map<String, Object> map);
    
    /**
     * 批量保存
     * 
     * @param list
     */
    void batchInsert(List<OrderDetail> list);
    
    /**
     * 批量更新
     * 
     * @param list
     */
    void batchUpdate(List<OrderDetail> list);
    
    /**
     * 删除数据库记录
     *
     * @param orderId
     */
    void deleteByOrderId(Integer orderId);
    
}
