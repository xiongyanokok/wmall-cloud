package com.xy.wmall.mapper;

import java.util.List;
import java.util.Map;

import com.xy.wmall.model.DeliverFlow;

/**
 * Mapper
 * 
 * @author admin
 * @date 2018年01月26日 下午02:19:05
 */
public interface DeliverFlowMapper {

    /**
     * 新增数据库记录
     *
     * @param deliverFlow
     */
    void insert(DeliverFlow deliverFlow);

    /**
     * 更新数据库记录
     *
     * @param deliverFlow
     */
    void update(DeliverFlow deliverFlow);

    /**
     * 根据map查询
     * 
     * @param map
     * @return
     */
    DeliverFlow getDeliverFlow(Map<String, Object> map);
    
    /**
     * 根据map查询
     * 
     * @param map
     * @return
     */
    List<DeliverFlow> listDeliverFlow(Map<String, Object> map);
    
    /**
     * 批量保存
     * 
     * @param list
     */
    void batchInsert(List<DeliverFlow> list);
    
    /**
     * 批量更新
     * 
     * @param list
     */
    void batchUpdate(List<DeliverFlow> list);
    
}
