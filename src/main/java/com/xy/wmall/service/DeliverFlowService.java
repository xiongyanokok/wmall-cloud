package com.xy.wmall.service;

import java.util.List;
import java.util.Map;

import com.xy.wmall.model.DeliverFlow;

/**
 * Service 接口
 * 
 * @author admin
 * @date 2018年01月28日 上午11:34:02
 */
public interface DeliverFlowService {

    /**
     * 保存数据
     *
     * @param deliverFlow
     */
    void save(DeliverFlow deliverFlow);

    /**
     * 修改数据
     *
     * @param deliverFlow
     */
    void update(DeliverFlow deliverFlow);
    
    /**
     * 根据ID查询
     *
     * @param id
     * @return
     */
    DeliverFlow getDeliverFlowById(Integer id);
    
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
    void batchSave(List<DeliverFlow> list);
    
    /**
     * 批量更新
     * 
     * @param list
     */
    void batchUpdate(List<DeliverFlow> list);
    
}
