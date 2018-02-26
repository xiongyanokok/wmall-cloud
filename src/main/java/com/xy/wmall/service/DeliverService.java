package com.xy.wmall.service;

import java.util.List;
import java.util.Map;

import com.xy.wmall.model.Deliver;
import com.xy.wmall.pojo.Statistics;

/**
 * Service 接口
 * 
 * @author admin
 * @date 2017年10月28日 上午08:53:59
 */
public interface DeliverService {

    /**
     * 保存数据
     *
     * @param deliver
     */
    void save(Deliver deliver);

    /**
     * 修改数据
     *
     * @param deliver
     */
    void update(Deliver deliver);
    
    /**
     * 删除数据
     * 
     * @param deliver
     */
    void remove(Deliver deliver);
    
    /**
     * 根据ID查询
     *
     * @param id
     * @return
     */
    Deliver getDeliverById(Integer id);
    
    /**
     * 根据map查询
     * 
     * @param map
     * @return
     */
    Deliver getDeliver(Map<String, Object> map);
    
    /**
     * 根据map查询
     * 
     * @param map
     * @return
     */
    List<Deliver> listDeliver(Map<String, Object> map);
    
    /**
     * 批量保存
     * 
     * @param list
     */
    void batchSave(List<Deliver> list);
    
    /**
     * 批量更新
     * 
     * @param list
     */
    void batchUpdate(List<Deliver> list);
    
    /**
     * 发货状态
     * 
     * @param deliver
     */
    void deliverStatus(Deliver deliver);
    
    /**
     * 发货统计
     * 
     * @param map
     * @return
     */
    List<Statistics> deliverStatistics(Map<String, Object> map);
    
    /**
     * 待发货数量
     * 
     * @param proxyId
     * @return
     */
    Integer countWaitDeliver(Integer proxyId);
    
}
