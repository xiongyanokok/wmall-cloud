package com.xy.wmall.service;

import java.util.Date;
import java.util.List;
import java.util.Map;

import com.xy.wmall.model.ServiceFree;

/**
 * Service 接口
 * 
 * @author admin
 * @date 2018年02月07日 下午02:27:32
 */
public interface ServiceFreeService extends BaseService<ServiceFree> {

    /**
     * 用户服务有效期
     * 
     * @param userIds
     * @return
     */
    Map<Integer, Date> listServiceDate(List<Integer> userIds);
    
}
