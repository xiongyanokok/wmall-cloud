package com.xy.wmall.mapper;

import java.util.List;
import java.util.Map;

import com.xy.wmall.model.ServiceFree;

/**
 * Mapper
 * 
 * @author admin
 * @date 2018年02月07日 下午02:27:32
 */
public interface ServiceFreeMapper extends BaseMapper<ServiceFree> {

    /**
     * 用户服务有效期
     * 
     * @param map
     * @return
     */
    List<ServiceFree> listServiceDate(Map<String, Object> map);
    
}
