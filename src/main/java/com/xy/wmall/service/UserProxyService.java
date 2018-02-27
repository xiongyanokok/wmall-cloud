package com.xy.wmall.service;

import java.util.List;
import java.util.Map;

import com.xy.wmall.model.UserProxy;

/**
 * Service 接口
 * 
 * @author admin
 * @date 2018年01月30日 下午02:32:17
 */
public interface UserProxyService extends BaseService<UserProxy> {

    /**
     * 查询代理用户
     * 
     * @param proxyIds
     * @return
     */
    Map<Integer, Integer> listUserByProxy(List<Integer> proxyIds);
    
}
