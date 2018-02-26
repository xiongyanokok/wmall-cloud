package com.xy.wmall.service;

import java.util.List;
import java.util.Map;

import com.xy.wmall.model.VerifyCode;

/**
 * Service 接口
 * 
 * @author admin
 * @date 2018年01月29日 下午02:43:45
 */
public interface VerifyCodeService {

    /**
     * 保存数据
     *
     * @param verifyCode
     */
    void save(VerifyCode verifyCode);

    /**
     * 修改数据
     *
     * @param verifyCode
     */
    void update(VerifyCode verifyCode);
    
    /**
     * 删除数据
     * 
     * @param verifyCode
     */
    void remove(VerifyCode verifyCode);
    
    /**
     * 根据ID查询
     *
     * @param id
     * @return
     */
    VerifyCode getVerifyCodeById(Integer id);
    
    /**
     * 根据map查询
     * 
     * @param map
     * @return
     */
    VerifyCode getVerifyCode(Map<String, Object> map);
    
    /**
     * 根据map查询
     * 
     * @param map
     * @return
     */
    List<VerifyCode> listVerifyCode(Map<String, Object> map);
    
    /**
     * 批量保存
     * 
     * @param list
     */
    void batchSave(List<VerifyCode> list);
    
    /**
     * 批量更新
     * 
     * @param list
     */
    void batchUpdate(List<VerifyCode> list);
    
}
