package com.xy.wmall.mapper;

import java.util.List;
import java.util.Map;

import com.xy.wmall.model.VerifyCode;

/**
 * Mapper
 * 
 * @author admin
 * @date 2018年01月29日 下午02:43:45
 */
public interface VerifyCodeMapper {

	/**
     * 根据主键查询
     *
     * @param id
     * @return
     */
    VerifyCode selectByPrimaryKey(Integer id);

    /**
     * 新增数据库记录
     *
     * @param verifyCode
     */
    void insert(VerifyCode verifyCode);

    /**
     * 更新数据库记录
     *
     * @param verifyCode
     */
    void update(VerifyCode verifyCode);

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
    void batchInsert(List<VerifyCode> list);
    
    /**
     * 批量更新
     * 
     * @param list
     */
    void batchUpdate(List<VerifyCode> list);
    
}
