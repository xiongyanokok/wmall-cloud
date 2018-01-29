package com.xy.wmall.model;

import java.io.Serializable;
import java.util.Date;

import lombok.Data;

/**
 * model 实体类
 * 
 * @author admin
 * @date 2018年01月29日 下午09:50:33
 */
@Data
public class DeliverFlow implements Serializable {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	
	/**
     * 主键
     */
	private Integer id;
	
	/**
     * 发货单ID
     */
	private Integer deliverId;
	
	/**
     * 代理ID
     */
	private Integer proxyId;
	
	/**
     * 上级代理ID
     */
	private Integer parentProxyId;
	
	/**
     * 流程状态：1开始，2处理，3上报，4完成
     */
	private Integer flowStatus;
	
	/**
     * 创建时间
     */
	private Date createTime;
	
}
