package com.xy.wmall.model;

import java.io.Serializable;
import java.util.Date;

import lombok.Data;

/**
 * model 实体类
 * 
 * @author admin
 * @date 2018年01月28日 上午11:34:02
 */
@Data
public class ProxyDeliver implements Serializable {

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
     * 创建时间
     */
	private Date createTime;
	
}
