package com.xy.wmall.model;

import java.io.Serializable;

import lombok.Data;

/**
 * model 实体类
 * 
 * @author admin
 * @date 2018年01月26日 下午02:19:05
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
	
}
