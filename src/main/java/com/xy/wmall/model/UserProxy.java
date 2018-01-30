package com.xy.wmall.model;

import java.io.Serializable;

import lombok.Data;

/**
 * model 实体类
 * 
 * @author admin
 * @date 2018年01月30日 下午03:45:09
 */
@Data
public class UserProxy implements Serializable {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	
	/**
     * 主键
     */
	private Integer id;
	
	/**
     * 用户ID
     */
	private Integer userId;
	
	/**
     * 代理ID
     */
	private Integer proxyId;
	
}
