package com.xy.wmall.model;

import java.io.Serializable;
import java.util.Date;

import lombok.Data;

/**
 * model 实体类
 * 
 * @author admin
 * @date 2018年01月30日 下午02:49:36
 */
@Data
public class VerifyCode implements Serializable {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	
	/**
     * 主键
     */
	private Integer id;
	
	/**
     * 代理ID
     */
	private Integer proxyId;
	
	/**
     * 验证码
     */
	private String code;
	
	/**
     * 创建人ID
     */
	private Integer createUserId;
	
	/**
     * 创建时间
     */
	private Date createTime;
	
	/**
     * 有效时间
     */
	private Date effectiveTime;
	
	/**
     * 使用状态：0未使用，1已使用
     */
	private Boolean useStatus;
	
}
