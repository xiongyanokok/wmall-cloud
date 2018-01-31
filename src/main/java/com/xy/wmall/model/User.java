package com.xy.wmall.model;

import java.io.Serializable;
import java.util.Date;

import lombok.Data;

/**
 * model 实体类
 * 
 * @author admin
 * @date 2018年01月29日 下午02:43:39
 */
@Data
public class User implements Serializable {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	
	/**
     * 主键
     */
	private Integer id;
	
	/**
     * 用户名
     */
	private String username;
	
	/**
     * 密码
     */
	private String password;
	
	/**
     * 是否禁用：1是，0否
     */
	private Boolean disabled;
	
	/**
     * 创建时间
     */
	private Date createTime;
	
	/**
     * 修改时间
     */
	private Date updateTime;
	
	/**
     * 删除：1删除，0未删除
     */
	private Boolean isDelete;
	
	
	/**
	 * 代理ID
	 */
	private Integer proxyId;
	
	/**
	 * 角色
	 */
	private String role;
	
}
