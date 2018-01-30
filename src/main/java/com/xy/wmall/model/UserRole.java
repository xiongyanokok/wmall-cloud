package com.xy.wmall.model;

import java.io.Serializable;

import lombok.Data;

/**
 * model 实体类
 * 
 * @author admin
 * @date 2018年01月30日 下午02:32:23
 */
@Data
public class UserRole implements Serializable {

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
     * 角色ID
     */
	private Integer roleId;
	
}
