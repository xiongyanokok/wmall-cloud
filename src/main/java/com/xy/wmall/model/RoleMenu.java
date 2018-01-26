package com.xy.wmall.model;

import java.io.Serializable;

import lombok.Data;

/**
 * model 实体类
 * 
 * @author admin
 * @date 2018年01月26日 下午02:19:17
 */
@Data
public class RoleMenu implements Serializable {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	
	/**
     * 主键
     */
	private Integer id;
	
	/**
     * 角色ID
     */
	private Integer roleId;
	
	/**
     * 菜单ID
     */
	private Integer menuId;
	
}
