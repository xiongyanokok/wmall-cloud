package com.xy.wmall.model;

import java.io.Serializable;
import java.util.Date;

import lombok.Data;

/**
 * model 实体类
 * 
 * @author admin
 * @date 2018年01月30日 下午02:31:50
 */
@Data
public class Menu implements Serializable {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	
	/**
     * 主键
     */
	private Integer id;
	
	/**
     * 父菜单ID
     */
	private Integer parentId;
	
	/**
     * 菜单排序
     */
	private Integer order;
	
	/**
     * 菜单名称
     */
	private String name;
	
	/**
     * 菜单图标
     */
	private String icon;
	
	/**
     * 菜单地址
     */
	private String uri;
	
	/**
     * 创建人ID
     */
	private Integer createUserId;
	
	/**
     * 创建时间
     */
	private Date createTime;
	
	/**
     * 修改人ID
     */
	private Integer updateUserId;
	
	/**
     * 修改时间
     */
	private Date updateTime;
	
	/**
     * 删除：1删除，0未删除
     */
	private Boolean isDelete;
	
}
